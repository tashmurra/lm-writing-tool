// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import { disconnect } from 'process';
import { json } from 'stream/consumers';
import * as vscode from 'vscode';
import * as Diff from 'diff';
import { getLineCol, calculateCorrections } from './correctionDiffing';
import { OllamaLLM } from './ollamaIntegration';
import { Task, TaskScheduler } from './taskScheduler';

type TextSnippet = {
	text: string;
	range: vscode.Range;
};

function splitTextByLine(text: string): TextSnippet[] {
	const snippets: TextSnippet[] = [];
	const lines = text.split('\n');
	let line = 0;
	for (const l of lines) {
		const range = new vscode.Range(line, 0, line, l.length);
		snippets.push({ text: l, range });
		line++;
	}
	return snippets;
}

type TextSnippetDiagnostic = {
	correctedVersion?: string;
	suggestedImprovements?: { explanation: string, improvedVersion: string }[];
}
type LocatedTextSnippetDiagnostic = {
	snippet: TextSnippet;
	diagnostic: TextSnippetDiagnostic;
}

type LocatedCorrection = {
	range: vscode.Range;
	toInsert: string;
}

type DocumentAnalysis = {
	snippetDiagnostics: Map<TextSnippet, TextSnippetDiagnostic>;
	document: vscode.TextDocument;
}

class LMWritingTool {
	lm: vscode.LanguageModelChat;
	diagnosticsCache: Map<string, TextSnippetDiagnostic>;
	textSplitterFunction: (text: string) => TextSnippet[];
	numRequests: number;
	dc: vscode.DiagnosticCollection;
	corrections: Map<string, LocatedCorrection[]>;
	taskScheduler: TaskScheduler;
	//lmCallback: (document: vscode.TextDocument) => void;

	/**
	 * 
	 * @param lm A language model to use for proofreading
	 * @param textSplitterFunction A function that splits the text into snippets
	 * @param dc A diagnostic collection to store the diagnostics
	 * @param lmCallback A callback function that is called when a new language model request is completed. Passes the document that the request was made for. Can be used to update the diagnostics.
	 */
	constructor(lm: vscode.LanguageModelChat, textSplitterFunction = splitTextByLine, dc: vscode.DiagnosticCollection) {

		this.lm = lm;
		this.diagnosticsCache = new Map();
		this.textSplitterFunction = textSplitterFunction;
		this.numRequests = 0;
		this.dc = dc;
		this.corrections = new Map();
		this.taskScheduler = new TaskScheduler(1);
		//this.lmCallback = lmCallback;
	}

	async getTextSnippetDiagnostic(text: string, token: vscode.CancellationToken): Promise<TextSnippetDiagnostic> {
		const response = await this.lm.sendRequest([
			vscode.LanguageModelChatMessage.User(`Proofread the following message in American English. If it is gramatically correct, just respond with the word "Correct". If it is gramatically incorrect or has spelling mistakes, respond with "Correction: ", followed by the corrected version. If you make a correction, write the whole corrected text, not just the segments with corrections. Do not add additional text or explanations. Do not change special commands, code, escape characters, or mathematical formulas. Only correct grammatical issues, do not change the content:\n${text}`)
		],{},token);
		this.numRequests++;
		let resp = '';
		for await (const message of response.text) {
			resp += message;
		}
		if(token.isCancellationRequested){
			console.info('Request cancelled at partial response ', resp);
			return {};
		}
		console.info(`Response: ${resp}`);
		if (resp.startsWith('Correction: ')) {
			const correctedVersion = resp.slice('Correction: '.length);
			// const trailingSpacesOfOriginal = text.match(/\s*$/)?.[0] || '';
			// const correctedVersionWithTrailingSpaces = trailingSpacesOfOriginal + correctedVersion.trimStart();
			return { correctedVersion };
		} else if (resp.startsWith('Correct')) {
			return {};
		} else {
			console.warn(`Unexpected response: ${resp}`);
			return {};
		}
	}
	async getSnippetDiagnostics(snippet: string, token: vscode.CancellationToken) {
		if (this.diagnosticsCache.has(snippet)) {
			return this.diagnosticsCache.get(snippet) || {};
		}
		const diagnostic = await this.getTextSnippetDiagnostic(snippet,token);
		this.diagnosticsCache.set(snippet, diagnostic);
	}

	getCachedSnippetDiagnosticsAtLocation(document: vscode.TextDocument, location: vscode.Position): LocatedTextSnippetDiagnostic[] {
		const text = document.getText();
		const snippets = this.textSplitterFunction(text);
		const snippetsAtLocation = snippets.filter(s => s.range.contains(location));

		return snippetsAtLocation.map(s => {
			if (this.diagnosticsCache.has(s.text)) {
				return {
					diagnostic: this.diagnosticsCache.get(s.text),
					snippet: s
				};
			}
			return undefined;
		}).filter(d => d !== undefined) as LocatedTextSnippetDiagnostic[];
	}
	sendLLMRequestsForDocument(document: vscode.TextDocument){
		const text = document.getText();
		const snippets = this.textSplitterFunction(text);
		const snippetTexts = [...new Set(snippets.map(s => s.text))];
		const currentlyActiveDocument = vscode.window.activeTextEditor?.document.uri.toString();
		for(const [id,t] of this.taskScheduler.pendingTasks){
			if(t.group !== currentlyActiveDocument){
				t.priority = 0;
			}
		}
		const tasks = new Map<string,Task>();
		const lmwt = this;
		
		for(const snippet of snippets){
			let priority = 0;
			if(currentlyActiveDocument === document.uri.toString()){
				priority = 1;
				const currentPosition= vscode.window.activeTextEditor?.selection.active;
				if(currentPosition && snippet.range.contains(currentPosition)){
					priority += 1;
				}
				const visibleRange = vscode.window.activeTextEditor?.visibleRanges?.[0];
				if(visibleRange && snippet.range.intersection(visibleRange)){
					priority += 1;
				}
			}
			if(!this.diagnosticsCache.has(snippet.text)){
				const cancellationToken = new vscode.CancellationTokenSource();
				tasks.set(snippet.text,{
					async run() {
						console.info(`Requesting diagnostics for ${snippet.text}`);
						await lmwt.getSnippetDiagnostics(snippet.text,cancellationToken.token);
						lmwt.checkDocument(document);
					},
					async abort() {
						console.info(`Aborting diagnostics request for ${snippet.text}`);
						cancellationToken.cancel();
					},
					priority: priority,
					group: document.uri.toString(),
					timeoutSeconds: 40
				});
			}
		}
		console.info(`updating tasks for ${document.uri.toString()}`);
		this.taskScheduler.setTasks(tasks, document.uri.toString());
		this.checkDocument(document);
	}

	checkDocument(document: vscode.TextDocument): vscode.Diagnostic[] {
		const text = document.getText();
		const snippets = splitTextByLine(text);
		const snippetTexts = [...new Set(snippets.map(s => s.text))];
		//await Promise.all(snippetTexts.map((ts) => this.getSnippetDiagnostics(ts)));
		const newCorrections: LocatedCorrection[] = [];
		const diagnostics: vscode.Diagnostic[] = [];
		for (const snippet of snippets) {
			const diagnostic = this.diagnosticsCache.get(snippet.text);
			if (diagnostic?.correctedVersion) {
				const corrections = calculateCorrections(snippet.text, diagnostic.correctedVersion);
				for (const correction of corrections) {
					const { line: startLineRelative, col: startColRelative } = getLineCol(snippet.text, correction.start);
					const { line: endLineRelative, col: endColRelative } = getLineCol(snippet.text, correction.end);
					const start = snippet.range.start.translate(startLineRelative, startColRelative);
					const end = snippet.range.start.translate(endLineRelative, endColRelative);
					const range = new vscode.Range(start, end);
					const text = correction.toInsert === "" ? "Remove" : `Change to: ${correction.toInsert}`;
					const diagnostic = new vscode.Diagnostic(range, text, vscode.DiagnosticSeverity.Information);
					newCorrections.push({ range, toInsert: correction.toInsert });
					diagnostic.source = 'LM Writing Tool';
					diagnostics.push(diagnostic);
				}
			}
		}
		this.dc.set(document.uri, diagnostics);
		this.corrections.set(document.uri.toString(), newCorrections);
		return diagnostics;
	}

	getDiagnostics(document: vscode.TextDocument, range: vscode.Range): vscode.Diagnostic[] {
		const diagnosticsInDocument = this.dc.get(document.uri);
		if (!diagnosticsInDocument) {
			return [];
		}
		const diagnostics = diagnosticsInDocument.filter(d => d.range.intersection(range));
		return diagnostics;
	}
	getCorrections(document: vscode.TextDocument, range: vscode.Range): LocatedCorrection[] {
		const corrections = this.corrections.get(document.uri.toString());
		if (!corrections) {
			return [];
		}
		const correctionsInRange = corrections.filter(c => c.range.intersection(range));
		return correctionsInRange;
	}
}

class WritingToolCodeActionsProvider implements vscode.CodeActionProvider {

	writingTool: LMWritingTool;
	constructor(writingTool: LMWritingTool) {
		this.writingTool = writingTool;
	}

	provideCodeActions(document: vscode.TextDocument, range: vscode.Range | vscode.Selection, context: vscode.CodeActionContext, token: vscode.CancellationToken): vscode.ProviderResult<(vscode.CodeAction | vscode.Command)[]> {
		//throw new Error('Method not implemented.');
		const actions: vscode.CodeAction[] = [];
		const diagnostics = this.writingTool.getCachedSnippetDiagnosticsAtLocation(document, range.start);
		const changesInRange = this.writingTool.getCorrections(document, range);
		for (const change of changesInRange) {
			const a = new vscode.CodeAction(change.toInsert === "" ? "Remove" : `Change to: ${change.toInsert}`, vscode.CodeActionKind.QuickFix);
			const edit = new vscode.WorkspaceEdit();
			edit.replace(document.uri, change.range, change.toInsert);
			a.edit = edit;
			actions.push(a);
		}
		for (const diagnostic of diagnostics) {
			if (diagnostic.diagnostic.correctedVersion) {
				const a = new vscode.CodeAction(`Correct to: ${diagnostic.diagnostic.correctedVersion}`, vscode.CodeActionKind.QuickFix);
				const edit = new vscode.WorkspaceEdit();
				edit.replace(document.uri, diagnostic.snippet.range, diagnostic.diagnostic.correctedVersion);
				a.edit = edit;
				actions.push(a);
			}
		}
		return actions;
	}
	resolveCodeAction?(codeAction: vscode.CodeAction, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CodeAction> {
		throw new Error('Method not implemented.');
	}
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "lm-writing-tool" is now active!');

	let _lmwt: LMWritingTool | undefined;
	async function getLMWT() {
		if (!_lmwt) {
			const models = await vscode.lm.selectChatModels({
				vendor: 'copilot',
				family: 'gpt-4o-mini'
			});
			models.push(new OllamaLLM());
			if (models.length === 0) {
				throw new Error("No models found.");
			}
			let model = models[0];
			if (models.length > 1) {
				function getQuickPickItem(m: vscode.LanguageModelChat) {
					return `${m.vendor}: ${m.family} ${m.version}`;
				}
				const response = await vscode.window.showQuickPick(models.map(getQuickPickItem), {
					placeHolder: "Select model"
				});
				const matchingModel = models.find(m => getQuickPickItem(m) === response);
				if (!matchingModel) {
					throw new Error("No model selected.");
				}
				model = matchingModel;
			}
			_lmwt = new LMWritingTool(model, splitTextByLine, dc);
		}
		return _lmwt;
	}
	const dc = vscode.languages.createDiagnosticCollection();
	context.subscriptions.push(dc);

	context.subscriptions.push(
		vscode.commands.registerTextEditorCommand('lm-writing-tool.checkCurrentDocument', async (te) => {
			const lmwt = await getLMWT();
			const openTextDocument = te.document;
			console.info('Checking document');
			const text = openTextDocument.getText();
			const diagnostics = await lmwt.checkDocument(openTextDocument);
			dc.set(openTextDocument.uri, diagnostics);
			console.info(`Number of requests: ${lmwt.numRequests}`);
		})
	);


	const textCheckJobs = new Map<vscode.TextEditor, NodeJS.Timeout>();

	context.subscriptions.push(
		vscode.commands.registerTextEditorCommand('lm-writing-tool.startTextCheckCurrentDocument', async (te) => {
			const lmwt = await getLMWT();
			const openTextDocument = te.document;
			textCheckJobs.set(te, setInterval(async () => {
				console.info('Checking document');
				lmwt.sendLLMRequestsForDocument(openTextDocument);
			}, 5000));

			context.subscriptions.push(vscode.languages.registerCodeActionsProvider('*', new WritingToolCodeActionsProvider(lmwt), {}));
		})
	);



	context.subscriptions.push(
		vscode.commands.registerTextEditorCommand('lm-writing-tool.stopTextCheckCurrentDocument', async (te) => {
			const interval = textCheckJobs.get(te);
			if (interval) {
				clearInterval(interval);
				textCheckJobs.delete(te);
			}
		})
	);
	// Cleanup the interval on extension deactivation
	context.subscriptions.push({
		dispose: () => {
			for (const interval of textCheckJobs.values()) {
				clearInterval(interval);
			}
		},
	});
}

// This method is called when your extension is deactivated
export function deactivate() { }
