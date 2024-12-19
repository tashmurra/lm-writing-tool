// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import { disconnect } from 'process';
import { json } from 'stream/consumers';
import * as vscode from 'vscode';

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

type DocumentAnalysis = {
	snippetDiagnostics: Map<TextSnippet, TextSnippetDiagnostic>;
	document: vscode.TextDocument;
}

class LMWritingTool {
	lm: vscode.LanguageModelChat;
	diagnosticsCache: Map<string, TextSnippetDiagnostic>;
	textSplitterFunction: (text: string) => TextSnippet[];
	numRequests: number;

	constructor(lm: vscode.LanguageModelChat, textSplitterFunction = splitTextByLine) {
		this.lm = lm;
		this.diagnosticsCache = new Map();
		this.textSplitterFunction = textSplitterFunction;
		this.numRequests = 0;
	}

	async getTextSnippetDiagnostic(text: string): Promise<TextSnippetDiagnostic> {
		const response = await this.lm.sendRequest([
			vscode.LanguageModelChatMessage.User(`
				Proofread the following message. If it is gramatically correct, just respond with the word "Correct". If it is gramatically incorrect, respond with "Correction: ", followed by the corrected version, no extra text:
				`),
			vscode.LanguageModelChatMessage.User(text)
		]);
		this.numRequests++;
		let resp = '';
		for await (const message of response.text) {
			resp += message;
		}
		if (resp === 'Correct') {
			return {};
		} else {
			const correctedVersion = resp.slice('Correction: '.length);
			return { correctedVersion };
		}
	}
	async getSnippetDiagnostics(snippet: string): Promise<TextSnippetDiagnostic> {
		if (this.diagnosticsCache.has(snippet)) {
			return this.diagnosticsCache.get(snippet) || {};
		}
		const diagnostic = await this.getTextSnippetDiagnostic(snippet);
		this.diagnosticsCache.set(snippet, diagnostic);
		return diagnostic;
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
	async checkDocument(document: vscode.TextDocument): Promise<vscode.Diagnostic[]> {
		const text = document.getText();
		const snippets = splitTextByLine(text);
		const snippetTexts = [...new Set(snippets.map(s => s.text))];
		await Promise.all(snippetTexts.map((ts) => this.getSnippetDiagnostics(ts)));

		const diagnostics: vscode.Diagnostic[] = [];
		for (const snippet of snippets) {
			const diagnostic = this.diagnosticsCache.get(snippet.text);
			if (diagnostic?.correctedVersion) {
				diagnostics.push({
					code: '',
					message: `Suggested correction: ${diagnostic.correctedVersion}`,
					range: snippet.range,
					severity: vscode.DiagnosticSeverity.Information,
					source: 'lm-writing-tool',
				});
			}
		}
		return diagnostics;
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
			if (models.length === 0) {
				vscode.window.showErrorMessage('No model available');
				throw new Error('No model available');
			}
			const model = models[0];
			_lmwt = new LMWritingTool(model);
		}
		return _lmwt;
	}
	const dc = vscode.languages.createDiagnosticCollection();
	context.subscriptions.push(dc);

	// Save mapping from text snippets to LM responses
	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	const disposable = vscode.commands.registerTextEditorCommand('lm-writing-tool.checkCurrentDocument', async (te) => {
	});

	context.subscriptions.push(disposable);

	const textCheckJobs = new Map<vscode.TextEditor, NodeJS.Timeout>();

	const startTextCheckDisposable = vscode.commands.registerTextEditorCommand('lm-writing-tool.startTextCheckCurrentDocument', async (te) => {

		const lmwt = await getLMWT();
		const openTextDocument = te.document;
		textCheckJobs.set(te, setInterval(async () => {
			vscode.window.showInformationMessage('Checking text');
			const text = openTextDocument.getText();
			const diagnostics = await lmwt.checkDocument(openTextDocument);
			dc.set(openTextDocument.uri, diagnostics);
		}, 5000));

		context.subscriptions.push(vscode.languages.registerCodeActionsProvider('*', new WritingToolCodeActionsProvider(lmwt), {}));
	});

	context.subscriptions.push(startTextCheckDisposable);


	const stopTextCheckDisposable = vscode.commands.registerTextEditorCommand('lm-writing-tool.stopTextCheckCurrentDocument', async (te) => {
		const interval = textCheckJobs.get(te);
		if (interval) {
			clearInterval(interval);
			textCheckJobs.delete(te);
		}
	});
	context.subscriptions.push(stopTextCheckDisposable);
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
