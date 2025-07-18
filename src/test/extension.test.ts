import * as assert from 'assert';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
import { applyCorrections, calculateCorrections } from '../correctionDiffing';
import { OllamaLLM } from '../ollamaIntegration';
// import * as myExtension from '../../extension';

suite('Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	test('Word diffing', () => {

		const original = "\n  This is a tes of wrds dffng.";
		const corrected = " \n  This is a test of words diffing.";
		const corrections = calculateCorrections(original, corrected);
		assert.strictEqual(corrections.length, 4, corrections.map(c => c.toInsert).join(", "));
		const applied = applyCorrections(original, corrections);
		assert.strictEqual(applied, corrected);

	});
	test('Ollama function calling', async function() {
		this.timeout(10000);
		const ollamaLLM = await OllamaLLM.create("llama3.2:3b");
		assert.ok(ollamaLLM, 'OllamaLLM instance should be created');
		const resp = await ollamaLLM.sendRequest([
			new vscode.LanguageModelChatMessage(vscode.LanguageModelChatMessageRole.User, 'Proofread the following message. Report all grammar errors step by step.'),
			new vscode.LanguageModelChatMessage(vscode.LanguageModelChatMessageRole.User, 'I likes green apples. The sky ist blue.')
		], {
			tools: [
				{
					description: 'Report a hard grammar error. Call this if you find something that is clearly wrong.',
					name: 'reportGrammarError',
					inputSchema: {
						type: 'object',
						properties: {
							section: {
								type: 'string',
								description: 'The part of the input text that contains the error. This argument must be an exect substring of the input text.',
							},
							explanation: {
								type: 'string',
								description: 'A short explanation of the error.',
							},
							correction: {
								type: 'string',
								description: 'A correction to replace the incorrect text with.',
							}
						},
						required: ['section']
					}
				}
			]
		});
		for await (const message of resp.stream) {
			if (message instanceof vscode.LanguageModelTextPart) {
				console.log(`Text part: ${message.value}`);
			} else if (message instanceof vscode.LanguageModelToolCallPart) {
				console.log(`Tool call: ${message.name} with args: ${JSON.stringify(message)}`);
			}
		}
	});
});
