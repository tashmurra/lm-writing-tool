import ollama, { ChatResponse } from 'ollama';
import * as vscode from 'vscode';

function llmMessageToString(message: vscode.LanguageModelChatMessage): string {
    let str = '';
    for (const part of message.content) {
        if (part instanceof vscode.LanguageModelTextPart) {
            str += part.value;
        }
    }
    return str;
}

export class OllamaLLM implements vscode.LanguageModelChat {
    name: string;
    id: string;
    vendor: string;
    family: string;
    version: string;
    maxInputTokens: number;
    constructor() {
        this.name = 'ollama';
        this.id = 'ollama';
        this.vendor = 'ollama';
        this.family = 'llama';
        this.version = '3.2';
        this.maxInputTokens = 1024;
    }
    async pull() {
        await ollama.pull({
            model: 'llama3.2',
        });
    }

    sendRequest(messages: vscode.LanguageModelChatMessage[], options?: vscode.LanguageModelChatRequestOptions, token?: vscode.CancellationToken): Thenable<vscode.LanguageModelChatResponse> {
        return new Promise(async (resolve, reject) => {
            vscode.LanguageModelChatMessageRole.User;
            const ROLE_TO_STRING = new Map([
                [vscode.LanguageModelChatMessageRole.User, 'user'],
                [vscode.LanguageModelChatMessageRole.Assistant, 'ass'],
            ]);
            const stringMessages = messages.map(message => {
                return {
                    role: ROLE_TO_STRING.get(message.role) || 'user',
                    content: llmMessageToString(message),
                };
            });
            const defaultOptions = {
                temperature: 0,
                top_p: 0.5,
                top_k: 40
            };
            const lmOptions = Object.assign({}, defaultOptions, options);
            try {
                const response = await ollama.chat({
                    model: 'llama3.2',
                    messages: stringMessages,
                    stream: true,
                    options: lmOptions
                });
                const abortPromise = new Promise<void>((resolve, reject) => {
                    token?.onCancellationRequested(() => {
                        resolve();
                    });
                });
                async function* responseTextGenerator() {
                    for await (const chunk of response) {
                        const result = await Promise.race([chunk, abortPromise]);
                        if (result) {
                            yield result.message.content;
                        } else {
                            break;
                        }
                    }
                }
                async function* responseStreamGenerator() {
                    for await (const chunk of response) {
                        const result = await Promise.race([chunk, abortPromise]);
                        if (result) {
                            yield new vscode.LanguageModelTextPart(chunk.message.content);
                        } else {
                            break;
                        }
                    }
                }
                resolve(({
                    text: responseTextGenerator(),
                    stream: responseStreamGenerator(),
                }));
            } catch (error) {
                reject(`Could not reach ollama: ${error}
                    Have you installed and started the ollama server?
                    Find the instructions here: https://ollama.com/`);
                return;
            }
        });
    }
    countTokens(text: string | vscode.LanguageModelChatMessage, token?: vscode.CancellationToken): Thenable<number> {
        return Promise.resolve(text.toString().split(' ').length);
    }

}