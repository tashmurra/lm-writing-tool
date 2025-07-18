import ollama, { ChatResponse, Ollama ,Tool as OllamaTool} from 'ollama';
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

type UsableModel = 'llama3.2:3b' | 'gemma3n:e2b' | 'deepseek-r1:1.5b';
export class OllamaLLM implements vscode.LanguageModelChat {
    name: string;
    id: string;
    vendor: string;
    family: string;
    version: string;
    maxInputTokens: number;
    model: UsableModel;
    constructor(model: UsableModel = 'llama3.2:3b') {
        const family_map = {
            'llama3.2:3b': 'llama',
            'gemma3n:e2b': 'gemma',
            'deepseek-r1:1.5b': 'deepseek',
        };
        const version_map = {
            'llama3.2:3b': '3.2',
            'gemma3n:e2b': '3.0',
            'deepseek-r1:1.5b': 'r1',
        };
        this.name = 'ollama';
        this.id = 'ollama';
        this.vendor = 'ollama';
        this.family = family_map[model];
        this.version = version_map[model];
        this.maxInputTokens = 1024;
        this.model = model;
    }

    static async create(model_name: UsableModel = 'llama3.2:3b'): Promise<OllamaLLM | undefined> {
        try {
            await ollama.list();
        }catch (error) {
            console.warn('Could not reach ollama server', error);
            vscode.window.showWarningMessage('Error creating OllamaLLM instance: ' + error);
            return;
        }
        const availableModels = await ollama.list();
        if(availableModels.models.filter(model=> model.model === model_name).length !==1){
            const res = await vscode.window.showQuickPick(['pull ' + model_name + ' model (2GB)', 'cancel'], {placeHolder: 'ollama model not found. Do you want to pull it?'});
            if(res === 'pull ' + model_name + ' model (2GB)'){
                await vscode.window.withProgress({
                    location: vscode.ProgressLocation.Notification,
                    title: 'Pulling ' + model_name + ' model (2GB)',
                    cancellable: true
                }, async (progress, token) => {
                    const downloadResp = await ollama.pull({model: model_name, stream: true});
                    token.onCancellationRequested(() => {
                        downloadResp.abort();
                    
                    });
                    let previous = 0;
                    for await (const chunk of downloadResp) {
                        console.log(chunk);
                        progress.report({increment: (chunk.completed-previous)/chunk.total*100});
                        previous = chunk.completed;
                    }
                    progress.report({increment: 100-previous});
                });
                vscode.window.showInformationMessage(model_name + ' model (2GB) pulled successfully.');
            }
            else{
                vscode.window.showInformationMessage('ollama model not found. Please pull it before using it.');
                return;
            }
        }
        return new OllamaLLM(model_name);
    }

    sendRequest(messages: vscode.LanguageModelChatMessage[], options?: vscode.LanguageModelChatRequestOptions, token?: vscode.CancellationToken): Thenable<vscode.LanguageModelChatResponse> {
        return new Promise(async (resolve, reject) => {
            vscode.LanguageModelChatMessageRole.User;
            const ROLE_TO_STRING = new Map([
                [vscode.LanguageModelChatMessageRole.User, 'user'],
                [vscode.LanguageModelChatMessageRole.Assistant, 'assistant'],
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
            const ollamaTools: OllamaTool[] | undefined = options?.tools?.map(tool => ({
                function: {
                    name: tool.name,
                    description: tool.description,
                    parameters: tool.inputSchema,
                    type: "function"
                },
                type: 'function',
            }));

            const lmOptions = Object.assign({}, defaultOptions, options);
            try {
                const response = await ollama.chat({
                    model: this.model,
                    messages: stringMessages,
                    stream: true,
                    options: lmOptions,
                    tools: ollamaTools,
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
                            if (result.message.tool_calls){
                                for (const toolCall of result.message.tool_calls) {
                                    // Generate new tool call id
                                    // console.log(`Tool call: ${toolCall.function.name} with args: ${JSON.stringify(toolCall.function.arguments)}`);
                                    const call_id = Math.random().toString(36).substring(2, 15);
                                    yield new vscode.LanguageModelToolCallPart(call_id, toolCall.function.name, toolCall.function.arguments);
                                }
                            }
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