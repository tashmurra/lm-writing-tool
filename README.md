# LLM Writing Tool

After Grammarly disabled its API, there is no similar grammar checking tool for VSCode. While [LTeX](https://marketplace.visualstudio.com/items?itemName=valentjn.vscode-ltex) catches spelling mistakes, it does not reach the level of understanding that Grammarly does.

This extension is a simple attempt to fill the gap through large language models (LLM). It chunks the text into paragraphs, asks an LLM to proofread each paragraph, and then highlights the errors. The user can then click on the error to see the suggested correction.

## Features

![LLM-based grammar checking](resources/demo.gif)
- LLM-based grammar checking
- Corrections via quick fixes
- Choose from local llama3.2:3b or gpt-40-mini through [VSCode LM api](https://code.visualstudio.com/api/extension-guides/language-model)

## Commands

- "Start Text Check for Current Document": Continuously checks the text in the current document
- "Stop Text Check for Current Document": Stops checking the text in the current document

## Installation

1. Install the extension from the [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=OlePetersen.lm-writing-tool)
2. Install [Ollama](https://ollama.com/) and pull llama3.2:3b for local grammar checking, or subscribe to GitHub Copilot to use an online model.