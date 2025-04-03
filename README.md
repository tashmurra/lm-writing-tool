# LLM Writing Tool  

After Grammarly disabled its API, no equivalent grammar-checking tool exists for VSCode. While [LTeX](https://marketplace.visualstudio.com/items?itemName=valentjn.vscode-ltex) catches spelling mistakes and some grammatical errors, it lacks the deeper linguistic understanding that Grammarly provides.  

This extension bridges the gap by leveraging large language models (LLMs). It chunks text into paragraphs, asks an LLM to proofread each paragraph, and highlights potential errors. Users can then click on highlighted errors to view and apply suggested corrections.  

## Features  

![LLM-based grammar checking](resources/demo.gif)  

- **LLM-powered grammar checking** in American English  
- **Inline corrections** via quick fixes  
- **Choice of models**: Use a local `llama3.2:3b` model via [Ollama](https://ollama.com/) or `gpt-40-mini` through the [VSCode LM API](https://code.visualstudio.com/api/extension-guides/language-model)  
- **Rewrite suggestions** to improve clarity  
- **Synonym recommendations** for better word choices  

## Commands  

When the first command is executed, a dialog appears allowing users to select either a local Ollama model or the GitHub Copilot model.  

- **Local Model**: Requires installing and running a local [Ollama server](https://ollama.com/).  
- **Online Model**: Requires a [GitHub Copilot](https://github.com/features/copilot) subscription.  

### Available Commands  

- **"LLM Writing Tool: Start Text Check for Current Document"**  
  Continuously checks the text in the current document. Prompts the user to select an LLM model.  
- **"LLM Writing Tool: Stop Text Check for Current Document"**  
  Stops real-time grammar checking.  
- **"LLM writing tool: Rewrite current selection"**
  Rewrites the selected text for clarity.
- **"LLM writing tool: Get synonyms for selection"**
  Suggests synonyms for the selected expression.
- **"LLM writing tool: Select model"**
  Selects the LLM model to use for grammar checking. Stops real-time grammar checking if it is running.

## Installation  

1. Install the extension from the [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=OlePetersen.lm-writing-tool).  
2. Install [Ollama](https://ollama.com/) and pull `llama3.2:3b` for local grammar checking, or subscribe to GitHub Copilot for online LLM access.  

## How It Works  

1. The extension **splits the text into sections** and sends them to the selected LLM for proofreading.  
2. It then **compares the LLM’s suggestions** with the original text to detect changes.  
3. **Detected errors are highlighted**, and users can apply quick fixes with a click.  
4. **Responses are cached** to minimize repeated API calls.  
5. Every **5 seconds,** the extension checks for text changes and reprocesses modified sections.  

## Roadmap  

- [ ] **On-disk caching** to improve startup times and reduce redundant API requests.  
- [ ] **Smarter text chunking** to ensure uniform section sizes (e.g., ~2 full lines per section instead of splitting by line).  
- [ ] **Support for additional languages**, starting with British English. Future versions may support any language available in the LLM.  
- [ ] **Evaluation of alternative models** for improved results, with prompt adjustments as needed.  

## Contributing  

Contributions are welcome! Feel free to:  

- Open an issue  
- Submit a pull request  
- Contact me directly: [peteole2707@gmail.com](mailto:peteole2707@gmail.com)
