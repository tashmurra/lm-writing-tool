# Change Log

All notable changes to the "lm-writing-tool" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [0.1.7] - 2025-01-08

### Added

- **Configurable Ollama Model**: The Ollama model used for local text processing is now configurable through VS Code settings
  - `lmWritingTool.ollama.model` - Specify which Ollama model to use (default: `llama3.2:3b`)
  - Supports any Ollama model available on your local server
  - Automatic model pulling if the specified model is not found locally
  - New command "Reset all settings to defaults" to restore all extension settings including the Ollama model

### Changed

- Ollama model is no longer hard-coded as `llama3.2:3b` and can be customized by users
- Improved error messages to include guidance about changing the model in settings
- Enhanced constructor to properly handle model family and version parsing

## [0.1.6] - 2025-01-07

### Added - System Prompts

- **Configurable System Prompts**: All system prompts used for proofreading, rewriting, and finding synonyms are now fully configurable through VS Code settings
  - `lmWritingTool.prompts.proofreading` - Customize grammar and spelling check behavior
  - `lmWritingTool.prompts.rewrite` - Customize text rewriting style and approach  
  - `lmWritingTool.prompts.synonyms` - Customize synonym finding behavior
  - Support for placeholder variables (`{text}` and `{expression}`) in custom prompts
  - New command "Reset prompts to defaults" to restore original prompt values
  - Enables customization of language variant (e.g., British vs American English), writing style, and LLM behavior

### Changed - Prompt System

- System prompts are no longer hard-coded and can be customized by users
- Extension now reads prompt configurations from VS Code settings on each LLM request

## [0.1.5] - Previous Release

- Initial release
