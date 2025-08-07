# Change Log

All notable changes to the "lm-writing-tool" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [0.1.6]

### Added

- **Configurable System Prompts**: All system prompts used for proofreading, rewriting, and finding synonyms are now fully configurable through VS Code settings
  - `lmWritingTool.prompts.proofreading` - Customize grammar and spelling check behavior
  - `lmWritingTool.prompts.rewrite` - Customize text rewriting style and approach  
  - `lmWritingTool.prompts.synonyms` - Customize synonym finding behavior
  - Support for placeholder variables (`{text}` and `{expression}`) in custom prompts
  - New command "Reset prompts to defaults" to restore original prompt values
  - Enables customization of language variant (e.g., British vs American English), writing style, and LLM behavior

### Changed

- System prompts are no longer hard-coded and can be customized by users
- Extension now reads prompt configurations from VS Code settings on each LLM request

## [0.1.5] - Previous Release

- Initial release
