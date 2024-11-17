# ADR: System Prompt Component

## Status
Proposed

## Context
- Need to compose system prompts for AI interactions
- Prompt templates stored in `ai/prompts` sub-directory of the project
  `dev` directory (as specified in the project config)
- Templates should be composable
- Component exposes prompt construction function
- Prompt construction requires list of template names
- Prompt construction requires data map for template substitution

## Decision
We will create a System Prompt component that:
- Reads prompt templates from the dev `ai/prompts` directory
- Templates are stored as markdown files
- Exposes `construct-prompt` function:
  - Takes a vector of template names
  - Takes a data map for value substitution
  - Returns the constructed prompt string
- Performs template composition and substitution
- Templates use `{{key}}` syntax for substitution
- Uses trace! logging on template reading and prompt construction
- Throws exceptions for missing templates or data
- The implement must use the `dado.ai.prompt` namespace
- The implement must use the `ai-prompt` component name

The `construct-prompt` function will:
1. Read and parse all specified templates
2. Compose templates in specified order
3. Substitute values from data map into final composed template
4. Return the final prompt string

Template composition rules:
- Templates are composed in specified order

Substitution rules:
- `{{key}}` syntax is used for substitution points
- All values are converted to strings before substitution
- Missing keys are an error
- The data map is a map from keywords to arbitrary values

Error conditions:
- Missing template file
- Missing substitution key in data map
- Malformed template file

## Consequences

### Positive
- Clear separation of prompt construction logic
- Supports flexible prompt composition
- Externalizes prompt content for easy editing
- Simplifies prompt construction for callers

### Negative
- Requires managing template files
- Must handle missing files and malformed templates

### Neutral
- May need to add default values for substitutions

## Error Types
- :error/missing-template
- :error/missing-data
- :error/malformed-template

## Event Taxonomy
- :prompt/template-read
- :prompt/template-parsed
- :prompt/substitution-made
- :prompt/prompt-constructed

## Validation
- All specified templates must exist
- Data map must contain all required substitution keys
- Templates must be valid EDN
- Final prompt should not contain any `{{key}}` sections

## Metrics
- Use `t/trace!` to capture metrics on overall prompt substitution and composition
