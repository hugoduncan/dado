# ADR: System Prompt Component

## Status
Proposed

## Context
- Need to compose system prompts for AI interactions
- Prompt templates should be overridable by projects
- Default templates provided by dado in resources
- Project templates stored in `ai/prompts` sub-directory of the project
  `dev` directory (as specified in the project config)
- Templates should be composable
- Component exposes prompt construction function
- Prompt construction requires list of template names
- Prompt construction requires data map for template substitution

## Decision
We will create a System Prompt component that:
- Reads prompt templates with fallback behavior:
  1. First tries project's dev `ai/prompts` directory
  2. If not found, falls back to dado's `dev/ai/prompts` resource path
- Templates are stored as markdown files
- Uses the selmer library for templating
- Exposes `construct-prompt` function:
  - Takes a vector of template names
  - Takes a data map for value substitution
  - Returns the constructed prompt string
- Performs template composition and substitution
- Templates use selmer syntax for substitution
- Uses trace! logging on template reading and prompt construction:
  - Log template search attempts in both locations
  - Log which location template was found in
- Throws exceptions for missing templates or data
- The implementation must use the `dado.ai.prompt` namespace
- The implementation must use the `ai-prompt` component name

The `construct-prompt` function will:
1. For each template name:
   a. Try to load from project directory
   b. If not found, try to load from dado resources
   c. If not found in either location, throw error
2. Parse all loaded templates
3. Compose templates in specified order
4. Render final composed template using selmer with provided data map
5. Return the final prompt string

Template composition rules:
- Templates are composed in specified order
- Project templates take precedence over dado templates
- Only one version of a template is loaded (first found)

Substitution rules:
- selmer syntax is used for substitution
- All values are converted to strings before substitution
- Missing keys are an error
- The data map is a map from keywords to arbitrary values

Error conditions:
- Template not found in either location
- Missing substitution key in data map
- Malformed template file

## Consequences

### Positive
- Clear separation of prompt construction logic
- Supports flexible prompt composition
- Externalizes prompt content for easy editing
- Simplifies prompt construction for callers
- Allows projects to override default templates
- Provides default templates out of the box

### Negative
- Requires managing template files in two locations
- Must handle missing files and malformed templates
- Need to maintain default template set
- Must document template override behavior

### Neutral
- May need to add default values for substitutions
- Projects need to know available template names

## Error Types
- :error/template-not-found
- :error/missing-data
- :error/malformed-template

## Event Taxonomy
- :prompt/template-search-project
- :prompt/template-search-resource
- :prompt/template-found
- :prompt/template-parsed
- :prompt/substitution-made
- :prompt/prompt-constructed

## Validation
- All specified templates must exist in at least one location
- Data map must contain all required substitution keys
- Templates must be valid selmer templates
- Final prompt should not contain any `{{key}}` sections

## Metrics
- Use `t/trace!` to capture metrics on:
  - Template search attempts
  - Template locations found
  - Overall prompt substitution and composition time
