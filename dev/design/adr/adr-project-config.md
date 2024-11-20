# ADR: Project Configuration Component

## Status
Accepted

## Context
- Project specific settings need to be managed
- Multiple AI providers must be supported
- Configuration should be version-controlled
- Configuration needs to be separate from AI context
- Configuration must be accessible to all components

## Decision
A dedicated Project Configuration component will:
- Store configuration in EDN format in a `dado.edn` file at the root of the
  project directory
- The project directory is assumed to be the working directory
  - Project directory structure configuration under `:directories` key
  - Standard directory locations:
    - `:dado/prompts` -> "dev/ai/prompts"
    - `:dado/adr` -> "dev/design/adr"
    - `:dado/implementation` -> "dev/design/implementation"
    - `:dado/scope` -> "dev/design/scope"
- All paths in `:directories` must be relative paths
- Directory paths can be customized but must maintain expected structure
- Provide validation for the configuration format
- Support environment variable overrides for sensitive data, such as API keys

Example configuration:
```edn
{:directories
 {:dado/prompts "dev/ai/prompts"
  :dado/adr "dev/design/adr"
  :dado/implementation "dev/design/implementation"
  :dado/scope "dev/design/scope"
  :custom/docs "docs/custom"}
 :ai-providers
 {:claude {:api-key "..."}}
}
```

## Consequences

### Positive
- Clear separation of configuration from AI context
- Project-specific settings easily version controlled
- Configuration accessible to all components
- Validation prevents misconfiguration

### Negative
- Need to handle sensitive data (API keys) carefully
- Must maintain backward compatibility for config format
- Need to manage environment variable interaction
