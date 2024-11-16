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
- The configuration data includes
  - AI provider specific configuration, per AI provider
  - Path to the `dev` directory, where project context and design
    documents are stored
- Provide validation for the configuration format
- Support environment variable overrides for sensitive data, such as API keys

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
