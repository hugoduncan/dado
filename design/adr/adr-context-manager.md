# ADR: AI Context Manager

## Status
Accepted

## Context
- System and project prompts must be managed
- Document selection for context needed
- Template management required
- Context switches should be explicit
- Templates should be composable

## Decision
We will:
- Create AI Context Manager for prompt and template handling
- Support composable templates and prompts
- Include document selection capabilities
- Require explicit context switching
- Handle template composition within manager
- Store templates and prompts in EDN format
- Support hierarchical template composition

## Consequences
### Positive
- Centralized prompt and template management
- Support for template composition
- Clear context switching control
- Flexible template system

### Negative
- Must handle complex template composition
- Need to manage context size limitations
- Must ensure template composition performance