# ADR: AI Port

## Status
Accepted

## Context
- Multiple AI providers must be supported
- Need provider-agnostic interface
- Must handle message format conversions
- Must support context files

## Decision
We will:
- Create provider-agnostic AI Port interface with a `send!` function that takes:
  - a provider config map
  - message thread
- Each provider will have its own polylith component implementing this interface
- Providers must:
  - Validate input using AI Message component validators in preconditions
  - Return responses in AI Message component format
  - Convert between common format and provider-specific formats using an
    explicit function for each conversion type.
  - Support context files from message thread metadata
  - Define schema for provider-specific formats
  - Use trace level  and metrics around API calls
- Configuration passed as parameter, not using project-config directly
- Initial implementations:
  - Claude AI Port (see separate ADR)
- API retry logic and rate limiting are out of scope initially
- Tests should use AI message schemas and provider-specific schemas

## Validation
- Message thread must conform to the message thread format provided by
  the AI Message component
- Provider configuration must be valid
- Response must conform to response format provided by the AI Message component

## Error Types
- :error/provider-connection
- :error/provider-response
- :error/provider-validation
- :error/provider-timeout

## Consequences
### Positive
- Easy addition of new providers
- Consistent interface across providers
- Clean abstraction boundary
- Clear validation requirements

### Negative
- Must handle provider-specific features
- Need to manage different provider capabilities
- Have to handle provider-specific errors

## Related ADRs
- ADR: Claude AI Port - Implementation for Claude provider
