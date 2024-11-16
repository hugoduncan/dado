# ADR: AI Port

## Status
Accepted

## Context
- Multiple AI providers must be supported.
- Initial support for Claude required
- Must handle HTTP operations cleanly

## Decision
We will:
- Create provider-agnostic AI Port interface.  This must provide a send!
  function that takes the project-config and a message thread as
  arguments.
- Each provider will have its own polylith component, all of which will
  have compatible interfaces.  The component is responsible for all
  provider specific features.
- Each provider should validate its input arguments as preconditions
  using the validators provider in the AI message component.
- Each provider should return values in the format specified in the AI
  message component.  These should be validated as post conditions with
  the AI response validator from the AI message component.
- each provider should have explicit functions to convert message thread
  and response formats to and from the AI provider specific formats.
- the provider ports must support context files from the message thread
  metadata.
- The provider must define schema for the provider specific formats,
  and these are to be used in post-conditions.
- Use jsonista for formatting http request bodies and parsing json
  responses.
- Create a Claude adapter using standard HTTP client.
- Handle provider-specific authentication, probably using the `:api-key`
  from the `config` argument.
- The Claude config may contain an optional `:api-url`, which
  should have a default value if unspecified.
- The Claude config may contain an optional `:model-name`, which should
  have a default value if unspecified.  This does not require validation
  as a valid model name, so that new models can be used without having
  to change the code.
- The Claude config may contain an optional `:max-tokens`, which should
  have a default value of 4096 if unspecified.  This must be set on the request.
- Use futures for async operations
- Implement proper request/response lifecycle
- Each implementation will use trace level logging for all api calls
- Each implementation will provide a elapsed-millis metric for each api call.
- configuration is passed as a parameter, do not directly use the
  project-config component.

- API retry logic, and rate limiting, are out of scope for now.
- tests should use the AI message schemas and the provider specific
  schemas to generate data.

## Validation
Validation is carried out via the AI message component interface functions.
- Message thread must conform to AI Message component format
- Provider configuration must be valid
- Response must conform to AI Message component format

## Error Types
- :error/provider-connection
- :error/provider-response
- :error/provider-validation
- :error/provider-timeout

## Consequences
### Positive
- Easy addition of new providers
- Consistent interface across providers
- Clean handling of HTTP operations
- Clear async boundaries

### Negative
- Must handle provider-specific features
- Need to manage different provider capabilities
- Have to handle HTTP failures properly
