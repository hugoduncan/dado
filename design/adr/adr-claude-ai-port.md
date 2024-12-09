# ADR: Claude AI Port

## Status
Accepted

## Context
- Need to implement AI Port interface for Claude
- Must handle Claude-specific HTTP operations
- Must support Claude's authentication and configuration
- Must convert between common message format and Claude's API format

## Decision
We will:
- Create a dedicated Claude AI port component implementing the AI Port interface
- Use standard HTTP client for API communication
- Support Claude-specific configuration:
- Required `:api-key` in config for authentication
- Optional `:api-url` with default value
- Optional `:model-name` with default value
  - Optional `:max-tokens` defaulting to 4096
- Use jsonista for JSON handling in HTTP requests/responses
- Implement message format conversion between AI Message format and
  Claude API format as explicit functions
- the metadata context from the input message thread should be used as
  content for the `:system` key of the claude request.
- Use promesa for async operations
- Use trace level logging for all API calls
- Provide elapsed-millis metric for each API call
- Validate the input config map, throwing an exception if invalid, not
  using preconditions, as this is effectively user input.
- Validate the message thread input using AI Message component validators, as preconditions
- Define schema for Claude-specific formats
- Configuration passed as parameter, not using project-config component directly


- The Claude request format is
```clojure
(def ClaudeRole
  [:enum "user" "assistant"])

(def ClaudeContent
  [:or
   string
   [:map
    [:type [:= "text"]]
    [:text string]]])

(def ClaudeSystemContent
  [:map
   [:type [:= "text"]]
   [:text string]
   [:cache_control {:optional true} [:enum "ephemeral"]]])

(def ClaudeMessage
  [:map
   [:role ClaudeRole]
   [:content ClaudeContent]
   [:name {:optional true} string]])

(def ClaudeRequest
  [:map
   [:model string]
   [:messages [:vector ClaudeMessage]]
   [:max_tokens {:optional true} pos-int?]
   [:system {:optional true} [:or string [:vector ClaudeSystemContent]]]
   [:temperature {:optional true} [:double {:min 0.0 :max 1.0}]]])
```

## Validation
- Message thread must conform to AI Message component format
- Claude configuration must be valid
- Response must conform to AI Message component format
- Claude-specific formats must conform to defined schemas

## Error Types
- :error/claude-connection
- :error/claude-response
- :error/claude-validation
- :error/claude-timeout

## Consequences
### Positive
- Clean separation of Claude-specific implementation
- Consistent interface with other AI providers
- Clear async boundaries
- Proper validation at provider boundaries

### Negative
- Must handle Claude API changes
- Need to manage Claude-specific error conditions
- Have to handle HTTP failures properly

## Notes
- API retry logic and rate limiting are out of scope initially
- Tests should use AI message schemas and Claude-specific schemas for data generation
