# ADR: Ollama AI Port

## Status
Proposed

## Context
- Need to implement AI Port interface for Ollama
- Must handle Ollama-specific HTTP operations
- Must convert between common message format and Ollama's API format
- Must support any Ollama model
- No authentication required
- Basic functionality focus - no special features

## Decision
We will:
- Create a dedicated Ollama AI port component implementing the AI Port interface
- Use standard HTTP client for API communication
- Support Ollama-specific configuration:
  - Required `:model` with default of "llama2:3.2"
  - Optional `:api-url` with default "http://localhost:11434"
- Use jsonista for JSON handling in HTTP requests/responses
- Implement message format conversion between AI Message format and
  Ollama API format as explicit functions
- Use promesa for async operations
- Use trace level logging for all API calls
- Provide elapsed-millis metric for each API call
- Validate the input config map, throwing an exception if invalid
- Define schema for Ollama-specific formats

The Ollama request format is:
```clojure
(def OllamaRole
  [:enum \"user\" \"assistant\" \"system\"])

(def OllamaMessage
  [:map
   [:role OllamaRole]
   [:content :string]])

(def OllamaRequest
  [:map
   [:model :string]
   [:messages [:vector OllamaMessage]]])
```

## Validation
- Message thread must conform to AI Message component format
- Ollama configuration must be valid
- Response must conform to AI Message component format
- Ollama-specific formats must conform to defined schemas

## Error Types
- :error/ollama-connection
- :error/ollama-response
- :error/ollama-validation
- :error/ollama-timeout

## Consequences
### Positive
- Clean separation of Ollama-specific implementation
- Consistent interface with other AI providers
- Simple configuration with sensible defaults
- Clear async boundaries
- Proper validation at provider boundaries

### Negative
- Must handle Ollama API changes
- Need to manage Ollama-specific error conditions
- Have to handle HTTP failures properly

## Notes
- API retry logic and rate limiting are out of scope initially
- Tests should use AI message schemas and Ollama-specific schemas for data generation
