# ADR: Error Handling System

## Status
Accepted

## Context
- Need consistent error handling across system
- Errors should be data-driven
- Must support condition system
- Should be debuggable
- Need to maintain error context

## Decision
We will:
- Use ex-info for all system exceptions
- Use the ex-info message for the human readable description of the error.
- Define standard error map structure:

```clojure
  {:type    :error/type
   :context {:keyword?  "any value"
             :component "dado.project-config"}
   :cause   original-exception}
```

  - Define standard error types:
  - :error/configuration
  - :error/ai-provider
  - :error/storage
  - :error/template
  - :error/context
  - :error/thread
  - :error/session
  - Support condition system handlers
- Maintain error chain for debugging
- Include component identity in context

## Consequences
### Positive
- Consistent error handling
- Rich error context
- Support for condition system
- Good debugging support

### Negative
- Must maintain error type consistency
- Need to handle error translation at boundaries
- Must manage error context size
