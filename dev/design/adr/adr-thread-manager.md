# ADR: AI Message Thread Manager

## Status
Accepted

## Context
- Conversation history must be maintained
- Previous threads need to be resumable
- Persistent storage required
- Must integrate with AI provider interface
- Thread operations should be synchronous

## Decision
We will:
- Create dedicated Thread Manager component
- Store threads as EDN files
- Support thread operations (create, resume, modify)
- Integrate with Storage Port for persistence
- Use synchronous operations
- Maintain thread metadata separately from content

## Consequences
### Positive
- Clear thread lifecycle management
- Persistent conversation history
- Support for thread operations
- Simple synchronous operation model

### Negative
- Must handle large thread histories efficiently
- Need to manage thread storage growth
- Must maintain thread format compatibility