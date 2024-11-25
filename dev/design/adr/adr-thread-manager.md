# ADR: AI Message Thread Manager

## Status
Accepted

## Context
- Need to maintain in-memory message thread state
- Message thread creation and content management is external to this component
- Must provide unique message thread identity for referencing message threads
- Must integrate with AI Message interface for message thread validation
- No persistent storage required - all state is transient

## Decision
We will:
- Create dedicated Message Thread Manager component that manages message thread state in memory
- Store in memory a map of message thread-id to message message thread
- Expose five core operations:
  1. `register-new` - Takes a message thread, adds it to the in-memory store
  2. `register-update` - Updates existing message thread, errors if ID unknown
  3. `remove` - Removes message thread by ID, errors if ID unknown
  4. `lookup` - Returns message thread by ID, errors if ID unknown
  5. `list` - Returns sequence of registered message thread IDs
- Use AI Message component validators to ensure message thread validity
- Use synchronous operations only

### Implementation Requirements
- Pre-conditions:
  - All operations except register-new and list require valid message thread ID
  - All message threads must pass AI Message thread validation
- Error conditions:
  - :error/unknown-message-thread-id - Message Thread ID not found
  - :error/invalid-message-thread - Message Thread fails validation

### Interface
```clojure
(register-new [message-thread]
  "Adds the message-thread to the in-memory store.
   Returns the message-thread unchanged.
   Throws :error/invalid-message-thread if validation fails.")

(register-update [message-thread]
  "Updates stored message-thread.
   Returns the message-thread unchanged.
   Throws :error/unknown-message-thread-id if id not found.
   Throws :error/invalid-message-thread if validation fails.")

(remove [id]
  "Removes message-thread with given id.
   Returns nil.
   Throws :error/unknown-message-thread-id if id not found.")

(lookup [id]
  "Returns message message-thread with given id.
   Throws :error/unknown-message-thread-id if id not found.")

(list []
  "Returns sequence of registered message thread IDs.
   Returns empty sequence if no message threads registered.")
```

## Consequences
### Positive
- Clear message thread lifecycle management
- Simple, focused interface
- Strong validation guarantees
- No persistence complexity
- Predictable synchronous operations
- Easy to test

### Negative
- Must restart message threads after system restart
- Memory usage grows with number of message threads
- Need careful cleanup of unused message threads
- All operations must maintain message thread validity

### Neutral
- Validation delegated to AI Message component

## Related ADRs
- ADR: AI Message - Provides message thread validation
