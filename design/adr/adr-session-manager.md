# ADR: AI Session Manager

## Status
Accepted

## Context
- Central coordination of AI interactions needed
- Must manage state across multiple components
- Direct REPL interaction required
- Must coordinate context and thread management
- Needs to handle synchronous operations cleanly

## Decision
We will:
- Create AI Session Manager as primary coordinator
- Handle session state and command interface
- Coordinate between Thread Manager and Context Manager
- Make directly accessible from REPL
- Use synchronous operations with futures where needed
- Maintain clear session lifecycle

## Consequences
### Positive
- Single point of coordination for AI interactions
- Clear session lifecycle management
- Simplified REPL interaction
- Predictable synchronous operation

### Negative
- Must ensure proper resource cleanup
- Need to handle session state consistently
- Must manage timeouts appropriately