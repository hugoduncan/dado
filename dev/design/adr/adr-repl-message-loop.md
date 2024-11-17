# ADR: REPL Message Loop Component

## Status
Proposed

## Context
- Need interactive REPL-based message loop for AI interactions
- Must integrate with AI Port for message sending
- Must handle message thread state
- Must process diffs from responses via AI Message component
- Must support exact "EXIT" command for termination
- Should follow existing component patterns
- Must coordinate multiple components safely

## Decision
We will:
- Create a dedicated REPL Message Loop component responsible for:
  - Managing interactive message loop
  - Coordinating with AI Message component for thread operations and response processing
  - Sending messages via AI Port
  - Applying patches via Patch component

- Component Interface:
  ```clojure
  (message-loop [message-thread config]
    "Runs interactive message loop starting with given thread.
     Returns final message thread when user enters 'EXIT'.

     Arguments:
     - message-thread: Valid message thread (validated via AI Message component)
     - config: Project configuration map")
  ```

- Use existing components:
  - AI Message component for:
    - Thread operations and validation
    - Extracting diff blocks from responses
  - AI Port for sending messages to Claude
  - Patch component for applying extracted diffs

- Required Operations:
  1. Validate input message thread
  2. Prompt user for message using println/read-line
  3. If message is empty/whitespace-only, return to step 2
  4. If message is exact "EXIT" string, return current message thread
  5. Add user message to thread
  6. Send updated thread to AI Port
  7. Extract diff blocks from response using AI Message component
     - If extraction errors occur, terminate with error
     - If no diffs found, skip to step 9
  8. Apply extracted diffs via Patch component
     - If patch application errors occur, terminate with error
  9. Add response to thread
  10. Return to step 2

- Exit Handling:
  - Only recognize exact "EXIT" string
  - Return current message thread state
  - No timeout is required on the reading of the messsage

- Input Handling:
  - Use standard println/read-line for IO operations
  - Empty or whitespace-only input is ignored, user is re-prompted
  - No input validation beyond checking for "EXIT"
  - No special formatting of displayed responses

- Error Handling:
  - Diff extraction errors terminate loop
  - Patch application errors terminate loop
  - No diffs found skips patch application step
  - Errors from component interactions are wrapped and propagated

- State Management:
  - Message thread maintained only in memory
  - Events should be logged with `t/event!`
  - No persistence requirements

- Validation:
  - Input message thread must be valid (via AI Message component)
  - Config must contain required AI provider settings
  - All component interactions must be validated

## Error Types
- :error/message-loop-validation
- :error/message-loop-io
- :error/message-loop-interaction

## Event Taxonomy
- :message-loop/started
- :message-loop/message-received
- :message-loop/response-processed
- :message-loop/diffs-applied
- :message-loop/exited

## Consequences
### Positive
- Clear coordination of components
- Simple, focused interface
- Explicit exit condition
- Proper validation at boundaries
- Consistent error handling
- Proper use of AI Message component for diff extraction

### Negative
- Must coordinate multiple components
- Need to handle component failures gracefully
- Must maintain message thread state correctly

## Validation
- Message thread must be valid
- Config must contain required fields

## Dependencies
- AI Message component
- AI Port component
- Patch component
- Project Configuration
- Use `dado.repl.message-loop` namespace
