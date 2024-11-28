ADR: Conversation Action

# ADR: Conversation Action

## Status
Proposed - Introduces new action pattern for UI interactions

## Context
- Need interface for UI/REPL/CLI/Emacs to interact with AI conversations
- Async responses required as clients may be single-threaded and AI responses take time
- Must coordinate AI Port, Agent, Thread Manager components
- Must manage AI Tool configuration for conversations
- Actions provide high-level interface for common operations

## Decision
We will create a Conversation Action that:

### Component Interface
```clojure
(create-conversation
  [port-id agent-id & [port-config agent-config]]
  "Creates new conversation with specified AI port and agent.
   Returns conversation ID (UUID).
   Throws :error/conversation-creation on failure.")

(send-message
  [conversation-id input]
  "Sends message and returns response asynchronously.
   Returns core.async channel that will receive response.
   Response format: {:content string? :error (optional) ex-info}
   Only execution errors are returned through channel.
   Other errors thrown immediately.
   Throws :error/unknown-conversation if ID not found.")

(end-conversation
  [conversation-id]
  "Ends conversation and cleans up resources.
   Returns nil.
   Throws :error/unknown-conversation if ID not found.")

(add-ai-tool
  [conversation-id tool]
  "Adds AI Tool to conversation.
   Returns updated conversation state.
   AI Tools passed by value, not ID.
   Throws :error/unknown-conversation if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid.")

(remove-ai-tool
  [conversation-id tool]
  "Removes AI Tool from conversation.
   Returns updated conversation state.
   AI Tools passed by value, not ID.
   Throws :error/unknown-conversation if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid.")
```

### Implementation Requirements
1. Component Coordination:
   - Use Thread Manager for conversation state
   - Use AI Port for message sending
   - Use AI Agent for context/prompt management

2. Asynchronous Operation:
   - Use core.async for non-blocking execution
   - Channel closes after response received
   - Only execution errors sent through channel
   - Other errors thrown immediately
   - Timeout handling deferred to caller

3. Configuration Management:
   - Port and agent configs override defaults
   - AI Tool state stored in conversation metadata

4. Conversation Lifecycle:
   - Creation validates all components
   - Message sending preserves conversation state
   - Ending conversation removes state
   - History access delegated to Thread Manager

### Error Types
- :error/conversation-creation
- :error/unknown-conversation
- :error/invalid-ai-tool
- :error/message-failed

### Event Taxonomy
- :conversation/created
- :conversation/message-sent
- :conversation/ended
- :conversation/tool-added
- :conversation/tool-removed

## Consequences

### Positive
- User-centric interface terminology
- Asynchronous messaging support
- Clear conversation lifecycle
- Component coordination hidden

### Negative
- Must handle async failures
- Need careful error propagation
- Must coordinate multiple components

## Dependencies
- Thread Manager component
- AI Port component
- AI Agent component
- core.async

## Notes
- Uses dado.actions.conversation namespace
- Internal implementation uses message threads
- Conversation IDs are UUIDs from Thread Manager
- History access via Thread Manager
