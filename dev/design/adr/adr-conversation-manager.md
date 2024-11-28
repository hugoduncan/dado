# ADR: AI Conversation Manager

## Status
Proposed

## Context
- Need to maintain in-memory conversation state
- Conversations encapsulate message threads and execution context
- Must provide unique conversation identity via message thread ID
- Must integrate with AI Message interface for thread validation
- No persistent storage required - all state is transient

## Decision
We will:
- Create dedicated Conversation Manager component that manages conversation state in memory
- Store in memory a map of conversation-id to conversation
- Each conversation contains:
  ```clojure
  (def Conversation
    [:map
     [:message-thread :message/thread]  ; Schema from AI Message component
     [:agent :agent/config]            ; Schema from AI Agent component
     [:ai-port [:enum :claude :ollama :chatgpt]] ; Supported AI ports
     [:ai-tools [:vector :tool/config]]  ; Schema from AI Tool component
     [:user-data map?]])               ; Arbitrary user-specific data

  (def ConversationUpdate
    [:map
     [:id string?]
     [:tools {:optional true} [:vector :tool/config]]
     [:user-data {:optional true} map?]])
  ```
- Expose core operations:
  1. `register-new` - Takes a conversation, adds it to the in-memory store
  2. `register-update` - Updates existing conversation, errors if ID unknown
  3. `remove` - Removes conversation by ID, errors if ID unknown
  4. `lookup` - Returns conversation by ID, errors if ID unknown
  5. `list` - Returns sequence of registered conversation IDs
  6. `update-tools` - Updates tools for a conversation
- Use AI Message component validators for thread validation
- Use AI Agent component validators for agent validation
- Use AI Tool component validators for tool validation
- Use synchronous operations only
- Conversation ID is taken from the message thread ID

### Implementation Requirements
- Pre-conditions:
  - All operations except register-new and list require valid conversation ID
  - All message threads must pass AI Message thread validation
  - All agents must pass AI Agent validation
  - All tools must pass AI Tool validation
- Error conditions:
  - :error/unknown-conversation-id - Conversation ID not found
  - :error/invalid-conversation - Conversation fails validation
  - :error/invalid-tools - Tools fail validation

### Interface
```clojure
(register-new [conversation]
  "Adds the conversation to the in-memory store.
   Returns the conversation unchanged.
   Throws :error/invalid-conversation if validation fails.")

(register-update [conversation]
  "Updates stored conversation.
   Returns the conversation unchanged.
   Throws :error/unknown-conversation-id if id not found.
   Throws :error/invalid-conversation if validation fails.")

(remove [id]
  "Removes conversation with given id.
   Returns nil.
   Throws :error/unknown-conversation-id if id not found.")

(lookup [id]
  "Returns conversation with given id.
   Throws :error/unknown-conversation-id if id not found.")

(list []
  "Returns sequence of registered conversation IDs.
   Returns empty sequence if no conversations registered.")

(update-tools [id tools]
  "Updates tools for conversation with given id.
   Returns updated conversation.
   Throws :error/unknown-conversation-id if id not found.
   Throws :error/invalid-tools if tools fail validation.")
```

## Event Taxonomy
- :conversation/registered
- :conversation/updated
- :conversation/removed
- :conversation/tools-updated

## Consequences
### Positive
- Clear conversation lifecycle management
- Simple, focused interface
- Strong validation guarantees
- No persistence complexity
- Predictable synchronous operations
- Encapsulates execution context with message thread
- Easy to test
- Flexible user data storage

### Negative
- Must restart conversations after system restart
- Memory usage grows with number of conversations
- Need careful cleanup of unused conversations
- All operations must maintain conversation validity

### Neutral
- Message thread validation delegated to AI Message component
- Conversation ID derived from message thread ID
- User data contents unrestricted

## Related ADRs
- ADR: AI Message - Provides message thread validation
- ADR: AI Agent - Provides agent validation
- ADR: AI Tool - Provides tool validation# ADR: AI Message Thread Manager
