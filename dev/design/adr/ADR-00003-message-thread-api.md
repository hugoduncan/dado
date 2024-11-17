# 3. Message Thread Management

Date: 2024-11-13
Status: Proposed

## Context

We need a system to manage AI conversation message threads that supports:
- Multiple AI providers (not just Claude)
- Filesystem-based storage and search
- Context management
- Message history tracking
- Thread analysis (by other components)

The message thread management system needs to be independent of specific AI providers while supporting the requirements of modern AI conversation patterns.

## Decision

1. Create three coordinated components:
   - Message Thread Management (`dado.ai.thread`)
   - AI Provider Integration (`dado.ai.claude`)
   - AI Interaction (`dado.ai.interaction`)

2. Core Data Models:
   ```clojure
   (defrecord MessageThread
     [id                ;; String (timestamp-slug)
      created-at       ;; Instant
      updated-at       ;; Instant
      title            ;; String
      status           ;; Keyword #{:active :archived}
      metadata         ;; Map including :system prompt
      context          ;; Map of context data
      messages])       ;; Vector of messages

   (defrecord Message
     [id                ;; UUID
      timestamp        ;; Instant
      role             ;; Keyword #{:human :assistant}
      content          ;; String
      context-refs     ;; Vector of document references
      metadata])       ;; Map of additional attributes
   ```

3. Message Thread Storage Structure:
   ```
   dev/
     └── ai/
         └── threads/
             ├── <timestamp>-<slug>/
             │   ├── thread.edn    # Thread metadata
             │   ├── messages.edn  # Message history
             │   └── context.edn   # Thread context
             └── index.edn         # Thread index
   ```

4. Core Component Responsibilities:
   - Message Thread Management:
     * Thread creation and persistence
     * Message addition
     * Context management
     * Thread retrieval and search

   - AI Provider Integration:
     * Provider API interaction
     * Rate limiting
     * Error handling
     * Response streaming

   - AI Interaction:
     * Coordinates between components
     * Manages system prompts
     * Handles message flow
     * Formats context for providers

5. Key Design Decisions:
   - Use EDN for persistence with custom readers
   - Stateless API interactions (credentials per request)
   - System prompts stored in thread metadata
   - Context documents referenced by path
   - Message roles as keywords
   - Timestamps using java.time.Instant

## Consequences

### Positive
- Clear separation of concerns between components
- EDN persistence enables standard tooling
- Provider-agnostic message format
- Flexible context management
- Stateless design improves reliability
- Rich metadata support
- Explicit system prompt handling

### Negative
- File-based storage may not scale to very large thread counts
- Concurrent writes need careful handling
- Partial loading could complicate some operations
- Provider-agnostic format may not capture all provider-specific features

### Neutral
- Need to establish conventions for thread archival
- Need to coordinate file access across components
- May need to add indexing for advanced search

## Implementation Notes

1. Persistence Implementation:
   ```clojure
   (def edn-readers
     {'dado.ai.interaction.model.MessageThread
