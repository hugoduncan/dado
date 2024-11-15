# AI Thread Component Design

## Overview
The thread component provides a filesystem-based thread management system for AI conversations. It handles thread creation, persistence, and retrieval in a provider-agnostic way. Located in the `org.hugoduncan.dado.ai.thread` namespace.

## Thread Storage Format

### Directory Structure
```
dev/
  └── ai/
      └── threads/
          ├── 20241112103000-initial_project_setup/
          │   ├── thread.edn     # Thread metadata
          │   ├── messages.edn   # Message history
          │   └── context.edn    # Thread context
          ├── 20241112145522-refactor_auth_system/
          └── index.edn          # Thread index (optional)
```

### Thread ID Format
`<timestamp>-<slug>`
- timestamp: `yyyyMMddHHmmss` format
- slug: lowercase with underscores, derived from title
- Example: `20241112103000-initial_project_setup`

## Data Models

```clojure
(ns org.hugoduncan.dado.ai.thread.model
  "Data models for AI conversation threads.")

(defrecord Thread
  [id                    ;; String (timestamp-slug)
   created-at           ;; Instant
   updated-at           ;; Instant
   title                ;; String
   status               ;; #{:active :archived}
   metadata             ;; Map of additional attributes])

(defrecord Message
  [id                    ;; UUID
   timestamp            ;; Instant
   role                 ;; Keyword (e.g., :human, :assistant)
   content              ;; String
   context-refs         ;; Vector of document references
   metadata             ;; Map of additional attributes])

(defrecord ThreadContext
  [documents            ;; Vector of document references
   metadata             ;; Map of additional attributes])
```

## Core Functions

```clojure
(ns org.hugoduncan.dado.ai.thread
  "Thread management for AI conversations."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [org.hugoduncan.dado.ai.thread.model :as model]))

;; Thread Management
(defn create-thread
  "Creates a new thread with given title and optional context.
   Returns thread record."
  [title & {:keys [context metadata]}]
  (let [timestamp (format-timestamp (java.time.Instant/now))
        slug (->slug title)
        thread-id (str timestamp "-" slug)]
    ...))

(defn get-thread
  "Retrieves a thread by ID. Returns thread record."
  [thread-id]
  ...)

(defn list-threads
  "Lists all threads, sorted by timestamp.
   Optional filter-fn can be provided."
  ([] ...)
  ([filter-fn] ...))

(defn add-message
  "Adds a message to thread. Returns updated thread."
  [thread-id role content & {:keys [context-refs metadata]}]
  ...)

(defn update-context
  "Updates thread context. Returns updated thread."
  [thread-id context-updates]
  ...)

;; File Operations
(defn- thread-dir
  "Returns thread directory path."
  [thread-id]
  ...)

(defn- save-edn
  "Saves EDN data to file."
  [file-path data]
  ...)

(defn- load-edn
  "Loads EDN data from file."
  [file-path]
  ...)

;; Utility Functions
(defn- format-timestamp
  "Formats instant as yyyyMMddHHmmss"
  [instant]
  ...)

(defn- ->slug
  "Converts title to slug format"
  [title]
  ...)

;; Validation
(defn- validate-thread
  "Validates thread data structure"
  [thread]
  ...)

(defn- validate-message
  "Validates message data structure"
  [message]
  ...)
```

## Error Handling

```clojure
(defn- throw-thread-error
  "Throws formatted ex-info for thread errors"
  [error-type thread-id data]
  (throw (ex-info "Thread operation failed"
                 {:type error-type
                  :thread-id thread-id
                  :data data})))

;; Error types:
;; :thread/not-found
;; :thread/invalid-id
;; :thread/invalid-data
;; :thread/io-error
```

## Implementation Notes

1. File Operations
   - Use EDN for all persistence
   - Atomic writes for data safety
   - File locks for concurrent access
   - Regular fsync for durability

2. Thread ID Generation
   - Use system clock for timestamp
   - Sanitize title for slug creation
   - Handle potential collisions
   - Validate ID format

3. Message Storage
   - Append-only message log
   - Sequential message IDs
   - Transactional updates

4. Context Management
   - Document reference validation
   - Efficient context updates
   - Lazy loading of context data

## Examples

```clojure
;; Creating a new thread
(create-thread 
  "Refactor Authentication System"
  :context {:documents ["adr/auth-system.md"
                       "src/auth/core.clj"]})
;; => #Thread{:id "20241112145522-refactor_auth_system" ...}

;; Adding a message
(add-message 
  "20241112145522-refactor_auth_system"
  :human
  "How should we restructure the auth system?"
  :context-refs ["adr/auth-system.md"])
;; => #Thread{...}

;; Listing threads
(list-threads #(= (:status %) :active))
;; => [#Thread{...} ...]
```

## Integration Considerations

1. File System
   - Handle path separators
   - Manage file permissions
   - Consider case sensitivity
   - Handle special characters

2. Concurrency
   - File locking strategy
   - Atomic operations
   - Race condition prevention

3. Resource Management
   - File handle cleanup
   - Buffer management
   - Memory usage
