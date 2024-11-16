(ns org.hugoduncan.dado.ai.thread.interface
  "Interface namespace for the AI conversation thread component.
   Provides functions for managing AI conversation threads, including creation,
   retrieval, message management, and context updates."
  (:require [org.hugoduncan.dado.ai.thread.core :as thread]))

;; Thread Management

(defn create-thread
  "Creates a new thread with given title and optional context.

   Parameters:
     title   - String, the thread title
     options - Optional map with keys:
               :context  - Map with :documents vector and :metadata map
               :metadata - Additional thread metadata

   Returns:
     Thread record with fields:
       :id         - String, format: yyyyMMddHHmmss-slug
       :created-at - java.time.Instant
       :updated-at - java.time.Instant
       :title      - String
       :status     - Keyword, :active or :archived
       :metadata   - Map

   Throws:
     Ex-info with :type :thread/exists if thread ID collision
     Ex-info with :type :thread/invalid-data for invalid inputs"
  [title & {:as options}]
  (thread/create-thread title options))

(defn get-thread
  "Retrieves a thread by ID.

   Parameters:
     thread-id - String, format: yyyyMMddHHmmss-slug

   Returns:
     Thread record if found, nil otherwise"
  [thread-id]
  (thread/get-thread thread-id))

(defn list-threads
  "Lists all threads, sorted by creation timestamp.

   Parameters:
     filter-fn - Optional predicate function to filter threads

   Returns:
     Vector of Thread records, sorted by :created-at"
  ([] (thread/list-threads))
  ([filter-fn] (thread/list-threads filter-fn)))

;; Message Management

(defn add-message
  "Adds a message to an existing thread.

   Parameters:
     thread-id    - String, thread identifier
     role         - Keyword, :human or :assistant
     content      - String, message content
     options      - Optional map with keys:
                    :context-refs - Vector of document references
                    :metadata     - Additional message metadata

   Returns:
     Updated Thread record

   Throws:
     Ex-info with :type :thread/invalid-data for invalid inputs"
  [thread-id role content & {:as options}]
  (thread/add-message thread-id role content options))

(defn get-messages
  "Retrieves all messages for a thread.

   Parameters:
     thread-id - String, thread identifier

   Returns:
     Vector of Message records, each with:
       :id           - UUID
       :timestamp    - java.time.Instant
       :role         - Keyword
       :content      - String
       :context-refs - Vector
       :metadata     - Map"
  [thread-id]
  (thread/get-messages thread-id))

;; Context Management

(defn get-context
  "Retrieves thread context.

   Parameters:
     thread-id - String, thread identifier

   Returns:
     ThreadContext record with:
       :documents - Vector of document references
       :metadata  - Map of context metadata"
  [thread-id]
  (thread/get-context thread-id))

(defn update-context
  "Updates thread context, merging with existing context.

   Parameters:
     thread-id       - String, thread identifier
     context-updates - Map with keys:
                      :documents - Vector of document references
                      :metadata  - Map of context metadata

   Returns:
     Updated Thread record

   Notes:
     - Document references are combined and deduplicated
     - Metadata is merged with existing metadata"
  [thread-id context-updates]
  (thread/update-context thread-id context-updates))

;; Thread Status

(defn archive-thread
  "Archives a thread, preventing further modifications.

   Parameters:
     thread-id - String, thread identifier

   Returns:
     Updated Thread record with :status :archived"
  [thread-id]
  (thread/archive-thread thread-id))

;; Data Specs

(def thread-id-regex
  "Regular expression for valid thread IDs.
   Format: yyyyMMddHHmmss-slug"
  #"\d{14}-[a-z0-9_]+")

(def valid-roles
  "Set of valid message roles."
  #{:human :assistant})

(def valid-statuses
  "Set of valid thread statuses."
  #{:active :archived})
