(ns org.hugoduncan.dado.ai.thread.model
  "Data models and EDN reader functions for AI interactions."
  (:require [clojure.instant :as inst]))

(defrecord MessageThreadContext
    [documents metadata])

(defrecord MessageThread
    [id                    ;; String (timestamp-slug)
     created-at           ;; Instant
     updated-at           ;; Instant
     title                ;; String
     status               ;; Keyword #{:active :archived}
     metadata             ;; Map of additional attributes
     context              ;; Map of context data
     messages])           ;; Vector of messages

(defrecord Message
    [id                    ;; UUID
     timestamp            ;; Instant
     role                 ;; Keyword #{:human :assistant}
     content              ;; String
     context-refs         ;; Vector of document references
     metadata])           ;; Map of additional attributes

;; ---- EDN Reader Functions ----

(defn read-message-thread-context
  "EDN reader function for MessageThreadContext records."
  [{:keys [documents metadata]}]
  (map->MessageThreadContext
   {:documents documents
    :metadata  metadata}))

(defn read-message
  "EDN reader function for Message records."
  [{:keys [id timestamp role content context-refs metadata]}]
  (map->Message
   {:id           (java.util.UUID/fromString id)
    :timestamp    (inst/read-instant-timestamp timestamp)
    :role         (keyword role)
    :content      content
    :context-refs (vec context-refs)
    :metadata     metadata}))

(defn read-message-thread
  "EDN reader function for MessageThread records."
  [{:keys [id created-at updated-at title status metadata context messages]}]
  (map->MessageThread
   {:id         id
    :created-at (inst/read-instant-timestamp created-at)
    :updated-at (inst/read-instant-timestamp updated-at)
    :title      title
    :status     (keyword status)
    :metadata   metadata
    :context    context
    :messages   (mapv read-message messages)}))

(def edn-readers
  "Map of EDN reader functions for AI interaction records."
  {'org.hugoduncan.dado.ai.interaction.model.MessageThreadContext
   read-message-thread-context
   'org.hugoduncan.dado.ai.interaction.model.MessageThread
   read-message-thread
   'org.hugoduncan.dado.ai.interaction.model.Message
   read-message})
