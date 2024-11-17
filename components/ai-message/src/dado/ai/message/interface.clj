(ns dado.ai.message.interface
  (:require [dado.ai.message.core :as core]
            [dado.ai.message.model :as model]))

(defn create-message
  "Creates a new message with the specified role and content.
   Optionally accepts a name for the message sender.
   Returns a validated message map.
   Throws if role or content are invalid."
  [role content & {:keys [name]}]
  (core/create-message role content :name name))

(defn create-message-thread
  "Creates a new message thread with the specified model and optional system prompt.
   Returns a validated message thread map with a unique ID and creation timestamp."
  [model & {:keys [system-prompt]}]
  (core/create-message-thread model :system-prompt system-prompt))

(defn update-system-prompt
  "Updates the system prompt for a message thread.
   Returns updated message thread with new system prompt.
   Throws if message thread is invalid."
  [message-thread system-prompt]
  (core/update-system-prompt message-thread system-prompt))

(defn add-message
  "Adds a message to a message thread. The message must have :role and :content keys.
   Returns updated message thread with message appended.
   Throws if message thread or message is invalid."
  [message-thread message]
  (core/add-message message-thread message))

(defn add-context-file
  "Adds a file's content to the message thread context.
   File must exist and be readable.
   Returns updated message thread with file content added to context.
   Throws if file does not exist or message thread is invalid."
  [message-thread file-path]
  (core/add-context-file message-thread file-path))

(defn add-response
  "Adds an AI response message to the message thread.
   Response must be a valid response message with usage statistics.
   Returns updated message thread with response appended.
   Throws if message thread or response is invalid."
  [message-thread response]
  (core/add-response message-thread response))

(defn extract-file-blocks
  "Extracts file blocks from an AI response message.
   Returns sequence of file block maps containing language, name and content.
   Throws if response is invalid."
  [response]
  (core/extract-file-blocks response))

(defn extract-diff-blocks
  "Extracts diff blocks from an AI response message.
   Returns string containing all extracted diffs.
   Throws if response is invalid."
  [response]
  (core/extract-diff-blocks response))

;; Expose validators for use by other components
(def message? model/message?)
(def message-thread? model/message-thread?)
(def response-message? model/response-message?)
(def file-block? model/file-block?)

;; Expose schemas for other components
(def message-schema model/Message)
(def message-thread-schema model/MessageThread)
(def response-message-schema model/ResponseMessage)
(def file-block-schema model/FileBlock)
