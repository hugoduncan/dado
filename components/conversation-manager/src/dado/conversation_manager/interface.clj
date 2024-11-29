(ns dado.conversation-manager.interface
  "Manages message thread lifecycle and state.
   Provides in-memory storage of message threads.
   Supports tool registration per conversation."
  (:require [dado.conversation-manager.core :as core]))

(defn register-new
  "Adds the message-thread to the in-memory store.
   Returns the message-thread unchanged.
   Throws :error/invalid-message-thread if validation fails."
  [message-thread]
  (core/register-new message-thread))

(defn register-update
  "Updates stored message-thread.
   Returns the message-thread unchanged.
   Throws :error/unknown-message-thread-id if id not found.
   Throws :error/invalid-message-thread if validation fails."
  [message-thread]
  (core/register-update message-thread))

(defn remove
  "Removes message-thread with given id.
   Returns nil.
   Throws :error/unknown-message-thread-id if id not found."
  [id]
  (core/remove id))

(defn lookup
  "Returns message message-thread with given id.
   Throws :error/unknown-message-thread-id if id not found."
  [id]
  (core/lookup id))

(defn add-ai-tool
  "Adds AI Tool to conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
  [conversation-id tool]
  (core/add-ai-tool conversation-id tool))

(defn remove-ai-tool
  "Removes AI Tool from conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
  [conversation-id tool]
  (core/remove-ai-tool conversation-id tool))

(defn list
  "Returns sequence of registered message thread IDs.
   Returns empty sequence if no message threads registered."
  []
  (core/list))