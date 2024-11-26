(ns dado.ai.msg-thread-mgr.interface
  "Manages message thread lifecycle and state.
   Provides in-memory storage of message threads."
  (:require [dado.ai.msg-thread-mgr.core :as core]))

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

(defn list
  "Returns sequence of registered message thread IDs.
   Returns empty sequence if no message threads registered."
  []
  (core/list))
