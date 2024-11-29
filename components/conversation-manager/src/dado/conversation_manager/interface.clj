(ns dado.conversation-manager.interface
  "Manages AI conversation lifecycle and state.
   Provides in-memory storage of conversations with message threads
   and tool configurations."
  (:require
   [dado.conversation-manager.core :as core]))

(defn conversation!
  "Creates new conversation with specified AI port and agent.
   Returns conversation ID (UUID).
   Throws :error/conversation-creation on failure."
  [ai-agent
   port-send-fn
   message-thread
   {:keys [ai-tools user-data] :as options}]
  (core/conversation ai-agent port-send-fn message-thread options))

(defn id
  "Return the conversation ID."
  [conversation]
  (core/id conversation))

#_(defn send-message
    "Sends message and returns response asynchronously.
   Returns core.async channel that will receive response.
   Response format: {:content string? :error (optional) ex-info}
   Only execution errors are returned through channel.
   Other errors thrown immediately.
   Throws :error/unknown-conversation if ID not found."
    [conversation-id input]
    (core/send-message conversation-id input))

#_(defn end-conversation
    "Ends conversation and cleans up resources.
   Returns nil.
   Throws :error/unknown-conversation if ID not found."
    [conversation-id]
    (core/end-conversation conversation-id))

(defn lookup
  "Returns conversation with given id.
   Throws :error/unknown-conversation if ID not found."
  [conversation-id]
  (core/lookup conversation-id))

(defn update!
  "Update the registered conversation.
   Throws :error/unknown-conversation if ID not found."
  [conversation]
  (core/update! conversation))

(defn remove!
  "Returns conversation with given id.
   Throws :error/unknown-conversation if ID not found."
  [conversation]
  (core/remove! conversation))

#_(defn add-ai-tool
    "Adds AI Tool to conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
    [conversation-id tool]
    (core/add-ai-tool conversation-id tool))

#_(defn remove-ai-tool
    "Removes AI Tool from conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
    [conversation-id tool]
    (core/remove-ai-tool conversation-id tool))

(defn list-conversations
  "Returns sequence of conversation IDs.
   Empty sequence if no conversations exist."
  []
  (core/list-conversations))
