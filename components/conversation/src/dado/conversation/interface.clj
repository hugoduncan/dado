(ns dado.conversation.interface
  "Interface for conversations between humans and AI"
  (:require
   [dado.conversation.core :as core]
   [dado.conversation.model :as model]))

(defn create
  "Creates a new conversation."
  [ai-agent port-send-fn message-thread {:keys [ai-tools user-data] :as options}]
  (core/create ai-agent port-send-fn message-thread options))

(defn id
  "Returns the conversation ID."
  [conversation]
  (core/id conversation))

(defn message-thread
  "Returns the conversation message-thread."
  [conversation]
  (core/message-thread conversation))

(defn ai-agent
  "Returns the conversation agent."
  [conversation]
  (core/ai-agent conversation))

(defn conversation? [x]
  (model/conversation? x))

(defn conversation-schema [])
(model/conversation-schema)
