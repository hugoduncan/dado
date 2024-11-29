(ns dado.conversation-manager.model
  "Data models for conversation management"
  (:require
   [dado.ai.agent.interface :as agent]
   [dado.ai.message.interface :as message]
   [dado.ai.tool.interface :as tool]
   [malli.core :as m]))

(def Conversation
  [:map
   [:message-thread message/message-thread-schema]
   [:ai-agent (agent/agent-schema)]
   [:port-send-fn fn?]
   [:ai-tools {:optional true} [:vector (tool/tool-schema)]]
   [:user-data [:map-of any? any?]]])

;; Validators
(def conversation? (m/validator Conversation))
