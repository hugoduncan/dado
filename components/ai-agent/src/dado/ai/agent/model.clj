(ns dado.ai.agent.model
  "Data models for AI agents"
  (:require [malli.core :as m]
            [dado.ai.tool.interface :as tool]))

(def AgentFn
  "Schema for agent functions"
  [:=> [:cat :any] :any])

(def Agent
  "Schema for AI agents"
  [:map
   [:name keyword?]
   [:prompt-fn AgentFn]
   [:context-fn AgentFn]
   [:process-response-fn AgentFn]
   [:ai-tools [:vector (tool/tool-schema)]]])

(def Document
  "Schema for loaded documents"
  [:map
   [:content string?]
   [:path string?]])

(def agent? (m/validator Agent))
