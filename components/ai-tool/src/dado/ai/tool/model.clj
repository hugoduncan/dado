(ns dado.ai.tool.model
  "Data models for AI tools"
  (:require [malli.core :as m]))

#_(def Parameter
    "Schema for tool parameter specification"
    [:map
     [:name string?]
     [:type any?]
     [:enum {:optional true} [:vector any?]]
     [:description string?]
     [:required? boolean?]
     [:default {:optional true} any?]])

(def ToolReturn
  "Schema for tool return value specification"
  [:map
   [:type keyword?]
   [:description string?]])

(def Tool
  "Schema for AI tool definition"
  [:map
   [:id keyword?]
   [:name string?]
   [:description string?]
   [:parameters [:fn (comp m/schema? m/schema)]]
   [:returns ToolReturn]
   [:prompt-fn fn?]
   [:recognize-fn fn?]
   [:execute-fn fn?]])

(def ExecutionResult
  "Schema for tool execution results"
  [:map
   [:result any?]
   [:error {:optional true} map?]
   [:metrics {:optional true} map?]])
