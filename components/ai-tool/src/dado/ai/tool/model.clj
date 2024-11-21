(ns dado.ai.tool.model
  "Data models for AI tools"
  (:require [malli.core :as m]))

(def Parameter
  "Schema for tool parameter specification"
  [:map
   [:name string?]
   [:type keyword?]
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
   [:structured-description map?]
   [:parameters [:sequential Parameter]]
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
