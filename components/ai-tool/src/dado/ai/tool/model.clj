(ns dado.ai.tool.model
  "Data models for AI tools"
  (:require [malli.core :as m]
            [malli.error :as me]))

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
   [:parameters
    {:gen/elements [[:map [:fred :string] [:z [:vector :int]]]]}
    [:fn (comp m/schema? m/schema)]]
   [:returns ToolReturn]
   [:prompt-fn
    {:gen/elements [(constantly "a prompt")]}
    fn?]
   [:recognize-fn
    {:gen/elements [(constantly "unused")]}
    fn?]
   [:execute-fn {:gen/elements [(constantly {:result "generated"})]}
    fn?]])

(def ExecutionResult
  "Schema for tool execution results"
  [:map
   [:context-mod [:map
                  [:files [:vector :string]]
                  [:operation [:enum :set! :add!]]]]
   [:content any?]
   [:is-error {:optional true} map?]
   [:metrics {:optional true} map?]])

(def tool-validator (m/validator Tool))
(defn tool? [x] (tool-validator x))

(def execution-result-validator (m/validator ExecutionResult))
(defn execution-result? [x]
  (execution-result-validator x))
