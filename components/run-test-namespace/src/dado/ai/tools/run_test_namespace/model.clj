(ns dado.ai.tools.run-test-namespace.model
  "Data models for run test namespace tool."
  (:require [malli.core :as m]))

(def TestStatus
  "Valid test status values"
  [:enum :pass :fail :error])

(def VarResult
  "Schema for test result details"
  [:map
   [:type [:enum :fail :error]]
   [:message {:optional true} string?]
   [:expected {:optional true} any?]
   [:actual {:optional true} any?]])

(def VarSummary
  "Schema for test var summary"
  [:map
   [:pass int?]
   [:fail int?]
   [:error int?]])

(def TestVarResult
  "Schema for individual test var results"
  [:map
   [:test-var string?]
   [:summary VarSummary]
   [:results [:vector VarResult]]])

(def TestResults
  "Schema for complete test execution results"
  [:map
   [:is-error :boolean]
   [:content [:vector
              [:map
               [:namespace string?]
               [:summary VarSummary]
               [:test-results [:map-of string? TestVarResult]]
               [:output string?]
               [:elapsed-ms int?]]]]])

(def ToolConfig
  "Schema for tool configuration"
  [:map
   [:namespace string?]
   [:async? {:optional true} boolean?]
   [:timeout {:optional true} pos-int?]])

;; Validators
(def test-results? (m/validator TestResults))
(def tool-config? (m/validator ToolConfig))
