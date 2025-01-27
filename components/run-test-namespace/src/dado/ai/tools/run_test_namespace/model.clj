(ns dado.ai.tools.run-test-namespace.model
  "Data models for run test namespace tool."
  (:require [malli.core :as m]))

(def TestStatus
  "Valid test status values"
  [:enum :pass :fail :error])

(def TestResult
  "Schema for individual test result"
  [:map
   [:test-var string?]
   [:status TestStatus]
   [:expected {:optional true} any?]
   [:actual {:optional true} any?]
   [:message {:optional true} string?]
   [:type {:optional true} string?]])

(def TestSummary
  "Schema for test execution summary"
  [:map
   [:test int?]
   [:pass int?]
   [:fail int?]
   [:error int?]])

(def TestOutput
  "Schema for captured test output"
  [:map
   [:stdout string?]
   [:stderr string?]])

(def TestResults
  "Schema for complete test execution results"
  [:map
   [:namespace string?]
   [:summary TestSummary]
   [:test-results [:vector TestResult]]
   [:output TestOutput]
   [:elapsed-ms int?]])

(def ToolConfig
  "Schema for tool configuration"
  [:map
   [:namespace string?]
   [:async? {:optional true} boolean?]
   [:timeout {:optional true} pos-int?]])

;; Validators
(def test-results? (m/validator TestResults))
(def tool-config? (m/validator ToolConfig))
