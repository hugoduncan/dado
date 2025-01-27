(ns dado.ai.tools.run-test-namespace.interface
  "Tool for running Clojure test namespaces and collecting results."
  (:require [dado.ai.tools.run-test-namespace.core :as core]
            [dado.ai.tools.run-test-namespace.model :as model]))

(def tool
  "Run Test Namespace Tool definition."
  {:id          :dado/run-test-namespace
   :name        "Run Test Namespace Tool"
   :description "Executes test namespace and collects results"
   :structured-description
   {:claude
    {:description "Tool for running Clojure test namespaces and collecting results."}}
   :parameters
   [:map
    [:namespace string?]     ; Namespace to run
    [:async? {:optional true}
     boolean?]               ; Run asynchronously (default false)
    [:timeout {:optional true}
     pos-int?]]             ; Timeout in ms (default 30000)
   :returns
   {:type        :map
    :description "Map containing test results and output"}
   :execute-fn   core/execute!})

(def test-results?
  "Validator for test results format."
  model/test-results?)

(def tool-config?
  "Validator for tool configuration."
  model/tool-config?)
