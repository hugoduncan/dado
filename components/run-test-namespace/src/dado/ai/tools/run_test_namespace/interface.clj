(ns dado.ai.tools.run-test-namespace.interface
  "Tool for running Clojure test namespaces and collecting results."
  (:require [dado.ai.tools.run-test-namespace.core :as core]
            [dado.ai.tools.run-test-namespace.model :as model]))

(def test-results?
  "Validator for test results format."
  model/test-results?)

(def tool-config?
  "Validator for tool configuration."
  model/tool-config?)


(defn create-tool
  "Creates namespace reload tool configuration.
   Tool reloads specified namespaces without validation or dependency handling.

   Input should be in Updated Namespaces List format:
   ```updated-namespaces
   my.project.utils
   my.project.core
   ```

   Returns map of reload results:
   {:reloaded [<successfully-reloaded-ns-symbols>]
    :errors [{:ns <failed-ns-symbol> :error <error-message>}]}"
  []
  (core/create-tool))
