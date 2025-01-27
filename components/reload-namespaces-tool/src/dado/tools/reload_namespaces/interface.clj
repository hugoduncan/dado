(ns dado.tools.reload-namespaces.interface
  "Interface for namespace reload tool"
  (:require
   [dado.tools.reload-namespaces.core :as core]))

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
