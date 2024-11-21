(ns dado.ai.tool.core
  "Core implementation of AI tool management"
  (:require [dado.ai.tool.model :as model]
            [malli.core :as m]
            [malli.error :as me]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have?]]))

;; Tool registry
(def ^:private tool-registry (atom {}))

(def tool? (m/validator model/Tool))
(def execution-result? (m/validator model/ExecutionResult))

(defn register-tool!
  "Registers a tool. Tool map must conform to Tool schema.
   Returns registered tool map if successful.
   Throws :error/tool-validation if validation fails."
  [tool-map]
  (t/trace! {:id :tool/registered}
    (if (tool? tool-map)
      (let [tool-id (:id tool-map)]
        (swap! tool-registry assoc tool-id tool-map)
        tool-map)
      (throw (ex-info "Invalid tool configuration"
                     {:type :error/tool-validation
                      :data (me/humanize (m/explain model/Tool tool-map))})))))

(defn lookup-tool
  "Looks up tool by keyword identifier.
   Returns tool map if found, nil if not found."
  [tool-id]
  (t/trace! {:id :tool/lookup}
    (get @tool-registry tool-id)))

(defn execute-tool!
  "Executes tool with given parameters.
   Returns result map containing:
   {:result any?      ; Tool execution result
    :error map?       ; Error details if execution failed
    :metrics map?}    ; Execution metrics (duration etc)
   Throws :error/tool-execution for validation/execution errors."
  [tool-id params]
  (t/trace! {:id :tool/executed}
    (if-let [tool (lookup-tool tool-id)]
      (try
        (let [result ((:execute-fn tool) params)]
          {:result result})
        (catch Exception e
          (throw (ex-info "Tool execution failed"
                         {:type :error/tool-execution
                          :tool-id tool-id
                          :cause e}))))
      (throw (ex-info "Tool not found"
                     {:type :error/tool-execution
                      :tool-id tool-id})))))

(defn validate-tool
  "Validates tool map structure.
   Returns tool map if valid.
   Throws :error/tool-validation if invalid."
  [tool-map]
  (t/trace! {:id :tool/validated}
    (if (tool? tool-map)
      tool-map
      (throw (ex-info "Invalid tool configuration"
                     {:type :error/tool-validation
                      :data (me/humanize (m/explain model/Tool tool-map))})))))
