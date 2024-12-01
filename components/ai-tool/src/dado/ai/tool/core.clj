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
  [tool params]
  (t/trace!
   {:id    :tool/executed
    :level :warn
    :data  {:tool tool :params params}}
   (try
     (t/log! {:level :warn :data {:params params}} "Execute")
     ((:execute-fn tool) params)
     (catch Exception e
       (throw (ex-info "Tool execution failed"
                       {:type    :error/tool-execution
                        :tool-id (:id  tool)
                        :params  params
                        :cause   e}))))))

(defn validate-tool
  "Validates tool map structure.
   Returns tool map if valid.
   Throws :error/tool-validation if invalid."
  [tool-map]
  (t/trace! {:id :tool/validated :data {:tool tool-map}}
            (if (tool? tool-map)
              tool-map
              (throw (ex-info "Invalid tool configuration"
                              {:type :error/tool-validation
                               :data (me/humanize (m/explain model/Tool tool-map))})))))

;; NOTE Prompt used by claude for tools

;; In this environment you have access to a set of tools you can use to answer
;; the user's question.
;; {{ FORMATTING INSTRUCTIONS }}

;; String and scalar parameters should be specified as is, while lists and
;; objects should use JSON format. Note that spaces for string values are not
;; stripped. The output is not expected to be valid XML and is parsed with
;; regular expressions.

;; Here are the functions available in JSONSchema format:
;; {{ TOOL DEFINITIONS IN JSON SCHEMA }}
;; {{ USER SYSTEM PROMPT }}
;; {{ TOOL CONFIGURATION }}
