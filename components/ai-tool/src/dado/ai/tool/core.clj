(ns dado.ai.tool.core
  "Core implementation of AI tool management"
  (:require
   [dado.ai.tool.model :as model]
   [dado.error-monad.interface :as em]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]
   [jsonista.core :as j]
   [clojure.string :as str]))

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


(defn- tool-ex [e]
  {:is-error? true
   :content
   [{:text "Failed to parse parameters as valid JSON"
     :type :text}
    {:text (ex-message e) :type :text}]})

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
     (let [{:keys [success? value] :as v}
           (em/do-error
            [params (if (string? params)
                      (try
                        (em/success
                         (j/read-value params j/keyword-keys-object-mapper))
                        (catch Exception e
                          (em/failure (tool-ex e))))
                      (em/success params))
             invalid (em/maybe
                      (t/spy! :warn
                              (m/validate
                               (:parameters tool)
                               params))
                      {:is-error? true
                       :content
                       [{:text "Invalid parameters" :type :text}
                        {:text (pr-str params) :type :text}
                        {:text (pr-str (me/humanize
                                        (m/explain
                                         (:parameters tool)
                                         params)))
                         :type :text}]})]
            params)]
       (t/log! {:level :warn
                :data  {:params   params
                        :schema   (:parameters tool)
                        :success? success?
                        :value    value
                        :v        v}}
               "Execute")
       (if success?
         ((:execute-fn tool) value)
         value))
     (catch Exception e
       (t/error!
        e
        {:level :error
         :id    ::execute-tool!
         :data  {:params params :tool tool}})
       {:is-error? true
        :content   [{:text (str "Unexpected exception: " (ex-message e))
                     :type :text}]}))))

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
