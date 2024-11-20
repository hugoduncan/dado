(ns dado.ai.agents.refactoring.core
  (:require
   [dado.ai.agent.interface :as agent]
   [dado.ai.agents.refactoring.model :as model]
   [dado.ai.message.interface :as msg]
   [dado.ai.prompt.interface :as prompt]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(defn- process-response
  [response]
  (t/trace! {:id :refactoring/process-response}
    (try
      (let [diffs (msg/extract-update-blocks response)]
        {:diffs diffs})
      (catch Exception e
        (throw (ex-info "Failed to process refactoring response"
                       {:type :error/refactoring-response
                        :cause e}))))))

(defn- get-context
  [files]
  (t/trace! {:id :refactoring/get-context}
    (when (seq files)
      {:files (mapv (fn [f] {:name f :content (slurp f)}) files)})))

(defn- get-prompt
  [_config]
  (t/trace! {:id :refactoring/get-prompt}
    (prompt/construct-prompt 
      ["refactoring/system-prompt"]
      {})))

(defn create-agent
  "Creates a refactoring agent for code modifications.
   Returns an agent map compatible with the AI Agent interface.
   
   The agent specializes in:
   - Code refactoring suggestions
   - Safe code modifications using FOD format
   - Context-aware refactoring
   
   Throws :error/agent-creation on validation failure."
  [config]
  (t/trace! {:id :refactoring/create-agent}
    (let [agent {:name :refactoring
                 :prompt-fn (partial get-prompt config)
                 :context-fn get-context
                 :process-response-fn process-response}]
      (if (model/Agent? agent)
        agent
        (throw (ex-info "Invalid refactoring agent configuration"
                       {:type :error/agent-creation
                        :data (me/humanize (m/explain model/Agent agent))}))))))
