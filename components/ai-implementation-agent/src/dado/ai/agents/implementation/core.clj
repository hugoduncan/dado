(ns dado.ai.agents.implementation.core
  (:require
   [babashka.fs :as fs]
   [dado.ai.agent.interface :as agent]
   [dado.ai.prompt.interface :as prompt]
   [dado.project-config.interface :as project-config]
   [dado.tools.file-operation.interface :as file-operation]
   [dado.tools.reload-namespaces.core :as reload-namespaces]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(defn- process-response
  [response]
  (t/trace! {:id :refactoring/process-response}
            (try
              response
              (catch Exception e
                (throw (ex-info "Failed to process refactoring response"
                                {:type  :error/refactoring-response
                                 :cause e}))))))

(defn- get-directory-files
  "Gets list of files in a configured directory"
  [project-config dir-key]
  (when-let [dir (project-config/get-directory project-config dir-key)]
    (mapv str (fs/list-dir dir))))

(defn- get-context
  [project-config additional-context-fn]
  (t/trace!
   {:id :refactoring/get-context}
   [(into (get-directory-files project-config :dado/scope)
          (get-directory-files project-config :dado/architecture))
    (get-directory-files project-config :dado/adr)
    (get-directory-files project-config :dado/implementation)
    #_(conj (mapv str (fs/list-dir "src" "*.clj")) "deps.edn")
    (reduce into [] (additional-context-fn))]))

(defn- get-prompt
  [project-config]
  (t/trace!
   {:id :refactoring/get-prompt}
   (prompt/construct-prompt
    project-config
    ["implementation"
     "context-files"
     "ask-missing-files"
     "adr-scope"
     "adr-implementation"]
    {})))

(defn create-agent
  "Creates a refactoring agent for code modifications.
   Returns an agent map compatible with the AI Agent interface."
  [project-config additional-context-fn]
  {:post [(have? (m/validator (agent/agent-schema))
                 :data (me/humanize (m/explain (agent/agent-schema) %)))]}
  (t/trace!
   {:id :refactoring/create-agent}
   {:name                :refactoring
    :prompt-fn           (partial get-prompt project-config)
    :context-fn          (partial get-context
                                  project-config
                                  additional-context-fn)
    :process-response-fn process-response
    :ai-tools            [(file-operation/create-tool)
                          (reload-namespaces/create-tool)]}))
