(ns dado.ai.agents.scope.core
  (:require
   [babashka.fs :as fs]
   [dado.ai.agent.interface :as agent]
   [dado.ai.prompt.interface :as prompt]
   [dado.ai.tools.matching-file.interface :as matching-file]
   [dado.project-config.interface :as project-config]
   [dado.tools.filesystem.interface :as filesystem]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(defn- process-response
  [response]
  (t/trace! {:id :scope/process-response}
            (try
              response
              (catch Exception e
                (throw (ex-info "Failed to process scope response"
                                {:type  :error/scope-response
                                 :cause e}))))))

(defn- get-directory-files
  "Gets list of files in a configured directory"
  [project-config dir-key]
  (when-let [dir (project-config/get-directory project-config dir-key)]
    (mapv str (fs/list-dir dir))))

(defn- get-context
  [project-config additional-context-fn]
  (t/trace!
   {:id :scope/get-context}
   [(get-directory-files project-config :dado/scope)
    (get-directory-files project-config :dado/architecture)
    (get-directory-files project-config :dado/adr)
    (reduce into [] (additional-context-fn))]))

(defn- get-prompt
  [project-config]
  (t/trace!
   {:id :scope/get-prompt}
   (prompt/construct-prompt
    project-config
    ["scope/scope"
     "context-files"
     "ask-missing-files"]
    {})))

(defn create-agent
  "Creates a scope agent for scope and feature management.
   Returns an agent map compatible with the AI Agent interface.

   The agent specializes in:
   - Scope definition and refinement
   - Feature analysis and requirements
   - Project boundaries and constraints
   - Feature interaction analysis

   Throws :error/agent-creation on validation failure."
  [project-config additional-context-fn]
  {:post [(have? (m/validator (agent/agent-schema))
                 :data (me/humanize (m/explain (agent/agent-schema) %)))]}
  (t/trace!
   {:id :scope/create-agent}
   {:name                :scope
    :prompt-fn           (partial get-prompt project-config)
    :context-fn          (partial get-context
                                  project-config
                                  additional-context-fn)
    :process-response-fn process-response
    :ai-tools            [(filesystem/create-edit-file-tool)
                          (filesystem/create-move-file-tool)
                          (filesystem/create-write-file-tool)
                          (filesystem/create-read-file-tool)
                          (filesystem/create-search-files-tool)
                          (filesystem/create-create-directory-tool)
                          (filesystem/create-directory-tree-tool)

                          (matching-file/create-tool)]}))
