(ns dado.ai.agents.refactoring.core
  (:require
   [babashka.fs :as fs]
   [dado.ai.agent.interface :as agent]
   [dado.ai.prompt.interface :as prompt]
   [dado.project-config.interface :as project-config]
   [dado.tools.file-operation.interface :as file-operation]
   [dado.tools.filesystem.interface :as filesystem]
   [dado.ai.tools.matching-file.interface :as matching-file]
   [dado.tools.reload-namespaces.interface :as reload-namespaces]
   [dado.ai.tools.run-test-namespace.interface :as run-test-namespace]
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
    (when (fs/exists? dir)
      (into [] (comp
                (map str)
                (filter fs/regular-file?))
            (fs/list-dir dir)))))

(defn- get-context
  [project-config additional-context-fn]
  (t/trace!
   {:id :refactoring/get-context}
   [(into (get-directory-files project-config :dado/scope)
          (get-directory-files project-config :dado/architecture))
    (get-directory-files project-config :dado/adr)
    (get-directory-files project-config :dado/implementation)
    #_(mapv str (fs/list-dir "src" "*.clj"))
    (reduce into [] (additional-context-fn))]))

(defn- get-prompt
  [project-config]
  (t/trace!
   {:id :refactoring/get-prompt}
   (prompt/construct-prompt
    project-config
    ["refactoring"
     "context-files"
     "ask-missing-files"
     "adr-scope"
     "adr-implementation"]
    {})))

(defn create-agent
  "Creates a refactoring agent for code modifications."
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
    :ai-tools            [#_(file-operation/create-tool)
                          (filesystem/create-edit-file-tool)
                          (filesystem/create-move-file-tool)
                          (filesystem/create-write-file-tool)
                          (filesystem/create-read-file-tool)
                          (filesystem/create-search-files-tool)
                          (filesystem/create-create-directory-tool)
                          (filesystem/create-directory-tree-tool)
                          (reload-namespaces/create-tool)
                          (run-test-namespace/create-tool)
                          (matching-file/create-tool)]}))
