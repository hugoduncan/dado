(ns dado.ai.agents.refactoring.core
  (:require
   [dado.ai.agent.interface :as agent]
   [dado.ai.agents.refactoring.model :as model]
   [dado.ai.message.interface :as msg]
   [dado.ai.prompt.interface :as prompt]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]
   [dado.project-config.interface :as project-config]))

(defn- process-response
  [response]
  (t/trace! {:id :refactoring/process-response}
            (try
              response
              (catch Exception e
                (throw (ex-info "Failed to process refactoring response"
                                {:type  :error/refactoring-response
                                 :cause e}))))))

;; TODO move these and abstract somehow
(defn adr-files [] (mapv str (babashka.fs/list-dir "dev/design/adr")))
(defn scope-files [] (mapv str (babashka.fs/list-dir "dev/design/scope")))
(defn implementation-files [] (mapv str (babashka.fs/list-dir "dev/design/implementation")))

(defn- get-context
  [additional-context-fn]
  (t/trace!
   {:id :refactoring/get-context}
   [(scope-files)
    (adr-files)
    (implementation-files)
    (reduce
     into
     []
     (additional-context-fn)
     #_[;; (prompt/implementation-paths "ai-claude")
        ;; (prompt/implementation-paths "ai-prompt")
        ;; (prompt/implementation-paths "ai-message")
        ;; (prompt/implementation-paths "patch")
        (prompt/implementation-paths "ai-agent")
        (prompt/implementation-paths "ai-refactoring-agent")
        ;; (prompt/implementation-paths "repl-message-loop")
        ;; (prompt/implementation-paths "update-extractor")
        []
        ;; "deps.edn"
        ;; "dev/design/implementation/search-replace-edit-format.md"
        ;; "dev/design/implementation/simplified-diff-format.md"
        ])]))

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
     "adr-implementation"
     "file-operation-directive-edits"
     "list-updated-namespaces"]
    {})))

(defn create-agent
  "Creates a refactoring agent for code modifications.
   Returns an agent map compatible with the AI Agent interface.

   The agent specializes in:
   - Code refactoring suggestions
   - Safe code modifications using FOD format
   - Context-aware refactoring

   Throws :error/agent-creation on validation failure."
  [project-config additional-context-fn]
  {:post [(have? model/Agent? :data (me/humanize (m/explain model/Agent %)))]}
  (t/trace!
   {:id :refactoring/create-agent}
   {:name                :refactoring
    :prompt-fn           (partial get-prompt project-config)
    :context-fn          (partial get-context additional-context-fn)
    :process-response-fn process-response}))
