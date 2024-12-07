(ns dado.ai.agent.core
  "Core implementation for AI agent component"
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [dado.ai.agent.model :as model]
   [dado.repl.message-loop.interface :as message-loop]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [dado.project-config.interface :as project-config]))

(def agent? (m/validator model/Agent))

(defn validate-agent
  "Validates agent map structure. Returns agent if valid.
   Throws :error/agent-validation if invalid."
  [agent]
  (t/trace! {:id :agent/validated}
    (if (agent? agent)
      agent
      (throw (ex-info "Invalid agent configuration"
                     {:type :error/agent-validation
                      :data (me/humanize (m/explain model/Agent agent))})))))

(defn- find-and-load-document
  "Tries to find and load document from filesystem or classpath.
   Returns map with :content and :path if found, nil otherwise."
  [project-config agent doc-name]
  (let [dev-dir  (get project-config :dev-dir "dev")
        fs-paths [(fs/path dev-dir "dado/ai/agents" (name (:name agent)) doc-name)
                  (fs/path dev-dir "dado/ai/agents/common" doc-name)]
        cp-paths [(str "dado/ai/agents/" (name (:name agent)) "/" doc-name)
                  (str "dado/ai/agents/common/" doc-name)]]
    (t/trace! {:id   :agent/document-search
               :data {:fs-paths fs-paths
                      :cp-paths cp-paths}}
              (or
               ;; Try filesystem paths
               (some (fn [path]
                       (prn :trying-fs path)
                       (when (fs/exists? path)
                         {:content (slurp path)
                          :path    (str path)}))
                     fs-paths)
               ;; Try classpath resources
               (some (fn [path]
                       (prn :trying-resources path)
                       (when-let [resource (io/resource path)]
                         {:content (slurp resource)
                          :path    path}))
                     cp-paths)))))

(defn load-agent-document
  "Loads document content for given name following fallback path.
   Returns map with :content and :path.
   Throws :error/document-not-found if docs missing."
  [project-config agent doc-name]
  (t/trace! {:id :agent/documents-loaded}
            (if-let [doc (find-and-load-document project-config agent doc-name)]
              doc
              (throw (ex-info "Document not found in search paths"
                              {:type     :error/document-not-found
                               :agent    (:name agent)
                               :document doc-name})))))

(defn message-loop
  "Runs interactive message loop with given agent and message thread."
  [ai-port-fn agent msg-thread]
  (message-loop/message-loop
   ai-port-fn
   msg-thread
   (:prompt-fn agent)
   (:context-fn agent)))

(defn lookup
  [agent-name]
  (let [create-fn-sym (symbol
                       (str "dado.ai.agents."
                            agent-name
                            ".interface/create-agent"))
        create-fn     (requiring-resolve  create-fn-sym)]
    (create-fn (project-config/load-config) (constantly nil))))
