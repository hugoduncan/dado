(ns dado.ai.agents.test.core
  "Core implementation of the AI Test Agent component."
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
  "Processes test agent responses, optimizing for test scenarios."
  [response]
  (t/trace!
   {:id :test/process-response}
   (try
     response
     (catch Exception e
       (throw (ex-info "Failed to process test response"
                       {:type  :error/test-response
                        :cause e}))))))

(defn- get-directory-files
  "Gets list of files in a configured directory"
  [project-config dir-key]
  (when-let [dir (project-config/get-directory project-config dir-key)]
    (mapv str (fs/list-dir dir))))

(defn- get-context
  "Gets test-relevant context files."
  [project-config additional-context-fn]
  (t/trace!
   {:id :test/get-context}
   (filterv seq [(reduce into [] (additional-context-fn))])))

(defn- get-prompt
  "Gets the test-specific system prompt."
  [project-config]
  (t/trace!
   {:id :test/get-prompt}
   (prompt/construct-prompt
    project-config
    ["test/test"
     "context-files"
     "ask-missing-files"]
    {})))

(defn create-agent
  "Creates a testing agent for test interactions.
   Returns an agent map compatible with the AI Agent interface.

   The agent specializes in:
   - Minimized token usage
   - Test-specific prompts
   - Test context management
   - Response optimization

   Throws :error/agent-creation on validation failure."
  [project-config additional-context-fn]
  {:post [(have? (m/validator (agent/agent-schema))
                 %
                 :data (me/humanize (m/explain (agent/agent-schema) %)))]}
  (t/trace!
   {:id :test/create-agent}
   {:name                :test
    :prompt-fn           (partial get-prompt project-config)
    :context-fn          (partial get-context project-config additional-context-fn)
    :process-response-fn process-response
    :ai-tools            [(file-operation/create-tool)
                          (reload-namespaces/create-tool)]}))
