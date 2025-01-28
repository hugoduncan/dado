(ns dado.nrepl-middleware.core
  "Middleware of using dado dev chat assistant."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [dado.ai.prompt.interface :as prompt]
   [dado.conversation-action.interface :as conversation-action]
   [dado.document-retrieval.interface :as document-retrieval]
   [nrepl.misc :refer [response-for] :as misc]
   [nrepl.transport :as transport]
   [org.hugoduncan.dado.operation.interface :as operation]
   [taoensso.telemere :as t]
   [dado.project-config.interface :as project-config]   )
  (:import
   [nrepl.transport
    Transport]))

(t/set-min-level! :warn)

(defn- dado-reply
  [{:keys [session ns language action args] :as msg}]
  (let [ns (if ns (symbol ns) (symbol (str (@session #'*ns*))))]
    (try
      (response-for
       msg
       {:status   :done
        :response #_ (operation/op language action args ns)
        {'ai-providers
         (sort (into `() (map name) (project-config/ai-providers)))}})
      (catch Exception e
        (do ;; binding [*out* (java.io.OutputStreamWriter. System/out)]
          (prn e))
        (response-for
         msg
         {:status #{:done :dado-error}})))))

;; the handler
(defn wrap-dado
  "Middleware that provides dado assistant.
  It understands the following params:

  * `system` - a symbol that determines the action to undertake
  * `ns`- the namespace in which the action occurs. Defaults to `*ns*`.
  * `args` – a map of arguments to pass to the action."
  [h]
  (fn [{:keys [op ^Transport transport] :as msg}]
    (if (= op "dado")
      (transport/send transport (dado-reply msg))
      (h msg))))

(defn- input->message-maps [message]
  (str (last message))
  #_(mapv
     #(hash-map "role" (str (first %)) "content" (str (last %)))
     messages))

(defn- empty->nil [s]
  (when-not (str/blank? s)
    s))

(defn- dado-chat-reply
  [{:keys [agent-name ai-port-name conversation-id message options]
    :as   msg}]
  (t/trace!
   {:id   ::dado-chat-reply
    :data {:agent-name      agent-name
           :ai-port-name    ai-port-name
           :conversation-id conversation-id
           :message         message}}
   (try
     (let [options          (-> (apply hash-map options)
                                (update-keys keyword))
           {:keys [invoke-file-path context-mode]}
           options
           invoke-file-path (empty->nil invoke-file-path)
           conversation-id  (if (str/blank? conversation-id)
                              (conversation-action/create-conversation!
                               agent-name
                               (keyword ai-port-name))
                              conversation-id)
           #_#_all-files    (document-retrieval/all-files ".")
           context-files    (case (keyword context-mode)
                              :all
                              (document-retrieval/all-files ".")

                              :single
                              (if (str/blank? invoke-file-path)
                                []
                                [invoke-file-path])

                              :file-dependencies
                              (when invoke-file-path
                                (document-retrieval/dependency-files
                                 invoke-file-path))

                              [])
           dirty-git-files (document-retrieval/git-dirty-files)
           git-diffs       (document-retrieval/git-uncommitted-diffs)

           this-namespace (some-> invoke-file-path
                                  (document-retrieval/path->namespace))
           project-config (project-config/load-config)

           context-values (cond-> [#_{:name "project-files"
                                      :context (str/join "\n" all-files)}]
                            invoke-file-path
                            (conj
                             {:name    "exophoric-file"
                              :content (prompt/render-template
                                        project-config
                                        "exophoric-file"
                                        {:this-file-path invoke-file-path})})
                            this-namespace
                            (conj
                             {:name    "exophoric-namespace"
                              :content (str (prompt/render-template
                                             project-config
                                             "exophoric-namespace"
                                             {:this-namespace this-namespace}))})
                            (not (str/blank? git-diffs))
                            (conj
                             {:name    "git-diffs"
                              :content (prompt/render-template
                                        project-config
                                        "git-diffs"
                                        {:uncommitted-diffs git-diffs})}))
           context-files  (vec (set/union
                                (set context-files)
                                (set dirty-git-files)
                                (set context-values)))
           response       (conversation-action/response!
                           conversation-id
                           message
                           ""
                           context-files)]
       (response-for
        msg
        {:status   :done
         :response response}))
     (catch Exception e
       (prn e)
       (response-for
        msg
        {:status #{:done :dado-chat-error}})))))

(defn wrap-dado-chat [h]
  "Middleware that provides dado chat.
  It understands the following params:

  * `message` - a message
  * `agent-name` - the name of the agent to talk to
  * `ai-port-name` - the name of the AI to talk with
  * `conversation-id` - the ID for the conversation
  * `options` - context options"
  [h]
  (fn [{:keys [op ^Transport transport] :as msg}]
    (if (= op "dado/chat")
      (t/trace!
       {:id ::wrap-dado-chat :level :debug :data {:msg msg} }
       (transport/send transport (dado-chat-reply msg)))
      (h msg))))
