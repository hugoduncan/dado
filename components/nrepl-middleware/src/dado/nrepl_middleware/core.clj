(ns dado.nrepl-middleware.core
  "Middleware of using dado dev chat assistant."
  (:require
   [clojure.string :as str]
   [dado.conversation-action.interface :as conversation-action]
   [dado.document-retrieval.interface :as document-retrieval]
   [nrepl.misc :refer [response-for] :as misc]
   [nrepl.transport :as transport]
   [org.hugoduncan.dado.operation.interface :as operation]
   [taoensso.telemere :as t])
  (:import
   [nrepl.transport
    Transport]))

(defn- dado-reply
  [{:keys [session ns language action args] :as msg}]
  (let [ns (if ns (symbol ns) (symbol (str (@session #'*ns*))))]
    (try
      (response-for
       msg
       {:status   :done
        :response (operation/op language action args ns)})
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

(defn- dado-chat-reply
  [{:keys [agent-name ai-port-name conversation-id message options]
    :as   msg}]
  (t/trace!
   {:id    ::dado-chat-reply
    :level :warn
    :data  {:agent-name      agent-name
            :ai-port-name    ai-port-name
            :conversation-id conversation-id
            :message         message}}
   (try
     (let [options         (-> (apply hash-map options)
                               (update-keys keyword))
           {:keys [invoke-file-path context-mode]}
           options
           conversation-id (if (str/blank? conversation-id)
                             (conversation-action/create-conversation!
                              agent-name
                              ai-port-name)
                             conversation-id)
           context-files   (case (keyword context-mode)
                             :all
                             (document-retrieval/all-files ".")

                             :single
                             (if (str/blank? invoke-file-path)
                               []
                               [invoke-file-path])

                             :file-dependencies
                             (document-retrieval/dependency-files
                              invoke-file-path)

                             [])
           response (conversation-action/response!
                     conversation-id message context-files)]

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
