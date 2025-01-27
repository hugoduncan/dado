(ns dado.conversation-action.interface
  "Provide conversation interaction and management"
  (:require
   [clojure.string :as str]
   [dado.ai.agent.interface :as ai-agent]
   [dado.ai.message.interface :as message]
   [dado.ai.port.interface :as ai-port]
   [dado.conversation-manager.interface :as conversation-manager]
   [dado.conversation.interface :as conversation]
   [dado.http.interface :as http]
   [dado.project-config.interface :as project-config]
   [dado.repl.message-loop.interface :as message-loop]
   [hato.client :as hc]
   [taoensso.truss :refer [have]]))

(defn create-conversation!
  [agent-name ai-port-name]
  (let [agent          (ai-agent/lookup agent-name)
        ai-config      (project-config/ai-provider-config
                        ai-port-name)
        {:keys [api api-key api-url model-name]}
        ai-config
        ai-port-send!  (ai-port/lookup-send! (or
                                              (some-> api name)
                                              (name ai-port-name)))
        model-name     (or model-name
                           ((ai-port/lookup-default-model-name ai-port-name)))
        context-files  (volatile! [])
        message-thread (-> (message/create-message-thread model-name)
                           (message/register-tools
                            (:ai-tools agent)))
        conversation   (conversation/create
                        agent
                        (partial
                         ai-port-send!
                         (get-in
                          (project-config/load-config)
                          [:ai-providers (keyword ai-port-name)])
                         (http/robust-request-fn hc/request))
                        message-thread
                        {:context-files context-files})]
    (conversation-manager/add! conversation)
    (conversation/id conversation)))

(defn- msg->text
  [msg]
  (str/join "\n" (mapv :text (:content msg))))

(defn response!
  [conversation-id message-text extra-prompt context-files]
  (let [conversation (conversation-manager/lookup conversation-id)
        _            (vreset! (:context-files conversation) (vec context-files))
        agent        (conversation/ai-agent conversation)
        msg-thread   (-> (conversation/message-thread conversation)
                         (message/add-message
                          {:role    :user
                           :content [{:text message-text :type :text}]}))
        n-messages   (-> msg-thread :messages count)
        msg-thread   (message-loop/complete-with-tools!
                      msg-thread
                      (fn []
                        (str ((have (:prompt-fn agent))) extra-prompt))
                      (fn []
                        (conj
                         ((:context-fn agent))
                         context-files))
                      (have (:port-send-fn conversation)))
        conversation (assoc conversation :message-thread msg-thread)]
    (conversation-manager/update! conversation)
    {:messages        (into []
                            (comp
                             (drop n-messages)
                             (map msg->text))
                            (:messages msg-thread))
     :conversation-id conversation-id}))
