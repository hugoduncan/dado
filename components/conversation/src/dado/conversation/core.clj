(ns dado.conversation.core
  (:require
   [dado.ai.agent.interface :as agent]
   [dado.ai.message.interface :as message]
   [dado.ai.tool.interface :as tool]
   [dado.conversation.model :as model]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(defn create
  "Creates new conversation."
  [ai-agent port-send-fn message-thread {:keys [ai-tools user-data]}]
  {:pre  [(have? message/message-thread? message-thread
                 :data (me/humanize
                        (m/explain message/message-thread-schema message-thread)))
          (have? fn? port-send-fn)
          (have? agent/agent? ai-agent)
          (have? tool/tool? :in ai-tools)]
   :post [(have? model/conversation? %
                 :data (me/humanize
                        (m/explain model/Conversation %)))]}
  (t/trace!
   {:id :conversation/created}
   {:message-thread message-thread
    :ai-agent       ai-agent
    :port-send-fn   port-send-fn
    :ai-tools       ai-tools
    :user-data      user-data}))

(defn id
  "Returns the conversation ID."
  [conversation]
  {:pre  [(have? model/conversation? conversation
                 :data (me/humanize
                        (m/explain model/Conversation conversation)))]
   :post [(have? string? %)]}
  (-> conversation :message-thread :id))
