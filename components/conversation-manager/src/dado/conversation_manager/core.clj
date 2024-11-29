(ns dado.conversation-manager.core
  (:require
   [dado.ai.agent.interface :as agent]
   [dado.ai.message.interface :as message]
   [dado.ai.tool.interface :as tool]
   [dado.conversation-manager.model :as model]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

;; Thread store atom - holds map of thread-id to thread
(defonce ^:private conversation-store (atom {}))

(defn conversation!
  "Creates new conversation with specified AI port and agent.
   Returns conversation.
   Throws :error/conversation-creation on failure."
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
   {:id :thread/registered}
   (let [conversation {:message-thread message-thread
                       :ai-agent       ai-agent
                       :port-send-fn   port-send-fn
                       :ai-tools       ai-tools
                       :user-data      user-data}]
     (swap! conversation-store assoc (:id message-thread) conversation)
     message-thread)))

(defn id [conversation]
  {:pre  [(have? model/conversation? conversation
                 :data (me/humanize
                        (m/explain model/Conversation conversation)))]
   :post [(have? string? %)]}
  (-> conversation :message-thread :id))

(defn- get-conversation-or-throw
  [id]
  (t/trace!
   {:id ::get-conversation-or-throw :data {:id id}}
   (when-not (get @conversation-store id)
     (throw (ex-info "Unknown message thread ID"
                     {:type :error/unknown-message-thread-id
                      :id   id})))))

(defn update!
  "Updates stored conversation.
   Returns the conversation unchanged.
   Throws :error/unknown-message-thread-id if id not found.
   Throws :error/invalid-message-thread if validation fails."
  [conversation]
  {:pre  [(have? model/conversation? conversation
                 :data (me/humanize
                        (m/explain model/Conversation conversation)))]
   :post [(have? model/conversation? conversation
                 :data (me/humanize
                        (m/explain model/Conversation conversation)))
          (have? (partial = conversation) %)]}
  (t/trace!
   {:id :thread/updated}
   (let [id (message/message-thread-id (:message-thread conversation))]
     (get-conversation-or-throw id)
     (swap! conversation-store assoc id conversation)
     conversation)))

(defn remove!
  "Removes conversation with given id.
   Returns nil.
   Throws :error/unknown-message-thread-id if id not found."
  [conversation]
  {:pre [(have? model/conversation? conversation
                :data (me/humanize
                       (m/explain model/Conversation conversation)))]}
  (t/trace!
   {:id :thread/removed}
   (let [c-id (id conversation)]
     (get-conversation-or-throw c-id)
     (swap! conversation-store dissoc c-id)
     nil)))

(defn lookup
  "Returns conversation with given id.
   Throws :error/unknown-message-thread-id if id not found."
  [id]
  {:pre  [(have? string? id)]
   :post [(have? model/Conversation %
                 :data (me/humanize (m/explain model/Conversation %)))]}
  (t/trace!
   {:id :thread/lookup}
   (get-conversation-or-throw id)))

#_(defn add-ai-tool
    "Adds AI Tool to conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
    [conversation-id tool]
    {:pre [(have? string? conversation-id)
           (have? tool/validate-tool tool)]}
    (t/trace! {:id :conversation/tool-added}
              (let [conversation (lookup conversation-id)
                    tools        (get-in conversation [:metadata :tools] [])
                    updated      (assoc-in conversation
                                           [:metadata :tools]
                                           (conj tools tool))]
                (register-update updated))))

#_(defn remove-ai-tool
    "Removes AI Tool from conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
    [conversation-id tool]
    {:pre [(have? string? conversation-id)
           (have? tool/validate-tool tool)]}
    (t/trace! {:id :conversation/tool-removed}
              (let [conversation (lookup conversation-id)
                    tools        (get-in conversation [:metadata :tools] [])
                    updated      (assoc-in conversation
                                           [:metadata :tools]
                                           (filterv #(not= (:id tool) (:id %)) tools))]
                (register-update updated))))

(defn list-conversations
  "Returns sequence of registered conversation IDs.
   Returns empty sequence if no conversations registered."
  []
  (t/trace!
   {:id :thread/list}
   (keys @conversation-store)))
