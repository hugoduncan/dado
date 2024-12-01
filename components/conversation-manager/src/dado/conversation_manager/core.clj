(ns dado.conversation-manager.core
  (:require
   [dado.ai.message.interface :as message]
   [dado.conversation.interface :as conversation]
   [dado.conversation-manager.model :as model]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

;; Thread store atom - holds map of thread-id to thread
(defonce ^:private conversation-store (atom {}))

(defn add!
  "Add a new conversation
   Returns conversation.
   Throws :error/conversation-creation on failure."
  [conversation]
  {:pre  [(have? conversation/conversation? conversation
                 :data (me/humanize
                        (m/explain
                         (conversation/conversation-schema)
                         conversation)))]
   :post [(have? conversation/conversation? %
                 :data (me/humanize
                        (m/explain model/Conversation %)))]}
  (t/trace!
   {:id :thread/registered}
   (do
     (swap!
      conversation-store
      assoc (:id (:message-thread conversation)) conversation)
     conversation)))

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
  {:pre  [(have? conversation/conversation? conversation
                 :data (me/humanize
                        (m/explain
                         (conversation/conversation-schema)
                         conversation)))]
   :post [(have? conversation/conversation? conversation
                 :data (me/humanize
                        (m/explain
                         (conversation/conversation-schema)
                         conversation)))
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
  {:pre [(have? conversation/conversation? conversation
                :data (me/humanize
                       (m/explain
                        (conversation/conversation-schema)
                        conversation)))]}
  (t/trace!
   {:id :thread/removed}
   (let [c-id (conversation/id conversation)]
     (get-conversation-or-throw c-id)
     (swap! conversation-store dissoc c-id)
     nil)))

(defn lookup
  "Returns conversation with given id.
   Throws :error/unknown-message-thread-id if id not found."
  [id]
  {:pre  [(have? string? id)]
   :post [(have? conversation/conversation? %
                 :data (me/humanize
                        (m/explain (conversation/conversation-schema) %)))]}
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
