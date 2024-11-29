(ns dado.conversation-manager.core
  (:refer-clojure :exclude [list])
  (:require
   [dado.ai.message.interface :as message]
   [dado.ai.tool.interface :as tool]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]
   [malli.core :as m]
   [malli.error :as me]))

;; Thread store atom - holds map of thread-id to thread
(defonce ^:private thread-store (atom {}))

(defn register-new
  "Adds the message-thread to the in-memory store.
   Returns the message-thread unchanged.
   Throws :error/invalid-message-thread if validation fails."
  [message-thread]
  {:pre  [(have? message/message-thread? message-thread
                 :data (me/humanize
                        (m/explain message/message-thread-schema message-thread)))]
   :post [(have? message/message-thread? %
                 :data (me/humanize
                        (m/explain message/message-thread-schema %)))
          (have? #(=  %) message-thread)]}
  (t/trace!
   {:id :thread/registered}
   (do
     (swap! thread-store assoc (:id message-thread) message-thread)
     message-thread)))

(defn register-update
  "Updates stored message-thread.
   Returns the message-thread unchanged.
   Throws :error/unknown-message-thread-id if id not found.
   Throws :error/invalid-message-thread if validation fails."
  [message-thread]
  {:pre  [(have? message/message-thread? message-thread
                 :data (me/humanize
                        (m/explain message/message-thread-schema message-thread)))]
   :post [(have? message/message-thread? %
                 :data (me/humanize
                        (m/explain message/message-thread-schema %)))
          (have? #(= %) message-thread)]}
  (t/trace! {:id :thread/updated}
            (let [thread-id (:id message-thread)]
              (when-not (contains? @thread-store thread-id)
                (throw (ex-info "Unknown message thread ID"
                                {:type :error/unknown-message-thread-id
                                 :id   thread-id})))
              (swap! thread-store assoc thread-id message-thread)
              message-thread)))

(defn remove
  "Removes message-thread with given id.
   Returns nil.
   Throws :error/unknown-message-thread-id if id not found."
  [id]
  {:pre [(have? string? id)]}
  (t/trace!
   {:id :thread/removed}
   (do
     (when-not (contains? @thread-store id)
       (throw (ex-info "Unknown message thread ID"
                       {:type :error/unknown-message-thread-id
                        :id   id})))
     (swap! thread-store dissoc id)
     nil)))

(defn lookup
  "Returns message message-thread with given id.
   Throws :error/unknown-message-thread-id if id not found."
  [id]
  {:pre  [(have? string? id)]
   :post [(have? message/message-thread? %
                 :data (me/humanize
                        (m/explain message/message-thread-schema %)))]}
  (t/trace!
   {:id :thread/lookup}
   (or (get @thread-store id)
       (throw (ex-info "Unknown message thread ID"
                       {:type :error/unknown-message-thread-id
                        :id   id})))))

(defn add-ai-tool
  "Adds AI Tool to conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
  [conversation-id tool]
  {:pre [(have? string? conversation-id)
         (have? tool/validate-tool tool)]}
  (t/trace! {:id :conversation/tool-added}
            (let [conversation (lookup conversation-id)
                  tools (get-in conversation [:metadata :tools] [])
                  updated (assoc-in conversation 
                                   [:metadata :tools]
                                   (conj tools tool))]
              (register-update updated))))

(defn remove-ai-tool
  "Removes AI Tool from conversation.
   Returns updated conversation state.
   Throws :error/unknown-conversation-id if conversation not found.
   Throws :error/invalid-ai-tool if tool invalid."
  [conversation-id tool]
  {:pre [(have? string? conversation-id)
         (have? tool/validate-tool tool)]}
  (t/trace! {:id :conversation/tool-removed}
            (let [conversation (lookup conversation-id)
                  tools (get-in conversation [:metadata :tools] [])
                  updated (assoc-in conversation 
                                   [:metadata :tools]
                                   (filterv #(not= (:id tool) (:id %)) tools))]
              (register-update updated))))

(defn list
  "Returns sequence of registered message thread IDs.
   Returns empty sequence if no message threads registered."
  []
  (t/trace!
   {:id :thread/list}
   (keys @thread-store)))