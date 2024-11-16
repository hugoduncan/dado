(ns dado.ai.message.core
  (:require [dado.ai.message.model :as model]
            [babashka.fs :as fs]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have?]]
            [malli.error :as me]
            [clojure.string :as str]))

(defn create-message-thread
  "Creates a new message thread with given metadata"
  [model & {:keys [system-prompt]}]
  (t/trace! {:id :message/created}
            (let [message-thread {:id         (str (random-uuid))
                                  :created-at (java.time.Instant/now)
                                  :messages   []
                                  :metadata   {:model model}}]
              (if system-prompt
                (assoc-in message-thread [:metadata :system-prompt] system-prompt)
                message-thread))))

(defn add-message
  "Adds a message to a message thread"
  [message-thread message]
  {:pre [(have? model/message-thread? message-thread
                :data (me/humanize (m/explain model/MessageThread message-thread)))
         (have? model/message? message
                :data (me/humanize (m/explain model/Message message)))]}
  (t/trace! {:id :message/added-to-message-thread}
            (update message-thread :messages conj message)))

(defn add-context-file
  "Adds a file's content to the message thread context"
  [message-thread file-path]
  {:pre [(have? model/message-thread? message-thread)]}
  (t/trace! {:id :message/file-added}
            (let [content      (slurp file-path)
                  file-context {:name    (str file-path)
                                :content content}]
              (update-in message-thread
                         [:metadata :context :files]
                         (fnil conj [])
                         file-context))))

(defn add-response
  "Adds a response message to the message thread"
  [message-thread response]
  {:pre [(have? model/message-thread? message-thread)
         (have? model/response-message? response)]}
  (t/trace! {:id :message/response-added}
            (update message-thread :messages conj response)))

(def file-block-regex #"```(\w+)\n[;#]+\s*(.+?)\n([\s\S]*?)```")

(defn extract-file-blocks
  "Extracts file blocks from a response content string"
  [response]
  {:pre [(have? model/response-message? response)]}
  (t/trace! {:id :message/code-extracted}
            (let [matches (re-seq file-block-regex (:content response))]
              (map (fn [[_ lang name content]]
                     {:language lang
                      :name     name
                      :content  (str/trim content)
                      :metadata {:block-type :text
                                 :name       name}})
                   matches))))


#_(defn extract-file-blocks [response]
    (t/trace!
     {:id :message/code-extracted}
     (let [content            (:content response)
           code-block-pattern #"```(\w+)\n[;#]+\s*(.+?)\n([\s\S]*?)```"
           matches            (re-seq code-block-pattern content)]
       (for [[_ lang name-comment code] matches]
         (let [block-type (if (re-find #"^@@" code) :unified-diff :text)
               name       (or (and name-comment (str/trim name-comment))
                              "unnamed")]
           {:language lang
            :name     name
            :content  (str/trim code)
            :metadata {:block-type block-type
                       :name       name}})))))
