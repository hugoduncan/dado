(ns dado.ai.message.core
  (:require [babashka.fs :as fs]
            [dado.ai.message.model :as model]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have?]]
            [malli.core :as m]
            [malli.error :as me]
            [clojure.string :as str]
            [dado.update-extractor.interface :as extractor]))

(defn create-message
  "Creates a new message with the given role and content"
  [role content & {:keys [name]}]
  (t/trace! {:id :message/created}
            (let [message {:role    role
                           :content content}]
              (if name
                (assoc message :name name)
                message))))

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

(defn update-system-prompt
  "Updates the system prompt for a message thread"
  [message-thread system-prompt]
  {:pre [(have? model/message-thread? message-thread
                :data (me/humanize (m/explain model/MessageThread message-thread)))
         (have? string? system-prompt)]}
  (t/trace! {:id :message/system-prompt-updated}
            (assoc-in message-thread [:metadata :system-prompt] system-prompt)))

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

(defn set-context-files
  "Set the file contexts on the message thread context."
  [message-thread context-file-paths]
  (reduce
   add-context-file
   (assoc-in message-thread [:metadata :context :files] [])
   context-file-paths))

(defn add-response
  "Adds a response message to the message thread"
  [message-thread response]
  {:pre [(have? model/message-thread? message-thread)
         (have? model/response-message?
                response
                :data (me/humanize (m/explain model/ResponseMessage response)))]}
  (t/trace! {:id :message/response-added}
            (update message-thread :messages conj (dissoc response :usage))))

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

(defn extract-simplified-diffs
  "Extracts simplified diff blocks from a response content string"
  [response]
  {:pre [(have? model/response-message? response)]}
  (t/trace! {:id :message/diff-extracted}
            (extractor/extract-simplified-diffs (:content response))))

(defn extract-file-operation-directives
  "Extracts File Operation Directives from a response content string"
  [response]
  {:pre [(have? model/response-message? response)]}
  (t/trace! {:id :message/diff-extracted}
            (extractor/extract-file-operation-directives (:content response))))

(defn extract-updated-namespaces
  "Extracts updated namespaces list from an AI response message"
  [response]
  {:pre [(have? model/response-message? response)]}
  (t/trace! {:id :message/updated-namespaces-extracted}
            (extractor/extract-updated-namespaces (:content response))))
