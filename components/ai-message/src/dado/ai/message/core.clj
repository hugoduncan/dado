(ns dado.ai.message.core
  (:require [babashka.fs :as fs]
            [dado.ai.message.model :as model]
            [dado.ai.tool.interface :as tool]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have?]]
            [malli.core :as m]
            [malli.error :as me]
            [clojure.string :as str]
            [dado.update-extractor.interface :as extractor]))

(defn text-content
  "Creates a text content map from a string"
  [text]
  {:type :text :text text})

(defn tool-result-content
  "Creates a tool result content map"
  [{:keys [tool-use-id content is-error]}]
  (cond-> {:type :tool-result
           :tool-use-id tool-use-id
           :content content}
    (some? is-error) (assoc :is-error is-error)))

(defn create-message
  "Creates a new message with the given role and no content"
  [role & {:keys [name]}]
  {:pre [(have? model/role? role
                :data (me/humanize (m/explain model/Role role)))]}
  (t/trace! {:id :message/created}
            (let [message {:role    role
                           :content []}]
              (if name
                (assoc message :name name)
                message))))

(defn add-message-content
  "Adds content to a message"
  [message content]
  (update message :content conj content))


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
  "Adds a file's content to the message thread context.
   If there is no existing sequence in the context files,
   starts a new sequence. If there is an existing sequence,
   adds to the last sequence."
  [message-thread file-path]
  {:pre [(have? model/message-thread? message-thread)]}
  (t/trace! {:id :message/file-added}
            (let [content      (slurp file-path)
                  file-context {:name    (str file-path)
                                :content content}]
              (update-in message-thread
                         [:metadata :context :files]
                         (fn [files]
                           (let [files (or files [[]])]
                             (if (empty? files)
                               [[file-context]]
                               (update files
                                       (dec (count files))
                                       conj
                                       file-context))))))))

(defn- file-path->content-map
  [file-path]
  {:pre [(have? (some-fn
                 string?
                 #(instance? java.nio.file.Path %)
                 #(instance? java.io.File %)) file-path)]}
  {:name    (str file-path)
   :content (slurp (fs/file file-path))})

(defn add-context-file-sequence
  "Starts a new sequence in the context files and adds the file to it."
  [message-thread file-sequence]
  {:pre [(have? model/message-thread? message-thread)
         (have? sequential? file-sequence)]}
  (t/trace! {:id :message/file-sequence-added}
            (update-in message-thread
                       [:metadata :context :files]
                       (fnil conj [])
                       (mapv file-path->content-map file-sequence))))

(defn set-context-files
  "Set the file contexts on the message thread context.
   Takes a sequence of sequences of file paths.
   Each inner sequence becomes a sequence in the context files."
  [message-thread context-file-sequences]
  (reduce
   (fn [message-thread file-paths]
     (add-context-file-sequence message-thread file-paths))
   (assoc-in message-thread [:metadata :context :files] [])
   context-file-sequences))

(defn register-tools
  "Registers tools for use in message thread"
  [message-thread tools]
  {:pre [(have? model/message-thread? message-thread)
         (have? tool/validate-tool :in tools)]}
  (t/trace!
   {:id :message/tools-registered}
   (assoc-in message-thread [:metadata :tools]  tools)))

(defn add-tool
  "Adds a tool to the message thread"
  [message-thread tool]
  {:pre [(have? model/message-thread? message-thread)
         (have? tool/validate-tool tool)]}
  (t/trace!
   {:id :message/tool-call-added}
   (update-in message-thread
              [:metadata :tools]
              (fnil conj [])
              tool)))

(defn registered-tools
  "Return the registered tools from the message thread. "
  [message-thread]
  (get-in message-thread [:metadata :tools]))

(defn add-tool-result
  "Adds a tool execution result to a tool call in the message thread"
  [message-thread tool-call-id result]
  {:pre [(have? model/message-thread? message-thread)
         (have? string? tool-call-id)]}
  (t/trace! {:id :message/tool-result-added}
            (update-in message-thread
                       [:metadata :tool-calls]
                       (fn [calls]
                         (mapv #(if (= tool-call-id (:id %))
                                  (assoc % :output result)
                                  %)
                               calls)))))

(defn add-response
  "Adds a response message to the message thread"
  [message-thread response]
  {:pre [(have? model/message-thread? message-thread)
         (have? model/response-message?
                response
                :data (me/humanize
                       (m/explain model/ResponseMessage response)))]}
  (t/trace! {:id :message/response-added}
            (cond-> message-thread
              true (update :messages conj (dissoc response :usage))
              (:tool-calls response)
              (update-in [:metadata :tool-calls] (fnil into []) (:tool-calls response)))))

(defn extract-tool-calls
  "Extracts tool calls from an AI response message."
  [response]
  (t/trace!
   {:id :message/tool-calls-extracted}
   (->> response
        :content
        (filterv (comp (partial = :tool-call) :type)))))

(def file-block-regex #"```(\w+)\n[;#]+\s*(.+?)\n([\s\S]*?)```")

(defn extract-file-blocks
  "Extracts file blocks from a response content string"
  [response]
  {:pre [(have? model/response-message? response
                :data (me/humanize
                       (m/explain model/ResponseMessage response)))]}
  (t/trace!
   {:id :message/code-extracted}
   (let [matches (re-seq file-block-regex (:text (first (:content response))))]
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
  {:pre [(have? model/response-message? response
                :data (me/humanize
                       (m/explain model/ResponseMessage response)))]}
  (t/trace!
   {:id :message/diff-extracted}
   (->> response
        :content
        (filterv (comp (some-fn nil? (partial = :text)) :type))
        (mapv (comp extractor/extract-simplified-diffs :text))
        (str/join "\n"))))

(defn extract-file-operation-directives
  "Extracts File Operation Directives from a response content string"
  [response]
  {:pre [(have? model/response-message? response
                :data (me/humanize
                       (m/explain model/ResponseMessage response)))]}
  (t/trace!
   {:id :message/diff-extracted}
   (->> response
        :content
        (filterv (comp (some-fn nil? (partial = :text)) :type))
        (mapv (comp extractor/extract-file-operation-directives  :text))
        (str/join "\n"))))

(defn extract-updated-namespaces
  "Extracts updated namespaces list from an AI response message"
  [response]
  {:pre [(have? model/response-message? response
                :data (me/humanize
                       (m/explain model/ResponseMessage response)))]}
  (t/trace!
   {:id :message/updated-namespaces-extracted}
   (->> response
        :content
        (filterv (comp (some-fn nil? (partial = :text)) :type))
        (mapv (comp extractor/extract-updated-namespaces  :text))
        (apply concat)
        vec)))

(defn message-thread-id
  [message-thread]
  {:pre [(have? model/message-thread? message-thread)]}
  (:id message-thread))
