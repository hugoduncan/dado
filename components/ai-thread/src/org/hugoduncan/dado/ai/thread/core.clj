(ns org.hugoduncan.dado.ai.thread.core
  "Thread management for AI conversations."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn :exclude [thread]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.hugoduncan.dado.ai.thread.model :as model]))

;; Configuration

(def ^:private thread-root "dev/ai/threads")

;; File Operations

(defn- ensure-thread-dir!
  "Ensures the thread directory exists."
  []
  (fs/create-dirs thread-root))

(defn- thread-path
  "Returns the full path for a thread directory."
  [thread-id]
  (str thread-root "/" thread-id))

(defn- save-edn!
  "Saves EDN data to file atomically."
  [file-path data]
  (let [tmp-file (str file-path ".tmp")]
    (try
      (with-open [w (io/writer tmp-file)]
        (binding [*out*          w
                  *print-length* nil
                  *print-level*  nil]
          (pr data)))
      (fs/move tmp-file file-path {:replace-existing true})
      (catch Exception e
        (fs/delete-if-exists tmp-file)
        (throw (ex-info "Failed to save EDN file"
                        {:type  :thread/io-error
                         :path  file-path
                         :cause (.getMessage e)}
                        e))))))

(defn- load-edn
  "Loads EDN data from file. Returns nil if file doesn't exist."
  [file-path]
  (try
    (when (fs/exists? file-path)
      (with-open [r (java.io.PushbackReader. (io/reader file-path))]
        (edn/read r)))
    (catch Exception e
      (throw (ex-info "Failed to load EDN file"
                      {:type  :thread/io-error
                       :path  file-path
                       :cause (.getMessage e)}
                      e)))))

;; Utility Functions

(defn- format-timestamp
  "Formats instant as yyyyMMddHHmmss"
  [instant]
  (.format
   (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
   (java.time.LocalDateTime/ofInstant
    instant
    (java.time.ZoneId/systemDefault))))

(defn- ->slug
  "Converts title to slug format"
  [title]
  (-> title
      str/lower-case
      (str/replace #"[^\w\s-]" "")
      (str/replace #"\s+" "_")
      (str/replace #"-+" "_")
      (str/replace #"^_+|_+$" "")))

(defn- valid-thread-id?
  "Validates thread ID format"
  [id]
  (boolean
   (when (string? id)
     (re-matches #"\d{14}-[a-z0-9_]+" id))))

;; Validation

(defn- validate-thread!
  "Validates thread data structure"
  [{:keys [id created-at updated-at title status] :as thread}]
  (when-not (valid-thread-id? id)
    (throw (ex-info "Invalid thread ID format"
                    {:type   :thread/invalid-id
                     :thread thread})))
  (when-not (instance? java.time.Instant created-at)
    (throw (ex-info "Invalid created-at timestamp"
                    {:type   :thread/invalid-data
                     :thread thread})))
  (when-not (instance? java.time.Instant updated-at)
    (throw (ex-info "Invalid updated-at timestamp"
                    {:type   :thread/invalid-data
                     :thread thread})))
  (when-not (string? title)
    (throw (ex-info "Invalid title"
                    {:type   :thread/invalid-data
                     :thread thread})))
  (when-not (#{:active :archived} status)
    (throw (ex-info "Invalid status"
                    {:type   :thread/invalid-data
                     :thread thread}))))

(defn- validate-message!
  "Validates message data structure"
  [{:keys [id timestamp role content] :as message}]
  (when-not (uuid? id)
    (throw (ex-info "Invalid message ID"
                    {:type    :thread/invalid-data
                     :message message})))
  (when-not (instance? java.time.Instant timestamp)
    (throw (ex-info "Invalid timestamp"
                    {:type    :thread/invalid-data
                     :message message})))
  (when-not (#{:human :assistant} role)
    (throw (ex-info "Invalid role"
                    {:type    :thread/invalid-data
                     :message message})))
  (when-not (string? content)
    (throw (ex-info "Invalid content"
                    {:type    :thread/invalid-data
                     :message message}))))

;; Thread Operations

(defn create-thread
  "Creates a new thread with given title and optional context.
   Returns thread record."
  [title & {:keys [context metadata]}]
  (ensure-thread-dir!)
  (let [now        (java.time.Instant/now)
        timestamp  (format-timestamp now)
        slug       (->slug title)
        thread-id  (str timestamp "-" slug)
        thread     (->MessageThread thread-id now now title :active metadata)
        thread-dir (thread-path thread-id)]
    (validate-thread! thread)
    (if (fs/exists? thread-dir)
      (throw (ex-info "Thread already exists"
                      {:type      :thread/exists
                       :thread-id thread-id}))
      (do
        (fs/create-dirs thread-dir)
        (save-edn! (str thread-dir "/thread.edn") thread)
        (save-edn! (str thread-dir "/messages.edn") [])
        (save-edn! (str thread-dir "/context.edn")
                   (->MessageThreadContext
                    (:documents context [])
                    (:metadata context {})))
        thread))))

(defn get-thread
  "Retrieves a thread by ID. Returns thread record or nil if not found."
  [thread-id]
  (when (valid-thread-id? thread-id)
    (let [thread-dir (thread-path thread-id)]
      (when (fs/exists? thread-dir)
        (load-edn (str thread-dir "/thread.edn"))))))

(defn list-threads
  "Lists all threads, sorted by timestamp.
   Optional filter-fn can be provided."
  ([] (list-threads (constantly true)))
  ([filter-fn]
   (ensure-thread-dir!)
   (->> (fs/list-dir thread-root)
        (filter #(fs/directory? %))
        (map #(get-thread (fs/file-name %)))
        (remove nil?)
        (filter filter-fn)
        (sort-by :created-at))))

(defn add-message
  "Adds a message to thread. Returns updated thread."
  [thread-id role content & {:keys [context-refs metadata]}]
  (when-let [thread (get-thread thread-id)]
    (let [thread-dir (thread-path thread-id)
          message    (->Message
                      (random-uuid)
                      (java.time.Instant/now)
                      role
                      content
                      (vec context-refs)
                      metadata)]
      (validate-message! message)
      (let [messages-file  (str thread-dir "/messages.edn")
            messages       (load-edn messages-file)
            updated-thread (assoc thread
                                  :updated-at (:timestamp message))]
        (validate-thread! updated-thread)
        (save-edn! (str thread-dir "/thread.edn") updated-thread)
        (save-edn! messages-file (conj messages message))
        updated-thread))))

(defn get-messages
  "Retrieves all messages for a thread."
  [thread-id]
  (when (get-thread thread-id)
    (load-edn (str (thread-path thread-id) "/messages.edn"))))

(defn get-context
  "Retrieves thread context."
  [thread-id]
  (when (get-thread thread-id)
    (load-edn (str (thread-path thread-id) "/context.edn"))))

(defn update-context
  "Updates thread context. Returns updated thread."
  [thread-id context-updates]
  (when-let [thread (get-thread thread-id)]
    (let [thread-dir      (thread-path thread-id)
          current-context (get-context thread-id)
          updated-context (->MessageThreadContext
                           (vec (distinct
                                 (concat (:documents current-context)
                                         (:documents context-updates))))
                           (merge (:metadata current-context)
                                  (:metadata context-updates)))
          updated-thread  (assoc thread
                                 :updated-at (java.time.Instant/now))]
      (validate-thread! updated-thread)
      (save-edn! (str thread-dir "/thread.edn") updated-thread)
      (save-edn! (str thread-dir "/context.edn") updated-context)
      updated-thread)))

(defn archive-thread
  "Archives a thread. Returns updated thread."
  [thread-id]
  (when-let [thread (get-thread thread-id)]
    (let [updated-thread (assoc thread
                                :status :archived
                                :updated-at (java.time.Instant/now))]
      (validate-thread! updated-thread)
      (save-edn! (str (thread-path thread-id) "/thread.edn")
                 updated-thread)
      updated-thread)))
