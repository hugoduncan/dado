(ns dado.tools.file-operation.core
  "Core implementation of file operation tool"
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [dado.tools.file-operation.model :as model]
            [jsonista.core :as j]
            [malli.core :as m]
            [malli.error :as me]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have]]
            [malli.json-schema :as json]))

(defn- op-summary [{:keys [operation path target-path]}]
  {:operation operation :paths (filterv identity [path target-path])})

(defn- validate-path
  "Validates a file path is relative and within project"
  [path]
  (when-not (and path (fs/relative? path))
    (throw (ex-info "Path must be relative"
                    {:type :error/tool-validation
                     :path path})))
  path)

(defn- validate-operation
  "Validates a file operation map.
  If valid, returns nil, otherwiae an error map."
  [{:keys [operation path target-path content search-blocks] :as op}]
  (t/trace!
   {:id ::validate-operation :data {:op op}}
   (if-not (model/file-operation? op)
     [{:op op} {:error (me/humanize (m/explain model/FileOperation op))}]
     (if (not path)
       [(op-summary op) {:error "Path required" }]
       (do
         ;; Validate paths
         (validate-path path)
         (when target-path
           (validate-path target-path))

         ;; Validate required fields per operation
         (case operation
           :create (when-not content
                     [(op-summary op) {:error "Create requires content" }])
           :edit   (when-not search-blocks
                     [(op-summary op) {:error "Edit requires search blocks"}])
           :move   (when-not target-path
                     [(op-summary op) {:error "Move requires target path"}])
           :copy   (when-not target-path
                     [(op-summary op) { :error "Copy requires target path"}])
           :delete nil))))))

(defn- create-parent-dirs
  [path]
  (some-> path
          fs/parent
          fs/create-dirs))

(defn- write-changes!
  "Write changes to filesystem, returns true on success"
  [op-info new-content]
  (t/trace!
   {:id   ::write-changes!
    :data {:op op-info :content new-content}}
   (let [{:keys [operation path target-path]} op-info]
     (try
       (case operation
         (:edit :create)
         (do
           (when (= operation :create)
             (create-parent-dirs path))
           (let [temp-path (str path ".tmp")]
             (spit temp-path new-content)
             (fs/move temp-path path {:replace-existing (= operation :edit)})))

         :delete
         (fs/delete path)

         :move
         (do
           (create-parent-dirs target-path)
           (fs/move path target-path))

         :copy
         (do
           (create-parent-dirs target-path)
           (fs/copy path target-path)))
       [(op-summary op-info) nil]

       (catch Exception e
         (t/event! ::write-failed! {:data {:execption e}})
         [(op-summary op-info)
          {:error :write-failed :cause (ex-message e)}])))))

(defn- apply-search-block
  "Apply a single search block to content, returns [new-content error-info]"
  [content {:keys [search replace] :as op-info}]
  (t/trace!
   ::apply-search-block
   (let [n-context (count search)
         index     (str/index-of content search)
         post-str  (when index (subs content (+ index n-context)))]
     (cond
       (nil? index)
       (do
         (t/event! :fop/search-replace-failed
                   {:level :warn
                    :data  {:context search
                            :content content}})
         [content {:error   :error/patch-context-mismatch
                   :context {:op-info op-info
                             :content content}}])

       (and (seq search) (str/index-of post-str search))
       (do
         (t/event! :error/patch-failed
                   {:level :warn
                    :data  {:context search
                            :content content}})
         [content {:error   :error/patch-insufficient-context
                   :context {:op-info op-info
                             :context search
                             :content content}}])

       :else
       [(str (->> [(subs content 0 index) replace post-str]
                  (filterv (complement str/blank?))
                  (apply str)))
        nil]))))

(defn- apply-search-blocks
  "Apply all search blocks to content, returns [new-content errors]"
  [content search-blocks]
  (loop [current-content  content
         remaining-blocks search-blocks
         errors           []]
    (if (empty? remaining-blocks)
      [current-content errors]
      (let [[new-content error] (apply-search-block
                                 current-content
                                 (first remaining-blocks))]
        (recur new-content
               (rest remaining-blocks)
               (if error
                 (conj errors error)
                 errors))))))

(defn- apply-file-operation
  "Apply changes to a single file operation, returns [op-info error-info]"
  [op-info]
  (t/trace!
   {:id :fot/apply-file-operation :data {:op-info op-info}}
   (let [{:keys [operation path search-blocks]} op-info]
     (try
       (case operation
         :edit
         (let [current-content      (slurp path)
               [new-content errors] (apply-search-blocks
                                     current-content
                                     search-blocks )]
           (if (seq errors)
             [(op-summary op-info) {:errors errors}]
             [(-> op-info
                  (assoc :content new-content)
                  (dissoc :search-blocks))
              nil]))

         :create
         [op-info nil]

         :delete
         [op-info nil]

         (:move :copy)
         [op-info nil])
       (catch Exception e
         [(op-summary op-info)
          {:error :file-access :cause (ex-message e)}])))))

(defn execture-op! [op-info]
  (t/trace!
   {:id :fot/execute-op! :data {:op-info op-info}}
   (case (:operation op-info)
     (:edit)
     (write-changes! op-info (:content op-info))

     (:create)
     (write-changes! op-info (:content op-info))

     (:delete :move :copy)
     (write-changes! op-info nil))))

(defn- operation->kw [op-info]
  (update op-info :operation #(if (string? %) (keyword %) %)))

(defn execute-operations!
  "Executes file operations using patch component.
   Returns sequence of operation results."
  [{:keys [operations]}]
  (t/trace!
   {:id   ::execute-operations!
    :data {:operations operations}}

   (let [operations (mapv operation->kw operations)
         invalid    (not-empty (vec (keep validate-operation operations)))]
     (or
      invalid
      (let [ops-errors (mapv apply-file-operation operations)
            errors     (not-empty (vec (filterv second ops-errors)))]
        (or
         errors
         (mapv execture-op! (mapv first ops-errors))))))))

(defn- result-content [result]
  (t/trace!
   {:id ::result-content :data {:result result}}
   {:content
    (mapv
     (fn [[{:keys [operation paths]} error-map]]
       (have operation)
       {:type :text
        :text (if error-map
                (str (name operation) " on " (pr-str paths) " failed: "
                     (pr-str error-map))
                (str (name operation) " on " (pr-str paths) " succeeded"))})
     result)
    :is-error (boolean (some last result))}))

(def description
  "Tool for performing atomic file operations within a project directory.

  All paths must be relative and within the project directory. Operations are
  executed atomically - either all operations succeed or none are applied.

  The tool supports the following operations:

  CREATE:
    - Creates a new file at the specified path
    - Required: \"path\", \"content\"
    - The target path must not exist
    - Parent directories will be created as needed

  EDIT:
    - Modifies an existing file using search and replace
    - Required: \"path\", \"search-blocks\"
    - File must exist
    - Each search block must match exactly once
    - Search blocks are applied in order

  MOVE:
    - Moves a file to a new location
    - Required: \"path\", \"target-path\"
    - Source must exist
    - Target must not exist
    - Parent directories will be created as needed

  COPY:
    - Copies a file to a new location
    - Required: \"path\", \"target-path\"
    - Source must exist
    - Target must not exist
    - Parent directories will be created as needed

  DELETE:
    - Removes an existing file
    - Required: \"path\"
    - File must exist

  Examples:

  1. Creating and deleting files:
  <example>
  {\"operations\":
  [{\"operation\": \"create\",
    \"content\": \"some content\",
    \"path\": \"file/to/create..md\"},
   {\"operation\": \"delete\", \"path\": \"file/to/delete.clj\"}]}
  </example>

  2. Editing file content:
  <example>
  {\"operations\":
  [{\"operation\": \"edit\",
    \"path\": \"file/to/edit.md\",
  \"search-blocks\":
    [{\"search\": \"text to modify\",
      \"replace\": \"text it should be replaced by\"},
     {\"search\": \"other text to modify\",
      \"replace\": \"text it should be replaced by\"}]}]}
  </example>

  3. Moving and copying files:
  <example>
  {\"operations\":
  [{\"operation\": \"copy\",
    \"path\": \"file/to/copy.md\",
    \"target-path\": \"location/to/copy/to.md\"},
   {\"operation\": \"move\",
    \"path\": \"file/to/move.md\",
    \"target-path\": \"location/to/move/to.md\"}]}
  </example>")

(def describe-tool
  "To change files, use the `<file-operation>` tag.

  <example>
  <file-operation type=\"create\" path=\"components/document/deps.edn\">
  This is some content for the new file
  </file-operation>
  </example>

  <example>
  <file-operation type=\"delete\" path=\"components/document/deps.edn\">
  </file-operation>
  </example>

  <example>
  <file-operation
     type=\"copy\"
     path=\"components/document/deps.edn\"
     target-path=\"components/document/deps.edn\">
  </file-operation>
  </example>

  <example>
  <file-operation
     type=\"move\"
     path=\"components/document/deps.edn\"
     target-path=\"components/document/deps.edn\">
  </file-operation>
  </example>

  <example>
  <file-operation
     type=\"edit\"
     path=\"components/document/deps.edn\">
  <search>Text to be replaced</search>
  <replace>Replacement text</replace>
  </file-operation>
  </example>")

(defn- parse-search-replace [body]
  (let [re-sr #"(?s)\s*<search>(.*?)</search>\s*<replace>(.*?)</replace>"]
    (->> (re-seq re-sr body)
         (mapv (fn [[_ s r]] {:search s :replace r})))))

(def ^:private valid-ops #{:edit :copy :move :delete :create})

(defn extract-operations
  "Extracts file operation blocks from a string.
   Each block is expected to be in the format described in
  `describe-tool`.  Return a list of file operation maps, as could be
  passed to `execute-operations!`."
  [text]
  (t/trace!
   {:id   ::extract-operations
    :data {:text text}}
   (let [re-ops          #"(?s)(<file-operation[^>]*>)(.*?)</file-operation>"
         re-attr         #"(?i)([\w-]+)\s*=\s*\"([^\"]*)\""
         parse-operation (fn [attrs body]
                           (let [operation (keyword (:type attrs))
                                 op        {:operation operation
                                            :path      (:path attrs)}]
                             (case operation
                               :edit
                               (assoc op :search-blocks (parse-search-replace body))
                               (:copy :move)
                               (assoc op :target-path (:target-path attrs))
                               :delete
                               op
                               :create
                               (assoc op :content body)
                               nil)))]
     (->> (re-seq re-ops text)
          (mapv (fn [[_ tag-expr body]]
                  (let [attrs (->> (re-seq re-attr tag-expr)
                                   (mapv (fn [[_ k v]] [(keyword k) v]))
                                   (into {}))]
                    (parse-operation
                     attrs
                     (str/triml body)))))
          (filterv some?)
          #_(mapv (fn [op] (cond-> op
                             (= (:type op) :edit) (assoc :search-blocks (:content op)))))))))

(defn make-prompt
  "Returns tool usage prompt"
  []
  describe-tool)

(defn recognize-operation?
  "Returns true if text appears to be requesting file operations"
  [text]
  (assert (not :implemented)))

(defn create-tool
  "Creates file operation tool configuration"
  []
  {:id           :dado/file-operation
   :name         "File Operation Tool"
   :description  description
   :structured-description
   {:claude
    {:description
     "Tool for performing file operations. All paths must be relative and within project directory."}}
   :parameters
   [:map
    [:operations
     [:vector model/FileOperationParameter]]]
   :returns
   {:type        :vector
    :description "Sequence of operation results"}
   :prompt-fn    make-prompt
   :recognize-fn recognize-operation?
   :execute-fn   (comp result-content execute-operations!)})
