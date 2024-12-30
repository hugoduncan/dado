(ns dado.tools.filesystem.core
  "Core implementation of filesystem tool"
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [dado.ai.tool.model :as tool]
   [dado.tools.filesystem.diff :as diff]
   [dado.tools.filesystem.glob :as glob]
   [dado.tools.filesystem.model :as model]
   [jsonista.core :as j]
   [taoensso.truss :refer [have?]]
   [malli.error :as me]
   [malli.core :as m]))

(def operation-descriptions
  {:read-file
   "Read the complete contents of a file from the file system.
    - Handles various text encodings and provides detailed error messages
    - Use this tool when you need to examine the contents of a single file
    - Required: 'path'
    - File must exist and be readable"

   :read-multiple-files
   "Read the contents of multiple files simultaneously.
    - More efficient than reading files one by one when you need to analyze
      or compare multiple files
    - Each file's content is returned with its path as a reference
    - Failed reads for individual files won't stop the entire operation
    - Required: 'paths' (array)
    - Files must exist and be readable"

   :write-file
   "Create a new file or completely overwrite an existing file with new content.
    - Use with caution as it will overwrite existing files without warning
    - Handles text content with proper encoding
    - Required: 'path', 'content'
    - Parent directories will be created as needed"

   :edit-file
   "Make line-based edits to a text file.
    - Each edit replaces exact line sequences with new content
    - Returns a git-style diff showing the changes made
    - Required: 'path', 'edits' (array of {oldText, newText})
    - Optional: 'dryRun' (boolean)
    - File must exist"

   :create-directory
   "Create a new directory or ensure a directory exists.
    - Can create multiple nested directories in one operation
    - If the directory already exists, this operation will succeed silently
    - Perfect for setting up directory structures for projects
    - Required: 'path'"

   :list-directory
   "Get a detailed listing of all files and directories in a specified path.
    - Results clearly distinguish between files and directories with [FILE] and [DIR] prefixes
    - Essential for understanding directory structure and finding specific files
    - Required: 'path'
    - Directory must exist"

   :directory-tree
   "Get a recursive tree view of files and directories as a JSON structure.
    - Each entry includes 'name', 'type' (file/directory), and 'children' for directories
    - Files have no children array, while directories always have a children array
    - Output is formatted with 2-space indentation for readability
    - Required: 'path'
    - Directory must exist"

   :move-file
   "Move or rename files and directories.
    - Can move files between directories and rename them in a single operation
    - If the destination exists, the operation will fail
    - Works across different directories
    - Required: 'source', 'destination'
    - Source must exist
    - Destination must not exist"

   :search-files
   "Recursively search for files and directories matching a pattern.
    - Searches through all subdirectories from the starting path
    - The search is case-insensitive and matches partial names
    - Returns full paths to all matching items
    - Required: 'path', 'pattern'
    - Optional: 'excludePatterns' (array)"

   :get-file-info
   "Retrieve detailed metadata about a file or directory.
    - Returns comprehensive information including size, creation time, last modified time,
      permissions, and type
    - Perfect for understanding file characteristics without reading content
    - Required: 'path'
    - Path must exist"

   :list-allowed-directories
   "List all allowed directories for file operations.
    - Shows the base directories where file operations are permitted
    - No required parameters"})

(defn- all-descritptions []
  (str "Tool for performing filesystem operations safely within allowed directories.

  Supports reading, writing, editing, moving and deleting files, as well as
  directory operations like listing, creating and getting directory trees.

  All paths must be relative and within allowed directories.

  Operations:

"
       (str/join "

"
                 (for [[op desc] operation-descriptions]
                   (str "  " (str/upper-case (name op)) ":
" desc)))))

(defn- create-base-tool
  [id name description parameters]
  {:id           id
   :name         name
   :description  description
   :structured-description
   {:claude
    {:description
     "Tool for performing filesystem operations safely within allowed directories."}}
   :parameters   parameters
   :returns      {:type        :map
                  :description "Operation result with success/failure and data"}
   :prompt-fn    (constantly description)
   :recognize-fn (constantly false)})

(defn- create-operation-tool
  [operation]
  (let [description (str (str/upper-case (name operation)) ":\n"
                         (get operation-descriptions operation))]
    (create-base-tool
     (keyword "dado" (name operation))
     (str (str/capitalize (name operation)) " Tool")
     description
     (get model/file-operations operation))))

;; Path validation functions
(defn- normalize-path
  "Normalize path to canonical form"
  [path]
  (str (fs/normalize path)))

(defn- validate-path
  "Validates a file path is relative and within project.
   Returns normalized path if valid, throws if not."
  [path allowed-dirs]
  (let [path (str path)]
    (when-not (fs/relative? path)
      (throw (ex-info "Path must be relative"
                      {:type :error/invalid-path
                       :path path})))
    ;; Convert to absolute for validation
    (let [absolute (fs/absolutize path)
          normal   (normalize-path absolute)
          allowed? (some #(str/starts-with? normal (normalize-path %))
                         allowed-dirs)]
      (if allowed?
        path
        (throw (ex-info "Path outside allowed directories"
                        {:type :error/invalid-path
                         :path path}))))))

(defn- validate-paths
  "Validate multiple paths"
  [paths allowed-dirs]
  (mapv #(validate-path % allowed-dirs) paths))

(defn- get-file-stats
  "Get file metadata"
  [path]
  {:size         (fs/size path)
   :created      (fs/creation-time path)
   :modified     (fs/last-modified-time path)
   :is-directory (#'fs/directory? path)
   :is-file      (fs/regular-file? path)
   :permissions  (fs/posix-file-permissions path)})

;; Operation implementations
(defmulti execute-operation
  "Execute filesystem operation"
  (fn [op _] (:operation op)))

(defmethod execute-operation :read-file
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [path (validate-path (:path args) allowed-dirs)]
    {:content [{:type :text
                :text (slurp (fs/file path))}]}))

(defmethod execute-operation :read-multiple-files
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [paths    (validate-paths (:paths args) allowed-dirs)
        contents (map (fn [p]
                        (str p ":\n" (slurp (fs/file p))))
                      paths)]
    {:content [{:type :text
                :text (str/join "\n---\n" contents)}]}))

(defmethod execute-operation :write-file
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [path (validate-path (:path args) allowed-dirs)]
    (fs/create-dirs (fs/parent path))
    (spit (fs/file path) (:content args))
    {:content [{:type :text
                :text (str "Successfully wrote to " path)}]}))

(defmethod execute-operation :create-directory
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [path (validate-path (:path args) allowed-dirs)]
    (fs/create-dirs path)
    {:content [{:type :text
                :text (str "Successfully created directory " path)}]}))

(defmethod execute-operation :list-directory
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [path      (validate-path (:path args) allowed-dirs)
        entries   (fs/list-dir path)
        formatted (map #(str (if (fs/directory? %) "[DIR]" "[FILE]") " "
                             (fs/file-name %))
                       entries)]
    {:content [{:type :text
                :text (str/join "\n" formatted)}]}))

(defmethod execute-operation :get-file-info
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [path (validate-path (:path args) allowed-dirs)
        info (get-file-stats path)]
    {:content [{:type :text
                :text (str/join "\n"
                                (map (fn [[k v]]
                                       (str (name k) ": " v))
                                     info))}]}))

;; String normalization and diffing utilities
(defn- normalize-line-endings
  "Normalize line endings to \n"
  [text]
  (str/replace text #"\r\n" "\n"))

(defn- apply-edit
  "Apply a single edit operation to content"
  [content {:keys [old-text new-text]}]
  (if (str/includes? content old-text)
    (str/replace content old-text new-text)
    (let [lines       (str/split-lines content)
          old-lines   (str/split-lines old-text)
          content-len (count lines)]
      (loop [i 0]
        (if (>= i content-len)
          (throw (ex-info "Could not find exact match for edit"
                          {:old-text old-text}))
          (let [potential-match (take (count old-lines) (drop i lines))]
            (if (every? #(= (str/trim %1) (str/trim %2))
                        old-lines potential-match)
              (let [new-lines (str/split-lines new-text)
                    prefix    (take i lines)
                    suffix    (drop (+ i (count old-lines)) lines)]
                (str/join "\n" (concat prefix new-lines suffix)))
              (recur (inc i)))))))))

(defn- apply-file-edits
  "Apply edits to content and return [diff new-content]"
  [content edits filepath]
  (let [normalized-content (normalize-line-endings content)
        new-content        (reduce apply-edit normalized-content edits)
        diff               (diff/create-unified-diff
                            normalized-content
                            new-content
                            filepath)]
    [diff new-content]))

(defmethod execute-operation :edit-file
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [path               (validate-path (:path args) allowed-dirs)
        content            (slurp (fs/file path))
        [diff new-content] (apply-file-edits content (:edits args) path)]
    (when-not (:dry-run args)
      (spit (fs/file path) new-content))
    {:content [{:type :text
                :text (str "```diff\n" diff "```")}]}))

(defmethod execute-operation :directory-tree
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [path (validate-path (:path args) allowed-dirs)
        tree (fn tree [dir]
               (let [entries (fs/list-dir dir)]
                 (vec
                  (for [entry entries
                        :let  [name (fs/file-name entry)]]
                    (if (fs/directory? entry)
                      {:name     name
                       :type     :directory
                       :children (tree entry)}
                      {:name name
                       :type :file})))))]
    {:content [{:type :text
                :text (j/write-value-as-string
                       (tree path)
                       (j/object-mapper {:pretty true}))}]}))

(defmethod execute-operation :move-file
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [source      (validate-path (:source args) allowed-dirs)
        destination (validate-path (:destination args) allowed-dirs)]
    (fs/move source destination)
    {:content [{:type :text
                :text (str "Successfully moved " source " to " destination)}]}))

(defmethod execute-operation :search-files
  [{:keys [args]} {:keys [allowed-dirs]}]
  (let [root             (validate-path (:path args) allowed-dirs)
        exclude-patterns (set (:exclude-patterns args))
        results          (atom [])
        glob-pattern     (:pattern args)]
    (letfn [(should-exclude? [path]
              (let [p (str (fs/relativize root path))]
                (some #(glob/matches-glob? p %) exclude-patterns)))
            (search [dir]
              (doseq [entry (fs/glob dir glob-pattern)]
                (when-not (should-exclude? entry)
                  (swap! results conj (str entry)))))]
      (search root)
      {:content [{:type :text
                  :text (if (seq @results)
                          (str/join "\n" @results)
                          "No matches found")}]})))

(defmethod execute-operation :list-allowed-directories
  [_ {:keys [allowed-dirs]}]
  {:content [{:type :text
              :text (str "Allowed directories:\n"
                         (str/join "\n" allowed-dirs))}]})

;; Error handling wrapper
(defn execute-with-error-handling [op config]
  (try
    (execute-operation op config)
    (catch Exception e
      (let [data (ex-data e)]
        {:content  [{:type :text
                     :text (str "Error: " (or (:message data)
                                              (.getMessage e)))}]
         :is-error true}))))


(defn create-read-file-tool []
  (assoc (create-operation-tool :read-file)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :read-file :args params}
                        {:allowed-dirs ["."]}))))

(defn create-read-multiple-files-tool []
  (assoc (create-operation-tool :read-multiple-files)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :read-multiple-files :args params}
                        {:allowed-dirs ["."]}))))

(defn create-write-file-tool []
  (assoc (create-operation-tool :write-file)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :write-file :args params}
                        {:allowed-dirs ["."]}))))

(defn create-edit-file-tool []
  (assoc (create-operation-tool :edit-file)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :edit-file :args params}
                        {:allowed-dirs ["."]}))))

(defn create-create-directory-tool []
  (assoc (create-operation-tool :create-directory)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :create-directory :args params}
                        {:allowed-dirs ["."]}))))

(defn create-list-directory-tool []
  (assoc (create-operation-tool :list-directory)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :list-directory :args params}
                        {:allowed-dirs ["."]}))))

(defn create-directory-tree-tool []
  (assoc (create-operation-tool :directory-tree)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :directory-tree :args params}
                        {:allowed-dirs ["."]}))))

(defn create-move-file-tool []
  {:post [(have? tool/tool? %
                 :data (me/humanize (m/explain tool/Tool %)))]}
  (assoc (create-operation-tool :move-file)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :move-file :args params}
                        {:allowed-dirs ["."]}))))

(defn create-search-files-tool []
  (assoc (create-operation-tool :search-files)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :search-files :args params}
                        {:allowed-dirs ["."]}))))

(defn create-get-file-info-tool []
  (assoc (create-operation-tool :get-file-info)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :get-file-info :args params}
                        {:allowed-dirs ["."]}))))

(defn create-list-allowed-directories-tool []
  (assoc (create-operation-tool :list-allowed-directories)
         :execute-fn (fn [params]
                       (execute-with-error-handling
                        {:operation :list-allowed-directories :args params}
                        {:allowed-dirs ["."]}))))

(defn create-tool
  "Creates filesystem tool configuration"
  []
  {:id          :dado/filesystem
   :name        "Filesystem Tool"
   :description description
   :structured-description
   {:claude
    {:description
     {:read-file
      "Read the complete contents of a file from the file system.
    - Handles various text encodings and provides detailed error messages
    - Use this tool when you need to examine the contents of a single file
    - Required: 'path'
    - File must exist and be readable"

      :read-multiple-files
      "Read the contents of multiple files simultaneously.
    - More efficient than reading files one by one when you need to analyze
      or compare multiple files
    - Each file's content is returned with its path as a reference
    - Failed reads for individual files won't stop the entire operation
    - Required: 'paths' (array)
    - Files must exist and be readable"

      :write-file
      "Create a new file or completely overwrite an existing file with new content.
    - Use with caution as it will overwrite existing files without warning
    - Handles text content with proper encoding
    - Required: 'path', 'content'
    - Parent directories will be created as needed"

      :edit-file
      "Make line-based edits to a text file.
    - Each edit replaces exact line sequences with new content
    - Returns a git-style diff showing the changes made
    - Required: 'path', 'edits' (array of {oldText, newText})
    - Optional: 'dryRun' (boolean)
    - File must exist"

      :create-directory
      "Create a new directory or ensure a directory exists.
    - Can create multiple nested directories in one operation
    - If the directory already exists, this operation will succeed silently
    - Perfect for setting up directory structures for projects
    - Required: 'path'"

      :list-directory
      "Get a detailed listing of all files and directories in a specified path.
    - Results clearly distinguish between files and directories with [FILE] and [DIR] prefixes
    - Essential for understanding directory structure and finding specific files
    - Required: 'path'
    - Directory must exist"

      :directory-tree
      "Get a recursive tree view of files and directories as a JSON structure.
    - Each entry includes 'name', 'type' (file/directory), and 'children' for directories
    - Files have no children array, while directories always have a children array
    - Output is formatted with 2-space indentation for readability
    - Required: 'path'
    - Directory must exist"

      :move-file
      "Move or rename files and directories.
    - Can move files between directories and rename them in a single operation
    - If the destination exists, the operation will fail
    - Works across different directories
    - Required: 'source', 'destination'
    - Source must exist
    - Destination must not exist"

      :search-files
      "Recursively search for files and directories matching a pattern.
    - Searches through all subdirectories from the starting path
    - The search is case-insensitive and matches partial names
    - Returns full paths to all matching items
    - Required: 'path', 'pattern'
    - Optional: 'excludePatterns' (array)"

      :get-file-info
      "Retrieve detailed metadata about a file or directory.
    - Returns comprehensive information including size, creation time, last modified time,
      permissions, and type
    - Perfect for understanding file characteristics without reading content
    - Required: 'path'
    - Path must exist"

      :list-allowed-directories
      "List all allowed directories for file operations.
    - Shows the base directories where file operations are permitted
    - No required parameters"}}}
   :parameters
   [:map
    [:operation [:enum :read-file :read-multiple-files :write-file
                 :edit-file :create-directory :list-directory
                 :directory-tree :move-file :search-files
                 :get-file-info :list-allowed-directories]]
    [:args model/FileOperation]]
   :returns
   {:type        :map
    :description "Operation result with success/failure and data"}
   :prompt-fn    (constantly description)
   :recognize-fn (constantly false) ;; Not implemented yet
   :execute-fn   (fn [params]
                   (let [operation (if (string? params)
                                     (j/read-value params j/keyword-keys-object-mapper)
                                     params)]
                     (execute-with-error-handling operation
                                                  {:allowed-dirs ["."]})))})

                                        ;
