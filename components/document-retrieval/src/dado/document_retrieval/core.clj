(ns dado.document-retrieval.core
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.tools.deps :as deps]
   [clojure.tools.namespace.dependency :as ns-deps]
   [clojure.tools.namespace.dir :as ns-dir]
   [clojure.tools.namespace.file :as ns-file]
   [clojure.tools.namespace.parse :as ns-parse]
   [clojure.tools.namespace.track :as ns-track]
   [taoensso.telemere :as t]))

;;; tools.deps

(defn- deps-edn-path [root]
  (fs/file root "deps.edn"))

(defn- slurp-deps [root]
  (let [f (deps-edn-path root)]
    (when (fs/exists? root)
      (deps/slurp-deps f))))

(defn- apply-aliases [deps-map aliases]
  (deps/merge-edns
   [(deps/root-deps)
    deps-map
    (deps/combine-aliases deps-map aliases)]))

(defn- source-paths [deps-map]
  (into (:paths deps-map) (:extra-paths deps-map)))

(defn deps-map-with-aliases
  [root aliases]
  (when-let [deps-map (slurp-deps root)]
    (apply-aliases deps-map aliases)))

(comment
  (-> (deps-map-with-aliases "." [:test]))
  (-> (deps-map-with-aliases
       "/Users/duncan/projects/rpl/redef-mock/"
       [:test])))

(defn- local-roots
  [root aliases]
  (as-> root x
    (slurp-deps x)
    (apply-aliases x aliases)
    (:deps x)
    (vals x)
    (into
     #{}
     (comp (keep :local/root)
           (map #(fs/normalize (fs/path root %))))
     x)))

(defn deps-roots
  "Recursively discovers all local dependency roots from deps.edn files.

   Parameters:
     root - Starting directory path
   Returns:
     Set of all discovered dependency root paths"
  [root aliases]
  (let [root (fs/path root)]
    (loop [seen-roots                 #{root}
           [current-root & remaining] [root]]
      (if-not current-root
        seen-roots
        (let [new-roots (set/difference
                         (local-roots current-root aliases)
                         seen-roots)]
          (recur (into seen-roots new-roots)
                 (into remaining new-roots)))))))

(defn deps-source-paths
  "Returns all source paths from a deps.edn configuration.

   Parameters:
     root    - Root directory containing deps.edn
     aliases - Aliases to apply from deps.edn

   Returns:
     Vector of source paths from deps.edn with aliases applied"
  [root aliases]
  (when-let [deps-map (deps-map-with-aliases root aliases)]
    (into []
          (comp
           (map #(fs/path root %))
           (filter fs/exists?))
          (source-paths deps-map))))

(defn deps-source-files
  "Returns a vector of Clojure source files (.clj) found in project paths.

  Reads path configuration from deps.edn, defaulting to ['src'] if no
  paths specified.

  Parameters:
    root    - Root directory of the project containing deps.edn
    aliases - Aliases to apply from deps.edn

  Returns:
    Vector of paths to .clj files"
  [root aliases]
  (let [paths      (deps-source-paths root aliases)
        path-xform (comp #_(filter fs/exists?)
                         #_(map #(fs/path root %))
                         (mapcat #(fs/glob % "**.clj")))]
    (into [] path-xform paths)))

(defn all-deps-source-paths
  [root aliases]
  (into [] (mapcat #(deps-source-paths % aliases)) (deps-roots root aliases)))

(defn all-deps-files
  [root aliases]
  (into [] (mapcat #(deps-source-files % aliases)) (deps-roots root aliases)))


(defn markdown-files [root]
  (fs/list-dir root "*.md"))

(defn all-files
  ([root]
   (all-files root [:test]))
  ([root aliases]
   (into
    (all-deps-files root aliases)
    (markdown-files root))))

;;; Direct dependencies

(defonce ns-tracker (volatile! (ns-track/tracker)))

(defn- deps-graph
  [paths]
  (vswap! ns-tracker ns-dir/scan-dirs (mapv fs/file paths))
  (::ns-track/deps @ns-tracker))

(defn- src->test [p]
  (apply
   fs/path
   (into []
         (map (fn [c] (if (= "src" (fs/file-name c)) "test" c)))
         (fs/components p))))

#_(all-deps-source-paths "." [:test])
#_(all-deps-files "." [:test])

(defn- parent-deps-edn-files
  [file-path]
  (loop [current (fs/path file-path)
         acc     []]
    (let [deps-file (fs/path current "deps.edn")
          acc       (if (fs/exists? deps-file)
                      (conj acc deps-file)
                      acc)]
      (if current
        (recur (fs/parent current) acc)
        acc))))

(defn dependency-files
  [file-path]
  (try
    (let [paths      (all-deps-source-paths "." [:test])
          graph      (deps-graph paths)
          ns-sym     (ns-parse/name-from-ns-decl
                      (ns-file/read-file-ns-decl file-path))
          related-ns (into
                      (ns-deps/immediate-dependents graph ns-sym)
                      (ns-deps/immediate-dependencies graph ns-sym))
          ns->files  (-> @ns-tracker
                         ::ns-file/filemap
                         set/map-invert)
          deps-paths (into
                      #{}
                      (comp (keep ns->files)
                            (map #(fs/relativize (fs/cwd) %)) )
                      related-ns)
          test-file  (let [file-name  (fs/file-name file-path)
                           component  (fs/parent file-path)
                           iface-test (-> (fs/path
                                           component
                                           "interface_test.clj")
                                          src->test)]
                       (when (and (not= "interface.clj" file-name)
                                  (not (deps-paths iface-test))
                                  (fs/exists? iface-test))
                         iface-test))

          related-paths (reduce into #{}
                                [(cond-> [(fs/path file-path)]
                                   test-file (conj test-file))
                                 deps-paths
                                 (parent-deps-edn-files file-path)])]
      ;; sort provides a stable order
      (vec (sort related-paths)))
    (catch Exception e
      (prn :ignoring e)
      [file-path])))

(defn path->namespace
  [file-path]
  (let [paths (all-deps-source-paths "." [:test])]
    (deps-graph paths))                          ; to refresh tracker
  (t/trace! {:id ::path->namespace :level :warn :data {:file-path file-path}}
            (or ((::ns-file/filemap @ns-tracker) (fs/file file-path))
                ((::ns-file/filemap @ns-tracker) (fs/file (fs/cwd) file-path)))))

;;; Git operations

(defn- modified-files
  "Returns a sequence of file paths that have been modified since last commit.
   Files are returned as relative paths from the current working directory.

   Returns empty sequence if no files modified.
   Throws ex-info with :error/git if git command fails."
  []
  (try
    (let [{:keys [out]} (process/shell {:out :string}
                                       "git" "status" "--porcelain" "-uno")]
      (->> (str/split-lines out)
           (remove str/blank?)
           (map #(-> % (subs 3) str/trim))
           (filter (complement str/blank?))
           (into [])))
    (catch Exception e
      (throw (ex-info "Failed to get modified files"
                      {:type  :error/git
                       :cause e})))))

(defn- uncommitted-diffs
  "Returns unified diff of all uncommitted changes.
   Returns empty string if no changes.
   Throws ex-info with :error/git if git command fails."
  []
  (try
    (let [{:keys [out]} (process/shell {:out :string}
                                       "git" "diff")]
      out)
    (catch Exception e
      (throw (ex-info "Failed to get uncommitted diffs"
                      {:type  :error/git
                       :cause e})))))

(defn files-with-uncommitted-changes
  []
  (modified-files))

(defn git-dirty-files
  []
  (modified-files))

(defn git-uncommitted-diffs
  []
  (uncommitted-diffs))


(comment
  (all-files ".")

  (dependency-files
   "components/document-retrieval/src/dado/document_retrieval/core.clj")
  (dependency-files
   "components/document-retrieval/src/dado/document_retrieval/interface.clj")
  (dependency-files "README.md")
  (parent-deps-edn-files
   "components/document-retrieval/src/dado/document_retrieval/interface.clj")

  (modified-files)
  (uncommitted-diffs))
