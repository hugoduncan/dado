(ns dado.document-retrieval.core
  (:require
   [babashka.fs :as fs]
   [clojure.set :as set]
   [clojure.tools.deps :as deps]
   [clojure.tools.namespace.dependency :as ns-deps]
   [clojure.tools.namespace.dir :as ns-dir]
   [clojure.tools.namespace.file :as ns-file]
   [clojure.tools.namespace.parse :as ns-parse]
   [clojure.tools.namespace.track :as ns-track]))

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

#_(all-deps-source-paths "." [:test])
#_(all-deps-files "." [:test])

(defn dependency-files
  [root]
  (try
    (let [paths         (all-deps-source-paths "." [:test])
          graph         (deps-graph paths)
          ns-sym        (ns-parse/name-from-ns-decl
                         (ns-file/read-file-ns-decl root))
          related-ns    (into
                         (ns-deps/immediate-dependents graph ns-sym)
                         (ns-deps/immediate-dependencies graph ns-sym))
          ns->files     (-> @ns-tracker
                            ::ns-file/filemap
                            set/map-invert)
          related-paths (into
                         [(fs/path root)]
                         (comp (keep ns->files)
                               (map #(fs/relativize (fs/cwd) %)) )
                         related-ns)]
      ;; sort provides a stable order
      (vec (sort related-paths)))
    (catch Exception e
      (prn :ignoring e)
      [root])))

(comment
  (all-files ".")

  (dependency-files
   "components/document-retrieval/src/dado/document_retrieval/core.clj")
  (dependency-files
   "components/document-retrieval/src/dado/document_retrieval/interface.clj")
  (dependency-files "README.md"))
