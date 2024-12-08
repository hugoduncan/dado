(ns dado.document-retrieval.core
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.tools.deps :as deps]))

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

(defn deps-source-files
  "Returns a vector of Clojure source files (.clj) found in project paths.

  Reads path configuration from deps.edn, defaulting to ['src'] if no
  paths specified.

  Parameters:
    root - Root directory of the project containing deps.edn

  Returns:
    Vector of paths to .clj files"
  [root aliases]
  (let [paths      (-> root
                       slurp-deps
                       (apply-aliases aliases)
                       source-paths)
        path-xform (comp (filter fs/exists?)
                         (map #(fs/path root %))
                         (mapcat #(fs/glob % "**.clj")))]
    (into [] path-xform paths)))

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

(comment
  (all-files ".")
  (all-files "/Users/duncan/projects/rpl/redef-mock/" [:test])
  (all-files "/Users/duncan/projects/rpl/redef-mock/"))
