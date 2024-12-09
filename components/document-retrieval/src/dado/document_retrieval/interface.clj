(ns dado.document-retrieval.interface
  (:require
   [dado.document-retrieval.core :as core]))

(defn all-files
  [root]
  (core/all-files root))

(defn dependency-files
  [file-path]
  (core/dependency-files file-path))

(defn git-uncommitted-diffs
  []
  (core/git-uncommitted-diffs))

(defn path->namespace
  [file-path]
  (core/path->namespace file-path))
