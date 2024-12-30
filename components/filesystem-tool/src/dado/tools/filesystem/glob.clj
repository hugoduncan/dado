(ns dado.tools.filesystem.glob
  "Provides utilities for glob pattern matching against file paths."
  (:require [clojure.string :as str]))


(defn glob->regex
  "Convert a glob pattern to a regex pattern.
   Handles *, **, and ? wildcards."
  [pattern]
  (-> pattern
      (str/replace "." "\\.")
      (str/replace "**/" "###")  ; temp placeholder
      (str/replace "*" "[^/]*")
      (str/replace "?" "[^/]")
      (str/replace "###" ".*")))

(defn matches-glob?
  "Returns true if path matches the given glob pattern.

   Parameters:
     path    - String path to test
     pattern - Glob pattern to match against

   Example:
     (matches-glob? \"foo/bar.txt\" \"foo/*.txt\") ;=> true"
  [path pattern]
  (let [regex (re-pattern (str "^" (glob->regex pattern) "$"))]
    (boolean (re-matches regex path))))
