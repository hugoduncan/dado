(ns dado.patch.interface
  "Component for applying simplified unified diffs to files.
   Validates and applies patches in memory before making any file system changes."
  (:require [dado.patch.core :as core]))

(defn apply-simplified-diff-patch!
  "Applies a patch to files, returns map of results per file.
   The patch-content is a string containing a simplified unified diff.

   First applies all changes in memory, collecting any errors.
   If any errors occur during in-memory application, throws an
   ex-info containing a sequence of all errors encountered.

   Only proceeds with file system changes if no errors occurred
   during in-memory application.

   Returns a map of file paths to change statistics on success:
   {\"path/to/file\" {:lines-added <n>
                      :lines-removed <n>}}"
  [patch-content]
  (core/apply-simplified-diff-patch! patch-content))

(defn apply-search-replace-diff-patch!
  "Applies a patch to files, returns map of results per file.
   The patch-content is a string containing a simplified unified diff.

   First applies all changes in memory, collecting any errors.
   If any errors occur during in-memory application, throws an
   ex-info containing a sequence of all errors encountered.

   Only proceeds with file system changes if no errors occurred
   during in-memory application.

   Returns a map of file paths to change statistics on success:
   {\"path/to/file\" {:lines-added <n>
                      :lines-removed <n>}}"
  [patch-content]
  (core/apply-search-replace-diff-patch! patch-content))
