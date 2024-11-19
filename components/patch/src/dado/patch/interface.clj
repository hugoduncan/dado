(ns dado.patch.interface
  "Component for applying simplified unified diffs to files.
   Validates and applies patches in memory before making any file system changes."
  (:require
   [dado.patch.core.file-operation :as fod]
   [dado.patch.core.simplified-diff :as simplified-diff]))

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
  (simplified-diff/apply-simplified-diff-patch! patch-content))

(defn apply-fod-diff-patch!
  "Applies a File Operation Directive (FOD) patch to files.
   The patch-content is a string containing FOD operations.

   First applies all changes in memory, collecting any errors.
   If any errors occur during in-memory application, throws an
   ex-info containing a sequence of all errors encountered.

   Only proceeds with file system changes if no errors occurred
   during in-memory application.

   Returns a sequence of operation result maps indicating successful operations:
   [{:op :edit|:create|:delete|:move|:copy
     :paths [<affected-paths>]}]"
  [patch-content]
  (fod/apply-fod-diff-patch! patch-content))
