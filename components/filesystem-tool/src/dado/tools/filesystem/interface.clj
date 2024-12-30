(ns dado.tools.filesystem.interface
  "Interface for filesystem tool"
  (:require [dado.tools.filesystem.core :as core]))

(defn create-tool
  "Creates filesystem tool configuration.
   Tool provides filesystem operations within allowed directories.
   
   Supports operations for:
   - Reading files (single and multiple)
   - Writing files
   - Editing files with diffs
   - Directory creation and listing
   - File/directory moving
   - File searching
   - File metadata retrieval
   
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-tool))
