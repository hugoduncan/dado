(ns dado.tools.filesystem.interface
  "Interface for filesystem tool operations"
  (:require
    [dado.tools.filesystem.core :as core]))

(defn create-read-file-tool
  "Creates a tool for reading single files.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-read-file-tool))

(defn create-read-multiple-files-tool
  "Creates a tool for reading multiple files.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-read-multiple-files-tool))

(defn create-write-file-tool
  "Creates a tool for writing files.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-write-file-tool))

(defn create-edit-file-tool
  "Creates a tool for editing files with diffs.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-edit-file-tool))

(defn create-create-directory-tool
  "Creates a tool for creating directories.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-create-directory-tool))

(defn create-list-directory-tool
  "Creates a tool for listing directory contents.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-list-directory-tool))

(defn create-directory-tree-tool
  "Creates a tool for getting directory tree structure.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-directory-tree-tool))

(defn create-move-file-tool
  "Creates a tool for moving files and directories.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-move-file-tool))

(defn create-search-files-tool
  "Creates a tool for searching files.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-search-files-tool))

(defn create-get-file-info-tool
  "Creates a tool for getting file information.
   All paths must be relative and within allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-get-file-info-tool))

(defn create-list-allowed-directories-tool
  "Creates a tool for listing allowed directories.
   Returns a tool configuration map compatible with the AI Tool component."
  []
  (core/create-list-allowed-directories-tool))

(defn create-tool
  "Creates combined filesystem tool configuration.
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