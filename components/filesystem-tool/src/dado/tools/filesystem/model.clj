(ns dado.tools.filesystem.model
  "Data models for filesystem tool operations"
  (:require [malli.core :as m]))

;; Basic path validation schema
(def RelativePath
  [:string {:min 1}])

;; Operation specific parameter schemas
(def ReadFileArgs
  [:map
   [:path RelativePath]])

(def ReadMultipleFilesArgs
  [:map
   [:paths [:vector RelativePath]]])

(def WriteFileArgs
  [:map
   [:path RelativePath]
   [:content :string]])

(def EditOperation
  [:map
   [:old-text :string]
   [:new-text :string]])

(def EditFileArgs
  [:map
   [:path RelativePath]
   [:edits [:vector EditOperation]]
   [:dry-run {:optional true} :boolean]])

(def CreateDirectoryArgs
  [:map
   [:path RelativePath]])

(def ListDirectoryArgs
  [:map
   [:path RelativePath]])

(def DirectoryTreeArgs
  [:map
   [:path RelativePath]])

(def MoveFileArgs
  [:map
   [:source RelativePath]
   [:destination RelativePath]])

(def SearchFilesArgs
  [:map
   [:path RelativePath]
   [:pattern :string]
   [:exclude-patterns {:optional true} [:vector :string]]])

(def GetFileInfoArgs
  [:map
   [:path RelativePath]])

(def FileOperation
  [:multi {:dispatch :operation}
   [:read-file ReadFileArgs]
   [:read-multiple-files ReadMultipleFilesArgs]
   [:write-file WriteFileArgs]
   [:edit-file EditFileArgs]
   [:create-directory CreateDirectoryArgs]
   [:list-directory ListDirectoryArgs]
   [:directory-tree DirectoryTreeArgs]
   [:move-file MoveFileArgs]
   [:search-files SearchFilesArgs]
   [:get-file-info GetFileInfoArgs]
   [:list-allowed-directories [:map]]])

;; Result schemas
(def FileInfo
  [:map
   [:size :int]
   [:created inst?]
   [:modified inst?]
   [:accessed inst?]
   [:is-directory :boolean]
   [:is-file :boolean]
   [:permissions :string]])

(def DirectoryEntry
  [:map
   [:name :string]
   [:type [:enum :file :directory]]
   [:children {:optional true} [:vector [:ref #'DirectoryEntry]]]])

;; Create validators
(def relative-path? (m/validator RelativePath))
(def file-operation? (m/validator FileOperation))
(def file-info? (m/validator FileInfo))
(def directory-entry? (m/validator DirectoryEntry))
