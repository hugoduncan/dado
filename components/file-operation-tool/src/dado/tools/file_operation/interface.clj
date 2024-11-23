(ns dado.tools.file-operation.interface
  "Interface for file operation tool"
  (:require [dado.tools.file-operation.core :as core]))

(defn create-tool
  "Creates file operation tool configuration.
   Tool executes file operations (create, edit, move, copy, delete).
   All operations are atomic with rollback on failure.
   
   Operations are specified as maps:
   - :operation - One of :create, :edit, :move, :copy, :delete
   - :path - Relative path to target file
   - :target-path - Required for :move/:copy, relative path for target
   - :content - Required for :create/:edit, file content
   - :search-blocks - Required for :edit, vector of search/replace maps
   
   Returns operation results:
   [{:operation <op-type>
     :path <file-path>
     :success <boolean>
     :error <string, optional>}]"
  []
  (core/create-tool))
