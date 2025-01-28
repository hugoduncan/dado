(ns dado.ai.tools.git.interface
  "Git tools interface for AI interactions."
  (:require
   [dado.ai.tools.git.core :as core]))

(defn create-tool
  "Create git tool for the specified tool ID.
   Returns tool definition map or nil if ID not found."
  [tool-id]
  (core/create-tool tool-id))
