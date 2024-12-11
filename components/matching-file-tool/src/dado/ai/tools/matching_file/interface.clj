(ns dado.ai.tools.matching-file.interface
  "AI Tool for matching files in project based on content.
   Supports exact and regex pattern matching with context lines."
  (:require [dado.ai.tools.matching-file.core :as core]))

(defn create-tool
  "Creates a refactoring agent for code modifications."
  []
  (core/create-tool))
