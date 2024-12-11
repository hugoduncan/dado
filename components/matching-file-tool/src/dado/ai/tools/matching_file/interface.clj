(ns dado.ai.tools.matching-file.interface
  "AI Tool for matching files in project based on content.
   Supports exact and regex pattern matching with context lines."
  (:require [dado.ai.tools.matching-file.core :as core]))

(defn create-agent
  create-agent
  "Creates a refactoring agent for code modifications."
  [project-config additional-context-fn]
  (core/create-agent project-config additional-context-fn))
