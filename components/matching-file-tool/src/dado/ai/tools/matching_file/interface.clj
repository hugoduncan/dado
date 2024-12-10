(ns dado.ai.tools.matching-file.interface
  "AI Tool for matching files in project based on content.
   Supports exact and regex pattern matching with context lines."
  (:require [dado.ai.tools.matching-file.core :as core]))

(defn tool-def
  "Returns tool definition map compatible with AI Tool component."
  []
  {:id           :dado/matching-file
   :name         "Matching File Tool"
   :description  "Searches project files for content matches and returns matching files."
   :structured-description
   {:claude
    {:description "Tool for finding files containing specific content. Supports exact and regex matching."}}
   :parameters
   [:map
    [:pattern :string]
    [:mode {:optional true} [:enum :exact :regex]]
    [:case-sensitive? {:optional true} :boolean]
    [:context-lines {:optional true} :int]
    [:max-matches {:optional true} :int]
    [:extensions {:optional true} [:vector :string]]]
   :returns
   {:type        :map
    :description "Map containing matched file paths and optional context"}
   :prompt-fn    (constantly "Use this tool to search for files containing specific content.")
   :recognize-fn #(boolean (re-find #"(?i)find files?|search.*files?" %))
   :execute-fn   core/execute-tool!})
