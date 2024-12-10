(ns dado.ai.tools.matching-file.model
  "Data models for file matching tool."
  (:require [malli.core :as m]))

(def SearchParams
  "Schema for file search parameters"
  [:map
   [:pattern :string]
   [:mode  [:enum :exact :regex]]
   [:case-sensitive? {:optional true} :boolean]
   [:context-lines {:optional true} :int]
   [:max-matches {:optional true} :int]
   [:extensions {:optional true} [:vector :string]]])

(def search-params? (m/validator SearchParams))

(def Match
  "Schema for a single file match result"
  [:map
   [:path :string]
   [:context {:optional true}
    [:sequential
     [:map
      [:line :string]
      [:line-number :int]]]]])

(def SearchResult
  "Schema for complete search results"
  [:map
   [:matches [:vector Match]]
   [:truncated? :boolean]])

(def search-result? (m/validator SearchResult))
