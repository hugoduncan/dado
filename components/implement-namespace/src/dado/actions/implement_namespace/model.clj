(ns dado.actions.implement-namespace.model
  "Data models for namespace implementation"
  (:require [malli.core :as m]))

(def Options
  "Schema for implementation options"
  [:map
   [:mode {:optional true} [:enum :interactive :non-interactive]]
   [:allow-overwrite {:optional true} boolean?]])

(def ValidationResult
  "Schema for ADR validation result"
  [:map
   [:valid? boolean?]
   [:errors {:optional true} [:sequential string?]]])

(def ComponentInfo
  "Schema for component information"
  [:map
   [:name string?]
   [:base-namespace symbol?]
   [:interface-ns symbol?]
   [:core-ns symbol?]
   [:dependencies [:sequential [:map [:lib symbol?] [:version string?]]]]])

(def ImplementationResult
  "Schema for implementation results"
  [:map
   [:component-name string?]
   [:component-path string?]
   [:files-created [:sequential string?]]
   [:namespaces-created [:sequential symbol?]]
   [:dependencies [:sequential [:map [:lib symbol?] [:version string?]]]]
   [:interface-ns symbol?]])
