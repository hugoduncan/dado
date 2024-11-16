(ns dado.project-config.model)

(def Config
  [:map
   [:dev-dir string?]
   [:ai-providers
    [:map-of keyword? [:map-of keyword? any?]]]])
