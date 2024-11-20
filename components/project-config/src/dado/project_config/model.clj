(ns dado.project-config.model)

(def RelativePath
  :string)

(def DirectoryConfig
  [:map-of {:error/message "Directory config must be keyword -> relative path"}
   keyword?
   RelativePath])

(def Config
  [:map
   [:directories DirectoryConfig]
   [:ai-providers
    [:map-of keyword? [:map-of keyword? any?]]]])
