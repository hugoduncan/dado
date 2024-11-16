(ns dado.project-config.interface
  (:require [dado.project-config.core :as core]))

(defn load-config
  "Load project configuration from dado.edn in current directory.
   Returns validated config map or throws ex-info if invalid."
  []
  (core/load-config))

(defn ai-provider-config
  "Get configuration for specific AI provider.
   Returns provider config map or nil if not found."
  ([provider-key]
   (core/ai-provider-config provider-key))
  ([config provider-key]
   (core/ai-provider-config config provider-key)))

(defn dev-dir
  "Get configured dev directory path."
  ([]
   (core/dev-dir))
  ([config]
   (core/dev-dir config)))
