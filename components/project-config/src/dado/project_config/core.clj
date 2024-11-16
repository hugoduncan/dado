(ns dado.project-config.core
  (:require [babashka.fs :as fs]
            [malli.core :as m]
            [dado.project-config.model :as model]
            [clojure.edn :as edn]))

(defn- read-config-file []
  (let [config-path "dado.edn"]
    (when-not (fs/exists? config-path)
      (throw (ex-info "dado.edn configuration file not found"
                     {:type :error/configuration
                      :path config-path})))
    (try
      (edn/read-string (slurp config-path))
      (catch Exception e
        (throw (ex-info "Failed to parse dado.edn"
                       {:type :error/configuration
                        :cause e}))))))

(defn load-config []
  (let [config (read-config-file)]
    (if (m/validate model/Config config)
      config
      (throw (ex-info "Invalid configuration format"
                      {:type        :error/configuration
                       :context     {:component "dado.project-config"}
                       :explanation (m/explain model/Config config)})))))

(defn ai-provider-config
  ([provider-key]
   (ai-provider-config (load-config) provider-key))
  ([config provider-key]
   (get-in config [:ai-providers provider-key])))

(defn dev-dir
  ([]
   (dev-dir (load-config)))
  ([config]
   (:dev-dir config)))
