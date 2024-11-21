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
                        {:type  :error/configuration
                         :cause e}))))))

(def ^:private default-directories
  {:dado/prompts        "dev/ai/prompts"
   :dado/adr            "dev/design/adr"
   :dado/architecture   "dev/design/architecture"
   :dado/implementation "dev/design/implementation"
   :dado/scope          "dev/design/scope"})

(defn load-config []
  (let [config (-> (read-config-file)
                   (update :directories #(merge default-directories %)))]
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

(defn get-directory
  "Gets configured directory path for given key.
   Returns path string if found, nil if not configured."
  [config dir-key]
  (get-in config [:directories dir-key]))
