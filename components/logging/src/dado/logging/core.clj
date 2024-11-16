(ns dado.logging.core
  "Core implementation of logging configuration component.
   Uses Telemere directly for logging functionality."
  (:require
   [taoensso.telemere :as t]))

(defn- configure-dev-sinks
  [{:keys [sinks] :as config}]
  ;; (let [console-config (-> (:console sinks)
  ;;                          (assoc :pretty-print? true))
  ;;       local-file     (-> (:local-file sinks)
  ;;                          (assoc :format :edn))]
  ;;   (t/add-sink! :console console-config)
  ;;   (t/add-sink! :local-file local-file))
  )

(defn- configure-prod-sinks
  [{:keys [sinks rotation] :as config}]
  ;; (let [rotating-files (-> (:rotating-files sinks)
  ;;                          (merge rotation)
  ;;                          (assoc :format :binary))
  ;;       metrics-sink   (-> (:metrics sinks))]
  ;;   (t/add-sink! :rotating-files rotating-files)
  ;;   (t/add-sink! :metrics metrics-sink)
  ;;   (when-let [remote (:remote sinks)]
  ;;     (t/add-sink! :remote remote)))
  )

(defn init-logging!
  "Initialize logging configuration based on environment and config map."
  [{:keys [environment] :or {environment :dev} :as config}]

  (case environment
    :dev  (configure-dev-sinks config)
    :prod (configure-prod-sinks config)))
