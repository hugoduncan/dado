(ns dado.logging.interface
  "Interface namespace for the logging component.
   Provides functions for configuring logging sinks based on environment."
  (:require [dado.logging.core :as core]))

(defn init!
  "Initialize logging system with provided configuration map.

   config-map keys:
   :environment - :dev or :prod
   :levels      - Map of namespace patterns to log levels
   :sinks       - Sink configurations
   :rotation    - Log rotation policies
   :metrics     - Metrics extraction rules
   :redaction   - Sensitive data redaction rules
   :context     - Context propagation settings
   :buffers     - Buffer size configurations
   :async       - Async processing options"
  [config-map]
  (core/init-logging! config-map))
