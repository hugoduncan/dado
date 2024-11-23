(ns dado.http.interface
  "Interface for resilient HTTP requests"
  (:require [dado.http.core :as core]))

(defn robust-request-fn
  "Makes a robust HTTP request using the provided request function.
   Handles retries and circuit breaking based on configuration.

   Parameters:
   - request-fn: Function that performs the actual HTTP request
   - config: Optional configuration map for resilience settings

   Returns:
   The request function if successful.

   Throws:
   Implementation specific exceptions for request failures."
  [request-fn & [config]]
  (core/robust-request-fn request-fn config))
