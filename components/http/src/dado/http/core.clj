(ns dado.http.core
  "Resilient HTTP client wrapper for retry and circuit breaker patterns.
  Provides robust handling of rate limiting, overload conditions, and
  service unavailability with configurable retry strategies and circuit
  breaker protection."
  (:require
   [clojure.string :as str]
   [dado.http.policy-builder :as policy-builder]
   [diehard.circuit-breaker :as cb]
   [diehard.core :as dh]
   [taoensso.telemere :as t]))

;;; HTTP Request Defaults

(defn wrap-request-defaults
  "Wraps an HTTP client function with sensible defaults.

   Parameters:
     request-fn - Function taking a request map and returning a response

   Returns:
     Function with same signature as request-fn"
  [request-fn]
  (fn [req-map]
    (request-fn
     (merge
      {:throw-exceptions? false}
      req-map))))

;;; Failure Predicates

(defn- maybe-retry-after-header
  [response]
  (some-> response :headers (get "retry-after")))

(defn- maybe-should-retry-header
  [response]
  (some-> response :headers (get "x-should-retry")))

(defn- retryable-error?
  [{:keys [status] :as response}]
  (or
   (>= status 500)
   (= status 429)
   (= "true" (maybe-should-retry-header response))))

(defn error-and-retry-after?
  [{:keys [status] :as response} _thrown-exception]
  (when (>= status 400)
    (t/event!
     ::error-and-retry-after?
     {:level :warn
      :data  {:headers     (:headers response)
              :retryable?  (retryable-error? response)
              :retry-after (maybe-retry-after-header response)}}))
  (and (retryable-error? response)
       (not (str/blank? (maybe-retry-after-header response)))))

(defn error-and-no-retry-after?
  [{:keys [status] :as response} _thrown-exception]
  (when (>= status 400)
    (t/event!
     ::error-and-no-retry-after?
     {:level :warn :data {:headers     (:headers response)
                          :retryable?  (retryable-error? response)
                          :retry-after (maybe-retry-after-header response)}}))
  (and (retryable-error? response)
       (str/blank? (maybe-retry-after-header response))))

;;; Circuit Breaker

(defn circuit-breaker [opts]
  (cb/circuit-breaker opts))

(defn wrap-circuit-breaker
  "Wraps an HTTP client function with a diehard circuit breaker.

   Parameters:
     request-fn - Function taking a request map and returning a response
     circuit-breaker - Diehard circuit breaker

   Returns:
     Function with same signature as request-fn"
  [request-fn circuit-breaker]
  (fn [req-map]
    (dh/with-circuit-breaker circuit-breaker
      (request-fn req-map))))

;;; Retry On Error

(defn wrap-retry
  "Wraps an HTTP client function with a retry policy.

   Parameters:
     request-fn - Function taking a request map and returning a response
     retry-policy - Diehard retry policy

   Returns:
     Function with same signature as request-fn"
  [request-fn retry-policy]
  (let [policy (policy-builder/retry-policy-from-config retry-policy)]
    (fn [req-map]
      (dh/with-retry {:policy policy}
        (request-fn req-map)))))


;;; Robust Client

(defn robust-request-fn
  [request-fn & [config]]
  (-> request-fn
      (wrap-request-defaults)
      (wrap-circuit-breaker
       (circuit-breaker
        (merge
         {:failure-threshold-ratio [8 10]
          :delay-ms                1000}
         config)))
      (wrap-retry
       (merge
        {:retry-if    error-and-retry-after?
         :delay-ms-fn policy-builder/context->retry-after-millis
         :max-retries 2}
        config))
      (wrap-retry
       (merge
        {:retry-if    error-and-no-retry-after?
         :backoff-ms  [1000 10000]
         :max-retries 5}
        config))))
