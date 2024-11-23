(ns dado.http.policy-builder
  (:require
   [diehard.core :as dh]
   [diehard.spec]
   [taoensso.telemere :as t])
  (:import
   [dev.failsafe ExecutionContext
    RetryPolicy]
   [dev.failsafe.function ContextualSupplier]
   [java.time Duration
    Instant]
   [java.time.format DateTimeFormatter]))

(defn- parse-rfc-1123-date-time [^String s]
  (.parse DateTimeFormatter/RFC_1123_DATE_TIME s Instant/from))

(defn context->retry-after-millis
  [result]
  (t/trace!
   {:id ::context->retry-after-millis :level :trace}
   (let [header-value (-> result :headers (get "retry-after"))]
     (or
      (when header-value
        (if-let [secs (try (Long/parseLong header-value) (catch Exception _))]
          (Duration/ofSeconds secs)
          (when-let [inst (try
                            (parse-rfc-1123-date-time header-value)
                            (catch Exception _))]
            (let [d (Duration/between (Instant/now) inst)]
              (if (.isNegative d)
                (Duration/ofSeconds 0)
                d)))))
      (Duration/ofSeconds 1)))))

(defn ^:no-doc retry-policy-from-config [policy-map]
  (let [policy (dh/retry-policy-from-config policy-map)
        policy (RetryPolicy/builder (.getConfig ^RetryPolicy policy))]

    (when-let [delay-fn (:delay-ms-fn policy-map)]
      (.withDelayFn policy (reify ContextualSupplier
                             (get [_# ^ExecutionContext ctx#]
                               (delay-fn (.getLastResult ctx#))))))
    (.build policy)))
