(ns dado.ai.tools.run-test-namespace.core
  "Core implementation of run test namespace tool."
  (:require [clojure.test :as test]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have!]]
            [dado.ai.tools.run-test-namespace.model :as model]))

(defn- require-test-ns
  "Load test namespace if not loaded.
   Returns nil on success, throws on failure."
  [ns-str]
  (t/trace! {:id ::require-test-ns}
    (try
      (require (symbol ns-str) :reload)
      nil
      (catch Exception e
        (throw (ex-info "Failed to load namespace"
                       {:type :error/namespace-load
                        :namespace ns-str
                        :cause e}))))))

(defn- test-var->result
  "Convert test-var results to test result format."
  [{:keys [var test-results] :as _test-ctx}]
  (let [result (first test-results)
        {:keys [type expected actual message]} result]
    {:test-var (str (:name (meta var)))
     :status (cond
              (nil? result) :pass
              (instance? Throwable actual) :error
              :else :fail)
     :expected expected
     :actual actual
     :message message
     :type (some-> type str)}))

(defn- run-test-var!
  "Run a single test var and return results."
  [test-var]
  (let [test-ctx (test/test-var test-var)]
    (test-var->result (assoc test-ctx :var test-var))))

(defn- namespace->vars
  "Get all test vars from namespace."
  [ns-sym]
  (->> (vals (ns-publics ns-sym))
       (filter (comp :test meta))))

(defn- run-ns-tests!
  "Run all tests in namespace and return results."
  [ns-str]
  (let [start-time (System/currentTimeMillis)
        ns-sym (symbol ns-str)
        stdout-str (new java.io.StringWriter)
        stderr-str (new java.io.StringWriter)
        test-results (binding [*out* stdout-str
                              *err* stderr-str]
                      (let [test-vars (namespace->vars ns-sym)
                            results (mapv run-test-var! test-vars)
                            {:keys [pass fail error]}
                            (group-by :status results)]
                        {:namespace ns-str
                         :summary {:test (count results)
                                  :pass (count pass)
                                  :fail (count fail)
                                  :error (count error)}
                         :test-results results
                         :output {:stdout (str stdout-str)
                                 :stderr (str stderr-str)}
                         :elapsed-ms (- (System/currentTimeMillis)
                                       start-time)}))]
    (have! model/test-results? test-results
           :data (me/humanize (m/explain model/TestResults test-results)))
    test-results))

(defn execute!
  "Execute test namespace and return results.
   See model/ToolConfig for config options.
   Returns test results map.
   Throws exceptions for invalid config or execution errors."
  [config]
  (have! model/tool-config? config
         :data (me/humanize (m/explain model/ToolConfig config)))
  
  (t/trace! {:id ::execute-tests}
    (let [{:keys [namespace async? timeout]} config
          timeout (or timeout 30000)]
      ;; Validate and load namespace
      (require-test-ns namespace)
      
      (if async?
        (let [result-future (future (run-ns-tests! namespace))]
          (try
            @(future (do @result-future
                        (deref result-future timeout nil)))
            (catch Exception e
              (throw (ex-info "Test execution failed"
                             {:type :error/test-execution
                              :namespace namespace
                              :cause e})))
            (catch java.util.concurrent.TimeoutException _
              (throw (ex-info "Test execution timed out"
                             {:type :error/test-timeout
                              :namespace namespace
                              :timeout timeout})))))
        ;; Synchronous execution
        (try
          (run-ns-tests! namespace)
          (catch Exception e
            (throw (ex-info "Test execution failed"
                           {:type :error/test-execution
                            :namespace namespace
                            :cause e}))))))))
