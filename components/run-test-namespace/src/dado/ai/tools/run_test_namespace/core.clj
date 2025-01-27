(ns dado.ai.tools.run-test-namespace.core
  "Core implementation of run test namespace tool."
  (:require
   [clojure.stacktrace :as stacktrace]
   [clojure.test :as test]
   [dado.ai.tools.run-test-namespace.model :as model]
   [taoensso.telemere :as t]
   [taoensso.truss :as truss]))

(defn execute!
  "Execute tests in a namespace and return results.

   Arguments:
   - config: Map containing:
     :namespace - string naming the namespace to test

   Returns results map matching TestResults schema."
  [{:keys [namespace] :as config}]
  {:pre [(truss/have? model/tool-config? config)]}
  (t/trace!
   {:id ::execute-tests}
   (try
     (require (symbol namespace) :reload)
     (let [start-time         (System/currentTimeMillis)
           output-str         (new java.io.StringWriter)
           zero-summary       {:test 0 :pass 0 :fail 0 :error 0}
           summary            (atom zero-summary)
           results            (atom [])
           var-results        (atom [])
           inc-report-counter (fn inc-report-counter [k]
                                (swap! summary update k inc))]
       (binding [test/*test-out* output-str]
         (binding [*err* *out*]
           (let [report-fn
                 (fn [m]
                   (case (:type m)
                     :begin-test-var
                     (do
                       (inc-report-counter :test)
                       (reset! var-results []))
                     :end-test-var
                     (swap! results conj
                            {:test-var (-> m :var meta :name str)
                             :results  @var-results})
                     :pass
                     (inc-report-counter :pass)
                     :fail
                     (do
                       (inc-report-counter :fail)
                       (swap! var-results conj
                              (select-keys m [:type :message :expected :actual])))
                     :error
                     (do
                       (inc-report-counter :error)
                       (swap! var-results conj
                              (select-keys m [:type :message :expected :actual])))
                     nil))]
             (binding [test/report report-fn]
               (test/test-ns (symbol namespace))))))
       {:is-error false
        :content  [{:namespace    namespace
                    :summary      @summary
                    :test-results @results
                    :output       (str output-str)
                    :elapsed-ms   (- (System/currentTimeMillis) start-time)}]})
     (catch Exception e
       (t/error! e)
       {:is-error true
        :content  (with-out-str
                    (stacktrace/print-cause-trace e))}))))

(def prompt-template
  "Tool for running test namespaces")

(defn make-prompt
  "Returns prompt string for tool usage"
  []
  prompt-template)

(def tool
  "Run Test Namespace Tool definition."
  {:id           :dado/run-test-namespace
   :name         "Run Test Namespace Tool"
   :description  "Executes test namespace and collects results"
   :structured-description
   {:claude
    {:description "Tool for running Clojure test namespaces and collecting results."}}
   :parameters
   [:map
    [:namespace string?]]
   :returns
   {:type        :map
    :description "Map containing test results and output"}
   :prompt-fn    make-prompt
   :recognize-fn (constantly nil)
   :execute-fn   execute!})

(defn create-tool
  []
  tool)
