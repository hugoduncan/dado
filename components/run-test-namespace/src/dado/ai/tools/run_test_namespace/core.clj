(ns dado.ai.tools.run-test-namespace.core
  "Core implementation of run test namespace tool."
  (:require
   [clojure.test :as test]
   [dado.ai.tools.run-test-namespace.model :as model]
   [taoensso.telemere :as t]
   [taoensso.truss :as truss]))

(defn execute!
  "Execute tests in a namespace and return results.

   Arguments:
   - config: Map containing:
     :namespace - string naming the namespace to test
     :async? - (optional) run asynchronously, default false
     :timeout - (optional) timeout in ms, default 30000

   Returns results map matching TestResults schema."
  [{:keys [namespace] :as config}]
  {:pre [(truss/have? model/tool-config? config)]}
  (t/trace!
   {:id ::execute-tests}
   (try
     (require (symbol namespace) :reload)
     (let [start-time (System/currentTimeMillis)
           output-str (new java.io.StringWriter)
           error-str  (new java.io.StringWriter)
           results
           (binding [test/*test-out* output-str]
             (let [summary   (atom {:test 0 :pass 0 :fail 0 :error 0})
                   results   (atom [])
                   report-fn (fn [m]
                               (case (:type m)
                                 :begin-test-var nil
                                 :end-test-var
                                 (let [{:keys [test pass fail error]} @summary
                                       status                         (cond
                                                                        error :error
                                                                        fail  :fail
                                                                        :else :pass)]
                                   (swap! results conj
                                          {:test-var (-> m :var meta :name str)
                                           :status   status}))
                                 :pass           (swap! summary update :pass inc)
                                 :fail           (swap! summary update :fail inc)
                                 :error          (swap! summary update :error inc)
                                 nil))]
               (binding [test/report report-fn]
                 (test/test-ns (symbol namespace)))
               {:namespace    namespace
                :summary      @summary
                :test-results @results
                :output       {:stdout (str output-str)
                               :stderr (str error-str)}
                :elapsed-ms   (- (System/currentTimeMillis) start-time)}))]
       results)
     (catch Exception e
       (throw (ex-info "Failed to execute tests"
                       {:error/type      :error/test-execution
                        :error/namespace namespace}
                       e))))))

(def prompt-template
  "Tool for reloading Clojure namespaces.

   Input should be in Updated Namespaces List format:
   ```updated-namespaces
   my.project.utils
   my.project.core
   ```

   Example usage:
   ```
   Please reload these namespaces:
   ```updated-namespaces
   my.project.model
   my.project.core
   ```
   ```

   Note:
   - No validation is performed on namespace names
   - Dependencies are not automatically handled
   - Namespaces are reloaded in the order specified")

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
  "Creates namespace reload tool configuration.
   Tool reloads specified namespaces without validation or dependency handling.

   Input should be in Updated Namespaces List format:
   ```updated-namespaces
   my.project.utils
   my.project.core
   ```

   Returns map of reload results:
   {:reloaded [<successfully-reloaded-ns-symbols>]
    :errors [{:ns <failed-ns-symbol> :error <error-message>}]}"
  []
  tool)
