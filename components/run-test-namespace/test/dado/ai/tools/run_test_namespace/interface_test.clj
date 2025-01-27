(ns dado.ai.tools.run-test-namespace.interface-test
  "Tests for run test namespace tool interface."
  (:require
   [clojure.test :refer [deftest is testing]]
   [malli.generator :as mg]
   [dado.ai.tools.run-test-namespace.interface :as run-test]
   [dado.ai.tools.run-test-namespace.model :as model]))

(deftest test-tool-definition
  (testing "tool map contains required keys"
    (is (= #{:id :name :description :structured-description
             :prompt-fn :recognize-fn :parameters :returns :execute-fn}
           (-> (run-test/create-tool) keys set)))
    (is (= :dado/run-test-namespace (:id (run-test/create-tool))))))

(deftest test-validators
  (testing "test results validator accepts valid data"
    (let [valid-results (mg/generate model/TestResults)]
      (is (run-test/test-results? valid-results))))

  (testing "tool config validator accepts valid data"
    (let [valid-config (mg/generate model/ToolConfig)]
      (is (run-test/tool-config? valid-config)))))

(deftest test-tool-execution
  (testing "executing tests on sample namespace"
    (let [config  {:namespace "dado.ai.tools.run-test-namespace.test-ns"}
          results ((:execute-fn (run-test/create-tool)) config)]
      (is (run-test/test-results? results))
      (is (= "dado.ai.tools.run-test-namespace.test-ns"
             (:namespace results)))
      (let [{:keys [test pass fail error]} (:summary results)]
        (is (= 3 test))
        (is (= 1 pass))
        (is (= 1 fail))
        (is (= 1 error))))))
