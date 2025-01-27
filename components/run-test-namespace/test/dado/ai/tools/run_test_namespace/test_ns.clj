(ns dado.ai.tools.run-test-namespace.test-ns
  (:require
   [clojure.test :refer [deftest is testing]]))

(deftest passing-test
  (testing "pass"
    (is (= 1 1))))

(deftest failing-test
  (testing "fail"
    (is (= 1 2))))

(deftest error-test
  (testing "error"
    (throw (ex-info "Test error" {:x 1}))))
