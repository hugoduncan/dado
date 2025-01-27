;; Sample test namespace for integration tests
(ns dado.ai.tools.run-test-namespace.test-ns
  (:require [clojure.test :refer [deftest is]]))

(deftest sample-passing-test
  (is (= 4 (+ 2 2))))

(deftest sample-failing-test
  (is (= 5 (+ 2 2))))

(deftest sample-error-test
  (throw (Exception. "Sample error")))
