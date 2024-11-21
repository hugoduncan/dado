(ns dado.actions.implement-namespace.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.actions.implement-namespace.interface :as sut]))

(def test-config
  {:dev-dir "dev"})

(deftest execute-test
  (testing "throws on missing ADR"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"ADR file not found"
         (sut/execute test-config "non-existent.md" {}))))

  (testing "validates ADR completeness"
    (with-redefs [slurp (constantly "## Status\n\nIncomplete ADR")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid ADR specification"
           (sut/execute test-config "incomplete.md" {})))))

  (testing "validates options"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid input"
         (sut/execute test-config "test.md" {:mode :invalid})))))
