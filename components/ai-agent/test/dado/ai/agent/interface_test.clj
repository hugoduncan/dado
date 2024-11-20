(ns dado.ai.agent.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.ai.agent.interface :as agent]
            [dado.ai.agent.model :as model]
            [malli.core :as m]))

(def valid-agent
  {:name :test-agent
   :prompt-fn (fn [_] "prompt")
   :context-fn (fn [_] {})
   :process-response-fn (fn [_] {})})

(def invalid-agent
  {:name "not-a-keyword"
   :prompt-fn "not-a-function"})

(deftest validate-agent-test
  (testing "valid agent configuration"
    (is (= valid-agent (agent/validate-agent valid-agent))))
  
  (testing "invalid agent configuration"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                         #"Invalid agent configuration"
                         (agent/validate-agent invalid-agent)))))

(deftest load-agent-document-test
  (testing "document not found"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                         #"Document not found in search paths"
                         (agent/load-agent-document
                          {:dev-dir "dev"}
                          valid-agent
                          "non-existent.md")))))
