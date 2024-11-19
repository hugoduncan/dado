(ns dado.repl.message-loop.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.repl.message-loop.interface :as message-loop]
            [dado.ai.message.interface :as ai-message]))

(deftest message-loop-test
  (testing "validates inputs"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invariant.*message-thread"
         (message-loop/message-loop {} {} (constantly "") (constantly [])))
        "should reject invalid message thread")

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invariant.*prompt-fn"
         (message-loop/message-loop
          {:id "test" :created-at (java.time.Instant/now) :messages [] :metadata {:model "test"}}
          {}
          "not-a-function"
          (constantly [])))
        "should reject invalid prompt-fn")

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invariant.*context-files-fn"
         (message-loop/message-loop
          {:id "test" :created-at (java.time.Instant/now) :messages [] :metadata {:model "test"}}
          {}
          (constantly "")
          "not-a-function"))
        "should reject invalid context-files-fn")))
