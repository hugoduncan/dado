(ns dado.repl.message-loop.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.repl.message-loop.interface :as message-loop]
            [dado.ai.message.interface :as ai-message]))

(deftest message-loop-test
  (testing "validates inputs"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"message-loop-validation"
         (message-loop/message-loop {} {}))
        "should reject invalid message thread")))
