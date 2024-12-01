(ns dado.conversation.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.conversation.interface :as conv]))

(def test-agent
  {:name :test
   :prompt-fn identity
   :context-fn identity
   :process-response-fn identity})

(def test-message-thread
  {:id "test-id"
   :created-at (java.time.Instant/now)
   :messages []
   :metadata {}})

(deftest conversation-operations-test
  (testing "create conversation"
    (let [conversation (conv/create test-agent identity test-message-thread {})]
      (is (= test-message-thread (:message-thread conversation)))
      (is (= test-agent (:ai-agent conversation)))
      (is (fn? (:port-send-fn conversation)))
      (is (map? (:user-data conversation))))))
