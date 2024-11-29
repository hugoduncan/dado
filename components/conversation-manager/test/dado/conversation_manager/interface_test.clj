(ns dado.conversation-manager.interface-test
  (:require
   [clojure.stacktrace :refer [root-cause]]
   [clojure.test :refer [deftest is testing]]
   [dado.ai.message.interface :as message]
   [dado.conversation-manager.interface :as conv]))

(def test-tool
  {:id :test/tool
   :name "Test Tool"
   :description "A test tool"
   :structured-description {}
   :parameters []
   :returns {:type :string
            :description "test"}
   :prompt-fn (constantly "test")
   :recognize-fn (constantly true)
   :execute-fn (constantly nil)})

(deftest conversation-operations-test
  (testing "create-conversation"
    (let [conv-id (conv/create-conversation :claude :test/agent)]
      (is (string? conv-id))
      
      (testing "lookup conversation"
        (let [conversation (conv/lookup conv-id)]
          (is (= :claude (get-in conversation [:config :port-id])))
          (is (= :test/agent (get-in conversation [:config :agent-id])))
          (is (map? (:message-thread conversation)))))
      
      (testing "send message"
        (let [response-chan (conv/send-message conv-id "test message")]
          (is (instance? clojure.core.async.impl.channels.ManyToManyChannel
                        response-chan))))
      
      (testing "add tool"
        (let [updated (conv/add-ai-tool conv-id test-tool)]
          (is (= [test-tool] (:tools updated)))))
      
      (testing "remove tool"
        (let [updated (conv/remove-ai-tool conv-id test-tool)]
          (is (empty? (:tools updated)))))
      
      (testing "list conversations"
        (is (= [conv-id] (conv/list-conversations))))
      
      (testing "end conversation"
        (is (nil? (conv/end-conversation conv-id)))
        (is (empty? (conv/list-conversations)))))))

(deftest error-conditions-test
  (testing "unknown conversation id"
    (let [bad-id "non-existent"]
      (try
        (conv/lookup bad-id)
        (catch clojure.lang.ExceptionInfo e
          (is (= :error/unknown-conversation (:type (ex-data e))))))
      
      (try
        (conv/send-message bad-id "test")
        (catch clojure.lang.ExceptionInfo e
          (is (= :error/unknown-conversation (:type (ex-data e))))))
      
      (try
        (conv/end-conversation bad-id)
        (catch clojure.lang.ExceptionInfo e
          (is (= :error/unknown-conversation (:type (ex-data e))))))
      
      (try
        (conv/add-ai-tool bad-id test-tool)
        (catch clojure.lang.ExceptionInfo e
          (is (= :error/unknown-conversation (:type (ex-data e))))))
      
      (try
        (conv/remove-ai-tool bad-id test-tool)
        (catch clojure.lang.ExceptionInfo e
          (is (= :error/unknown-conversation (:type (ex-data e))))))))
  
  (testing "invalid port id"
    (try
      (conv/create-conversation :invalid :test/agent)
      (catch clojure.lang.ExceptionInfo e
        (is (= :error/conversation-creation (:type (ex-data e))))))))
