(ns dado.conversation-manager.interface-test
  (:require
   [clojure.stacktrace :refer [root-cause]]
   [clojure.test :refer [deftest is testing]]
   [dado.ai.message.interface :as message]
   [dado.conversation-manager.interface :as thread]))

(def test-message-thread
  {:id "test-thread"
   :created-at (java.time.Instant/now)
   :messages []
   :metadata {:model "test-model"}})

(deftest thread-operations-test
  (testing "register-new"
    (let [thread (thread/register-new test-message-thread)]
      (is (= test-message-thread thread))))

  (testing "lookup"
    (let [thread (thread/lookup "test-thread")]
      (is (= test-message-thread thread))))

  (testing "register-update"
    (let [updated (assoc test-message-thread
                        :messages
                        [(message/create-message :user)])
          result (thread/register-update updated)]
      (is (= updated result))
      (is (= updated (thread/lookup "test-thread")))))

  (testing "list"
    (is (= ["test-thread"] (thread/list))))

  (testing "tool operations"
    (let [tool {:id :test/tool
               :name "Test Tool"
               :description "A test tool"
               :structured-description {}
               :parameters []
               :returns {:type :string
                        :description "test"}
               :prompt-fn (constantly "test")
               :recognize-fn (constantly true)
               :execute-fn (constantly nil)}]
      
      (testing "add-ai-tool"
        (let [updated (thread/add-ai-tool "test-thread" tool)]
          (is (= [tool] (get-in updated [:metadata :tools])))))

      (testing "remove-ai-tool"
        (let [removed (thread/remove-ai-tool "test-thread" tool)]
          (is (empty? (get-in removed [:metadata :tools])))))))

  (testing "remove"
    (is (nil? (thread/remove "test-thread")))
    (is (empty? (thread/list)))))

(deftest error-conditions-test
  (testing "unknown thread id"
    (try
      (thread/lookup "non-existent")
      (is false "should throw")
      (catch clojure.lang.ExceptionInfo e
        (is (re-matches
             #"Unknown message thread ID"
             (ex-message (root-cause e))))))

    (try
      (thread/register-update
       (assoc test-message-thread :id "non-existent"))
      (is false "should throw")
      (catch clojure.lang.ExceptionInfo e
        (is (re-matches
             #"Unknown message thread ID"
             (ex-message (root-cause e))))))

    (try
      (thread/remove "non-existent")
      (is false "should throw")
      (catch clojure.lang.ExceptionInfo e
        (is (re-matches
             #"Unknown message thread ID"
             (ex-message (root-cause e)))))))

  (testing "invalid message thread"
    (is (thrown?
         clojure.lang.ExceptionInfo
         (thread/register-new {:invalid "format"})))

    (is (thrown?
         clojure.lang.ExceptionInfo
         (thread/register-update {:invalid "format"})))))
