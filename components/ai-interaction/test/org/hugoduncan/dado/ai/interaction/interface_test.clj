(ns org.hugoduncan.dado.ai.interaction.interface-test
  "Tests for AI interaction interface.
   Focuses on testing the public API contract."
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [org.hugoduncan.dado.ai.interaction.interface :as interaction]
            [org.hugoduncan.dado.ai.thread.interface :as thread]
            [org.hugoduncan.dado.ai.claude.interface :as claude]))

;; ---- Test Data ----

(def test-api-key "test-key")

(def test-thread-id "20241113120000-test_interaction")

(def test-context-docs
  ["adr/test-doc.md"
   "src/test/core.clj"])

(def test-system-prompt
  "You are a helpful AI assistant focused on software development.")

(def test-metadata
  {:test-key "test-value"})

;; ---- Fixtures ----

(defn with-mock-apis
  "Test fixture that mocks thread and Claude APIs."
  [f]
  (with-redefs [claude/create-message (fn [api-key opts]
                                        {:content  "Mock AI response"
                                         :metadata {:response-id "test-123"}})
                thread/create-thread  (fn [title & {:as opts}]
                                        {:id       test-thread-id
                                         :title    title
                                         :metadata (:metadata opts)})
                thread/add-message    (fn [thread-id role content & opts]
                                        {:thread-id thread-id
                                         :role      role
                                         :content   content})
                thread/get-thread     (fn [id]
                                        {:id       id
                                         :metadata {:system test-system-prompt}})
                thread/get-messages   (fn [id]
                                        [{:role    :human
                                          :content "Test message"}])
                thread/update-context (fn [id metadata]
                                        {:id       id
                                         :metadata metadata})]
    (f)))

(use-fixtures :each with-mock-apis)

;; ---- Tests ----

(deftest start-interaction-test
  (testing "starting new interaction"
    (let [title   "Test Interaction"
          message "Initial test message"
          result  (interaction/start-interaction
                   test-api-key
                   title
                   message
                   :context-docs test-context-docs
                   :system test-system-prompt
                   :metadata test-metadata)]

      (testing "returns thread with correct structure"
        (is (map? result))
        (is (= test-thread-id (:id result)))
        (is (= title (:title result))))

      (testing "includes initial message"
        (let [messages (:messages result)]
          (is (sequential? messages))
          (is (= 1 (count messages)))
          (is (= :human (:role (first messages))))
          (is (= message (:content (first messages)))))))))

(deftest send-message-test
  (testing "sending message in existing thread"
    (let [message "Test follow-up message"
          result  (interaction/send-message
                   test-api-key
                   test-thread-id
                   message
                   :context-docs test-context-docs
                   :system "Updated system prompt"
                   :metadata test-metadata)]

      (testing "returns updated thread"
        (is (map? result))
        (is (= test-thread-id (:id result))))

      (testing "handles system prompt updates"
        (is (= "Updated system prompt"
               (get-in result [:metadata :system]))))

      (testing "includes AI response"
        (let [messages (thread/get-messages test-thread-id)]
          (is (sequential? messages))
          (is (some #(= :assistant (:role %)) messages)))))))

(deftest update-thread-context-test
  (testing "updating thread context"
    (let [result (interaction/update-thread-context
                  test-thread-id
                  test-context-docs
                  :metadata test-metadata)]

      (testing "returns updated thread"
        (is (map? result))
        (is (= test-thread-id (:id result))))

      (testing "updates context documents"
        (is (= test-context-docs
               (get-in result [:context :documents])))))))

(deftest error-handling-test
  (testing "handles API errors"
    (with-redefs [claude/create-message
                  (fn [& _]
                    (throw (ex-info "API Error"
                                    {:type :claude/api-error})))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"AI interaction error"
           (interaction/send-message
            test-api-key
            test-thread-id
            "Test message")))))

  (testing "handles thread errors"
    (with-redefs [thread/get-thread
                  (fn [& _]
                    (throw (ex-info "Thread Error"
                                    {:type :thread/not-found})))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"AI interaction error"
           (interaction/send-message
            test-api-key
            "invalid-id"
            "Test message"))))))

;; ---- Integration Tests ----

;; These tests would typically be in a separate namespace
;; and only run when integration testing is enabled

(deftest ^:integration live-claude-test
  (testing "live Claude API interaction"
    (when (System/getenv "CLAUDE_API_KEY")
      (let [api-key (System/getenv "CLAUDE_API_KEY")
            thread  (interaction/start-interaction
                     api-key
                     "Integration Test"
                     "Echo test message"
                     :system "You are a test assistant.
                            Simply echo back the input message.")]
        (is (map? thread))
        (is (:id thread))

        (let [response (interaction/send-message
                        api-key
                        (:id thread)
                        "Test message")]
          (is (= "Test message"
                 (-> response
                     :messages
                     last
                     :content
                     str/trim))))))))
