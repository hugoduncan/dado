(ns org.hugoduncan.dado.ai.claude.interface-test
  "Tests for Claude API interface functionality."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [org.hugoduncan.dado.ai.claude.interface :as claude]
            [org.hugoduncan.dado.ai.claude.core :as claude-core]
            [org.hugoduncan.dado.ai.claude.schema :as schema
             :refer [Config Message MessageRequest]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.error :as me]))

;; Test Configuration Schema

;; Test Configuration
(def test-config
  {:api-key  "test-key"
   :base-url "https://api.anthropic.com/v1"})

;; Test Data Generation
(defn gen-valid-message
  "Generate a valid message using the schema."
  []
  (mg/generate schema/message-request))

(defn gen-message-without
  "Generate a message without a specific required field."
  [field]
  (dissoc (gen-valid-message) field))

;; Validation Helpers
(defn validate-message
  "Validate a message against the schema. Returns nil if valid,
   error details if invalid."
  [message]
  (when-not (m/validate schema/message-request message)
    (me/humanize (m/explain schema/message-request message))))

(defn validate-config
  "Validate configuration against the config schema."
  [config]
  (when-not (m/validate Config config)
    (me/humanize (m/explain Config config))))

;; Test Fixtures
(defn with-mock-core
  "Fixture to mock claude-core responses for testing."
  [f]
  (with-redefs [claude-core/create-message  (fn [_ message-data]
                                              {:id       "msg_test"
                                               :model    (:model message-data)
                                               :messages (:messages message-data)})
                claude-core/stream-response (fn [_ message-data]
                                              {:status 200
                                               :body   "test response"})]
    (f)))

(use-fixtures :each with-mock-core)

;; Tests
(deftest message-schema-validation-test
  (testing "message schema validation"
    (let [valid-message (gen-valid-message)]
      (is (nil? (validate-message valid-message))
          "Generated message should be valid"))

    (testing "invalid messages"
      (doseq [field [:model :messages]]
        (let [invalid-message (gen-message-without field)
              errors          (validate-message invalid-message)]
          (is (some? errors)
              (str "Should detect missing " (name field))))))))

(deftest create-message-test
  (testing "successful message creation"
    (let [valid-message (gen-valid-message)
          response      (claude/create-message test-config valid-message)]
      (is (m/validate schema/message-request valid-message)
          "Generated message should be valid")
      (is (= (:model valid-message) (:model response))
          "Should preserve model specification")
      (is (= (:messages valid-message) (:messages response))
          "Should preserve message content")))

  (testing "message creation with various temperatures"
    (doseq [temp [0.0 0.5 1.0]]
      (let [message  (assoc (gen-valid-message) :temperature temp)
            response (claude/create-message test-config message)]
        (is (nil? (validate-message message))
            (str "Should accept temperature " temp)))))

  (testing "error handling for invalid config"
    (is (thrown? clojure.lang.ExceptionInfo
                 (claude/create-message {} (gen-valid-message)))
        "Should throw on missing API key"))

  (testing "error handling for invalid message data"
    (doseq [field [:model :messages]]
      (let [invalid-message (gen-message-without field)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (claude/create-message test-config invalid-message))
            (str "Should throw on missing " (name field)))))))

(deftest stream-response-test
  (testing "successful response streaming"
    (let [valid-message (gen-valid-message)
          response      (claude/stream-response test-config valid-message)]
      (is (= 200 (:status response))
          "Should return success status code")
      (is (= "test response" (:body response))
          "Should return expected response body")))

  (testing "streaming with generated valid messages"
    (dotimes [_ 5] ; Test multiple generated messages
      (let [message (gen-valid-message)]
        (is (nil? (validate-message message))
            "Generated message should be valid")
        (is (map? (claude/stream-response test-config message))
            "Should handle generated message"))))

  (testing "error handling for streaming"
    (let [invalid-config (dissoc test-config :api-key)]
      (is (some? (validate-config invalid-config))
          "Should detect invalid config")
      (is (thrown? clojure.lang.ExceptionInfo
                   (claude/stream-response invalid-config (gen-valid-message)))
          "Should throw on invalid config"))))

;; Integration Test Helpers (disabled by default)
(defn ^:integration create-real-message
  "Creates a real message using actual API. Requires valid API key.
   Only runs when integration tests are enabled."
  [message]
  (when-not (validate-message message)
    (claude/create-message
     {:api-key (System/getenv "CLAUDE_API_KEY")}
     message)))

(defn ^:integration test-real-api
  "Integration test using real API.
   Only runs when integration tests are enabled and API key is available."
  []
  (when-let [api-key (System/getenv "CLAUDE_API_KEY")]
    (testing "real API interaction"
      (let [message  (gen-valid-message)
            response (create-real-message message)]
        (is (nil? (validate-message message))
            "Should generate valid message")
        (is (some? response)
            "Should successfully interact with real API")))))

;; Run integration tests only when explicitly enabled
(when (System/getenv "RUN_INTEGRATION_TESTS")
  (test-real-api))
