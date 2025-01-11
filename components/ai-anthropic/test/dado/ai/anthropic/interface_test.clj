(ns dado.ai.claude.interface-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [dado.ai.claude.core :as claude-core]
   [dado.ai.claude.interface :as claude]
   [dado.ai.claude.model :as model]
   [dado.ai.message.interface :as message]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]
   [malli.generator :as mg]))

(deftest send!-test
  (testing "validates provider config"
    (let [invalid-config {}
          message-thread {:id         "test-thread"
                          :created-at (java.time.Instant/now)
                          :messages   [{:role    :user
                                        :content "test"}]
                          :metadata   {:model "claude-3-opus-20240229"}}]
      (is (thrown? clojure.lang.ExceptionInfo
                   (claude/send! invalid-config message-thread)))
      (let [ex (try
                 (claude/send! invalid-config message-thread)
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (= :error/claude-validation (:type (ex-data ex))))
        (is (= "dado.ai.claude" (get-in (ex-data ex) [:context :component]))))))

  #_(testing "handles system prompt and context correctly"
      (let [config         {:api-key "test-key"}
            message-thread {:id         "test-thread"
                            :created-at (java.time.Instant/now)
                            :messages   [{:role :user :content "test"}]
                            :metadata   {:model         "claude-3-opus-20240229"
                                         :system-prompt "Be concise"
                                         :context       {:files [{:name    "test.txt"
                                                                  :content "test content"}]}}}]
        ;; Should pass validation but fail on HTTP call
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"Claude API error"
             (claude/send! config message-thread))))))

(deftest ->system-content-test
  (let [content-fn #'dado.ai.claude.core/->system-content]
    (testing "empty sequence"
      (is (empty? (content-fn []))))

    (testing "single file sequence"
      (let [files  [[{:name    "test1.txt"
                      :content "content1"}]]
            result (content-fn files)]
        (is (= 1 (count result)))
        (let [content (first result)]
          (is (= "text" (:type content)))
          (is (string? (:text content)))
          (is (str/includes? (:text content) "test1.txt"))
          (is (str/includes? (:text content) "content1")))))

    (testing "multiple files in sequence"
      (let [files  [[{:name    "test1.txt"
                      :content "content1"}
                     {:name    "test2.txt"
                      :content "content2"}]]
            result (content-fn files)]
        (is (= 2 (count result)))
        (let [content1 (first result)
              content2 (second result)]
          (is (= "text" (:type content1)))
          (is (= "text" (:type content2)))
          (is (str/includes? (:text content1) "test1.txt"))
          (is (str/includes? (:text content2) "test2.txt")))))

    (testing "multiple sequences"
      (let [files  [[{:name    "test1.txt"
                      :content "content1"}]
                    [{:name    "test2.txt"
                      :content "content2"}]]
            result (content-fn files)]
        (is (= 2 (count result)))
        (let [content1 (first result)
              content2 (second result)]
          (is (= "text" (:type content1)))
          (is (= "text" (:type content2)))
          (is (str/includes? (:text content1) "test1.txt"))
          (is (str/includes? (:text content2) "test2.txt")))))

    (testing "cache control"
      (let [files  [[{:name    "test1.txt"
                      :content "content1"}]
                    [{:name    "test2.txt"
                      :content "content2"}]
                    [{:name    "test3.txt"
                      :content "content3"}]
                    [{:name    "test4.txt"
                      :content "content4"}]
                    [{:name    "test5.txt"
                      :content "content5"}]]
            result (content-fn files)]
        (is (= 5 (count result)))
        ;; First 4 files should have cache control
        (doseq [content (take 4 result)]
          (is (= "ephemeral" (get-in content [:cache_control :type]))))
        ;; Last file should not have cache control
        (is (nil? (get-in (last result) [:cache_control])))))))

(def ^:private simple-request
  (j/read-value
   "
{\"model\": \"claude-3-5-sonnet-20241022\", \"max_tokens\": 1024,
 \"messages\": [ {\"role\": \"user\", \"content\": \"Hello, world\"}]}"
   j/keyword-keys-object-mapper))

(def ^:private request-with-tools
  (j/read-value
   "
{\"model\": \"claude-3-5-sonnet-20241022\", \"max_tokens\": 1024,
 \"messages\": [ {\"role\": \"user\", \"content\": \"Hello, world\"}],
 \"tools\" :[
  {\"name\": \"get_stock_price\",
    \"description\": \"Get the current stock price for a given ticker symbol.\",
    \"input_schema\": {
      \"type\": \"object\",
      \"properties\": {
        \"ticker\": {
          \"type\": \"string\",
          \"description\": \"The stock ticker symbol, e.g. AAPL for Apple Inc.\"}},
      \"required\": [\"ticker\"]}}]}"
   j/keyword-keys-object-mapper))


(def test-config
  {:api-key  "test-key"
   :base-url "https://api.anthropic.com/v1"})

;; Test Data Generation
(defn gen-valid-message
  "Generate a valid message using the schema."
  []
  (mg/generate message/message-schema))

(defn gen-valid-message-thread
  "Generate a valid message thread using the schema."
  []
  (mg/generate message/message-thread-schema {:size 5}))

(defn gen-message-without
  "Generate a message without a specific required field."
  [field]
  (dissoc (gen-valid-message) field))

;; Validation Helpers
(defn validate-message
  "Validate a message against the schema. Returns nil if valid,
   error details if invalid."
  [message]
  (when-not (m/validate message/message-schema message)
    (me/humanize (m/explain message/message-schema message))))

(defn validate-config
  "Validate configuration against the config schema."
  [config]
  (when-not (m/validate model/ClaudeConfig config)
    (me/humanize (m/explain model/ClaudeConfig config))))

;; Test Fixtures
(defn with-mock-core
  "Fixture to mock claude-core responses for testing."
  [f]
  (with-redefs [claude-core/send! (fn [_ message-data]
                                    {:id       "msg_test"
                                     :model    (:model message-data)
                                     :messages (:messages message-data)})]
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

#_(deftest create-message-test
    (testing "successful message creation"
      (let [valid-message (gen-valid-message)
            response      (claude/send! test-config valid-message)]
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

;; Integration Test Helpers (disabled by default)
#_(defn ^:integration create-real-message
    "Creates a real message using actual API. Requires valid API key.
   Only runs when integration tests are enabled."
    [message]
    (when-not (validate-message message)
      (claude/create-message
       {:api-key (System/getenv "CLAUDE_API_KEY")}
       message)))

#_(defn ^:integration test-real-api
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
#_(when (System/getenv "RUN_INTEGRATION_TESTS")
    (test-real-api))


(deftest claude-request-test
  (testing "should preserve model specification"
    (let [message-thread (gen-valid-message-thread)
          config         {:api-key    "Fred"
                          :model-name "Bloggs"}
          request        (#'claude-core/to-claude-request
                          message-thread
                          config)]
      (is (= (:model-name config) (:model request))
          "Should preserve model specification"))))
