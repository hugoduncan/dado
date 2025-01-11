(ns dado.ai.anthropic.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [dado.ai.anthropic.core :as core]
   [dado.ai.anthropic.model :as model]
   [dado.ai.message.interface :as msg]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]))

(deftest ->system-content-test
  (let [content-fn #'core/->system-content]
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
      (let [files  [[{:name "test1.txt" :content "content1"}]
                    [{:name "test2.txt" :content "content2"}]
                    [{:name "test3.txt" :content "content3"}]
                    [{:name "test4.txt" :content "content4"}]
                    [{:name "test5.txt" :content "content5"}]]
            result (content-fn files)]
        (is (= 5 (count result)) (prn-str result))
        ;; First 4 files should have cache control
        (doseq [content (take 3 result)]
          (is (= "ephemeral" (get-in content [:cache_control :type]))))
        ;; Last file should not have cache control
        (is  (get-in (last result) [:cache_control]))))))

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


(deftest claude-reauest-schema-test
  ;; Test with examples from the API docs
  (is (nil? (me/humanize (m/explain model/ClaudeRequest simple-request))))
  (is (nil? (me/humanize (m/explain model/ClaudeRequest request-with-tools)))))

(def simple-response (j/read-value
                      "{
  \"content\": [{
      \"text\": \"Hi! My name is Claude.\",
      \"type\": \"text\"}],
  \"id\": \"msg_013Zva2CMHLNnXjNJJKqJ2EF\",
  \"model\": \"claude-3-5-sonnet-20241022\",
  \"role\": \"assistant\",
  \"stop_reason\": \"end_turn\",
  \"stop_sequence\": null,
  \"type\": \"message\",
  \"usage\": {
    \"input_tokens\": 2095,
    \"output_tokens\": 503
  }
}"
                      j/keyword-keys-object-mapper))

(def ^:private tool-use-response
  (j/read-value
   "{
  \"content\": [
  { \"type\": \"tool_use\",
    \"id\": \"toolu_01D7FLrfh4GYq7yT1ULFeyMV\",
    \"name\": \"get_stock_price\",
    \"input\": { \"ticker\": \"^GSPC\" }}],
  \"id\": \"msg_013Zva2CMHLNnXjNJJKqJ2EF\",
  \"model\": \"claude-3-5-sonnet-20241022\",
  \"role\": \"assistant\",
  \"stop_reason\": \"end_turn\",
  \"stop_sequence\": null,
  \"type\": \"message\",
  \"usage\": {
    \"input_tokens\": 2095,
    \"output_tokens\": 503
  }
}"
   j/keyword-keys-object-mapper))

(deftest claude-response-schema-test
  ;; Test with examples from the API docs
  (is (nil? (me/humanize (m/explain model/ClaudeResponse simple-response))))
  (is (nil? (me/humanize (m/explain model/ClaudeResponse tool-use-response)))))

(deftest from-claude-response-test
  (let [convert @#'core/from-claude-response]
    (is (convert simple-response))
    (is (nil? (me/humanize
               (m/explain
                msg/response-message-schema
                (convert simple-response)))))

    (is (convert tool-use-response))
    (is (nil? (me/humanize
               (m/explain
                msg/response-message-schema
                (convert tool-use-response)))))))
