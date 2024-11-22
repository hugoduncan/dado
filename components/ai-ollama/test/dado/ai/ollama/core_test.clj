(ns dado.ai.ollama.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dado.ai.ollama.core :as core]
            [dado.ai.ollama.model :as model]
            [jsonista.core :as j]
            [malli.core :as m]
            [malli.error :as me]))

(def simple-request
  (j/read-value
   "
{\"model\": \"llama2:3.2\",
 \"messages\": [ {\"role\": \"user\", \"content\": \"Hello, world\"}]}"
   j/keyword-keys-object-mapper))

(deftest ollama-request-schema-test
  (testing "request schema validation"
    (is (nil? (me/humanize (m/explain model/OllamaRequest simple-request)))))

  (testing "converts system prompt and context files"
    (let [msg-thread {:id         "test"
                     :created-at  (java.time.Instant/now)
                     :messages    [{:role    :user
                                  :content "test message"}]
                     :metadata    {:model "llama2:3.2"
                                 :system-prompt "Be helpful"
                                 :context {:files [[{:name    "test.txt"
                                                   :content "test content"}]]}}}
          request   (#'dado.ai.ollama.core/to-ollama-request msg-thread {:model-name "llama2:3.2"})
          messages (:messages request)]

      ;; Check message order and content
      (is (= "system" (:role (first messages))) "First message should be system prompt")
      (is (= "Be helpful" (:content (first messages))))

      (is (= "user" (:role (second messages))) "Second message should be context file")
      (is (str/includes? (:content (second messages)) "test.txt"))
      (is (str/includes? (:content (second messages)) "test content"))

      (is (= "user" (:role (nth messages 2))) "Third message should be user message")
      (is (= "test message" (:content (nth messages 2)))))))

(def simple-response
  (j/read-value
   "{
     \"model\": \"llama2:3.2\",
     \"created_at\": \"2024-03-21T12:34:56Z\",
     \"message\": {
       \"role\": \"assistant\",
       \"content\": \"Hello! How can I help you today?\"
     },
     \"done\": true,
     \"total_duration\": 1234567890,
     \"load_duration\": 123456,
     \"prompt_eval_count\": 42,
     \"eval_count\": 123,
     \"eval_duration\": 987654
   }"
   j/keyword-keys-object-mapper))

(deftest ollama-response-schema-test
  (testing "response schema validation"
    (is (nil? (me/humanize (m/explain model/OllamaResponse simple-response))))))
