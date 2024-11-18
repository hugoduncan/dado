(ns dado.ai.claude.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.ai.claude.interface :as claude]))

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
