(ns dado.ai.claude.interface-test
  (:require
   [clojure.string :as str]
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
