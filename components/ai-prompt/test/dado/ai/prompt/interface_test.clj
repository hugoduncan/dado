(ns dado.ai.prompt.interface-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [dado.ai.prompt.interface :as prompt]
            [babashka.fs :as fs]))

(deftest construct-prompt-test
  (fs/with-temp-dir [temp-dir {:prefix "dado-prompt-test-"}]
    (let [prompt-dir (fs/path temp-dir "ai" "prompts")]
      (fs/create-dirs prompt-dir)
      (spit (fs/file prompt-dir "template1.md") "Hello {{name}}")
      (spit (fs/file prompt-dir "template2.md") "Your request is: {{request}}")

      (let [project-config {:dev-dir (str temp-dir)}]
        (testing "successful prompt construction from project templates"
          (let [result (prompt/construct-prompt
                       project-config
                       ["template1" "template2"]
                       {:name "AI" :request "Tell me a joke"})]
            (is (= "Hello AI\nYour request is: Tell me a joke" result))))

        (testing "template fallback to resources"
          (with-redefs [io/resource (fn [path] 
                                    (when (= path "dev/ai/prompts/resource-template.md")
                                      (io/input-stream (.getBytes "Resource {{type}}"))))]
            (let [result (prompt/construct-prompt
                         project-config
                         ["resource-template"]
                         {:type "test"})]
              (is (= "Resource test" result)))))

        (testing "template not found in either location"
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"Template not found in project or resources"
               (prompt/construct-prompt
                project-config
                ["missing-template"]
                {}))))

        (testing "malformed template"
          (spit (fs/file prompt-dir "bad-template.md") "{{unclosed")
          (is (thrown? clojure.lang.ExceptionInfo
                      (prompt/construct-prompt
                       project-config
                       ["bad-template"]
                       {}))))

        (testing "missing data"
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"Missing data for prompt substitution"
               (prompt/construct-prompt
                project-config
                ["template1"]
                {}))))))))
