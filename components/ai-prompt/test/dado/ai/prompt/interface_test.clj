(ns dado.ai.prompt.interface-test
  (:require [clojure.test :refer [deftest testing is]]
            [dado.ai.prompt.interface :as prompt]
            [babashka.fs :as fs]))

(deftest construct-prompt-test
  (fs/with-temp-dir [temp-dir {:prefix "dado-prompt-test-"}]
    (let [prompt-dir (fs/path temp-dir "ai" "prompts")]
      (fs/create-dirs prompt-dir)
      (spit (fs/path prompt-dir "template1.md") "Hello {{name}}")
      (spit (fs/path prompt-dir "template2.md") "Your request is: {{request}}")

      (binding [dado.ai.prompt.core/prompt-dir (str prompt-dir)]
        (testing "successful prompt construction"
          (let [result (prompt/construct-prompt ["template1" "template2"]
                                                {:name "AI" :request "Tell me a joke"})]
            (is (= "Hello AI\nYour request is: Tell me a joke" result))))

        (testing "missing template"
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Missing template file"
                                (prompt/construct-prompt ["missing-template"] {}))))

        (testing "malformed template"
          (spit (fs/path prompt-dir "bad-template.md") "{{unclosed")
          (is (thrown-with-msg? selmer.parser.TemplateParserException
                                #""
                                (prompt/construct-prompt ["bad-template"] {}))))

        (testing "missing data"
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Missing data for prompt substitution"
                                (prompt/construct-prompt ["template1"] {}))))))))
