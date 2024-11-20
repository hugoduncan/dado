(ns dado.ai.prompt.interface-test
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dado.ai.prompt.interface :as prompt]))

(deftest construct-prompt-test
  (fs/with-temp-dir [temp-dir {:prefix "dado-prompt-test-"}]
    (let [prompt-dir (fs/path temp-dir "ai" "prompts")]
      (fs/create-dirs prompt-dir)
      (spit (fs/file prompt-dir "template1.md") "Hello {{name}}")
      (spit (fs/file prompt-dir "template2.md") "Your request is: {{request}}")

      (let [project-config {:directories {:dado/prompts (str prompt-dir)}}]
        (testing "successful prompt construction from project templates"
          (let [result (prompt/construct-prompt
                        project-config
                        ["template1" "template2"]
                        {:name "AI" :request "Tell me a joke"})]
            (is (= "Hello AI\nYour request is: Tell me a joke" result))))

        (testing "template fallback to resources"
          (with-redefs
            [io/resource (fn [path]
                           (when (= path "dado/ai/prompts/resource-template.md")
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

(deftest interface-paths-test
  (fs/with-temp-dir [temp-dir {:prefix "dado-prompt-test-"}]
    (let [component-name "test-component"
          component-dir  (fs/path temp-dir
                                  "components"
                                  component-name
                                  "src"
                                  "dado"
                                  "test")
          interface-file (fs/path component-dir "interface.clj")]
      (fs/create-dirs component-dir)
      (spit (fs/file interface-file) "(ns dado.test.interface)")

      (testing "finds interface files"
        (with-redefs [fs/cwd (constantly (fs/file temp-dir))]
          (let [paths (prompt/interface-paths "test-component")]
            (is (= 1 (count paths)))
            (is (str/ends-with? (first paths) "interface.clj"))))))))

(deftest implementation-paths-test
  (fs/with-temp-dir [temp-dir {:prefix "dado-prompt-test-"}]
    (let [component-name "test-component"
          component-dir  (fs/path temp-dir
                                  "components"
                                  component-name
                                  "src"
                                  "dado"
                                  "test")
          edn-file       (fs/file temp-dir "components" component-name "deps.edn")
          core-file      (fs/path component-dir "core.clj")
          model-file     (fs/path component-dir "model.clj")
          proto-file     (fs/path component-dir "protocol.clj")]
      (fs/create-dirs component-dir)
      (spit (fs/file edn-file) "{}")
      (spit (fs/file core-file) "(ns dado.test.core)")
      (spit (fs/file model-file) "(ns dado.test.model)")
      (spit (fs/file proto-file) "(ns dado.test.protocol)")

      (testing "finds implementation files"
        (with-redefs [fs/cwd (constantly (fs/file temp-dir))]
          (let [paths (prompt/implementation-paths component-name)]
            (prn :paths paths)
            (is (= 4 (count paths)))
            (is (some #(str/ends-with? % "core.clj") paths))
            (is (some #(str/ends-with? % "model.clj") paths))
            (is (some #(str/ends-with? % "protocol.clj") paths))))))))
