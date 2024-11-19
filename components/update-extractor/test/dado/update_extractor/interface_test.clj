(ns dado.update-extractor.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.update-extractor.interface :as extractor]))

(deftest extract-diffs-test
  (testing "validates input is string"
    (is (thrown? clojure.lang.ExceptionInfo
                 (extractor/extract-simplified-diffs nil))))

  (testing "returns empty string when no diff blocks found"
    (is (= "" (extractor/extract-simplified-diffs "no diff blocks here"))))

  (testing "extracts single diff block"
    (let [input    "Some text\n```diff\n@@ test @@\n-old\n+new\n```\nmore text"
          expected "@@ test @@\n-old\n+new\n"]
      (is (= expected (extractor/extract-simplified-diffs input)))))

  (testing "concatenates multiple diff blocks"
    (let [input    (str "```diff\n@@ block1 @@\n-a\n+b\n```\n"
                        "between\n"
                        "```diff\n@@ block2 @@\n-c\n+d\n```")
          expected "@@ block1 @@\n-a\n+b\n@@ block2 @@\n-c\n+d\n"]
      (is (= expected (extractor/extract-simplified-diffs input))))))

(deftest extract-updated-namespaces-test
  (testing "validates input is string"
    (is (thrown? clojure.lang.ExceptionInfo
                 (extractor/extract-updated-namespaces nil))))

  (testing "returns empty sequence when no namespace blocks found"
    (is (empty? (extractor/extract-updated-namespaces "no namespace blocks here"))))

  (testing "extracts single namespace block"
    (let [input    "Some text\n```updated-namespaces\nmy.project.model\nmy.project.core\n```\nmore text"
          expected ["my.project.model" "my.project.core"]]
      (is (= expected (extractor/extract-updated-namespaces input)))))

  (testing "handles empty lines in namespace block"
    (let [input    "```updated-namespaces\nmy.project.model\n\nmy.project.core\n```"
          expected ["my.project.model" "my.project.core"]]
      (is (= expected (extractor/extract-updated-namespaces input)))))

  (testing "combines multiple namespace blocks"
    (let [input    (str "```updated-namespaces\nmy.project.model\n```\n"
                        "between\n"
                        "```updated-namespaces\nmy.project.core\n```")
          expected ["my.project.model" "my.project.core"]]
      (is (= expected (extractor/extract-updated-namespaces input))))))
