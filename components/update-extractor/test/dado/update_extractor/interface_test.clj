(ns dado.update-extractor.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.update-extractor.interface :as extractor]))

(deftest extract-diffs-test
  (testing "validates input is string"
    (is (thrown? clojure.lang.ExceptionInfo
                 (extractor/extract-diffs nil))))

  (testing "returns empty string when no diff blocks found"
    (is (= "" (extractor/extract-diffs "no diff blocks here"))))

  (testing "extracts single diff block"
    (let [input    "Some text\n```diff\n@@ test @@\n-old\n+new\n```\nmore text"
          expected "@@ test @@\n-old\n+new\n"]
      (is (= expected (extractor/extract-diffs input)))))

  (testing "concatenates multiple diff blocks"
    (let [input    (str "```diff\n@@ block1 @@\n-a\n+b\n```\n"
                        "between\n"
                        "```diff\n@@ block2 @@\n-c\n+d\n```")
          expected "@@ block1 @@\n-a\n+b\n@@ block2 @@\n-c\n+d\n"]
      (is (= expected (extractor/extract-diffs input))))))
