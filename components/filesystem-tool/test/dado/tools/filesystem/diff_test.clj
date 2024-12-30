(ns dado.tools.filesystem.diff-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.tools.filesystem.diff :as diff]))

(deftest normalize-line-endings-test
  (testing "converts CRLF to LF"
    (is (= "line1\nline2\nline3"
           (diff/normalize-line-endings "line1\r\nline2\r\nline3")))
    (is (= "single line"
           (diff/normalize-line-endings "single line"))))

  (testing "handles mixed line endings"
    (is (= "line1\nline2\nline3"
           (diff/normalize-line-endings "line1\nline2\r\nline3")))))

(deftest diff-lines-test
  (testing "identical content returns context lines"
    (let [lines ["line1" "line2"]
          result (diff/diff-lines lines lines)]
      (is (= [[nil "line1"]
              [nil "line2"]]
             result))))

  (testing "handles deletions"
    (let [orig ["line1" "delete" "line3"]
          mod  ["line1" "line3"]
          result (diff/diff-lines orig mod)]
      (is (= [[nil "line1"]
              [:- "delete"]
              [nil "line3"]]
             result))))

  (testing "handles additions"
    (let [orig ["line1" "line3"]
          mod  ["line1" "new" "line3"]
          result (diff/diff-lines orig mod)]
      (is (= [[nil "line1"]
              [:+ "new"]
              [nil "line3"]]
             result))))

  (testing "handles modifications"
    (let [orig ["line1" "old" "line3"]
          mod  ["line1" "new" "line3"]
          result (diff/diff-lines orig mod)]
      (is (= [[nil "line1"]
              [:- "old"]
              [:+ "new"]
              [nil "line3"]]
             result)))))

(deftest create-unified-diff-test
  (testing "creates unified diff format"
    (let [original "line1\nold\nline3"
          modified "line1\nnew\nline3"
          result   (diff/create-unified-diff original modified "test.txt")]
      (is (= (str "--- test.txt\n"
                  "+++ test.txt\n"
                  " line1\n"
                  "-old\n"
                  "+new\n"
                  " line3")
             result))))

  (testing "handles empty files"
    (is (= (str "--- test.txt\n"
                "+++ test.txt\n")
           (diff/create-unified-diff "" "" "test.txt"))))

  (testing "normalizes line endings in input"
    (let [original "line1\r\nold\r\nline3"
          modified "line1\nnew\nline3"
          result   (diff/create-unified-diff original modified "test.txt")]
      (is (= (str "--- test.txt\n"
                  "+++ test.txt\n"
                  " line1\n"
                  "-old\n"
                  "+new\n"
                  " line3")
             result)))))
