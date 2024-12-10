(ns dado.ai.tools.matching-file.core-test
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [dado.ai.tools.matching-file.core :as core]
   [taoensso.encore :refer [throws?]]))

(def ^:private test-files
  {"test1.txt" "This is a test file
                  with multiple lines
                  and some test content
                  that we can search"

   "test2.txt" "Another test file
                  for our testing needs
                  with TEST in caps"

   "dir/test3.txt" "Nested file
                      with test data
                      in a subdirectory"})

(def ^:dynamic *temp-dir* nil)

(defn- setup-test-files [f]
  (fs/create-dirs "target")
  (fs/with-temp-dir [dir {:dir "target"}]
    (fs/create-dirs (fs/path dir "dir"))
    (binding [*temp-dir* dir]
      (doseq [[path content] test-files]
        (let [file (fs/file dir path)]
          (spit file content)))
      (f))
    (fs/delete-tree dir)))

(use-fixtures :each setup-test-files)

(deftest execute-tool-test
  (let [glob @#'fs/glob]
    (with-redefs [fs/glob
                  (fn [_ gpattern] (glob *temp-dir* gpattern nil))]
      (testing "exact match search"
        (let [result (core/execute-tool! {:pattern "test"})]
          (is (= #{:matches :truncated?} (set (keys result))))
          (is (= 3 (count (:matches result)))
              "finds matches in all files")
          (is (false? (:truncated? result))
              "results not truncated")))

      (testing "case sensitive search"
        (let [result (core/execute-tool!
                      {:pattern         "TEST"
                       :case-sensitive? true})]
          (is (= 1 (count (:matches result)))
              "only finds exact case matches")))

      (testing "regex search"
        (let [result (core/execute-tool!
                      {:pattern "test +"
                       :mode    :regex})]
          (is (= 3 (count (:matches result)))
              "finds regex matches")))

      (testing "with file extensions"
        (let [result (core/execute-tool!
                      {:pattern    "test"
                       :extensions [".txt"]})]
          (is (= 3 (count (:matches result)))
              "finds matches in .txt files")))

      (testing "with max matches"
        (let [result (core/execute-tool!
                      {:pattern     "test"
                       :max-matches 2})]
          (is (= 2 (count (:matches result)))
              "limits number of matches")

          (is (true? (:truncated? result))
              "indicates results were truncated"))

        (testing "with context lines"
          (let [result (core/execute-tool!
                        {:pattern       "test"
                         :context-lines 1})]
            (is (every?
                 (comp (fn [outer]
                         (every? #(contains? % :context) (:matches outer)))
                       :matches)
                 (:matches result))
                "includes context in results")))

        (testing "invalid parameters"
          (is (throws?
               clojure.lang.ExceptionInfo
               #"Invalid regex pattern"
               (core/execute-tool!
                {:pattern "[invalid"
                 :mode    :regex}))
              "throws on invalid regex")

          (is (throws?
               clojure.lang.ExceptionInfo
               #"Invalid search parameters"
               (core/execute-tool! {}))
              "throws on missing pattern"))))))
