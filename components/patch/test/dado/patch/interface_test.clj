(ns dado.patch.interface-test
  (:require
   [babashka.fs :as fs]
   [clojure.stacktrace :refer [root-cause]]
   [clojure.test :refer [deftest is testing]]
   [dado.patch.interface :as patch]))

(deftest apply-patch!-test
  (testing "successful patch application"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file1 (fs/file temp-dir "file1.txt")
            file2 (fs/file temp-dir "file2.txt")
            _     (spit file1 "line1\nline2\nline3\nline4")
            _     (spit file2 "line1\nline2\nline3\nline4")
            patch (str "--- " (.getPath file1) "\n"
                       "+++ " (.getPath file1) "\n"
                       "@@ ... @@\n"
                       " line1\n"
                       "-line2\n"
                       "+line2 updated\n"
                       " line3\n"
                       " line4\n"
                       "--- " (.getPath file2) "\n"
                       "+++ " (.getPath file2) "\n"
                       "@@ ... @@\n"
                       " line1\n"
                       "-line2\n"
                       "+line2 updated\n"
                       " line3\n"
                       "-line4\n"
                       "+line4 updated\n")]
        (is (= {(.getPath file1) {:lines-added 1 :lines-removed 1}
                (.getPath file2) {:lines-added 2 :lines-removed 2}}
               (patch/apply-patch! patch)))
        (is (= "line1\nline2 updated\nline3\nline4\n" (slurp file1)))
        (is (= "line1\nline2 updated\nline3\nline4 updated\n" (slurp file2))))))

  (testing "patch with invalid format"
    (let [invalid-patch "invalid patch content"]
      (try (patch/apply-patch! invalid-patch)
           (is false "should throw")
           (catch Exception e
             (is (= "Invalid patch format" (ex-message (root-cause e)))))))

    (testing "patch with missing file"
      (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
        (let [missing-file (fs/file temp-dir "missing.txt")
              patch        (str "--- " (.getPath missing-file) "\n"
                                "+++ " (.getPath missing-file) "\n"
                                "@@ ... @@\n"
                                " line1\n"
                                "-line2\n"
                                "+line2 updated")]
          (try (patch/apply-patch! patch)
               (is false "should throw")
               (catch Exception e
                 (is (= "Patch application failed"
                        (ex-message (root-cause e))))))))))

  (testing "patch with context mismatch"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "line1\nline2\nline3")
            patch (str "--- " (.getPath file) "\n"
                       "+++ " (.getPath file) "\n"
                       "@@ ... @@\n"
                       " line1\n"
                       "-line2 modified\n"
                       "+line2 updated")]
        (try (patch/apply-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed"
                      (ex-message (root-cause e))))))        )))

  (testing "patch creating a new file"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [new-file (fs/file temp-dir "new-file.txt")
            patch    (str "--- /dev/null\n"
                          "+++ " (.getPath new-file) "\n"
                          "@@ ... @@\n"
                          "+new line1\n"
                          "+new line2\n")]
        (is (= {(.getPath new-file) {:lines-added 2 :lines-removed 0}}
               (patch/apply-patch! patch)))
        (is (= "new line1\nnew line2\n" (slurp new-file))))))

  (testing "patch updating an empty file"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "")
            patch (str "--- " (.getPath file) "\n"
                       "+++ " (.getPath file) "\n"
                       "@@ ... @@\n"
                       "+new line1\n"
                       "+new line2\n")]
        (is (= {(.getPath file) {:lines-added 2 :lines-removed 0}}
               (patch/apply-patch! patch)))
        (is (= "new line1\nnew line2\n" (slurp file))))))

  (testing "patch updating with empty final line"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "line1")
            patch (str "--- " (.getPath file) "\n"
                       "+++ " (.getPath file) "\n"
                       "@@ ... @@\n"
                       "-line1\n"
                       "+line1\n"
                       )]
        (is (= {(.getPath file) {:lines-added 1 :lines-removed 1}}
               (patch/apply-patch! patch)))
        (is (= "line1\n" (slurp file))))))

  (testing "patch updating final line with no newline"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "line1")
            patch (str "--- " (.getPath file) "\n"
                       "+++ " (.getPath file) "\n"
                       "@@ ... @@\n"
                       "-line1\n"
                       "+line11\n"
                       "\\ No newline at end of file"
                       )]
        (is (= {(.getPath file) {:lines-added 1 :lines-removed 1}}
               (patch/apply-patch! patch)))
        (is (= "line11" (slurp file))))))

  (testing "patch with insufficient context"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "line1")
            patch (str "--- " (.getPath file) "\n"
                       "+++ " (.getPath file) "\n"
                       "@@ ... @@\n"
                       "-line1"
                       "+line1 updated\n")]
        (is (= "line1" (slurp file)))
        (try (patch/apply-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed"
                      (ex-message (root-cause e)))))))))

  (testing "patch with insufficient context"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file
                        (str
                         "line A\n"
                         "line B\n"
                         "line C\n"
                         "line A\n"
                         "line B\n"
                         "line D\n"))
            patch (str "--- " (.getPath file) "\n"
                       "+++ " (.getPath file) "\n"
                       "@@ ... @@\n"
                       " line A\n"
                       " line B\n"
                       "-line D\n"
                       "+line D updated\n")]
        (is (= {(.getPath file) {:lines-added 1 :lines-removed 1}}
               (patch/apply-patch! patch)))

        (is (= "line A\nline B\nline C\nline A\nline B\nline D updated"
               (slurp file)))))))
