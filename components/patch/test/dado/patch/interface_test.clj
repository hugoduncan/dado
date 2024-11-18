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
               (patch/apply-simplified-diff-patch! patch)))
        (is (= "line1\nline2 updated\nline3\nline4\n" (slurp file1)))
        (is (= "line1\nline2 updated\nline3\nline4 updated\n" (slurp file2))))))

  (testing "patch with invalid format"
    (let [invalid-patch "invalid patch content"]
      (try (patch/apply-simplified-diff-patch! invalid-patch)
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
          (try (patch/apply-simplified-diff-patch! patch)
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
        (try (patch/apply-simplified-diff-patch! patch)
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
               (patch/apply-simplified-diff-patch! patch)))
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
               (patch/apply-simplified-diff-patch! patch)))
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
               (patch/apply-simplified-diff-patch! patch)))
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
               (patch/apply-simplified-diff-patch! patch)))
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
        (try (patch/apply-simplified-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed"
                      (ex-message (root-cause e)))))))))

  (testing "patch with non-trivial context"
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
               (patch/apply-simplified-diff-patch! patch)))

        (is (= "line A\nline B\nline C\nline A\nline B\nline D updated\n"
               (slurp file))))))

  (testing "patch with post context content"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file
                        (str
                         "line A\n"
                         "line B\n"
                         "line C\n"
                         "line D\n"))
            patch (str "--- " (.getPath file) "\n"
                       "+++ " (.getPath file) "\n"
                       "@@ ... @@\n"
                       " line A\n"
                       "-line B\n"
                       "+line B updated\n"
                       " line C\n"                       )]
        (is (= {(.getPath file) {:lines-added 1 :lines-removed 1}}
               (patch/apply-simplified-diff-patch! patch)))

        (is (= "line A\nline B updated\nline C\nline D\n"
               (slurp file)))))))

(deftest apply-search-replace-diff-patch!-test
  (testing "successful edit operation"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file1 (fs/file temp-dir "file1.txt")
            file2 (fs/file temp-dir "file2.txt")
            _     (spit file1 "line1\nline2\nline3\nline4")
            _     (spit file2 "line1\nline2\nline3\nline4")
            patch (str "EDIT " (.getPath file1) "\n"
                       "<<<<<<< SEARCH\n"
                       "line1\n"
                       "line2\n"
                       "line3\n"
                       "line4\n"
                       "=======\n"
                       "line1\n"
                       "line2 updated\n"
                       "line3\n"
                       "line4\n"
                       ">>>>>>> REPLACE\n"
                       "EDIT " (.getPath file2) "\n"
                       "<<<<<<< SEARCH\n"
                       "line1\n"
                       "line2\n"
                       "line3\n"
                       "line4\n"
                       "=======\n"
                       "line1\n"
                       "line2 updated\n"
                       "line3\n"
                       "line4 updated\n"
                       ">>>>>>> REPLACE\n")]
        (is (= [{:op :edit :paths [(.getPath file1)]}
                {:op :edit :paths [(.getPath file2)]}]
               (patch/apply-search-replace-diff-patch! patch)))
        (is (= "line1\nline2 updated\nline3\nline4\n" (slurp file1)))
        (is (= "line1\nline2 updated\nline3\nline4 updated\n" (slurp file2))))))

  (testing "invalid patch format"
    (let [invalid-patches ["invalid patch content"
                           "EDIT\n" ; missing path
                           "MOVE target/path\n" ; missing source
                           "COPY target/path\n"]] ; missing source
      (doseq [invalid-patch invalid-patches]
        (testing (str "patch: " invalid-patch)
          (try (patch/apply-search-replace-diff-patch! invalid-patch)
               (is false "should throw")
               (catch Exception e
                 (prn :e e)
                 (is (= "Invalid patch format"
                        (ex-message (root-cause e))))))))))

  (testing "patch with missing file"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [missing-file (fs/file temp-dir "missing.txt")
            patch        (str "EDIT " (.getPath missing-file) "\n"
                              "<<<<<<< SEARCH\n"
                              "line1\n"
                              "line2\n"
                              "=======\n"
                              "line1\n"
                              "line2 updated"
                              ">>>>>>> REPLACE")]
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed"
                      (ex-message (root-cause e)))))))))

  (testing "patch with context mismatch"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "line1\nline2\nline3")
            patch (str "EDIT " (.getPath file) "\n"
                       "<<<<<<< SEARCH\n"
                       "line1\n"
                       "line2 modified\n"
                       "=======\n"
                       "line1\n"
                       "line2 updated"
                       ">>>>>>> REPLACE")]
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed"
                      (ex-message (root-cause e))))))        )))

  (testing "patch creating a new file"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [new-file (fs/file temp-dir "new-file.txt")
            patch    (str "CREATE " (.getPath new-file) "\n"
                          "new line1\n"
                          "new line2\n")]
        (is (= [{:op :create :paths [(.getPath new-file)]}]
               (patch/apply-search-replace-diff-patch! patch)))
        (is (= "new line1\nnew line2\n" (slurp new-file))))))

  (testing "patch updating an empty file"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "")
            patch (str "EDIT " (.getPath file) "\n"
                       "<<<<<<< SEARCH\n"
                       "=======\n"
                       "new line1\n"
                       "new line2\n"
                       ">>>>>>> REPLACE")]
        (is (= [{:op :edit :paths [(.getPath file)]}]
               (patch/apply-search-replace-diff-patch! patch)))
        (is (= "new line1\nnew line2\n" (slurp file))))))

  (testing "patch updating with final line with no newline"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "line1") ; no \n
            patch (str "EDIT " (.getPath file) "\n"
                       "<<<<<<< SEARCH\n"
                       "line1\n"
                       "=======\n"
                       "new line1\n"
                       ">>>>>>> REPLACE"
                       )]
        (is (= [{:op :edit :paths [(.getPath file)]}]
               (patch/apply-search-replace-diff-patch! patch)))
        (is (= "new line1\n" (slurp file))))))

  (testing "patch with insufficient context"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file
                        (str "line1\n"
                             "line1\n"
                             "line1\n"))
            patch (str "EDIT " (.getPath file) "\n"
                       "<<<<<<< SEARCH\n"
                       "line1\n"
                       "=======\n"
                       "new line1\n"
                       ">>>>>>> REPLACE")]
        (is (= "line1\nline1\nline1\n" (slurp file)))
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed"
                      (ex-message (root-cause e)))))))))

  (testing "patch with non-trivial context"
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
            patch (str "EDIT " (.getPath file) "\n"
                       "<<<<<<< SEARCH\n"
                       "line A\n"
                       "line B\n"
                       "line D\n"
                       "=======\n"
                       "line A\n"
                       "line B\n"
                       "line D updated\n"
                       ">>>>>>> REPLACE" )]
        (is (=  [{:op :edit :paths [(.getPath file)]}]
                (patch/apply-search-replace-diff-patch! patch)))

        (is (= "line A\nline B\nline C\nline A\nline B\nline D updated\n"
               (slurp file))))))

  (testing "successful delete operation"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file "content to delete")
            patch (str "DELETE " (.getPath file))]
        (is (fs/exists? file))
        (is (=  [{:op :delete :paths [(.getPath file)]}]
                (patch/apply-search-replace-diff-patch! patch)))
        (is (not (fs/exists? file))))))

  (testing "delete non-existent file"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "does-not-exist.txt")
            patch (str "DELETE " (.getPath file))]
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed" (ex-message (root-cause e)))))))))

  (testing "successful move operation"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-" :dir "."}]
      (let [source (fs/file temp-dir "source.txt")
            target (fs/file temp-dir "target.txt")
            _      (spit source "content to move")
            patch  (str "MOVE " (.getPath source) " " (.getPath target))]
        (is (fs/exists? source))
        (is (not (fs/exists? target)))
        (is (= [{:op :move :paths [(.getPath source) (.getPath target)]}]
               (patch/apply-search-replace-diff-patch! patch)))
        (is (not (fs/exists? source)))
        (is (fs/exists? target))
        (is (= "content to move" (slurp target))))))

  (testing "move with missing source"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [source (fs/file temp-dir "missing.txt")
            target (fs/file temp-dir "target.txt")
            patch  (str "MOVE " (.getPath target) " " (.getPath source))]
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed" (ex-message (root-cause e)))))))))

  (testing "move with existing target"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [source (fs/file temp-dir "source.txt")
            target (fs/file temp-dir "target.txt")
            _      (spit source "source content")
            _      (spit target "target content")
            patch  (str "MOVE " (.getPath target) " " (.getPath source))]
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed" (ex-message (root-cause e)))))))))

  (testing "successful copy operation"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [source (fs/file temp-dir "source.txt")
            target (fs/file temp-dir "target.txt")
            _      (spit source "content to copy")
            patch  (str "COPY " (.getPath source) " " (.getPath target))]
        (is (fs/exists? source))
        (is (not (fs/exists? target)))
        (is (= [{:op :copy :paths [(.getPath source) (.getPath target)]}]
               (patch/apply-search-replace-diff-patch! patch)))
        (is (fs/exists? source))
        (is (fs/exists? target))
        (is (= "content to copy" (slurp target))))))

  (testing "copy with missing source"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [source (fs/file temp-dir "missing.txt")
            target (fs/file temp-dir "target.txt")
            patch  (str "COPY " (.getPath target) " " (.getPath source))]
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed" (ex-message (root-cause e)))))))))

  (testing "copy with existing target"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [source (fs/file temp-dir "source.txt")
            target (fs/file temp-dir "target.txt")
            _      (spit source "source content")
            _      (spit target "target content")
            patch  (str "COPY " (.getPath target) " " (.getPath source))]
        (try (patch/apply-search-replace-diff-patch! patch)
             (is false "should throw")
             (catch Exception e
               (is (= "Patch application failed" (ex-message (root-cause e)))))))))

  (testing "multiple operations in single patch"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [edit-file   (fs/file temp-dir "edit.txt")
            source-file (fs/file temp-dir "source.txt")
            move-target (fs/file temp-dir "moved.txt")
            copy-target (fs/file temp-dir "copied.txt")
            delete-file (fs/file temp-dir "delete.txt")
            _           (spit edit-file "line1\nline2\nline3")
            _           (spit source-file "move/copy content")
            _           (spit delete-file "to be deleted")
            patch       (str "EDIT " (.getPath edit-file) "\n"
                             "<<<<<<< SEARCH\n"
                             "line1\n"
                             "line2\n"
                             "line3\n"
                             "=======\n"
                             "line1\n"
                             "updated\n"
                             "line3\n"
                             ">>>>>>> REPLACE\n"
                             "COPY " (.getPath source-file) " " (.getPath copy-target) "\n"
                             "MOVE " (.getPath source-file) " " (.getPath move-target) "\n"
                             "DELETE " (.getPath delete-file))]
        (let [result (patch/apply-search-replace-diff-patch! patch)]
          (is (= [{:op :edit :paths [(.getPath edit-file)]}
                  {:op :copy :paths [(.getPath source-file) (.getPath copy-target)]}
                  {:op :move :paths [(.getPath source-file) (.getPath move-target)]}
                  {:op :delete :paths [(.getPath delete-file)] }]
                 result)))
        (is (= "line1\nupdated\nline3\n"
               (slurp edit-file)))
        (is (not (fs/exists? source-file)))
        (is (= "move/copy content"
               (slurp move-target)))
        (is (= "move/copy content"
               (slurp copy-target)))
        (is (not (fs/exists? delete-file))))))

  (testing "patch with post context content"
    (fs/with-temp-dir [temp-dir {:prefix "dado-patch-test-"}]
      (let [file  (fs/file temp-dir "file.txt")
            _     (spit file
                        (str
                         "line A\n"
                         "line B\n"
                         "line C\n"
                         "line D\n"))
            patch (str "EDIT " (.getPath file) "\n"
                       "<<<<<<< SEARCH\n"
                       "line A\n"
                       "line B\n"
                       "line C\n"
                       "=======\n"
                       "line A\n"
                       "line B updated\n"
                       "line C\n"
                       ">>>>>>> REPLACE")]
        (is (= [ {:op :edit :paths [(.getPath file)]}]
               (patch/apply-search-replace-diff-patch! patch)))

        (is (= "line A\nline B updated\nline C\nline D\n"
               (slurp file)))))))
