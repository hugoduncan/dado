(ns dado.ai.tools.git.integration-test
  "Integration tests for git tools."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [dado.ai.tools.git.interface :as git]
   [dado.ai.tools.git.core :as git-core]
   [dado.ai.tools.git.test-helpers :as helpers]))

(defn with-git-repo
  "Test fixture that creates a temporary git repository."
  [f]
  (let [repo-dir (helpers/create-temp-git-repo!)]
    (try
      (binding [git-core/*repo-dir* repo-dir]
        ;; Change to repo directory for running tests
        (let [original-dir (System/getProperty "user.dir")]
          (try
            (System/setProperty "user.dir" repo-dir)
            (f)
            (finally
              (System/setProperty "user.dir" original-dir)))))
      (finally
        (helpers/delete-temp-git-repo! repo-dir)))))

(use-fixtures :each with-git-repo)

(deftest git-status-integration-test
  (testing "status with no changes shows only initial files"
    (let [tool   (git/create-tool :dado/git-status)
          result ((:execute-fn tool) {})]
      (is (= "main" (get-in result [:content :branch])))
      (is (empty? (get-in result [:content :modified])))
      (is (empty? (get-in result [:content :staged])))
      (is (empty? (get-in result [:content :untracked])))))

  (testing "status shows untracked files"
    (helpers/create-test-file! git-core/*repo-dir* "test.txt" "test content")
    (let [tool   (git/create-tool :dado/git-status)
          result ((:execute-fn tool) {})]
      (is (= ["test.txt"] (get-in result [:content :untracked])))))

  (testing "status shows modified files"
    (helpers/create-test-file! git-core/*repo-dir* "test.txt" "test content")
    (let [stage-tool  (git/create-tool :dado/git-stage)
          _           ((:execute-fn stage-tool) {:paths ["test.txt"]})
          commit-tool (git/create-tool :dado/git-commit)
          _           ((:execute-fn commit-tool) {:message "Add test file"})
          _           (helpers/modify-test-file!
                       git-core/*repo-dir* "test.txt" "\nmore content")
          status-tool (git/create-tool :dado/git-status)
          result      ((:execute-fn status-tool) {})]
      (is (= ["test.txt"] (get-in result [:content :modified]))))))

(deftest git-stage-integration-test
  (testing "stages specific files"
    (helpers/create-test-file! git-core/*repo-dir* "test1.txt" "test content 1")
    (helpers/create-test-file! git-core/*repo-dir* "test2.txt" "test content 2")
    (let [tool   (git/create-tool :dado/git-stage)
          result ((:execute-fn tool) {:paths ["test1.txt"]})]
      (is (= ["test1.txt"] (get-in result [:content :staged])))
      (is (empty? (get-in result [:content :errors])))))

  (testing "stages all changes when no paths specified"
    (helpers/create-test-file! git-core/*repo-dir* "test1.txt" "test content 1")
    (helpers/create-test-file! git-core/*repo-dir* "test2.txt" "test content 2")
    (let [tool   (git/create-tool :dado/git-stage)
          result ((:execute-fn tool) {:paths nil})]
      (is (= #{"test1.txt" "test2.txt"}
             (set (get-in result [:content :staged]))))
      (is (empty? (get-in result [:content :errors]))))))

(deftest git-commit-integration-test
  (testing "commits staged changes"
    (helpers/create-test-file! git-core/*repo-dir* "test.txt" "test content")
    (let [stage-tool  (git/create-tool :dado/git-stage)
          _           ((:execute-fn stage-tool) {:paths ["test.txt"]})
          commit-tool (git/create-tool :dado/git-commit)
          result      ((:execute-fn commit-tool) {:message "Add test file"})]
      (is (string? (get-in result [:content :commit :hash])))
      (is (= "Add test file" (get-in result [:content :commit :message])))
      (let [summary (get-in result [:content :commit :summary])]
        (is (= 1 (:files summary)))
        (is (= 1 (:insertions summary)))
        (is (= 0 (:deletions summary)))))))
