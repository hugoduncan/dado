(ns dado.ai.tools.git.test-helpers
  "Test helpers for git tool tests."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [taoensso.truss :refer [have?]]))

(defn create-temp-git-repo!
  "Create a temporary git repository.
   Returns path to repo directory.
   Caller must delete directory when done."
  []
  (let [temp-dir (str (fs/create-temp-dir))]
    @(p/process ["git" "init"] {:dir temp-dir})
    ;; Set git config for tests
    @(p/process ["git" "config" "user.email" "test@example.com"] {:dir temp-dir})
    @(p/process ["git" "config" "user.name" "Test User"] {:dir temp-dir})
    ;; Create initial commit
    (let [readme-file (fs/file temp-dir "README.md")]
      (spit readme-file "# Test Repository")
      @(p/process ["git" "add" "README.md"] {:dir temp-dir})
      @(p/process ["git" "commit" "-m" "Initial commit"] {:dir temp-dir}))
    ;; Set up origin tracking
    @(p/process ["git" "branch" "-M" "main"] {:dir temp-dir})
    temp-dir))

(defn delete-temp-git-repo!
  "Delete temporary git repository."
  [repo-dir]
  (fs/delete-tree repo-dir))

(defn create-test-file!
  "Create a test file in the repository."
  [repo-dir filename content]
  {:pre [(have? repo-dir)]}
  (let [file-path (fs/path repo-dir filename)]
    (fs/create-dirs (fs/parent file-path))
    (spit (fs/file file-path) content)))

(defn modify-test-file!
  "Modify an existing test file in the repository."
  [repo-dir filename content]
  (let [file-path (fs/path repo-dir filename)]
    (spit (fs/file file-path) content :append true)))
