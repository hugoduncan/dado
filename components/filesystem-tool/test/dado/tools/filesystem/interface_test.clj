(ns dado.tools.filesystem.interface-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [dado.tools.filesystem.interface :as filesystem]
   [dado.tools.filesystem.core :as core]
   [jsonista.core :as j]))

;; Test utilities
(def ^:dynamic *temp-dir* nil)

(defn with-temp-dir
  "Test fixture that creates a temporary directory"
  [f]
  (fs/with-temp-dir [temp-dir {:prefix "fst-"}]
    (binding [*temp-dir* temp-dir]
      (f))))

(use-fixtures :each with-temp-dir)

(defn- create-test-file!
  "Creates a test file with content in temp dir"
  [path content]
  (let [full-path (fs/path *temp-dir* path)]
    (fs/create-dirs (fs/parent full-path))
    (spit (fs/file full-path) content)
    path))

(defn- file-content
  "Gets content of file in temp dir"
  [path]
  (slurp (fs/file *temp-dir* path)))

(defn- create-test-dir!
  "Creates a test directory in temp dir"
  [path]
  (fs/create-dirs (fs/path *temp-dir* path))
  path)

(defn- path-exists?
  "Checks if path exists in temp dir"
  [path]
  (fs/exists? (fs/path *temp-dir* path)))

(defn- execute-operation
  "Executes filesystem operation in test environment"
  [operation args]
  (with-redefs [core/validate-path (fn [p _] (fs/path *temp-dir* p))]
    (let [tool   (filesystem/create-tool)
          result ((:execute-fn tool) {:operation operation :args args})]
      (update result :content
              (fn [content]
                (mapv #(update % :text
                               (fn [text]
                                 (str/replace text
                                              (str *temp-dir*)
                                              "[TEMP]")))
                      content))))))

(defn- create-test-file!
  "Creates a test file with content in temp dir"
  [path content]
  (let [full-path (fs/path *temp-dir* path)]
    (fs/create-dirs (fs/parent full-path))
    (spit (fs/file full-path) content)
    path))

(defn- file-content
  "Gets content of file in temp dir"
  [path]
  (slurp (fs/file *temp-dir* path)))

(defn- create-test-dir!
  "Creates a test directory in temp dir"
  [path]
  (fs/create-dirs (fs/path *temp-dir* path))
  path)

(defn- path-exists?
  "Checks if path exists in temp dir"
  [path]
  (fs/exists? (fs/path *temp-dir* path)))

(defn- execute-operation
  "Executes filesystem operation in test environment"
  [operation args]
  (with-redefs [core/validate-path (fn [p _] (fs/path *temp-dir* p))]
    (let [tool   (case operation
                   :read-file                (filesystem/create-read-file-tool)
                   :read-multiple-files      (filesystem/create-read-multiple-files-tool)
                   :write-file               (filesystem/create-write-file-tool)
                   :edit-file                (filesystem/create-edit-file-tool)
                   :create-directory         (filesystem/create-create-directory-tool)
                   :list-directory           (filesystem/create-list-directory-tool)
                   :directory-tree           (filesystem/create-directory-tree-tool)
                   :move-file                (filesystem/create-move-file-tool)
                   :search-files             (filesystem/create-search-files-tool)
                   :get-file-info            (filesystem/create-get-file-info-tool)
                   :list-allowed-directories (filesystem/create-list-allowed-directories-tool)
                   (filesystem/create-tool))
          result ((:execute-fn tool) args)]
      (update result :content
              (fn [content]
                (mapv #(update % :text
                               (fn [text]
                                 (str/replace text
                                              (str *temp-dir*)
                                              "[TEMP]")))
                      content))))))

;; Individual tool tests
(deftest read-file-tool-test
  (testing "read-file operation"
    (let [content "test content\nwith multiple lines"
          path    (create-test-file! "test.txt" content)
          result  (execute-operation :read-file {:path path})]
      (is (= content (get-in result [:content 0 :text]))
          "Should return file content"))))

(deftest read-multiple-files-tool-test
  (testing "read-multiple-files operation"
    (let [files  {"file1.txt" "content 1"
                  "file2.txt" "content 2"}
          paths  (mapv #(create-test-file! (key %) (val %)) files)
          result (execute-operation :read-multiple-files {:paths paths})]
      (is (str/includes? (get-in result [:content 0 :text]) "content 1")
          "Should contain first file content")
      (is (str/includes? (get-in result [:content 0 :text]) "content 2")
          "Should contain second file content"))))

(deftest write-file-tool-test
  (testing "write-file operation"
    (let [content "new content"
          path    "new-file.txt"
          result  (execute-operation :write-file {:path path :content content})]
      (is (str/includes? (get-in result [:content 0 :text])
                         "Successfully wrote")
          "Should indicate success")
      (is (= content (file-content path))
          "File should have correct content"))))

(deftest edit-file-tool-test
  (testing "edit-file operation"
    (let [original "original line 1\nline to change\noriginal line 3"
          path     (create-test-file! "edit.txt" original)
          edits    [{:old-text "line to change"
                     :new-text "changed line"}]]
      (testing "dry run"
        (let [result (execute-operation :edit-file
                                        {:path    path
                                         :edits   edits
                                         :dry-run true})]
          (is (str/includes? (get-in result [:content 0 :text]) "diff")
              "Should return diff")
          (is (= original (file-content path))
              "File should be unchanged")))

      (testing "actual edit"
        (let [result (execute-operation :edit-file
                                        {:path  path
                                         :edits edits})]
          (is (str/includes? (get-in result [:content 0 :text]) "diff")
              "Should return diff")
          (is (= (str/replace original
                              "line to change"
                              "changed line")
                 (file-content path))
              "File should be changed"))))))

(deftest create-directory-tool-test
  (testing "create-directory operation"
    (let [path   "new/nested/dir"
          result (execute-operation :create-directory {:path path})]
      (is (str/includes? (get-in result [:content 0 :text])
                         "Successfully created")
          "Should indicate success")
      (is (path-exists? path)
          "Directory should exist"))))

(deftest list-directory-tool-test
  (testing "list-directory operation"
    (let [dir    "list-test"
          _      (create-test-dir! dir)
          _      (create-test-file! (str dir "/file1.txt") "content")
          _      (create-test-dir! (str dir "/subdir"))
          result (execute-operation :list-directory {:path dir})]
      (is (str/includes? (get-in result [:content 0 :text]) "[FILE] file1.txt")
          "Should list file")
      (is (str/includes? (get-in result [:content 0 :text]) "[DIR] subdir")
          "Should list directory"))))

(deftest directory-tree-tool-test
  (testing "directory-tree operation"
    (let [dir    "tree-test"
          _      (create-test-dir! dir)
          _      (create-test-file! (str dir "/file1.txt") "content")
          _      (create-test-dir! (str dir "/subdir"))
          _      (create-test-file! (str dir "/subdir/file2.txt") "content")
          result (execute-operation :directory-tree {:path dir})
          tree   (j/read-value (get-in result [:content 0 :text])
                               j/keyword-keys-object-mapper)]
      (is (vector? tree) "Should return vector")
      (is (= #{:name :type :children}
             (set (keys (first (filter #(= (:type %) "directory") tree)))))
          "Directory entry should have correct keys")
      (is (= #{:name :type}
             (set (keys (first (filter #(= (:type %) "file") tree)))))
          "File entry should have correct keys"))))

(deftest move-file-tool-test
  (testing "move-file operation"
    (let [content     "move test content"
          source      (create-test-file! "source.txt" content)
          destination "moved.txt"
          result      (execute-operation :move-file
                                         {:source      source
                                          :destination destination})]
      (is (str/includes? (get-in result [:content 0 :text])
                         "Successfully moved")
          "Should indicate success")
      (is (not (path-exists? source))
          "Source should not exist")
      (is (= content (file-content destination))
          "Destination should have content"))))

(deftest search-files-tool-test
  (testing "search-files operation"
    (let [_      (create-test-file! "test1.txt" "content")
          _      (create-test-file! "dir/test2.txt" "content")
          _      (create-test-file! "exclude/test3.txt" "content")
          result (execute-operation
                  :search-files
                  {:path             "."
                   :pattern          "**test*"
                   :exclude-patterns ["exclude/**"]})]
      (is (str/includes? (get-in result [:content 0 :text]) "test1.txt")
          "Should find file in root")
      (is (str/includes? (get-in result [:content 0 :text]) "test2.txt")
          "Should find file in subdirectory")
      (is (not (str/includes? (get-in result [:content 0 :text]) "test3.txt"))
          "Should not find excluded file"))))

(deftest get-file-info-tool-test
  (testing "get-file-info operation"
    (let [content   "test content"
          path      (create-test-file! "info.txt" content)
          result    (execute-operation :get-file-info {:path path})
          info-text (get-in result [:content 0 :text])]
      (is (str/includes? info-text "size:")
          "Should include size")
      (is (str/includes? info-text "created:")
          "Should include creation time")
      (is (str/includes? info-text "is-file: true")
          "Should indicate file type"))))

(deftest list-allowed-directories-tool-test
  (testing "list-allowed-directories operation"
    (let [result (execute-operation :list-allowed-directories {})]
      (is (str/includes? (get-in result [:content 0 :text])
                         "Allowed directories")
          "Should list allowed directories"))))


;; Tests
(deftest filesystem-tool-test
  (testing "read-file operation"
    (let [content "test content\nwith multiple lines"
          path    (create-test-file! "test.txt" content)
          result  (execute-operation :read-file {:path path})]
      (is (= content (get-in result [:content 0 :text]))
          "Should return file content")))

  (testing "read-multiple-files operation"
    (let [files  {"file1.txt" "content 1"
                  "file2.txt" "content 2"}
          paths  (mapv #(create-test-file! (key %) (val %)) files)
          result (execute-operation :read-multiple-files {:paths paths})]
      (is (str/includes? (get-in result [:content 0 :text]) "content 1")
          "Should contain first file content")
      (is (str/includes? (get-in result [:content 0 :text]) "content 2")
          "Should contain second file content")))

  (testing "write-file operation"
    (let [content "new content"
          path    "new-file.txt"
          result  (execute-operation :write-file {:path path :content content})]
      (is (str/includes? (get-in result [:content 0 :text])
                         "Successfully wrote")
          "Should indicate success")
      (is (= content (file-content path))
          "File should have correct content")))

  (testing "edit-file operation"
    (let [original "original line 1\nline to change\noriginal line 3"
          path     (create-test-file! "edit.txt" original)
          edits    [{:old-text "line to change"
                     :new-text "changed line"}]
          result   (execute-operation :edit-file
                                      {:path    path
                                       :edits   edits
                                       :dry-run true})]
      (testing "dry run"
        (is (str/includes? (get-in result [:content 0 :text]) "diff")
            "Should return diff")
        (is (= original (file-content path))
            "File should be unchanged"))

      (testing "actual edit"
        (let [result (execute-operation :edit-file
                                        {:path  path
                                         :edits edits})]
          (is (str/includes? (get-in result [:content 0 :text]) "diff")
              "Should return diff")
          (is (= (str/replace original
                              "line to change"
                              "changed line")
                 (file-content path))
              "File should be changed")))))

  (testing "create-directory operation"
    (let [path   "new/nested/dir"
          result (execute-operation :create-directory {:path path})]
      (is (str/includes? (get-in result [:content 0 :text])
                         "Successfully created")
          "Should indicate success")
      (is (path-exists? path)
          "Directory should exist")))

  (testing "list-directory operation"
    (let [dir    "list-test"
          _      (create-test-dir! dir)
          _      (create-test-file! (str dir "/file1.txt") "content")
          _      (create-test-dir! (str dir "/subdir"))
          result (execute-operation :list-directory {:path dir})]
      (is (str/includes? (get-in result [:content 0 :text]) "[FILE] file1.txt")
          "Should list file")
      (is (str/includes? (get-in result [:content 0 :text]) "[DIR] subdir")
          "Should list directory")))

  (testing "directory-tree operation"
    (let [dir    "tree-test"
          _      (create-test-dir! dir)
          _      (create-test-file! (str dir "/file1.txt") "content")
          _      (create-test-dir! (str dir "/subdir"))
          _      (create-test-file! (str dir "/subdir/file2.txt") "content")
          result (execute-operation :directory-tree {:path dir})
          tree   (j/read-value (get-in result [:content 0 :text])
                               j/keyword-keys-object-mapper)]
      (is (vector? tree) "Should return vector")
      (is (= #{:name :type :children}
             (set (keys (first (filter #(= (:type %) "directory") tree)))))
          "Directory entry should have correct keys")
      (is (= #{:name :type}
             (set (keys (first (filter #(= (:type %) "file") tree)))))
          "File entry should have correct keys")))

  (testing "move-file operation"
    (let [content     "move test content"
          source      (create-test-file! "source.txt" content)
          destination "moved.txt"
          result      (execute-operation :move-file
                                         {:source      source
                                          :destination destination})]
      (is (str/includes? (get-in result [:content 0 :text])
                         "Successfully moved")
          "Should indicate success")
      (is (not (path-exists? source))
          "Source should not exist")
      (is (= content (file-content destination))
          "Destination should have content")))

  (testing "search-files operation"
    (let [_      (create-test-file! "test1.txt" "content")
          _      (create-test-file! "dir/test2.txt" "content")
          _      (create-test-file! "exclude/test3.txt" "content")
          result (execute-operation
                  :search-files
                  {:path             "."
                   :pattern          "**test*"
                   :exclude-patterns ["exclude/**"]})]
      (is (str/includes? (get-in result [:content 0 :text]) "test1.txt")
          "Should find file in root")
      (is (str/includes? (get-in result [:content 0 :text]) "test2.txt")
          "Should find file in subdirectory")
      (is (not (str/includes? (get-in result [:content 0 :text]) "test3.txt"))
          "Should not find excluded file")))

  (testing "get-file-info operation"
    (let [content   "test content"
          path      (create-test-file! "info.txt" content)
          result    (execute-operation :get-file-info {:path path})
          info-text (get-in result [:content 0 :text])]
      (is (str/includes? info-text "size:")
          "Should include size")
      (is (str/includes? info-text "created:")
          "Should include creation time")
      (is (str/includes? info-text "is-file: true")
          "Should indicate file type"))))
