(ns dado.tools.file-operation.interface-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [dado.ai.tool.interface :as tool]
   [dado.tools.file-operation.interface :as file-operation]
   [dado.tools.file-operation.core :as core]
   [malli.generator :as mg]
   [taoensso.truss :refer [have?]]))

(def ^:dynamic *temp-dir* nil)

;; Test fixtures
(defn with-temp-dir
  "Test fixture that creates a temporary directory"
  [f]
  (fs/with-temp-dir [temp-dir {:prefix "fot-"}]
    (binding [*temp-dir* temp-dir]
      (f))))

(use-fixtures :each with-temp-dir)

(defn- is-temp-path? [path]
  (fs/starts-with? path *temp-dir*))

(defn- abs-path [path]
  (str (fs/path *temp-dir* path)))

(defn- file-exists? [path]
  (fs/exists? (abs-path path)))

(defn- content! [path content]
  (spit (fs/file (abs-path path)) content))

(defn- has-content? [path content]
  (if (= content (slurp (fs/file (abs-path path))))
    true
    (throw (ex-info
            "content mismatch"
            {:expected content
             :actual   (slurp (fs/file (abs-path path)))}))))

(defn- temp-path
  "Return a relative path for a temp file.
  The file will not exist."
  []
  {:post [(have? (complement (comp fs/exists? abs-path)) %)]}
  (let [p (fs/create-temp-file {:dir *temp-dir*})]
    (fs/delete p)
    (fs/file-name p)))

(defn- temp-file!
  "Return a relative path to a temp file with the given content."
  [content]
  {:post [(have? (comp fs/exists? abs-path) %)]}
  (let [p (fs/create-temp-file {:dir *temp-dir*})]
    (spit (fs/file p) content)
    (fs/file-name p)))

(defmacro is-result?
  [result-sym results]
  `(let [results# ~results
         result#  ~result-sym]
     (is (= (count results#) (count result# ))
         (pr-str {:expected results#
                  :actual   result#}))
     (doseq [[i# [op# & paths#]] (mapv vector (range) results#)]
       (is (= (str op# " on " (mapv abs-path paths#) " succeeded" )
              (get-in result# [:content i# :text]))
           "Succeass message"))))

(defmacro is-failed-result?
  [result-expr results]
  `(let [results# ~results
         result#  ~result-expr]
     (is (= (count results#) (count result#))
         (pr-str (:content result#)))
     (doseq [[i# [op# paths# msg#]] (mapv vector (range) results#)]
       (is (re-matches
            (re-pattern
             (str
              (java.util.regex.Pattern/quote
               (str op# " on " (str paths#) " failed"))
              ".*"
              msg#
              ".*"))
            (get-in result# [:content i# :text]))))))

;; Test creating and validating operations
(deftest file-operation-tool-test
  (testing "creates valid tool configuration"
    (let [tool (file-operation/create-tool)]
      (is (tool/validate-tool tool))
      (is (= :dado/file-operation (:id tool)))))

  (testing "executes"
    (let [tool-config (file-operation/create-tool)
          execute-fn  (:execute-fn tool-config)]
      (with-redefs [core/validate-path is-temp-path?]

        (testing "create operation"
          (let [content   "test content"
                file-name (temp-path)
                operation {:operation :create
                           :path      (abs-path file-name)
                           :content   content}
                result    (execute-fn {:operations [operation]})]
            (is-result? result [["create" file-name]])
            (is (file-exists? file-name) "File should be created")
            (is (has-content? file-name content) "File has content")))

        (testing "edit operation"
          ;; First create a file to edit
          (let [orig-content "original content"
                new-content  "modified content"
                file-name    (temp-file! orig-content)
                operation    {:operation     :edit
                              :path          (abs-path file-name)
                              :search-blocks [{:search  orig-content
                                               :replace new-content}]}
                result       (execute-fn {:operations [operation]})]
            (is-result? result [["edit" file-name]])
            (is (has-content? file-name new-content)
                "File content should be modified")))

        (testing "copy operation"
          (let [content     "copy test content"
                source-file (temp-file! content)
                target-file (temp-path)
                operation   {:operation   :copy
                             :path        (abs-path source-file)
                             :target-path (abs-path target-file)}
                result      (execute-fn {:operations [operation]})]
            (is-result? result [["copy" source-file target-file]])
            (is (has-content? source-file content)
                "Source file should still have source content")
            (is (has-content? target-file content)
                "Target file should have source content")))

        (testing "move operation"
          (let [content     "move test content"
                source-file (temp-file! content)
                target-file (temp-path)

                operation {:operation   :move
                           :path        (abs-path source-file)
                           :target-path (abs-path  target-file)}
                result    (execute-fn {:operations [operation]})]
            (is-result? result [["move" source-file target-file]])
            (is (not (file-exists? source-file))
                "Source file should be gone")
            (is (has-content? target-file content)
                "Target file should have source content")))

        (testing "delete operation"
          (let [file-name (temp-file! :create)
                operation {:operation :delete
                           :path      (abs-path file-name)}
                result    (execute-fn {:operations [operation]})]
            (is-result? result [["delete" file-name]])
            (is (not (file-exists? file-name)) "File should be deleted")))

        (testing "multiple operations"
          (let [first-file     (temp-path)
                second-file    (temp-path)
                first-content  "content 1"
                second-content "content 2"
                operations     [{:operation :create
                                 :path      (abs-path first-file)
                                 :content   first-content}
                                {:operation :create
                                 :path      (abs-path second-file)
                                 :content   second-content}]
                result         (execute-fn {:operations operations})]
            (is (= 2 (count (:content result))) "Should return two results")
            (is (every?
                 #(str/ends-with? % "succeeded")
                 (map :text (:content result))))
            (is (has-content? first-file first-content) )
            (is (has-content? second-file second-content) )))

        (testing "validates operations"
          (testing "create without content"
            (is-failed-result?
             (execute-fn
              {:operations
               [{:operation :create
                 :path      "test.txt"}]})
             [["create" ["test.txt"] "requires content"]]))

          (testing "edit without search blocks"
            (is-failed-result?
             (execute-fn
              {:operations
               [{:operation :edit
                 :path      "test.txt"}]})
             [["edit" ["test.txt"] "search block"]]))

          (testing "move without target path"
            (is-failed-result?
             (execute-fn
              {:operations
               [{:operation :move
                 :path      "test.txt"}]})
             [["move" ["test.txt"] "requires target path"]]))

          (testing "copy without target path"
            (is-failed-result?
             (execute-fn
              {:operations
               [{:operation :copy
                 :path      "test.txt"}]})
             [["copy" ["test.txt"] "requires target path"]])))))))


(deftest extract-operations-test
  (testing "extracts operations from text"
    (let [input
          "Some text with file operations:
<file-operation type=\"create\" path=\"test.txt\">
This is some content for the new file
</file-operation>
<file-operation type=\"delete\" path=\"old.txt\">
</file-operation>
<file-operation type=\"edit\" path=\"edit.txt\">
<search>This is the original content</search>
<replace>This is the new content</replace>
</file-operation>
<file-operation type=\"move\" path=\"move.txt\" target-path=\"moved.txt\">
</file-operation>
<file-operation type=\"copy\" path=\"copy.txt\" target-path=\"copied.txt\">
</file-operation>"
          expected-ops [{:operation :create,
                         :path      "test.txt",
                         :content   "This is some content for the new file\n"}
                        {:operation :delete, :path "old.txt"}
                        {:operation :edit,
                         :path      "edit.txt",
                         :search-blocks
                         [{:search  "This is the original content",
                           :replace "This is the new content"}]}
                        {:operation   :move,
                         :path        "move.txt",
                         :target-path "moved.txt"}
                        {:operation   :copy,
                         :path        "copy.txt",
                         :target-path "copied.txt"}]]
      (is (= expected-ops (core/extract-operations input)))))

  (testing "handles no operations in text"
    (let [input        "No file operations here."
          expected-ops []]
      (is (= expected-ops (core/extract-operations input)))))

  (testing "handles invalid operations"
    (let [input        "Invalid operation block:
                 <file-operation type=\"invalid\" path=\"invalid.txt\">
                 </file-operation>"
          expected-ops []]
      (is (= expected-ops (core/extract-operations input)))))

  (testing "handles mixed valid and invalid operations"
    (let [input        "Mixed operations:
<file-operation type=\"create\" path=\"test.txt\">
This is some content for the new file
</file-operation>
<file-operation type=\"invalid\" path=\"invalid.txt\">
</file-operation>"
          expected-ops [{:operation :create, :path "test.txt",
                         :content   "This is some content for the new file\n"}]]
      (is (= expected-ops (core/extract-operations input))))))
