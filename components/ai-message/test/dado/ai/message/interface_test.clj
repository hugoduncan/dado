(ns dado.ai.message.interface-test
  (:require [clojure.test :refer :all]
            [dado.ai.message.interface :as message]
            [malli.generator :as mg]))

(deftest text-content-test
  (testing "creates text content map"
    (let [content (message/text-content "test text")]
      (is (= :text (:type content)))
      (is (= "test text" (:text content))))))

(deftest tool-result-content-test
  (testing "creates tool result content map"
    (let [content (message/tool-result-content
                  {:tool-use-id "test-id"
                   :content "result"})]
      (is (= :tool-result (:type content)))
      (is (= "test-id" (:tool-use-id content)))
      (is (= "result" (:content content)))
      (is (nil? (:is-error content)))))

  (testing "includes is-error when specified"
    (let [content (message/tool-result-content
                  {:tool-use-id "test-id"
                   :content "error"
                   :is-error true})]
      (is (:is-error content)))))

(deftest create-message-test
  (testing "creates valid message"
    (let [msg (message/create-message :user)]
      (is (= :user (:role msg)))
      (is (= [] (:content msg)))))

  (testing "creates message with name"
    (let [msg (message/create-message :system :name "config")]
      (is (= "config" (:name msg)))
      (is (= [] (:content msg)))))

  (testing "validates message role"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"model/role?"
         (message/create-message :invalid)))))

(deftest add-message-content-test
  (testing "adds text content"
    (let [msg (message/create-message :user)
          content (message/text-content "test text")
          result (message/add-message-content msg content)]
      (is (= [content] (:content result)))))

  (testing "adds tool result content"
    (let [msg (message/create-message :assistant)
          content (message/tool-result-content
                  {:tool-use-id "test"
                   :content "result"})
          result (message/add-message-content msg content)]
      (is (= [content] (:content result)))))

  (testing "preserves existing content"
    (let [msg (message/create-message :user)
          content1 (message/text-content "first")
          content2 (message/text-content "second")
          result (-> msg
                    (message/add-message-content content1)
                    (message/add-message-content content2))]
      (is (= [content1 content2] (:content result))))))


(deftest add-message-test
  (testing "adds message to thread"
    (let [thread {:id         "test"
                  :created-at (java.util.Date.)
                  :messages   []
                  :metadata   {:model "test-model"}}
          msg    (message/create-message :user "test")
          result (message/add-message thread msg)]
      (is (= [msg] (:messages result)))))

  (testing "validates thread format"
    (is (thrown? clojure.lang.ExceptionInfo
                 (message/add-message {} (message/create-message :user "test"))))))

(deftest extract-file-blocks-test
  (testing "extracts code blocks"
    (let [response {:role          :assistant,
                    :finish-reason :end-turn
                    :content
                    [{:text "```python\n# test.py\nprint('hello')\n```"}]}
          blocks   (message/extract-file-blocks response)]
      (is (= 1 (count blocks)))
      (is (= "python" (:language (first blocks))))
      (is (= "test.py" (get-in (first blocks) [:metadata :name]))))
    (let [response
          {:role          :assistant
           :finish-reason :end-turn
           :content       [{:text
                            "```clojure\n;; test.clj\n(print 'hello')\n```"}]}
          blocks (message/extract-file-blocks response)]
      (is (= 1 (count blocks)))
      (is (= "clojure" (:language (first blocks))))
      (is (= "test.clj" (get-in (first blocks) [:metadata :name]))))))

(deftest extract-updated-namespaces-test
  (testing "extracts updated namespaces"
    (let [response
          {:role          :assistant
           :content       [{:text
                            "```updated-namespaces\nmy.project.model\nmy.project.core\n```"}]
           :finish-reason :stop}
          namespaces (message/extract-updated-namespaces response)]
      (is (= ["my.project.model" "my.project.core"] namespaces))))

  (testing "returns empty sequence when no namespaces found"
    (let [response   {:role          :assistant
                      :content       [{:text "no namespaces here"}]
                      :finish-reason :stop}
          namespaces (message/extract-updated-namespaces response)]
      (is (empty? namespaces))))

  (testing "validates response format"
    (is (thrown? clojure.lang.ExceptionInfo
                 (message/extract-updated-namespaces {:invalid "format"})))))

(deftest add-context-file-test
  (testing "adds file to context"
    (let [thread    {:id         "test"
                     :created-at (java.time.Instant/now)
                     :messages   []
                     :metadata   {:model   "test-model"
                                  :context {:files [[]]}}}
          temp-file (java.io.File/createTempFile "test" ".txt")]
      (spit temp-file "test content")
      (let [result (message/add-context-file thread (.getPath temp-file))]
        (is (= "test content"
               (get-in result [:metadata :context :files 0 0 :content]))))))

  (testing "validates nested context files structure"
    (let [thread {:id         "test"
                  :created-at (java.time.Instant/now)
                  :messages   []
                  :metadata   {:model   "test-model"
                               :context {:files [[{:name    "test1.txt"
                                                   :content "content1"}]
                                                 [{:name    "test2.txt"
                                                   :content "content2"}]]}}}]
      (is (message/message-thread? thread))
      ;; Should fail with non-nested files structure
      (is (not (message/message-thread?
                (assoc-in thread [:metadata :context :files]
                          [{:name "test.txt" :content "content"}]))))))

  (testing "add-context-file with no existing sequence"
    (let [thread    {:id         "test"
                     :created-at (java.time.Instant/now)
                     :messages   []
                     :metadata   {:model "test-model"}}
          temp-file (java.io.File/createTempFile "test" ".txt")]
      (try
        (spit temp-file "test content")
        (let [result (message/add-context-file thread (.getPath temp-file))]
          (is (= [[{:name    (str temp-file)
                    :content "test content"}]]
                 (get-in result [:metadata :context :files])))
          (is (message/message-thread? result)))
        (finally
          (.delete temp-file)))))

  (testing "add-context-file adds to existing sequence"
    (let [thread    {:id         "test"
                     :created-at (java.time.Instant/now)
                     :messages   []
                     :metadata   {:model   "test-model"
                                  :context {:files [[{:name    "existing.txt"
                                                      :content "existing"}]]}}}
          temp-file (java.io.File/createTempFile "test" ".txt")]
      (try
        (spit temp-file "test content")
        (let [result (message/add-context-file thread (.getPath temp-file))]
          (is (= [[{:name    "existing.txt"
                    :content "existing"}
                   {:name    (str temp-file)
                    :content "test content"}]]
                 (get-in result [:metadata :context :files])))
          (is (message/message-thread? result)))
        (finally
          (.delete temp-file)))))

  (testing "add-context-file-sequence starts new sequence"
    (let [msg-thread {:id         "test"
                      :created-at (java.time.Instant/now)
                      :messages   []
                      :metadata   {:model   "test-model"
                                   :context {:files [[{:name    "existing.txt"
                                                       :content "existing"}]]}}}
          temp-file  (java.io.File/createTempFile "test" ".txt")]
      (try
        (spit temp-file "test content")
        (let [result (message/add-context-file-sequence
                      msg-thread
                      [(.getPath temp-file)])]
          (is (= [[{:name    "existing.txt"
                    :content "existing"}]
                  [{:name    (str temp-file)
                    :content "test content"}]]
                 (get-in result [:metadata :context :files])))
          (is (message/message-thread? result)))
        (finally
          (.delete temp-file)))))

  (testing "set-context-files with multiple sequences"
    (let [files    (repeatedly 3 #(java.io.File/createTempFile "test" ".txt"))
          contents ["content1" "content2" "content3"]]
      (try
        ;; Create test files
        (doseq [[file content] (map vector files contents)]
          (spit file content))

        (let [thread    {:id         "test"
                         :created-at (java.time.Instant/now)
                         :messages   []
                         :metadata   {:model "test-model"}}
              ;; Create two sequences: [file1] [file2 file3]
              sequences [[(first files)]
                         [(second files) (nth files 2)]]
              result    (message/set-context-files thread sequences)]

          ;; Verify structure
          (let [result-files (get-in result [:metadata :context :files])]
            (is (= 2 (count result-files)) "Should have two sequences")
            (is (= 1 (count (first result-files))) "First sequence should have one file")
            (is (= 2 (count (second result-files))) "Second sequence should have two files")

            ;; Verify contents
            (is (= "content1" (get-in result-files [0 0 :content])))
            (is (= "content2" (get-in result-files [1 0 :content])))
            (is (= "content3" (get-in result-files [1 1 :content])))

            ;; Verify file names
            (is (= (str (first files)) (get-in result-files [0 0 :name])))
            (is (= (str (second files)) (get-in result-files [1 0 :name])))
            (is (= (str (nth files 2)) (get-in result-files [1 1 :name]))))

          (is (message/message-thread? result)))

        (finally
          (doseq [file files]
            (.delete file))))))

  (testing "error handling"
    (testing "non-existent file"
      (let [thread {:id         "test"
                    :created-at (java.time.Instant/now)
                    :messages   []
                    :metadata   {:model "test-model"}}]
        (is (thrown? java.io.FileNotFoundException
                     (message/add-context-file thread "non-existent.txt")))
        (is (thrown? java.io.FileNotFoundException
                     (message/add-context-file-sequence thread ["non-existent.txt"])))
        (is (thrown? java.io.FileNotFoundException
                     (message/set-context-files thread [["non-existent.txt"]])))))

    (testing "invalid thread"
      (let [invalid-thread {:invalid "structure"}]
        (let [temp-file (java.io.File/createTempFile "test" ".txt")]
          (try
            (is (thrown? Exception
                         (message/add-context-file invalid-thread (.getPath temp-file))))
            (is (thrown? Exception
                         (message/add-context-file-sequence invalid-thread (.getPath temp-file))))
            (is (thrown? Exception
                         (message/set-context-files invalid-thread [[(.getPath temp-file)]])))
            (finally
              (.delete temp-file))))))))

(deftest message-thread-schema-generator-test
  (is (message/message-thread?
       (mg/generate message/message-thread-schema))))
