(ns dado.ai.message.interface-test
  (:require [clojure.test :refer :all]
            [dado.ai.message.interface :as message]
            [malli.generator :as mg]))

(deftest create-message-test
  (testing "creates valid message"
    (let [msg (message/create-message :user "test content")]
      (is (= :user (:role msg)))
      (is (= "test content" (:content msg)))))

  (testing "creates message with name"
    (let [msg (message/create-message :system "test content" :name "config")]
      (is (= "config" (:name msg)))))

  (testing "validates message format"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Invalid message format"
                          (message/create-message :invalid "content")))))

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
    (let [response {:content "```python\n# test.py\nprint('hello')\n```"}
          blocks   (message/extract-file-blocks response)]
      (is (= 1 (count blocks)))
      (is (= "python" (:language (first blocks))))
      (is (= "test.py" (get-in (first blocks) [:metadata :name]))))
    (let [response {:content "```clojure\n;; test.clj\n(print 'hello')\n```"}
          blocks   (message/extract-file-blocks response)]
      (is (= 1 (count blocks)))
      (is (= "clojure" (:language (first blocks))))
      (is (= "test.clj" (get-in (first blocks) [:metadata :name]))))))

(deftest extract-updated-namespaces-test
  (testing "extracts updated namespaces"
    (let [response   {:role          :assistant
                      :content       "```updated-namespaces\nmy.project.model\nmy.project.core\n```"
                      :finish-reason :stop}
          namespaces (message/extract-updated-namespaces response)]
      (is (= ["my.project.model" "my.project.core"] namespaces))))

  (testing "returns empty sequence when no namespaces found"
    (let [response   {:role          :assistant
                      :content       "no namespaces here"
                      :finish-reason :stop}
          namespaces (message/extract-updated-namespaces response)]
      (is (empty? namespaces))))

  (testing "validates response format"
    (is (thrown? clojure.lang.ExceptionInfo
                 (message/extract-updated-namespaces {:invalid "format"})))))

(deftest add-context-file-test
  (testing "adds file to context"
    (let [thread    {:id         "test"
                     :created-at (java.util.Date.)
                     :messages   []
                     :metadata   {:model "test-model"}}
          temp-file (java.io.File/createTempFile "test" ".txt")]
      (spit temp-file "test content")
      (let [result (message/add-context-file thread (.getPath temp-file))]
        (is (= "test content"
               (get-in result [:metadata :context :files 0 :content])))))))
