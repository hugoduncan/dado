(ns dado.repl.message-loop.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [dado.repl.message-loop.core :as core]
   [dado.ai.message.interface :as message]
   [babashka.fs :as fs]))

(deftest message-loop-add-tool-results-test
  (fs/with-temp-dir [dir {:prefix "target"}]
    (let [msg-thread  (message/create-message-thread "")
          file-path   (fs/path dir "some-file.clj")
          _           (spit (fs/file file-path) "some content")
          exec-result {:content
                       [{:type :text, :text "some text"}],
                       :is-error false,
                       :context-mod
                       {:files     [file-path],
                        :operation :set!}}
          tool-result {:tool-result-content
                       (#'core/exec-result->message {:id "123"} exec-result)
                       :context-mod (:context-mod exec-result) }
          msg-thread  (#'core/add-tool-results msg-thread [tool-result])]
      (is (= [{:role :user,
               :content
               [{:type        :tool-result,
                 :tool-use-id "123",
                 :content     [{:type :text, :text "some text"}],
                 :is-error    false}]}]
             (:messages msg-thread)))
      (is (= {:files #{{:name (str file-path) :content "some content"}}}
             (:ai-managed-context (:metadata msg-thread)))))))
