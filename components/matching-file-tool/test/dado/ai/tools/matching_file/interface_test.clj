(ns dado.ai.tools.matching-file.interface-test
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer [deftest is testing]]
   [dado.ai.tool.interface :as tool]
   [dado.ai.tools.matching-file.core :as core]
   [dado.ai.tools.matching-file.interface :as mf]
   [malli.core :as m]
   [malli.error :as me]))

(deftest tool-def-test
  (testing "tool definition structure"
    (let [tool (mf/create-tool)]
      (is (= :dado/matching-file (:id tool))
          "has correct tool id")

      (is (fn? (:execute-fn tool))
          "has execute function")

      (is (fn? (:recognize-fn tool))
          "has recognize function")

      (is (fn? (:prompt-fn tool))
          "has prompt function")

      (testing "parameters schema"
        (let [params (:parameters tool)]
          (is (some? params)
              "has parameters schema")

          (is (= #{[:max-matches {:optional true} :int]
                   [:pattern :string]
                   [:case-sensitive {:optional true} :boolean]}
                 (->> params
                      rest
                      (partition 2)
                      (map first)
                      set))
              "has all expected parameters"))))))

(deftest recognize-fn-test
  (testing "recognizes valid search requests"
    (let [recognize-fn (:recognize-fn (mf/create-tool))]
      (is (recognize-fn "find files containing pattern"))
      (is (recognize-fn "search for files with text"))
      (is (recognize-fn "Find file that matches"))
      (is (not (recognize-fn "create a new file")))
      (is (not (recognize-fn "delete the file"))))))

(deftest matching-file-matches-correct-files-test
  (fs/with-temp-dir [dir {:dir "target"}]
    (let [execute-fn (:execute-fn (mf/create-tool))
          file-path  (fs/path dir "some-file.clj")]
      (with-redefs [core/find-files (constantly [file-path])]

        (testing "with a matching file"
          (spit (fs/file file-path) "some content to match")
          (let [result (execute-fn {:pattern "content"})]
            (is (= {:content
                    [{:type :text, :text (str file-path)}],
                    :is-error false,
                    :context-mod
                    {:files     [(str file-path)],
                     :operation :set!}}
                   result))
            (is (tool/execution-result? result))
            (is (nil?
                 (me/humanize
                  (m/explain (tool/execution-result-schema) result))))))))))
