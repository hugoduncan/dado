(ns dado.ai.tools.matching-file.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.ai.tools.matching-file.interface :as mf]))

(deftest tool-def-test
  (testing "tool definition structure"
    (let [tool (mf/tool-def)]
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

          (is (= #{:pattern :mode :case-sensitive? :context-lines
                   :max-matches :extensions}
                 (->> params
                      rest
                      (partition 2)
                      (map first)
                      set))
              "has all expected parameters"))))))

(deftest recognize-fn-test
  (testing "recognizes valid search requests"
    (let [recognize-fn (:recognize-fn (mf/tool-def))]
      (is (recognize-fn "find files containing pattern"))
      (is (recognize-fn "search for files with text"))
      (is (recognize-fn "Find file that matches"))
      (is (not (recognize-fn "create a new file")))
      (is (not (recognize-fn "delete the file"))))))
