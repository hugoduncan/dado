(ns dado.ai.tools.git.interface-test
  "Tests for git tools interface."
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.ai.tools.git.interface :as git]
   [dado.ai.tools.git.model :as model]
   [malli.core :as m]))

(deftest create-tool-test
  (testing "creates stage tool"
    (let [tool (git/create-tool :dado/git-stage)]
      (is (= :dado/git-stage (:id tool)))
      (is (= model/StageParameters (m/form (:parameters tool))))
      (is (fn? (:execute-fn tool)))))

  (testing "creates commit tool"
    (let [tool (git/create-tool :dado/git-commit)]
      (is (= :dado/git-commit (:id tool)))
      (is (= model/CommitParameters (m/form (:parameters tool))))
      (is (fn? (:execute-fn tool)))))

  (testing "creates status tool"
    (let [tool (git/create-tool :dado/git-status)]
      (is (= :dado/git-status (:id tool)))
      (is (= model/StatusParameters (m/form (:parameters tool))))
      (is (fn? (:execute-fn tool)))))

  (testing "returns nil for unknown tool"
    (is (nil? (git/create-tool :unknown-tool)))))
