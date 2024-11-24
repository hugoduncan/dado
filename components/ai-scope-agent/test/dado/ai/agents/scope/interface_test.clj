(ns dado.ai.agents.scope.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.ai.agents.scope.interface :as scope]))

(deftest create-scope-agent-test
  (testing "creates valid scope agent"
    (let [agent (scope/create-agent {} (constantly []))]
      (is (= :scope (:name agent)))
      (is (fn? (:prompt-fn agent)))
      (is (fn? (:context-fn agent)))
      (is (fn? (:process-response-fn agent))))))
