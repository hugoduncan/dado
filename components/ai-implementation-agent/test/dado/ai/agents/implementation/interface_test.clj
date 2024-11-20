(ns dado.ai.agents.implementation.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.ai.agents.implementation.interface :as implementation]))

(deftest create-implementation-agent-test
  (testing "creates valid implementation agent"
    (let [agent (implementation/create-agent {} (constantly []))]
      (is (= :implementation (:name agent)))
      (is (fn? (:prompt-fn agent)))
      (is (fn? (:context-fn agent)))
      (is (fn? (:process-response-fn agent))))))
