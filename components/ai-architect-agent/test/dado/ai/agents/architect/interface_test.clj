(ns dado.ai.agents.architect.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.ai.agents.architect.interface :as architect]))

(deftest create-architect-agent-test
  (testing "creates valid architect agent"
    (let [agent (architect/create-agent {} (constantly []))]
      (is (= :architect (:name agent)))
      (is (fn? (:prompt-fn agent)))
      (is (fn? (:context-fn agent)))
      (is (fn? (:process-response-fn agent))))))
