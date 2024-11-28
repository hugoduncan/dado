(ns dado.ai.agents.test.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.ai.agents.test.interface :as test]))

(deftest create-agent-test
  (testing "creates valid test agent"
    (let [agent (test/create-agent {} (constantly []))]
      (is (= :test (:name agent)))
      (is (fn? (:prompt-fn agent)))
      (is (fn? (:context-fn agent)))
      (is (fn? (:process-response-fn agent))))))
