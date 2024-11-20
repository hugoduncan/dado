(ns dado.ai.agents.refactoring.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.ai.agent.interface :as agent]
            [dado.ai.agents.refactoring.interface :as sut]
            [dado.ai.agents.refactoring.model :as model]
            [dado.ai.prompt.interface :as prompt]))

(deftest create-agent-test
  (testing "creates valid refactoring agent"
    (with-redefs [prompt/construct-prompt (constantly "test prompt")]
      (let [config {}
            agent (sut/create-agent config)]
        (is (model/Agent? agent))
        (is (= :refactoring (:name agent)))
        (is (fn? (:prompt-fn agent)))
        (is (fn? (:context-fn agent)))
        (is (fn? (:process-response-fn agent)))))))
