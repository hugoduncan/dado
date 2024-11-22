(ns dado.ai.ollama.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.ai.ollama.interface :as ollama]))

(deftest send!-test
  (testing "validates provider config"
    (let [invalid-config {:model-name 123} ; model-name should be string
          message-thread {:id         "test-thread"
                         :created-at (java.time.Instant/now)
                         :messages   [{:role    :user
                                     :content "test"}]
                         :metadata   {:model "llama2:3.2"}}]
      (is (thrown? clojure.lang.ExceptionInfo
                   (ollama/send! invalid-config message-thread)))
      (let [ex (try
                 (ollama/send! invalid-config message-thread)
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (= :error/ollama-validation (:type (ex-data ex))))
        (is (= "dado.ai.ollama" (get-in (ex-data ex) [:context :component])))))))
