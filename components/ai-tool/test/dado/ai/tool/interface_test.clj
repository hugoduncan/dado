(ns dado.ai.tool.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.ai.tool.interface :as sut]
            [dado.ai.tool.model :as model]
            [malli.core :as m]))

(def valid-tool
  {:id :test/example
   :name "Example Tool"
   :description "An example tool for testing"
   :structured-description {:claude {:description "Claude specific description"}}
   :parameters [{:name "input"
                :type :string
                :description "Input value"
                :required? true}]
   :returns {:type :string
             :description "Processed input"}
   :prompt-fn (fn [_] "Tool prompt")
   :recognize-fn (fn [_] true)
   :execute-fn (fn [params] (:input params))})

(def invalid-tool
  (dissoc valid-tool :name :description))

(deftest register-tool!-test
  (testing "registers valid tool"
    (let [result (sut/register-tool! valid-tool)]
      (is (= valid-tool result))))
  
  (testing "throws on invalid tool"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid tool configuration"
         (sut/register-tool! invalid-tool)))))

(deftest lookup-tool-test
  (testing "finds registered tool"
    (sut/register-tool! valid-tool)
    (is (= valid-tool (sut/lookup-tool :test/example))))
  
  (testing "returns nil for unknown tool"
    (is (nil? (sut/lookup-tool :unknown/tool)))))

(deftest execute-tool!-test
  (testing "executes registered tool"
    (sut/register-tool! valid-tool)
    (let [params {:input "test"}
          result (sut/execute-tool! :test/example params)]
      (is (= "test" (:result result)))))
  
  (testing "throws on unknown tool"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Tool not found"
         (sut/execute-tool! :unknown/tool {})))))

(deftest validate-tool-test
  (testing "validates valid tool"
    (is (= valid-tool (sut/validate-tool valid-tool))))
  
  (testing "throws on invalid tool"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid tool configuration"
         (sut/validate-tool invalid-tool)))))
