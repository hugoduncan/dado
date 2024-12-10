(ns dado.ai.tool.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.ai.tool.interface :as tool]
            [dado.ai.tool.core :refer [bind do-error failure fmap success]]
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
    (let [result (tool/register-tool! valid-tool)]
      (is (= valid-tool result))))

  (testing "throws on invalid tool"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid tool configuration"
         (tool/register-tool! invalid-tool)))))

(deftest lookup-tool-test
  (testing "finds registered tool"
    (tool/register-tool! valid-tool)
    (is (= valid-tool (tool/lookup-tool :test/example))))

  (testing "returns nil for unknown tool"
    (is (nil? (tool/lookup-tool :unknown/tool)))))

(deftest execute-tool!-test
  (testing "executes registered tool"
    (tool/register-tool! valid-tool)
    (let [params {:input "test"}
          result (tool/execute-tool! :test/example params)]
      (is (= "test" (:result result)))))

  (testing "throws on unknown tool"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Tool not found"
         (tool/execute-tool! :unknown/tool {})))))

(deftest execute-tool-with-invalid-json-test
  (testing "execute-tool!"
    (testing "with invalid json string as params"
      (let [params "{:invalid \"json\" :map} "
            result (tool/execute-tool! :test/example params)]
        (is (:is-error? result))
        (is (re-find #"Failed to parse" (-> result :content first :text)))))
    (testing "with params that do not validate"
      (let [params {:fred "bloggs"}
            result (tool/execute-tool!
                    {:parameters [:vector :string]
                     :execute-fn (fn [params]
                                   (throw (ex-info "ERROR" {})))}
                    params)]
        (is result)
        (is (:is-error? result))
        (is (re-find #"Invalid parameters" (-> result :content first :text)))))
    (testing "with a tool that throws"
      (let [params {:fred "bloggs"}
            result (tool/execute-tool!
                    {:parameters [:map [:fred :string]]
                     :execute-fn (fn [params]
                                   (throw (ex-info "ERROR" {})))}
                    params)]
        (is result)
        (is (:is-error? result))
        (is (re-find #"Unexpected.*ERROR" (-> result :content first :text)))))))

(deftest validate-tool-test
  (testing "validates valid tool"
    (is (= valid-tool (tool/validate-tool valid-tool))))

  (testing "throws on invalid tool"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid tool configuration"
         (tool/validate-tool invalid-tool)))))
