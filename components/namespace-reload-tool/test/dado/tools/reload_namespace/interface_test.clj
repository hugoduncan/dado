(ns dado.tools.reload-namespace.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.tools.reload-namespace.interface :as sut]
            [dado.ai.tool.interface :as tool]))

(def test-input
  "```updated-namespaces
clojure.string
does.not.exist
```")

(def test-json-input "[\"clojure.string\", \"does.not.exist\"]")

(deftest create-tool-test
  (testing "creates valid tool configuration"
    (let [tool-config (sut/create-tool)]
      (is (tool/validate-tool tool-config))
      (is (= :tool/reload-namespace (:id tool-config)))))

  (testing "tool recognizes reload requests"
    (let [tool-config  (sut/create-tool)
          recognize-fn (:recognize-fn tool-config)]
      (is (recognize-fn "Please reload these namespaces:"))
      (is (recognize-fn test-input))
      (is (not (recognize-fn "Something else")))))

  (testing "tool execution"
    (let [tool-config (sut/create-tool)
          execute-fn  (:execute-fn tool-config)
          result      (execute-fn {:namespaces test-json-input})]
      (is (= [{:text "Reloaded: clojure.string"}
              {:text
               "does.not.exist failed to reload: Could not locate does/not/exist__init.class, does/not/exist.clj or does/not/exist.cljc on classpath."}]
             result)))))
