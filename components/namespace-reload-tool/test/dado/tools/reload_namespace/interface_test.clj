(ns dado.tools.reload-namespace.interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.tools.reload-namespace.interface :as sut]
            [dado.ai.tool.interface :as tool]))

(def test-input
  "```updated-namespaces
clojure.string
does.not.exist
```")

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
          result      (execute-fn test-input)]
      (is (= ['clojure.string] (:reloaded result)))
      (is (= 1 (count (:errors result))))
      (is (= 'does.not.exist (:ns (first (:errors result))))))))
