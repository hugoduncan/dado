;; test/interface_test.clj
(ns dado.project-config.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.project-config.core :as core]
   [dado.project-config.interface :as config]))

(deftest load-config-test
  (testing "loading valid configuration"
    (with-redefs [core/read-config-file
                  (constantly
                   {:dev-dir      "dev"
                    :ai-providers {:test {}}})]
      (is (map? (config/load-config))))))
