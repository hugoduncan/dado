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

(def valid-config
  {:directories
   {:dado/prompts "dev/ai/prompts"
    :dado/adr     "dev/design/adr"
    :custom/path  "path/to/custom"}})

(def invalid-config
  {:directories
   {:dado/prompts "/absolute/path"}})

(deftest get-directory-test
  (testing "getting configured directory"
    (is (= "dev/ai/prompts"
           (config/get-directory valid-config :dado/prompts))))

  (testing "getting custom directory"
    (is (= "path/to/custom"
           (config/get-directory valid-config :custom/path))))

  (testing "getting unconfigured directory"
    (is (nil? (config/get-directory valid-config :missing/dir)))))
