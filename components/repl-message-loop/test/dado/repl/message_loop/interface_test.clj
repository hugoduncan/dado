(ns dado.repl.message-loop.interface-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [dado.repl.message-loop.interface :as message-loop]))

(def required-namespaces (atom []))

(defn with-tracked-requires
  "Test fixture to track namespace requires"
  [f]
  (let [orig-require @#'require]
    (with-redefs [require (fn [ns-sym & [reload]]
                            (when reload
                              (swap! required-namespaces conj (str ns-sym))))]
      (f))))

(use-fixtures :each with-tracked-requires)

(deftest message-loop-validation-test
  (testing "validates inputs"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invariant.*message-thread"
         (message-loop/message-loop {} {} (constantly "") (constantly [])))
        "should reject invalid message thread")

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invariant.*prompt-fn"
         (message-loop/message-loop
          {:id "test" :created-at (java.time.Instant/now) :messages [] :metadata {:model "test"}}
          {}
          "not-a-function"
          (constantly [])))
        "should reject invalid prompt-fn")

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invariant.*context-files-fn"
         (message-loop/message-loop
          {:id "test" :created-at (java.time.Instant/now) :messages [] :metadata {:model "test"}}
          {}
          (constantly "")
          "not-a-function"))
        "should reject invalid context-files-fn")))

(deftest message-loop-namespace-reload-test
  (testing "reloads updated namespaces"
    (let [test-thread {:id         "test"
                       :created-at (java.time.Instant/now)
                       :messages   []
                       :metadata   {:model "test"}}
          response    {:role          :assistant
                       :content       "```updated-namespaces\nmy.project.model\nmy.project.core\n```"
                       :finish-reason :stop}
          mock-port   (constantly response)
          input       (atom '("some test" "EXIT"))]

      ;; Mock AI interaction to return our test response
      (with-redefs [print identity
                    read-line                      (fn [& _]
                                                     (let [resp (peek @input)]
                                                       (when-not resp (assert false ))
                                                       (swap! input pop)
                                                       resp))]
        (message-loop/message-loop
         mock-port
         test-thread
         (constantly "test prompt")
         (constantly []))

        ;; Check that the namespaces were required with :reload
        (is (= ["my.project.model" "my.project.core"]
               @required-namespaces)
            "Should have reloaded the updated namespaces")))))
