(ns org.hugoduncan.dado.util.interface-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [org.hugoduncan.dado.util.interface :as util]))

(deftest test-word-wrap
  (testing "test wrapping a single word"
    (is (= ["test"]
           (util/word-wrap "test" 10))))

  (testing "test wrapping a few words"
    (is (= ["this is a"
            "few words"
            "wrapped in"
            "text"]
           (util/word-wrap "this is a few words wrapped in text" 10))))

  (testing "test wrapping text with long words"
    (is (= ["this is a"
            "very-long-word"
            "that"
            "cannot be"
            "broken up"
            "wrapped in"
            "text"]
           (util/word-wrap "this is a very-long-word that cannot be broken up wrapped in text" 10))))

  (testing "test wrapping empty string"
    (is (= [""] (util/word-wrap "" 10))))

  (testing "test wrapping text with no spaces"
    (is (= ["thisisistextwithnospaces"]
           (util/word-wrap "thisisistextwithnospaces" 7)))))
