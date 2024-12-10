(ns dado.error-monad.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.error-monad.interface :as em]))

(deftest monad-laws-test
  (testing "left identity: (bind (success x) f) ≡ (f x)"
    (let [f #(em/success (inc %))]
      (is (= (em/bind (em/success 1) f)
             (f 1)))))

  (testing "right identity: (bind m success) ≡ m"
    (is (= (em/bind (em/success 1) em/success)
           (em/success 1)))
    (is (= (em/bind (em/failure "error") em/success)
           (em/failure "error"))))

  (testing "associativity: (bind (bind m f) g) ≡ (bind m #(bind (f %) g))"
    (let [f #(em/success (inc %))
          g #(em/success (* % 2))
          m (em/success 1)]
      (is (= (em/bind (em/bind m f) g)
             (em/bind m #(em/bind (f %) g)))))))

(deftest basic-operations-test
  (testing "success creation and access"
    (let [m (em/success 1)]
      (is (:success? m))
      (is (= 1 (:value m)))))

  (testing "failure creation and access"
    (let [m (em/failure "error")]
      (is (not (:success? m)))
      (is (= "error" (:value m)))))

  (testing "bind with success"
    (is (= (em/success 2)
           (em/bind (em/success 1) #(em/success (inc %))))))

  (testing "bind with failure"
    (is (= (em/failure "error")
           (em/bind (em/failure "error") #(em/success (inc %))))))

  (testing "fmap with success"
    (is (= (em/success 2)
           (em/fmap inc (em/success 1)))))

  (testing "fmap with failure"
    (is (= (em/failure "error")
           (em/fmap inc (em/failure "error"))))))

(deftest do-error-test
  (testing "do-error with successful chain"
    (is (= (em/success 6)
           (em/do-error [a (em/success 1)
                      b (em/success (inc a))
                      c (em/success (* b 2))]
                     (em/success (+ c 2))))))

  (testing "do-error fails fast on first error"
    (is (= (em/failure "boom")
           (em/do-error [a (em/success 1)
                      b (em/failure "boom")
                      c (em/success 3)]
                     (em/success (+ a b c))))))

  (testing "do-error with empty binding vector"
    (is (= (em/success 1)
           (em/do-error []
                     (em/success 1))))))