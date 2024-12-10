(ns dado.error-monad.interface-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [dado.error-monad.interface :refer [bind do-error failure fmap maybe success]]))


;;; Error Monad

(deftest monad-laws-test
  (testing "left identity: (bind (success x) f) ≡ (f x)"
    (let [f #(success (inc %))]
      (is (= (bind (success 1) f)
             (f 1)))))

  (testing "right identity: (bind m success) ≡ m"
    (is (= (bind (success 1) success)
           (success 1)))
    (is (= (bind (failure "error") success)
           (failure "error"))))

  (testing "associativity: (bind (bind m f) g) ≡ (bind m #(bind (f %) g))"
    (let [f #(success (inc %))
          g #(success (* % 2))
          m (success 1)]
      (is (= (bind (bind m f) g)
             (bind m #(bind (f %) g)))))))

(deftest basic-operations-test
  (testing "success creation and access"
    (let [m (success 1)]
      (is (:success? m))
      (is (= 1 (:value m)))))

  (testing "failure creation and access"
    (let [m (failure "error")]
      (is (not (:success? m)))
      (is (= "error" (:value m)))))

  (testing "bind with success"
    (is (= (success 2)
           (bind (success 1) #(success (inc %))))))

  (testing "bind with failure"
    (is (= (failure "error")
           (bind (failure "error") #(success (inc %))))))

  (testing "fmap with success"
    (is (= (success 2)
           (fmap inc (success 1)))))

  (testing "fmap with failure"
    (is (= (failure "error")
           (fmap inc (failure "error"))))))

(deftest do-error-test
  (testing "do-error with successful chain"
    (is (= (success 6)
           (do-error [a (success 1)
                      b (success (inc a))
                      c (success (* b 2))]
                     (success (+ c 2))))))

  (testing "do-error fails fast on first error"
    (is (= (failure "boom")
           (do-error [a (success 1)
                      b (failure "boom")
                      c (success 3)]
                     (success (+ a b c))))))

  (testing "do-error with empty binding vector"
    (is (= (success 1)
           (do-error []
                     (success 1))))))
