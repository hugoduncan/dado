(ns dado.error-monad.core
  "Core implementation of error monad")

(defrecord ErrorMonadValue
    [success? value])

(defn success
  [x]
  (->ErrorMonadValue true x))

(defn failure
  [x]
  (->ErrorMonadValue false x))

(defn maybe
  [success? value]
  (->ErrorMonadValue success? value))

(defn bind
  [m f]
  (if (:success? m) (f (:value m)) m))

(defn fmap
  [f m]
  (bind m (fn cont [x] (success (f x)))))

(defn do-error
  [bindings expr]
  (let [[sym m & rest] bindings]
    (if sym
      `(bind ~m (fn [~sym] ~(do-error (vec rest) expr)))
      expr)))
