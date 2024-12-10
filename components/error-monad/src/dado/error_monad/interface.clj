(ns dado.error-monad.interface
  "Error monad interface namespace.

  Provides monadic error handling capabilities."
  (:require
   [dado.error-monad.core :as core]))

(defn success
  "Creates a success value in the error monad.
   Returns an ErrorMonadValue with success? set to true."
  [x]
  (core/success x))

(defn failure
  "Creates a failure value in the error monad.
   Returns an ErrorMonadValue with success? set to false."
  [x]
  (core/failure x))

(defn maybe
  "Creates an error monad value with explicit success flag.
   Returns an ErrorMonadValue with given success? and value."
  [success? value]
  (core/maybe success? value))

(defn bind
  "Chains a computation in the error monad.
   If m is successful, applies f to its value.
   If m is a failure, returns m unchanged."
  [m f]
  (core/bind m f))

(defn fmap
  "Maps a function over an error monad value.
   If m is successful, applies f to its value and wraps in success.
   If m is a failure, returns m unchanged."
  [f m]
  (core/fmap f m))

(defmacro do-error
  "Chains multiple computations in the error monad.
   Similar to clojure.core/do but for monadic computation.
   Bindings should be pairs of symbol and monadic computation.
   Returns the result of evaluating expr in monadic context."
  [bindings expr]
  (core/do-error bindings expr))
