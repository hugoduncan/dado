(ns dado.error-monad.interface
  "Provides a monadic approach to error handling in Clojure.

  The Error Monad is a functional programming pattern that allows for
  explicit, chainable error handling while maintaining clean, composable code.

  Key Features:
  - Explicit success or failure states
  - Chainable computations with error short-circuiting
  - Functional error propagation
  - Encourages early validation and error detection

  Usage Patterns:
  1. Creating successful or failed computations
  2. Chaining computations that may fail
  3. Mapping functions over potentially failing computations

  Example:
  (do-error
    [x (success 10)
     y (success (inc x))]
    (success (* x y)))
  ; Returns a successful computation with value 110

  Prefer this approach over nested try/catch or multiple conditional branches."
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
