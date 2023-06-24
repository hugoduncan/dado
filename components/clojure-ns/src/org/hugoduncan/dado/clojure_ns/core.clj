(ns org.hugoduncan.dado.clojure-ns.core
  "Assistant functions for authoring clojure namespaces."
  (:require
   [org.hugoduncan.dado.chat-gpt.interface :as chat-gpt]
   [org.hugoduncan.dado.clojure-target.interface :as clojure-target]
   [org.hugoduncan.dado.operation.interface :as operation]))

(defn- ns-docstring
  "Return the doc string for the namespace with the given namespace symbol."
  [ns-sym]
  (-> (the-ns ns-sym) meta :doc))

(def suggest-ns-impl-system
  "You will describe alternatives to implement a function described by the user.

Enumerate different algorithms and implementation strategies, comparing their
advantages and disadvantages.

Describe alternatives implement the function described by the user.")

(defn suggest-ns-impl
  "Suggests implementation trade-offs for a given namespace.
  The namespace's docstring is retrieved from its metadata."
  [ns-sym]
  (->
   (chat-gpt/generate-completion
    suggest-ns-impl-system
    (ns-docstring ns-sym))
   (clojure-target/format-parsed-completion-as-code)))

(defmethod operation/op ["clojure""suggest-ns-impl"]
  [_language _action args ns]
  {"suggestion" (suggest-ns-impl ns)})

(def critique-ns-system
  "You will critique the clojure namespace given by the user.

Compare the doc string against the implementation of the namespace, and comment
on any inconsistencies.

Comment on doc strings that could be improved.

Comment on style and clojure idioms.

Comment on namespace naming.

Comment on function naming.

Comment on variable naming.

Comment on function and namespace complexity.

Comment on function and namespace size.

Suggest improvements.

Critique the clojure namespace given by the user.")

(defn critique-ns
  "Critique the namespace with the given source."
  [ns-source]
  (->> ns-source
       (chat-gpt/generate-completion critique-ns-system)
       clojure-target/format-parsed-completion-as-code))

(defmethod operation/op ["clojure" "critique-ns"]
  [_language _action args ns]
  {"suggestion" (critique-ns (:ns-source args))})

(def implement-ns-system
  "You will generate clojure code to solve a problem described by the user.

The suggested code should be represented as a series of steps, each with their
own code and explanation.

All suggested functions must have doc strings describing their purpose.

Generate clojure code as described by the user.")

(defn implement-ns
  "Generate an implementation of the namespace for the given description."
  [ns-sym]
  (->> (the-ns ns-sym)
       ns-docstring
       (chat-gpt/generate-completion critique-ns-system)
       clojure-target/format-parsed-completion-as-code))

(defmethod operation/op ["clojure" "implement-ns"]
  [_language _action args ns]
  {"suggestion" (implement-ns ns)})

(def generate-ns-docstring-system
  "You will write a doc string for the clojure namespace given by the user.

The doc string should start with a one line summary of the namespace.

The doc string should describe the purpose of the namespace.

Write a doc string for the namespace given by the user.")

(defn generate-ns-docstring
  "Generate a docstring for the namespace with the given source."
  [ns-source]
  (->> ns-source
       (chat-gpt/generate-completion critique-ns-system)
       clojure-target/format-parsed-completion-as-code))

(defmethod operation/op ["clojure" "generate-ns-docstring"]
  [_language _action args ns]
  {"suggestion" (generate-ns-docstring (:ns-source args))})
