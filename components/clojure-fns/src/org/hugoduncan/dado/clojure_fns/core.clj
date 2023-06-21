(ns org.hugoduncan.dado.clojure-fns.core
  "Assistant functions for authoring clojure functions."
  (:require
   [clojure.string :as str]
   [org.hugoduncan.dado.chat-gpt.interface :as chat-gpt]
   [org.hugoduncan.dado.clojure-target.interface :as clojure-target]
   [org.hugoduncan.dado.operation.interface :as operation]))

(def suggest-fn-impl-system
  "You will describe alternatives to implement a function described by the user.

Enumerate different algorithms and implementation strategies, comparing their
advantages and disadvantages.

Describe alternatives implement the function described by the user.")

(defn suggest-fn-impl
  "Suggests implementation trade-offs for a given function.
  The function's purpose and arguments are retrieved from its metadata.  Use
  hd.environment.chat-gpt/generate-chat-completion to call the OpenAI endpoint.
  Parse the completion with hd.environment.chat-gpt/parse-completion-string, and
  format it with hd.environment.chat-gpt/format-parsed-completion."
  [ns-sym fn-name]
  (let [fn-var         (ns-resolve ns-sym fn-name)
        fn-meta        (meta fn-var)
        fn-docstring   (:doc fn-meta)
        fn-args-string (->> fn-meta
                            :arglists
                            (mapv #(str/join " " %))
                            (mapv str)
                            (str/join "\n"))
        completion     (chat-gpt/generate-completion
                        suggest-fn-impl-system
                        (str fn-docstring "\n\nArguments:\n" fn-args-string))]
    (->
     completion
     (clojure-target/format-parsed-completion-as-code))))

(defmethod operation/op ["clojure""suggest-fn-impl"]
  [_language _action args ns]
  {"suggestion" (suggest-fn-impl ns (symbol (:fn-name args)))})

(def implement-fn-system
  "You will generate clojure code to solve a problem described by the user.

The suggested code should be represented as a series of steps, each with their
own code and explanation.

All suggested functions must have doc strings describing their purpose.

Prefer functions with a low cyclometric complexity.

Generate clojure code as described by the user.")

(defn implement-fn
  "Generates code for a given function.
  The function's purpose and arguments are retrieved from its metadata.  Use
  hd.environment.chat-gpt/generate-chat-completion to call the OpenAI endpoint.
  Parse the completion with hd.environment.chat-gpt/parse-completion-string, and
  format it with hd.environment.chat-gpt/format-parsed-completion."
  [ns-sym fn-name]
  (let [fn-var         (ns-resolve ns-sym fn-name)
        fn-meta        (meta fn-var)
        fn-docstring   (:doc fn-meta)
        fn-args-string (->> fn-meta
                            :arglists
                            (mapv #(str/join " " %))
                            (mapv str)
                            (str/join "\n"))
        completion     (chat-gpt/generate-completion
                        implement-fn-system
                        (str fn-docstring "\n\nArguments:\n" fn-args-string))]
    (->
     completion
     (clojure-target/format-parsed-completion-as-code))))

(defmethod operation/op ["clojure" "implement-fn"]
  [_language _action args ns]
  {"suggestion" (implement-fn ns (symbol (:fn-name args)))})


(def implement-fn-test-system
  "You will generate test code for the function given by the user.

The tests should use the `clojure.test` test framework.


All suggested functions must have doc strings describing their purpose.

Prefer functions with a low cyclometric complexity.

Prefer referring a namespace with an alias rather than referring symbols.

Prefer referring `clojure.test` by referring individual symbols.

Use `clojure.test/testing` forms to describe the purpose of each test assertion.

Generate clojure test code to test the function given by the user.")

(defn implement-fn-test
  "Generates code to implement tests for the function with the given name."
  [ns-sym fn-name]
  (let [fn-var  (ns-resolve ns-sym (symbol fn-name))
        ns-name (ns-name  (:ns (meta fn-var)))
        ns-str  (str "(ns " ns-name ")\n")
        fn-str  (clojure-target/source-fn
                 (symbol (str ns-name) (name fn-name)))]
    (->> (str ns-str fn-str)
         (chat-gpt/generate-completion implement-fn-test-system)
         clojure-target/format-parsed-completion-as-code)))

(defmethod operation/op ["clojure" "implement-fn-test"]
  [_language _action args ns]
  {"suggestion" (implement-fn-test ns (symbol (:fn-name args)))})

(def critique-fn-system
  "You will critique the clojure function given by the user.

Compare the doc string against the implementation of the function, and comment
on any inconsistencies.

Comment on style and clojure idioms.

Comment on function naming.

Comment on variable naming.

Comment on complexity.

Critique the clojure function given by the user.")

(defn critique-fn
  "Critique the function with the given name using the OpenAI chat
  completion API."
  [ns-sym fn-name]
  (let [fn-var  (ns-resolve ns-sym (symbol fn-name))
        ns-name (ns-name  (:ns (meta fn-var)))
        ns-str  (str "(ns " ns-name ")\n")
        fn-str  (clojure-target/source-fn
                 (symbol (str ns-name) (name fn-name)))]
    (->> (str ns-str fn-str)
         (chat-gpt/generate-completion critique-fn-system)
         clojure-target/format-parsed-completion-as-code)))

(defmethod operation/op ["clojure" "critique-fn"]
  [_language _action args ns]
  {"suggestion" (critique-fn ns (symbol (:fn-name args)))})

(def generate-fn-docstring-system
  "You will write a doc string for the clojure function given by the user.

The doc string should start with a one line summary of the function.

The doc string should describe the purpose of the function.

The doc string should describe the input arguments and the return value.

Write a doc string for function given by the user.")

(defn generate-fn-docstring
  "Write a doc string for the function with the given name."
  [ns-sym fn-name]
  (let [fn-var  (ns-resolve ns-sym (symbol fn-name))
        ns-name (ns-name  (:ns (meta fn-var)))
        ns-str  (str "(ns " ns-name ")\n")
        fn-str  (clojure-target/source-fn
                 (symbol (str ns-name) (name fn-name)))]
    (->> (str ns-str fn-str)
         (chat-gpt/generate-completion generate-fn-docstring-system)
         clojure-target/format-parsed-completion-as-code)))

(defmethod operation/op ["clojure" "generate-fn-docstring"]
  [_language _action args ns]
  {"suggestion" (generate-fn-docstring ns (symbol (:fn-name args)))})
