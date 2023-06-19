(ns org.hugoduncan.dado.elisp-funs.core
  (:require
   [org.hugoduncan.dado.chat-gpt.interface :as chat-gpt]
   [org.hugoduncan.dado.elisp-target.interface :as elisp-target]
   [org.hugoduncan.dado.operation.interface :as operation]))


(def suggest-fun-impl-system
  "You will describe alternatives to implement a function described by the user.

Enumerate different algorithms and implementation strategies, comparing their
advantages and disadvantages.

Describe alternatives implement the function described by the user.")

(defn suggest-fun-impl
  "Suggests implementation trade-offs for a given function.
  The function's purpose and arguments are retrieved from its metadata.  Use
  hd.environment.chat-gpt/generate-chat-completion to call the OpenAI endpoint.
  Parse the completion with hd.environment.chat-gpt/parse-completion-string, and
  format it with hd.environment.chat-gpt/format-parsed-completion."
  [fn-name fn-docstring fn-args]
  (let [completion (chat-gpt/generate-completion
                    suggest-fun-impl-system
                    (str fn-docstring "\n\nFunction Name: " fn-name
                         "\nArguments:\n" fn-args))]
    (->
     completion
     (elisp-target/format-parsed-completion-as-code))))

(defmethod operation/op ["elisp" "suggest-fn-impl"]
  [_language _action args _ns]
  {"suggestion" (suggest-fun-impl
                 (symbol (:fn-name args))
                 (:docstring args)
                 (:arguments args))})

(def implement-fun-system
  "You will generate elisp code to implement the function described by the user.

The suggested code should be represented as a series of steps, each with their
own code and explanation.

All suggested functions must have doc strings describing their purpose.

Prefer functions with a low cyclometric complexity.

Generate elisp code as described by the user.")

(defn implement-fun
  "Implements a given function.
  The function's purpose and arguments are retrieved from its metadata.  Use
  hd.environment.chat-gpt/generate-chat-completion to call the OpenAI endpoint.
  Parse the completion with hd.environment.chat-gpt/parse-completion-string, and
  format it with hd.environment.chat-gpt/format-parsed-completion."
  [fn-name fn-docstring fn-args]
  (let [ completion (chat-gpt/generate-completion
                     implement-fun-system
                     (str fn-docstring "\n\nFunction Name: " fn-name
                          "\nArguments:\n" fn-args))]
    (->
     completion
     (elisp-target/format-parsed-completion-as-code))))

(defmethod operation/op ["elisp" "implement-fn"]
  [_language _action args _ns]
  {"suggestion" (implement-fun
                 (symbol (:fn-name args))
                 (:docstring args)
                 (:arguments args))})

(def implement-fun-test-system
  "You will generate test code for the function given by the user.

All suggested functions must have doc strings describing their purpose.

Prefer functions with a low cyclometric complexity.

Prefer referring a namespace with an alias rather than referring symbols.

Generate elisp test code to test the function given by the user.")

(defn implement-fun-test
  "Implements a given function.
  The function's purpose and arguments are retrieved from its metadata.  Use
  hd.environment.chat-gpt/generate-chat-completion to call the OpenAI endpoint.
  Parse the completion with hd.environment.chat-gpt/parse-completion-string, and
  format it with hd.environment.chat-gpt/format-parsed-completion."
  [fn-name fn-docstring fn-args fn-definition]
  (let [ completion (chat-gpt/generate-completion
                     implement-fun-test-system
                     (str fn-docstring "\n\nFunction Name: " fn-name
                          "\nArguments:\n" fn-args
                          "\n\nDefinition:\n " fn-definition))]
    (->
     completion
     (elisp-target/format-parsed-completion-as-code))))

(defmethod operation/op ["elisp" "implement-fn-test"]
  [_language _action args _ns]
  {"suggestion" (implement-fun-test
                 (symbol (:fn-name args))
                 (:docstring args)
                 (:arguments args)
                 (:fn-definition args))})

(def critique-fun-system
  "You will critique the elisp function given by the user.

Compare the doc  string against the implementation of the function, and comment
on any inconsistencies.

Comment on style and elisp idioms.

Comment on function naming.

Comment on variable naming.

Comment on complexity.

Critique the function given by the user and generate an improved function
defintion.")

(defn critique-fun
  "Critique a given function. "
  [fn-name fn-docstring fn-args fn-definition]
  (let [ completion (chat-gpt/generate-completion
                     critique-fun-system
                     (str fn-docstring
                          "\n\nFunction Name: " fn-name
                          "\nArguments:\n" fn-args
                          "\n\nDefinition:\n " fn-definition ))]
    (->
     completion
     (elisp-target/format-parsed-completion-as-code))))

(defmethod operation/op ["elisp" "critique-fn"]
  [_language _action args _ns]
  {"suggestion" (critique-fun
                 (symbol (:fn-name args))
                 (:docstring args)
                 (:arguments args)
                 (:fn-definition args))})

(def generate-fun-docstring-system
  "You will write a doc string for the elisp function given by the user.

The doc string should start with a one line summary of the function.

The doc string should describe the purpose of the function.

The doc string should describe the input arguments and the return value.

Write a doc string for function given by the user.")

(defn generate-fun-docstring
  "Critique a given function. "
  [fn-name fn-docstring fn-args fn-definition]
  (let [ completion (chat-gpt/generate-completion
                     generate-fun-docstring-system
                     (str fn-docstring
                          "\n\nFunction Name: " fn-name
                          "\nArguments:\n" fn-args
                          "\n\nDefinition:\n " fn-definition))]
    (->
     completion
     (elisp-target/format-parsed-completion-as-code))))

(defmethod operation/op ["elisp" "generate-fn-docstring"]
  [_language _action args _ns]
  {"suggestion" (generate-fun-docstring
                 (symbol (:fn-name args))
                 (:docstring args)
                 (:arguments args)
                 (:fn-definition args))})
#_(defn generate-code-completion
    [message]
    (->> message
         (chat-gpt/generate-completion implement-fun-system)
         chat-gpt/format-parsed-completion-verbatim))

#_(def bootstrap-1
    (generate-code-completion
     "A function to return a symbol representing the coding language of BUFFER.

BUFFER defaults to the current buffer.

Recognises emacs lisp and clojure.

Return 'elisp for emacs lisp, 'clojure for clojure, or nil otherwise."))

#_(def bootstrap-2
    (generate-code-completion
     "A function to return the doc string for symbol at point.
Return  the doc string if available, or nil otherwise."))

#_(def bootstrap-3
    (generate-code-completion
     "A function to return the arguments for symbol that refers to a function."))
