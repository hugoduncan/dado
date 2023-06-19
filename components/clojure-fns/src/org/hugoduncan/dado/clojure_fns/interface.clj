(ns org.hugoduncan.dado.clojure-fns.interface
  "Assistant functions for authoring clojure functions."
  (:require
   [org.hugoduncan.dado.clojure-fns.core :as clojure-fns-core]))

(defn suggest-fn-impl
  "Suggests implementation trade-offs for a given function using OpenAI ChatGPT.
  The function's purpose and arguments are retrieved from its metadata.  Use
  hd.environment.chat-gpt/generate-chat-completion to call the OpenAI endpoint.
  Parse the completion with hd.environment.chat-gpt/parse-completion-string, and
  format it with hd.environment.chat-gpt/format-parsed-completion."
  [fn-name]
  (clojure-fns-core/suggest-fn-impl fn-name))
