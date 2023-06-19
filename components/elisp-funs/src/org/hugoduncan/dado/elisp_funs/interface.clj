(ns org.hugoduncan.dado.elisp-funs.interface
  (:require
   [org.hugoduncan.dado.elisp-funs.core :as elisp-funs-core]))

(defn suggest-fn-impl
  "Suggests implementation trade-offs for a given function.
  The function's purpose and arguments are retrieved from its metadata.  Use
  hd.environment.chat-gpt/generate-chat-completion to call the OpenAI endpoint.
  Parse the completion with hd.environment.chat-gpt/parse-completion-string, and
  format it with hd.environment.chat-gpt/format-parsed-completion."
  [fn-name]
  (elisp-funs-core/suggest-fn-impl fn-name))
