(ns org.hugoduncan.dado.chat-gpt.interface
  (:require
   [clojure.string :as str]
   [org.hugoduncan.dado.chat-gpt.core :as chat-gpt-core]
   [org.hugoduncan.dado.util.interface :as util]))

(defn generate-completion
  "Generate a completion using ChatGPT.
  Return a sequence of code block and explanation texts."
  [system content]
  (->
   (chat-gpt-core/generate-completion system content)
   chat-gpt-core/parse-completion-string))

(defn generate-chat-completion
  "Generate a completion using ChatGPT.
  Return a sequence of code block and explanation texts."
  [messages]
  (->
   (chat-gpt-core/generate-chat-completion messages)))

(defn format-parsed-completion-verbatim
  "Format the parsed-completion argument to reconstruct the completion verbatim.

  The parsed-completion argument is a sequence as returned by
  `parse-completion-string`.  Each element of the sequence is a map.  A
  map with a `:code` key is a code block element and is output directly.
  A map with a `:text` key is an explanation text element. An
  explanation text element contains multiple lines of text.  Each line
  should be word-wrapped to be shorter than 80 characters, and then each
  line is output as a clojure comment.

  Return the concatenation of each formatted element."
  [parsed-completion]
  (letfn [(format-element [elem]
            (if (:code elem)
              (str "```" (:code elem) "```")
              (->> (:text elem)
                   (str/split-lines)
                   (map #(str/replace % #"\n" ""))
                   (map #(str/join "\n;; " (util/word-wrap % 80)))
                   (apply str))))] ;; concatenate all wrapped lines
    (str/join "\n" (mapv format-element parsed-completion))))
