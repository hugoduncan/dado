(ns org.hugoduncan.dado.elisp-target.interface
  (:require
   [clojure.string :as str]
   [org.hugoduncan.dado.util.interface :as util]))

(defn format-parsed-completion-as-code
  "Format the parsed-completion argument as clojure code.

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
              (str (:code elem) "\n")
              (->> (:text elem)
                   (str/split-lines)
                   (map #(str/replace % #"\n" ""))
                   (map #(str/join "\n;; " (util/word-wrap % 77)))
                   (map #(if (str/blank? %) "\n" (str ";; " % "\n")))
                   (apply str))))] ;; concatenate all wrapped lines
    (str/join "\n" (mapv format-element parsed-completion))))
