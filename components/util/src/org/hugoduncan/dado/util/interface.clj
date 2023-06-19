(ns org.hugoduncan.dado.util.interface
  (:require
   [clojure.string :as str]))

(defn word-wrap
  "Word wrap text into lines no longer than width."
  [text width]
  (let [words (str/split text #"\s+")]
    (loop [lines [] in-progress-line [] remaining-words words]
      (if (empty? remaining-words)
        (conj lines (str/join " " in-progress-line))
        (let [next-word (first remaining-words)
              next-line (str/join " " (conj in-progress-line next-word))]
          (if (or (<= (count next-line) width) (= 0 (count in-progress-line)))
            (recur
             lines
             (conj in-progress-line next-word)
             (rest remaining-words))
            (recur
             (conj lines (str/join " " in-progress-line))
             [next-word]
             (rest remaining-words))))))))
