(ns dado.update-extractor.core
  (:require [taoensso.telemere :as t]
            [taoensso.truss :refer [have?]]
            [clojure.string :as str]))

(defn extract-simplified-diffs
  "Implementation of simplifies diff extraction."
  [text]
  {:pre [(have? string? text)]}

  (t/event!    :update/extraction-started)

  (let [diff-pattern #"(?s)``` ?diff\n(.*?)```"
        matches      (re-seq diff-pattern text)]

    (t/event! :update/extraction-completed)

    (if (seq matches)
      (str/join (map second matches))
      "")))

(defn extract-search-replace-diffs
  "Implementation of search/replace diff extraction."
  [text]
  {:pre [(have? string? text)]}

  (t/event!    :update/extraction-started)

  (let [diff-pattern #"(?s)``` ?searchreplace\n(.*?)```"
        matches      (re-seq diff-pattern text)]

    (t/event! :update/extraction-completed)

    (if (seq matches)
      (str/join (map second matches))
      "")))

(defn extract-updated-namespaces
  "Implementation of updated namespaces list extraction."
  [text]
  {:pre [(have? string? text)]}

  (t/event! :update/extraction-started)

  (let [namespace-pattern #"(?s)```updated-namespaces\n(.*?)```"
        matches           (re-seq namespace-pattern text)]

    (t/event! :update/extraction-completed)

    (if (seq matches)
      (->> matches
           (map second)
           (mapcat #(str/split-lines %))
           (remove str/blank?)
           (vec))
      [])))
