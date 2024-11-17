(ns dado.update-extractor.core
  (:require [taoensso.telemere :as t]
            [taoensso.truss :refer [have?]]
            [clojure.string :as str]))

(defn extract-diffs
  "Implementation of diff block extraction."
  [text]
  {:pre [(have? string? text)]}

  (t/event!    :update/extraction-started)

  (let [diff-pattern #"(?s)```diff\n(.*?)```"
        matches      (re-seq diff-pattern text)]

    (t/event! :update/extraction-completed)

    (if (seq matches)
      (str/join (map second matches))
      "")))
