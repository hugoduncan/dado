(ns dado.tools.filesystem.diff
  "Diff utilities for filesystem tools"
  (:require [clojure.string :as str]))

(defn normalize-line-endings
  "Normalize line endings to \n."
  [text]
  (str/replace text #"\r\n" "\n"))

(defn diff-lines
  "Generate diff between original and modified lines.
   Returns sequence of [operation line] where operation is
   one of :-, :+ or nil (for context lines)."
  [original modified]
  (let [orig (vec original)
        mod  (vec modified)]
    (loop [idx-o  0
           idx-m  0
           result []]
      (let [remaining-o (- (count orig) idx-o)
            remaining-m (- (count mod) idx-m)]
        (cond
          ;; Both sequences exhausted
          (and (zero? remaining-o)
               (zero? remaining-m))
          result

          ;; Original exhausted - all remaining are adds
          (zero? remaining-o)
          (into result (map #(vector :+ %)
                            (subvec mod idx-m)))

          ;; Modified exhausted - all remaining are deletes
          (zero? remaining-m)
          (into result (map #(vector :- %)
                            (subvec orig idx-o)))

          ;; Lines match
          (= (nth orig idx-o)
             (nth mod idx-m))
          (recur (inc idx-o)
                 (inc idx-m)
                 (conj result [nil (nth orig idx-o)]))

          ;; Look ahead one line in each sequence
          (and (< (inc idx-o) (count orig))
               (= (nth orig (inc idx-o))
                  (nth mod idx-m)))
          (recur (inc idx-o)
                 idx-m
                 (conj result [:- (nth orig idx-o)]))

          (and (< (inc idx-m) (count mod))
               (= (nth orig idx-o)
                  (nth mod (inc idx-m))))
          (recur idx-o
                 (inc idx-m)
                 (conj result [:+ (nth mod idx-m)]))

          ;; Neither matches - treat as replacement
          :else
          (recur (inc idx-o)
                 (inc idx-m)
                 (-> result
                     (conj [:- (nth orig idx-o)])
                     (conj [:+ (nth mod idx-m)]))))))))

(defn- split-lines [content]
  (when-not (str/blank? content)
    (str/split-lines content)))

(defn create-unified-diff
  "Create unified diff between original and new content.
   Returns string in unified diff format."
  [original-content new-content filepath]
  (let [original-lines (split-lines (normalize-line-endings original-content))
        new-lines      (split-lines (normalize-line-endings new-content))
        diff-lines     (diff-lines original-lines new-lines)
        diff-text      (str/join "\n"
                                 (map (fn [[op line]]
                                        (str (case op
                                               :- "-"
                                               :+ "+"
                                               " ") line))
                                      diff-lines))]
    (prn :new-lines new-lines)
    (prn :diff-lines diff-lines)
    (prn :diff-text diff-text)
    (str "--- " filepath "\n"
         "+++ " filepath "\n"
         diff-text)))
