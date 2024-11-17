(ns dado.patch.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have!]]))

(def ^:private file-header-pattern
  #"^--- (?:/dev/null|[^/\n].+)\n\+\+\+ ([^/\n].+)$")

(def ^:private hunk-header-pattern
  #"^@@ .+ @@\n")

(defn- parse-file
  "Parse a single file section from a patch.
   Returns {:target-path <path> :is-new? <bool> :hunks <hunks>}
   or error info map."
  [file-section]
  (let [[header & lines] (str/split-lines file-section)]
    (if-let [[_ target-path] (re-matches file-header-pattern header)]
      (let [is-new? (str/starts-with? header "--- /dev/null")
            hunks   (loop [remaining-lines lines
                           current-hunk    []
                           hunks           []]
                      (cond
                        (empty? remaining-lines)
                        (if (empty? current-hunk)
                          hunks
                          (conj hunks current-hunk))

                        (re-matches hunk-header-pattern (first remaining-lines))
                        (recur (rest remaining-lines)
                               [(first remaining-lines)]
                               (if (empty? current-hunk)
                                 hunks
                                 (conj hunks current-hunk)))

                        :else
                        (recur (rest remaining-lines)
                               (conj current-hunk (first remaining-lines))
                               hunks)))]
        {:target-path target-path
         :is-new?     is-new?
         :hunks       hunks})
      {:error   :invalid-file-header
       :content file-section})))

(defn- apply-hunk
  "Apply a single hunk to content, returns [new-content error-info]"
  [content hunk]
  (let [[header & changes] hunk
        context-lines      (->> changes
                                (filter #(str/starts-with? % " "))
                                (map #(subs % 1)))
        content-lines      (str/split-lines content)]
    (if (< (count context-lines) 2)
      [content {:error :insufficient-context
                :hunk  hunk}]
      (let [content-str (str/join "\n" content-lines)
            context-str (str/join "\n" context-lines)]
        (if (str/includes? content-str context-str)
          (let [new-content (->> changes
                                 (remove #(str/starts-with? % "-"))
                                 (map #(if (str/starts-with? % "+")
                                         (subs % 1)
                                         (subs % 1)))
                                 (str/join "\n"))]
            [new-content nil])
          [content {:error :context-mismatch
                    :hunk  hunk}])))))

(defn- apply-hunks
  "Apply all hunks to content, returns [new-content errors]"
  [content hunks]
  (loop [current-content content
         remaining-hunks hunks
         errors          []]
    (if (empty? remaining-hunks)
      [current-content errors]
      (let [[new-content error] (apply-hunk current-content (first remaining-hunks))]
        (recur new-content
               (rest remaining-hunks)
               (if error
                 (conj errors error)
                 errors))))))

(defn- count-changes
  "Count lines added and removed in a file's hunks"
  [hunks]
  (reduce (fn [counts line]
            (cond
              (str/starts-with? line "+")
              (update counts :lines-added inc)

              (str/starts-with? line "-")
              (update counts :lines-removed inc)

              :else counts))
          {:lines-added 0 :lines-removed 0}
          (mapcat rest hunks)))

(defn- apply-file-changes
  "Apply changes to a single file, returns [stats error-info]"
  [{:keys [target-path is-new? hunks]}]
  (t/trace!
   {:id :dado.patch/apply-file}
   (try
     (cond
       (and (not is-new?) (not (fs/exists? target-path)))
       [nil {:error :file-not-found
             :path  target-path}]

       (and is-new? (fs/exists? target-path))
       [nil {:error :file-exists
             :path  target-path}]

       :else
       (let [current-content      (if is-new? "" (slurp target-path))
             [new-content errors] (apply-hunks current-content hunks)]
         (if (seq errors)
           [nil {:errors errors
                 :path   target-path}]
           [(count-changes hunks) nil])))

     (catch Exception e
       [nil {:error :file-access
             :path  target-path
             :cause e}]))))

(defn- write-changes!
  "Write changes to filesystem, returns true on success"
  [{:keys [target-path is-new? hunks]} new-content]
  (t/trace!
   {:id :dado.patch/write-file}
   (try
     (when is-new?
       (fs/create-dirs (fs/parent target-path)))

     (let [temp-path (str target-path ".tmp")]
       (spit temp-path new-content)
       (fs/move temp-path target-path {:replace-existing true}))
     true

     (catch Exception e
       (throw (ex-info "Failed to write changes"
                       {:type    :error/file-access
                        :context {:component "dado.patch"
                                  :path      target-path}
                        :cause   e}))))))

(defn apply-patch!
  "See dado.patch.interface/apply-patch! for documentation"
  [patch-content]
  (t/trace!
   {:id :dado.patch/apply-patch}
   (let [file-sections (str/split patch-content #"(?m)^(?=---)")]
     ;; First pass - parse and validate all files
     (let [parsed-files (mapv parse-file file-sections)
           parse-errors (->> parsed-files
                             (filter :error)
                             (mapv #(assoc % :type :error/patch-validation)))]
       (if (seq parse-errors)
         (throw (ex-info "Invalid patch format"
                         {:type    :error/patch-validation
                          :context {:component "dado.patch"
                                    :errors    parse-errors}}))

         ;; Second pass - apply all changes in memory
         (let [valid-files      (remove :error parsed-files)
               [results errors] (reduce (fn [[results errors] file-info]
                                          (let [[stats error] (apply-file-changes file-info)]
                                            [(if stats
                                               (assoc results (:target-path file-info) stats)
                                               results)
                                             (if error
                                               (conj errors error)
                                               errors)]))
                                        [{} []]
                                        valid-files)]
           (if (seq errors)
             (throw (ex-info "Patch application failed"
                             {:type    :error/patch-application
                              :context {:component "dado.patch"
                                        :errors    errors}}))

             ;; Final pass - write changes to filesystem
             (do
               (doseq [file-info valid-files]
                 (let [current-content (if (:is-new? file-info)
                                         ""
                                         (slurp (:target-path file-info)))
                       [new-content _] (apply-hunks current-content (:hunks file-info))]
                   (write-changes! file-info new-content)))
               results))))))))
