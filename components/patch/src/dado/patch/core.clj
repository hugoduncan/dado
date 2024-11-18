(ns dado.patch.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have! have]]))

(def ^:private file-header-pattern
  #"^--- (?:/dev/null|[^\n].+)\n\+\+\+ ([^\n].+)$")

(def ^:private search-replace-file-header-pattern
  #"^(EDIT|CREATE|DELETE|MOVE|COPY)\s([^\s\n]+)(?:\s+([^\s\n]+))?$")

(def ^:private simplified-hunk-header-pattern
  #"^@@ .+ @@.*$")

(def ^:private search-replace-hunk-header-pattern
  #"^<<<<<<< SEARCH$")

(defn- parse-simplified-file-diff
  "Parse a single file section from a simplified diff patch.
   Returns {:target-path <path> :is-new? <bool> :hunks <hunks>}
   or error info map."
  [file-section]
  (t/trace!
   {:id :dado.patch/parse-file-diff}
   (let [all-lines      (str/split-lines file-section)
         [header lines] [(str/join "\n" (take 2 all-lines)) (drop 2 all-lines)]]
     (if-let [[_ target-path] (re-matches file-header-pattern header)]
       (let [target-path (str/trim target-path)
             is-new?     (str/starts-with? header "--- /dev/null")
             hunks       (loop [remaining-lines lines
                                current-hunk    []
                                hunks           []]
                           (cond
                             (empty? remaining-lines)
                             (if (empty? current-hunk)
                               hunks
                               (conj hunks current-hunk))

                             (re-matches
                              simplified-hunk-header-pattern
                              (first remaining-lines))
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
       {:error   :error/invalid-patch-file-header
        :context {:hunks  file-section
                  :header header}}))))

(defn- parse-hunks [lines]
  (loop [remaining-lines lines
         current-hunk    []
         hunks           []]
    (cond
      (empty? remaining-lines)
      (if (empty? current-hunk)
        hunks
        (conj hunks current-hunk))

      (re-matches
       search-replace-hunk-header-pattern
       (first remaining-lines))
      (recur (rest remaining-lines)
             [(first remaining-lines)]
             (if (empty? current-hunk)
               hunks
               (conj hunks current-hunk)))

      :else
      (recur (rest remaining-lines)
             (conj current-hunk (first remaining-lines))
             hunks))))

(defn- parse-search-replace-file-diff
  "Parse a single file section from a search-replace diff patch.
   Returns a map with operation details or error info map."
  [file-section]
  (t/trace!
   {:id :dado.patch/parse-file-diff}
   (let [all-lines      (str/split-lines file-section)
         [header lines] [(first all-lines) (rest all-lines)]]
     (if-let [[_ verb path-1 path-2] (re-matches
                                      search-replace-file-header-pattern
                                      header)]
       (let [path-2 (when path-2 (str/trim path-2))
             path-1 (when path-1 (str/trim path-1))
             op     (keyword (str/lower-case verb))]
         (case op
           :edit   {:op          :edit
                    :target-path path-1
                    :hunks       (parse-hunks lines)}
           :create {:op          :create
                    :target-path path-1
                    :hunks       (parse-hunks lines)}
           :delete {:op          :delete
                    :target-path path-1}
           :move   (if (str/blank? path-2)
                     {:error   :error/invalid-patch-file-header
                      :context {:section file-section
                                :header  header}}
                     {:op          :move
                      :source-path path-1
                      :target-path path-2})
           :copy   (if (str/blank? path-2)
                     {:error   :error/invalid-patch-file-header
                      :context {:section file-section
                                :header  header}}
                     {:op          :copy
                      :source-path path-1
                      :target-path path-2})))
       {:error   :error/invalid-patch-file-header
        :context {:section file-section
                  :header  header}}))))

(defn- context-line? [line]
  (str/starts-with? line " "))

(defn- addition-line? [line]
  (str/starts-with? line "+"))

(defn- deletion-line? [line]
  (str/starts-with? line "-"))

(defn- apply-simplified-hunk
  "Apply a single hunk to content, returns [new-content error-info]"
  [content hunk]
  (let [[_header & diff-lines] hunk
        diff-lines             (mapv
                                ;; fix an issue with blank lines not being
                                ;; marked as context
                                #(if (= "" %) " " %)
                                diff-lines)
        context                (->> diff-lines
                                    (filterv
                                     (some-fn context-line? deletion-line?))
                                    (mapv #(subs % 1))
                                    (str/join "\n"))
        n-context              (count context)
        index                  (str/index-of content context)
        post-str               (when index (subs content (+ index n-context)))]
    (cond
      (nil? index)
      (do
        (t/event! :error/patch-failed
                  {:level :warn
                   :data  {:context context
                           :content content}})
        [content {:error   :error/patch-context-mismatch
                  :context {:hunk    hunk
                            :context context
                            :content content}}])

      (and (seq context) (str/index-of post-str context))
      (do
        (t/event! :error/patch-failed
                  {:level :warn
                   :data  {:context context
                           :content content}})
        [content {:error   :error/patch-insufficient-context
                  :context {:hunk    hunk
                            :context context
                            :content content}}])

      :else
      (let [pre-str       (subs content 0 index)
            context-lines (str/split-lines context)
            no-nl?        (= (last diff-lines)
                             "\\ No newline at end of file")
            diff-lines    (cond-> diff-lines no-nl? (butlast))]
        (loop [diff-lines    diff-lines
               context-lines context-lines
               new-lines     []]
          (if (seq diff-lines)
            (if (or (seq context-lines) (addition-line? (first diff-lines)))
              (cond
                (context-line? (first diff-lines))
                (if (= (first context-lines) (subs (first diff-lines) 1))
                  (recur
                   (rest diff-lines)
                   (rest context-lines)
                   (conj new-lines (first context-lines)))
                  (recur
                   diff-lines
                   (rest context-lines)
                   (conj new-lines (first context-lines))))

                (deletion-line? (first diff-lines))
                (if (= (first context-lines) (subs (first diff-lines) 1))
                  (recur
                   (rest diff-lines)
                   (rest context-lines)
                   new-lines)
                  [content {:error   :context-mismatch
                            :context {:hunk hunk
                                      :line (first context-lines)
                                      :edit (first diff-lines)}}])

                (addition-line? (first diff-lines))
                (recur
                 (rest diff-lines)
                 context-lines
                 (conj new-lines (subs (first diff-lines) 1))))
              (do
                (t/event!
                 :event/failed
                 {:level :warn
                  :data  {:hunk hunk :content content :context context}})
                [content {:error   :context-mismatch
                          :context {:hunk           hunk
                                    :line           "--- End of File ---"
                                    :edit           (first diff-lines)
                                    :remaining-diff diff-lines}}]))
            [(str
              pre-str
              (str/join "\n" new-lines)
              post-str
              (when (and (not no-nl?) (empty? post-str))
                "\n"))
             nil]))))))

(defn- apply-simplified-hunks
  "Apply all hunks to content, returns [new-content errors]"
  [content hunks]
  (loop [current-content content
         remaining-hunks hunks
         errors          []]
    (if (empty? remaining-hunks)
      [current-content errors]
      (let [[new-content error] (apply-simplified-hunk current-content (first remaining-hunks))]
        (recur new-content
               (rest remaining-hunks)
               (if error
                 (conj errors error)
                 errors))))))

(defn- count-simplified-changes
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

(defn- apply-simplified-file-changes
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
       (let [current-content       (if is-new? "" (slurp target-path))
             [_new-content errors] (apply-simplified-hunks current-content hunks)]
         (if (seq errors)
           [nil {:errors errors
                 :path   target-path}]
           [(count-simplified-changes hunks) nil])))

     (catch Exception e
       [nil {:error :file-access
             :path  target-path
             :cause e}]))))

(defn- split-search-replace-hunk
  [hunk]
  (let [sections (->> hunk
                      (reduce
                       (fn [acc item]
                         (case item
                           "<<<<<<< SEARCH"  (assoc acc :current-section [])
                           "======="         (-> acc
                                                 (update :result conj (:current-section acc))
                                                 (assoc :current-section []))
                           ">>>>>>> REPLACE" (update acc :result conj (:current-section acc))
                           (update acc :current-section conj item)))
                       {:result          []
                        :current-section []})
                      :result)]
    (have #(= 2 (count %)) sections)
    (mapv #(str/join "\n" %) sections)))

(defn- apply-search-replace-hunk
  "Apply a single hunk to content, returns [new-content error-info]"
  [content hunk]
  (let [[_header & hunk-lines] hunk
        [search replace]       (split-search-replace-hunk hunk)
        n-context              (count search)
        index                  (str/index-of content search)
        post-str               (when index (subs content (+ index n-context)))]
    (cond
      (nil? index)
      (do
        (t/event! :error/patch-failed
                  {:level :warn
                   :data  {:context search
                           :content content}})
        [content {:error   :error/patch-context-mismatch
                  :context {:hunk    hunk
                            :context search
                            :content content}}])

      (and (seq search) (str/index-of post-str search))
      (do
        (t/event! :error/patch-failed
                  {:level :warn
                   :data  {:context search
                           :content content}})
        [content {:error   :error/patch-insufficient-context
                  :context {:hunk    hunk
                            :context search
                            :content content}}])

      :else
      [(str (->> [(subs content 0 index) replace post-str]
                 (filterv (complement str/blank?))
                 (apply str))
            (when (str/blank? post-str) "\n"))
       nil])))

(defn- apply-search-replace-hunks
  "Apply all hunks to content, returns [new-content errors]"
  [content hunks]
  (loop [current-content content
         remaining-hunks hunks
         errors          []]
    (if (empty? remaining-hunks)
      [current-content errors]
      (let [[new-content error] (apply-search-replace-hunk
                                 current-content
                                 (first remaining-hunks))]
        (recur new-content
               (rest remaining-hunks)
               (if error
                 (conj errors error)
                 errors))))))

(defn- apply-create
  "Apply all hunks to content, returns [new-content errors]"
  [hunks]
  (prn :apply-create :hunks hunks)
  [(str (str/join "\n" (first hunks)) "\n") nil])

(defn- apply-search-replace-file-changes
  "Apply changes to a single file operation, returns [stats error-info]"
  [op-info]
  (t/trace!
   {:id :dado.patch/apply-file :data {:op-info op-info}}
   (try
     (let [{:keys [op source-path target-path hunks]} op-info]
       (case op
         :edit
         (cond
           (not (fs/exists? target-path))
           [nil {:error   :file-not-found
                 :context {:path target-path
                           :op   :edit}}]

           :else
           (let [current-content      (slurp target-path)
                 [new-content errors] (apply-search-replace-hunks current-content hunks)]
             (if (seq errors)
               [nil {:errors errors :path target-path}]
               [{:op op :paths [target-path]} nil])))

         :create
         (let [[new-content errors] (apply-create hunks)]
           (if (seq errors)
             [nil {:errors errors :path target-path}]
             [{:op op :paths [target-path]} nil]))

         :delete
         (if (not (fs/exists? target-path))
           [nil {:error :file-not-found :path target-path}]
           [{:op op :paths [target-path]} nil])

         (:move :copy)
         (cond
           (not (fs/exists? source-path))
           [nil {:error   :file-not-found
                 :context {:path source-path
                           :op   op}}]

           (fs/exists? target-path)
           [nil {:error   :file-exists
                 :context {:path target-path
                           :op   op}}]

           :else
           [{:op op :paths [source-path target-path]} nil])))

     (catch Exception e
       [nil {:error :file-access :cause e}]))))

(defn- write-changes!
  "Write changes to filesystem, returns true on success"
  [op-info new-content]
  (t/trace!
   {:id   :dado.patch/write-file
    :data {:op op-info :content new-content}}
   (try
     (let [{:keys [op target-path source-path]} op-info]
       (case op
         (:edit :create)
         (do
           (when (= op :create)
             (fs/create-dirs (fs/parent target-path)))
           (let [temp-path (str target-path ".tmp")]
             (spit temp-path new-content)
             (fs/move temp-path target-path {:replace-existing true}))
           (t/event! (if (= op :create)
                       :patch/file-created
                       :patch/applied)
                     {:level :debug :path target-path}))

         :delete
         (do
           (fs/delete target-path)
           (t/event! :patch/file-deleted
                     {:level :debug :path target-path}))

         :move
         (do
           (fs/create-dirs (fs/parent target-path))
           (fs/move source-path target-path)
           (t/event! :patch/file-moved
                     {:level  :debug
                      :source source-path
                      :target target-path}))

         :copy
         (do
           (fs/create-dirs (fs/parent target-path))
           (fs/copy source-path target-path)
           (t/event! :patch/file-copied
                     {:level  :debug
                      :source source-path
                      :target target-path})))
       true)

     (catch Exception e
       (throw (ex-info "Failed to write changes"
                       {:type    :error/file-access
                        :context {:component "dado.patch"
                                  :op        op-info}
                        :cause   e}))))))

(defn apply-simplified-diff-patch!
  "See dado.patch.interface/apply-simplified-diff-patch! for documentation"
  [patch-content]
  (t/trace!
   {:id :dado.patch/apply-patch}
   (let [file-sections (str/split patch-content #"(?m)^(?=---)")]
     ;; First pass - parse and validate all files
     (let [parsed-files (mapv parse-simplified-file-diff file-sections)
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
                                          (let [[stats error] (apply-simplified-file-changes file-info)]
                                            [(if stats
                                               (assoc results (:target-path file-info) stats)
                                               results)
                                             (if error
                                               (conj errors error)
                                               errors)]))
                                        [{} []]
                                        valid-files)]
           (if (seq errors)
             (do
               (t/event!
                :patch/failed
                {:level :warn :errors errors})
               (throw (ex-info "Patch application failed"
                               {:type    :error/patch-application
                                :context {:component "dado.patch"
                                          :errors    errors}})))

             ;; Final pass - write changes to filesystem
             (do
               (doseq [file-info valid-files]
                 (let [current-content (if (:is-new? file-info)
                                         ""
                                         (slurp (:target-path file-info)))
                       [new-content _] (apply-simplified-hunks current-content (:hunks file-info))]
                   (write-changes! file-info new-content)))
               results))))))))

(defn apply-search-replace-diff-patch!
  "See dado.patch.interface/apply-simplified-diff-patch! for documentation"
  [patch-content]
  (t/trace!
   {:id :dado.patch/apply-patch}
   (let [file-sections (str/split patch-content #"(?m)^(?=EDIT|CREATE|DELETE|MOVE|COPY)")]
     ;; First pass - parse and validate all operations
     (let [parsed-ops   (mapv parse-search-replace-file-diff file-sections)
           parse-errors (->> parsed-ops
                             (filter :error)
                             (mapv #(assoc % :type :error/patch-validation)))]
       (if (seq parse-errors)
         (throw (ex-info "Invalid patch format"
                         {:type    :error/patch-validation
                          :context {:component "dado.patch"
                                    :errors    parse-errors}}))

         ;; Second pass - validate all operations in memory
         (let [valid-ops        (remove :error parsed-ops)
               [results errors] (reduce
                                 (fn [[results errors] op-info]
                                   (let [[stats error]
                                         (apply-search-replace-file-changes op-info)]
                                     [(if stats
                                        (conj results stats)
                                        results)
                                      (if error
                                        (conj errors error)
                                        errors)]))
                                 [[] []]
                                 valid-ops)]
           (if (seq errors)
             (do
               (t/event!
                :patch/failed
                {:level :warn :errors errors})
               (throw (ex-info "Patch application failed"
                               {:type    :error/patch-application
                                :context {:component "dado.patch"
                                          :errors    errors}})))

             ;; Final pass - execute all operations
             (t/trace!
              {:id   :dado.patch/write-patched
               :data {:valid-ops valid-ops}}
              (do
                (doseq [op-info valid-ops]
                  (case (:op op-info)
                    (:edit)
                    (let [current-content (slurp (:target-path op-info))
                          [new-content _] (apply-search-replace-hunks
                                           current-content
                                           (:hunks op-info))]
                      (write-changes! op-info new-content))

                    (:create)
                    (let [[new-content _] (apply-create (:hunks op-info))]
                      (write-changes! op-info new-content))

                    (:delete :move :copy)
                    (write-changes! op-info nil)))
                results)))))))))
