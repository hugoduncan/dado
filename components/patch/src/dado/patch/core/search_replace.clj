(ns dado.patch.core.search-replace
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have]]
            [dado.patch.core.common :as common]))

(def ^:private search-replace-file-header-pattern
  #"^(EDIT|CREATE|DELETE|MOVE|COPY)\s([^\s\n]+)(?:\s+([^\s\n]+))?$")

(def ^:private search-replace-hunk-header-pattern
  #"^<<<<<<< SEARCH$")

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
                      (common/write-changes! op-info new-content))

                    (:create)
                    (let [[new-content _] (apply-create (:hunks op-info))]
                      (common/write-changes! op-info new-content))

                    (:delete :move :copy)
                    (common/write-changes! op-info nil)))
                results)))))))))
