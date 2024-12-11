(ns dado.ai.tools.matching-file.core
  "Core implementation of file matching tool."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [dado.ai.tools.matching-file.model :as model]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have have?]]))

(def ^:private default-max-matches 20)
(def ^:private default-context-lines 2)

(defn- normalize-pattern [mode case-sensitive? pattern]
  (if case-sensitive?
    pattern
    (if (= :exact mode)
      (str/lower-case pattern)
      (str "(?i)" pattern))))

(defn- make-matcher [mode pattern case-sensitive?]
  (let [pattern (normalize-pattern mode case-sensitive? pattern)]
    (case mode
      :regex (let [p (re-pattern pattern)]
               (fn [line]
                 (prn :line line :pattern pattern :match (re-find p line))
                 (re-find p line)))
      :exact (if case-sensitive?
               (fn literal-match [s]
                 (str/includes? s pattern))
               (fn case-insensitive-literal-match [s]
                 (str/includes? (str/lower-case s) pattern))))))

(defn- get-context-lines
  "Returns n lines before and after the current line number from the file."
  [lines line-num n]
  (let [start (max 0 (- line-num n))
        end   (min (count lines) (+ line-num n 1))]
    (map-indexed
     (fn [idx line]
       {:line-number (+ start idx)
        :line        line})
     (subvec lines start end))))

(defn- search-file
  "Search single file for matches.
  Returns match data if found, nil if no matches."
  [path matcher context-lines]
  (t/trace!
   {:id   :matching-file/search-file
    :data {:path path :matcher matcher}}
   (try
     (with-open [rdr (io/reader (fs/file path))]
       (let [lines (vec (line-seq rdr))]
         (when-let [matches
                    (seq
                     (keep-indexed
                      (fn [idx line]
                        (when (matcher line)
                          (cond-> {:line-number idx
                                   :line line}
                            context-lines
                            (assoc :context
                                   (get-context-lines
                                    lines
                                    idx
                                    context-lines)))))
                      lines))]
           {:path    path
            :matches matches})))
     (catch Exception e
       (t/log! :warn {:msg   "Error reading file"
                      :path  path
                      :error (ex-message e)})
       nil))))

(defn- find-files
  "Returns seq of all files in project directory."
  [extensions]
  (->> (fs/glob "." "**")
       (filter fs/regular-file?)
       (filter #(or (empty? extensions)
                    (some (fn [ext] (str/ends-with? (str %) ext))
                          extensions)))))

(defn- valid-regex? [pattern]
  (try
    (re-pattern pattern)
    true
    (catch Exception _
      false)))

(defn- validate-search-params [{:keys [pattern mode] :as params}]
  (when-let [explain-data (m/explain model/SearchParams params)]
    (throw (ex-info "Invalid search parameters"
                    {:type :error/tool-validation
                     :data (me/humanize explain-data)})))

  (when (= mode :regex)
    (when-not (valid-regex? pattern)
      (throw (ex-info "Invalid regex pattern"
                      {:type    :error/invalid-search-pattern
                       :pattern pattern}))))
  (when-not (#{:exact :regex} mode)
    (throw (ex-info "Invalid mode (must be :exact or :regex)"
                    {:type    :error/invalid-search-pattern
                     :pattern pattern}))))

(defn execute-tool!
  "Executes file matching search based on parameters.

   Required parameters:
   - pattern: String to search for

   Optional parameters:
   - mode: :exact or :regex (default :exact)
   - case-sensitive?: boolean (default false)
   - context-lines: Number of context lines (default 2)
   - max-matches: Maximum matches to return (default 20)
   - extensions: Vector of file extensions to search (default all)

   Returns map containing:
   - :matches - Vector of match results
   - :truncated? - Whether results were limited by max-matches"
  [{:keys [pattern
           mode
           case-sensitive?
           context-lines
           max-matches
           extensions]
    :as   params}]
  (let [{:keys [pattern
                mode
                case-sensitive?
                context-lines
                max-matches
                extensions]
         :as   params}
        (merge
         {:mode            :exact
          :case-sensitive? false
          :context-lines   default-context-lines
          :max-matches     default-max-matches
          :extensions      [".clj" ".cljc" ".cljs" ".edn" ".md" ".txt"]}
         params)]
    (t/trace!
     {:id           :matching-file/search
      :catch->error {:rethrow? true}
      :rethrow?     true
      :data         {:pattern    pattern
                     :mode       mode
                     :extensions extensions}}
     (try
       ;; Validate parameters
       (validate-search-params params)

       ;; Create matcher function
       (let [matcher  (make-matcher mode pattern case-sensitive?)
             files    (find-files extensions)
             searcher (fn [p] (search-file p matcher context-lines))
             results  (sequence (keep searcher) files)]
         {:matches    (vec (take max-matches results))
          :truncated? (boolean (seq (drop max-matches results)))})

       (catch Exception e
         (if (contains? #{:error/tool-validation
                          :error/invalid-search-pattern}
                        (:type (ex-data e)))
           (throw e)
           (throw (ex-info "File search failed"
                           {:type  :error/file-search
                            :cause e}))))))))

(def ^:private description
  "Searches project files for content matches and returns matching files.")

(defn create-agent
  create-agent
  "Creates a refactoring agent for code modifications."
  [project-config additional-context-fn]
  {:id           :dado/matching-file
   :name         "Matching File Tool"
   :description  description
   :structured-description
   {:claude
    {:description "Tool for finding files containing specific content. Supports exact and regex matching."}}
   :parameters
   [:map
    [:pattern :string]
    [:mode {:optional true} [:enum :exact :regex]]
    [:case-sensitive? {:optional true} :boolean]
    [:context-lines {:optional true} :int]
    [:max-matches {:optional true} :int]
    [:extensions {:optional true} [:vector :string]]]
   :returns
   {:type        :map
    :description "Map containing matched file paths and optional context"}
   :prompt-fn    (constantly "Use this tool to search for files containing specific content.")
   :recognize-fn #(boolean (re-find #"(?i)find files?|search.*files?" %))
   :execute-fn   execute-tool!})
