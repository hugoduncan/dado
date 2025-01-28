(ns dado.ai.tools.git.core
  "Core implementation of git tools."
  (:require
   [babashka.process :as p]
   [clojure.string :as str]
   [dado.ai.tool.model :as tool.model]
   [dado.ai.tools.git.model :as model]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(def ^:dynamic *repo-dir* (System/getProperty "user.dir"))

(defn- git-command
  "Execute git command and return string output.
   Throws ex-info with :error/git-operation on failure."
  [args]
  (try
    (let [result    @(p/process
                      (into ["git"] args)
                      {:out :string
                       :dir *repo-dir*})
          exit-code (:exit result)]
      (if (zero? exit-code)
        (:out result)
        (throw (ex-info "Git command failed"
                        {:type    :error/git-operation
                         :command (str/join " " args)
                         :error   (slurp  (:err result))}))))
    (catch Exception e
      (throw (ex-info "Git command failed"
                      {:type    :error/git-operation
                       :command (str/join " " args)}
                      e)))))

(defn- invalid-git-repo?
  "Check if current directory is in a git repository."
  []
  (try
    (git-command ["rev-parse" "--git-dir"])
    false
    (catch Exception e
      e)))

(defn- validate-repo!
  "Validate we're in a git repository.
   Throws ex-info with :error/not-git-repo if not."
  []
  (when-let [e (invalid-git-repo?)]
    (throw (ex-info "Not in a git repository" {:type :error/not-git-repo} e))))

(defn- parse-status-output
  "Parse git status porcelain output into structured format."
  [status-out]
  (let [lines      (remove str/blank? (str/split-lines status-out))
        categorize (fn [line]
                     (have? (complement str/blank?)
                            line :data {:status-out status-out})
                     (case (subs line 0 2)
                       " M" [:modified (subs line 3)]
                       "M " [:staged (subs line 3)]
                       "??" [:untracked (subs line 3)]
                       nil))
        categories (group-by first (keep categorize lines))]
    {:modified  (mapv second (get categories :modified []))
     :staged    (mapv second (get categories :staged []))
     :untracked (mapv second (get categories :untracked []))}))

(defn git-status!
  "Get repository status.
  Returns ExecutionResult containing a status map conforming to
  StatusResponse schema."
  [_params]
  (t/trace!
   {:id :git/status-checked}
   (do
     (validate-repo!)
     (let [status-out       (git-command ["status" "--porcelain"])
           {:keys [modified staged untracked]}
           (parse-status-output status-out)
           branch           (str/trim
                             (git-command
                              ["rev-parse" "--abbrev-ref" "HEAD"]))
           ahead-behind     (str/split
                             (try
                               (git-command
                                ["rev-list"
                                 "--left-right"
                                 "--count"
                                 "HEAD...@{u}"])
                               (catch clojure.lang.ExceptionInfo _
                                 "0\t0"))
                             #"\t")
           [ahead behind]   (mapv parse-long ahead-behind)
           result           {:branch    branch
                             :modified  modified
                             :staged    staged
                             :untracked untracked
                             :ahead     ahead
                             :behind    behind}
           execution-result {:content (j/write-value-as-string result)}]
       (have? tool.model/execution-result? execution-result
              :data (fn [x]
                      (me/humanize (m/explain tool.model/ExecutionResult x))))
       execution-result))))

(defn git-stage!
  "Stage files in git repository.
  Returns ExecutionResult containing a stage result map conforming to
  StageResponse schema."
  [{:keys [paths force]}
   & {:keys [no-validate?]}]
  (t/trace!
   {:id :git/stage-started}
   (do
     (validate-repo!)
     (when-not no-validate?
       (have? model/stage-parameters? {:paths paths :force force}
              :data (fn [x] (me/humanize (m/explain model/StageParameters x)))))
     (let [paths-to-stage   (or paths
                                (let [{:keys [modified untracked]}
                                      (git-status! {})]
                                  (cond-> modified
                                    force (into untracked))))
           staging-result   {:staged []
                             :errors []}
           staged           (reduce (fn [acc path]
                                      (try
                                        (git-command ["add" path])
                                        (update acc :staged conj path)
                                        (catch Exception e
                                          (update acc :errors conj
                                                  {:path   path
                                                   :reason (-> e ex-data :error)}))))
                                    staging-result
                                    paths-to-stage)
           execution-result {:content (j/write-value-as-string staged)}]
       (have? tool.model/execution-result? execution-result
              :data (fn [x]
                      (me/humanize (m/explain tool.model/ExecutionResult x))))
       (t/trace!
        {:id :git/stage-completed :data staged}
        execution-result)))))

(defn git-commit!
  "Commit staged changes.
  Returns ExecutionResult containing a commit result map conforming to
  CommitResponse schema."
  [{:keys [message allow-empty]}
   & {:keys [no-validate?]}]
  (t/trace!
   {:id :git/commit-started}
   (do
     (validate-repo!)
     (when-not no-validate?
       (have? model/commit-parameters?
              {:message message :allow-empty allow-empty}
              :data (fn [x]
                      (me/humanize (m/explain model/CommitParameters x)))))
     (let [args                         (cond-> ["commit" "-m" message]
                                          allow-empty (conj "--allow-empty"))
           output                       (git-command args)
           hash                         (str/trim
                                         (git-command ["rev-parse" "HEAD"]))
           stat-out                     (git-command
                                         ["diff" "--stat" "HEAD^" "HEAD"])
           [files insertions deletions] (if (str/blank? stat-out)
                                          [0 0 0]
                                          (let [stats (last
                                                       (str/split-lines stat-out))
                                                nums  (re-seq #"\d+" stats)]
                                            (mapv parse-long nums)))]
       (let [result           {:commit
                               {:hash    hash
                                :message message
                                :summary {:files      files
                                          :insertions insertions
                                          :deletions  deletions}}}
             execution-result {:content (j/write-value-as-string result)}]
         (have? tool.model/execution-result? execution-result
                :data (fn [x]
                        (me/humanize (m/explain tool.model/ExecutionResult x))))
         (t/trace!
          {:id :git/commit-completed}
          execution-result))))))



(def ^:private git-tools
  [{:id           :dado/git-stage
    :name         "Git Stage Tool"
    :description  "Stage modified files in git repository"
    :structured-description
    {:claude
     {:description "Tool for staging files in git repository."}}
    :prompt-fn    (constantly "")
    :recognize-fn (constantly nil)
    :parameters   model/StageParameters
    :returns      {:type :map
                   :description
                   "ExecutionResult containing map of staged files and errors"}
    :execute-fn   git-stage!}

   {:id           :dado/git-commit
    :name         "Git Commit Tool"
    :description  "Commit staged changes"
    :structured-description
    {:claude
     {:description "Tool for committing staged changes."}}
    :prompt-fn    (constantly "")
    :recognize-fn (constantly nil)
    :parameters   model/CommitParameters
    :returns      {:type :map
                   :description
                   "ExecutionResult containing map of commit result"}
    :execute-fn   git-commit!}

   {:id           :dado/git-status
    :name         "Git Status Tool"
    :description  "Get repository status"
    :structured-description
    {:claude
     {:description "Tool for getting git repository status."}}
    :prompt-fn    (constantly "")
    :recognize-fn (constantly nil)
    :parameters   model/StatusParameters
    :returns      {:type :map
                   :description
                   "ExecutionResult containing map of modified, staged, and untracked files"}
    :execute-fn   git-status!}])

(defn create-tool
  "Create git tool for the specified tool ID.
   Returns tool definition map or nil if ID not found."
  [tool-id]
  (t/trace! {:id :git/tool-created :data {:tool-id tool-id}}
            (first (filter #(= tool-id (:id %)) git-tools))))
