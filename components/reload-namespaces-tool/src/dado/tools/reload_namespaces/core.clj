(ns dado.tools.reload-namespaces.core
  "Core implementation of namespace reload tool"
  (:require
   [clojure.string :as str]
   [clojure.stacktrace :as stacktrace]
   [dado.ai.message.interface :as message]
   [jsonista.core :as j]
   [taoensso.telemere :as t]))

(defn- parse-namespaces
  "Parse namespace symbols from Updated Namespaces List format string.
   Returns sequence of namespace symbols."
  [input]
  (t/trace!
   {:id :reloac/parse-ns :data {:input input}}
   (j/read-value input)
   #_(->> (str/split-lines input)
          (remove str/blank?)
          (map symbol))))

(defn- exception->map
  "Converts exception to a map with complete details."
  [^Exception e]
  (let [root-ex (if-let [cause (.getCause e)] cause e)]
    {:message     (ex-message root-ex)
     :data        (ex-data root-ex)
     :stacktrace  (with-out-str (stacktrace/print-stack-trace root-ex))}))

(defn- capture-output
  "Executes a function, capturing stdout and stderr.
   Returns [result stdout stderr]."
  [f]
  (let [stdout (java.io.StringWriter.)
        stderr (java.io.StringWriter.)]
    (binding [*out* stdout
              *err* stderr]
      [(f) (str stdout) (str stderr)])))

(defn- reload-namespace
  "Attempts to reload a single namespace.
   Returns [true nil output] on success,
   [false error-info output] on failure.
   Output is a map containing :stdout and :stderr strings."
  [ns-sym]
  (t/trace!
   {:id :reload/attempt :data {:ns-sym ns-sym}}
   (let [[result stdout stderr]
         (capture-output
          #(try
             (require ns-sym :reload)
             (t/event! :reload/success {:data {:ns ns-sym}})
             [true nil]
             (catch Exception e
               (t/event! :reload/failed {:data {:ns ns-sym :error (ex-message e)}})
               [false (merge {:ns ns-sym}
                           (exception->map e))])))]
     (conj result {:stdout stdout :stderr stderr}))))

(defn reload-namespaces
  "Reloads specified namespaces.
   Returns map of results with :reloaded and :errors keys."
  [{:keys [namespaces] :as parameters}]
  (t/trace!
   {:id :reload/started :data {:parameters parameters}}
   (let [[ ns-syms errors] (if (string? namespaces)
                             (try
                               [(j/read-value namespaces)]
                               (catch Exception e
                                 [nil [{:ns      "unknown"
                                        :message (ex-message e)}]]))
                             [namespaces])]
     (loop [remaining ns-syms
            reloaded  []
            errors    []]
       (if (seq remaining)
         (let [ns-sym           (symbol (first remaining))
               [success? error] (reload-namespace ns-sym)]
           (recur (rest remaining)
                  (cond-> reloaded success? (conj ns-sym))
                  (cond-> errors (not success?) (conj error))))
         {:reloaded reloaded
          :errors   errors})))))

(defn- format-error [error]
  (let [{:keys [ns message stacktrace data]} error]
    (str ns " failed to reload: " message
         (when stacktrace (str "\nStacktrace:\n" stacktrace))
         (when data (str "\nException data:\n" (pr-str data))))))

(defn- format-output [outputs ns-sym]
  (when-let [{:keys [stdout stderr]} (get outputs ns-sym)]
    (str (when (seq stdout) (str "\nStandard output:\n" stdout))
         (when (seq stderr) (str "\nStandard error:\n" stderr)))))

(defn- result-content
  [{:keys [reloaded errors outputs] :as result-map}]
  {:content  (cond-> [{:text (str "Reloaded: " (str/join ", " reloaded))}]
               (seq errors) (conj (message/text-content
                                   (str/join "\n\n" (mapv format-error errors))))
               (seq outputs) (into (for [ns-sym (into reloaded (map :ns errors))
                                       :let [output (format-output outputs ns-sym)]
                                       :when (seq output)]
                                   (message/text-content output))))
   :is-error (boolean (seq errors))})

(def description
  "This tool reloads a list of clojure namespaces.

  Use this to reload namespaces so the user has the latest code installed.

  The `namespaces` parameter is a list of namespaces in dependency order.  If a
  namespace `a` depends on namespace `b`, then `b` should be listed before `a`.

  If the tool fails to reload a namespace, the failed namespace will be returned
  in a list passsed on the `:failed-namespaces` key.
")

(def prompt-template
  "Tool for reloading Clojure namespaces.

   Input should be in Updated Namespaces List format:
   ```updated-namespaces
   my.project.utils
   my.project.core
   ```

   Example usage:
   ```
   Please reload these namespaces:
   ```updated-namespaces
   my.project.model
   my.project.core
   ```
   ```

   Note:
   - No validation is performed on namespace names
   - Dependencies are not automatically handled
   - Namespaces are reloaded in the order specified")

(defn make-prompt
  "Returns prompt string for tool usage"
  []
  prompt-template)

(defn recognize-reload-request?
  "Returns true if text appears to be a namespace reload request"
  [text]
  (and (string? text)
       (or (re-find #"(?i)reload.*namespaces?" text)
           (re-find #"```updated-namespaces\n" text))))

(defn create-tool
  "Creates namespace reload tool configuration"
  []
  {:id           :tool/reload-namespace
   :name         "Namespace Reload Tool"
   :description  description
   :parameters   [:map
                  [:namespaces
                   {:description "A list of namespaces to reload"}
                   [:vector :string]]]
   :returns      {:type        :map
                  :description "Results map with :reloaded and :errors keys"}
   :prompt-fn    make-prompt
   :recognize-fn recognize-reload-request?
   :execute-fn   (comp result-content reload-namespaces)})
