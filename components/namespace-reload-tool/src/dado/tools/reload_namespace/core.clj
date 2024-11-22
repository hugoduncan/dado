(ns dado.tools.reload-namespace.core
  "Core implementation of namespace reload tool"
  (:require
   [clojure.string :as str]
   [jsonista.core :as j]
   [taoensso.telemere :as t]
   [malli.core :as m]))

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

(defn- reload-namespace
  "Attempts to reload a single namespace.
   Returns [true nil] on success,
   [false error-info] on failure."
  [ns-sym-str]
  (t/trace!
   {:id :reload/attempt :data {:ns-sym ns-sym-str}}
   (try
     (require (symbol ns-sym-str) :reload)
     (t/event! :reload/success {:data {:ns ns-sym-str}})
     [true nil]
     (catch Exception e
       (t/event! :reload/failed {:data {:ns ns-sym-str :error (ex-message e)}})
       [false {:ns ns-sym-str :error (ex-message e)}]))))

(defn reload-namespaces
  "Reloads specified namespaces.
   Returns map of results with :reloaded and :errors keys."
  [{:keys [namespaces] :as parameters}]
  (t/trace!
   {:id :reload/started :data {:parameters parameters}}
   (let [ns-syms namespaces]
     (loop [remaining ns-syms
            reloaded  []
            errors    []]
       (if (seq remaining)
         (let [[success? error] (reload-namespace (first remaining))]
           (recur (rest remaining)
                  (cond-> reloaded success? (conj (first remaining)))
                  (cond-> errors (not success?) (conj error))))
         {:reloaded reloaded
          :errors   errors})))))

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
   :execute-fn   reload-namespaces})
