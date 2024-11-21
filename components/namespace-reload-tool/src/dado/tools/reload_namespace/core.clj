(ns dado.tools.reload-namespace.core
  "Core implementation of namespace reload tool"
  (:require [clojure.string :as str]
            [taoensso.telemere :as t]))

(defn- parse-namespaces
  "Parse namespace symbols from Updated Namespaces List format string.
   Returns sequence of namespace symbols."
  [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map symbol)))

(defn- reload-namespace
  "Attempts to reload a single namespace.
   Returns [true nil] on success,
   [false error-info] on failure."
  [ns-sym]
  (try
    (require ns-sym :reload)
    (t/event! :reload/success {:ns ns-sym})
    [true nil]
    (catch Exception e
      (t/event! :reload/failed {:ns ns-sym :error (ex-message e)})
      [false {:ns ns-sym :error (ex-message e)}])))

(defn reload-namespaces
  "Reloads specified namespaces.
   Returns map of results with :reloaded and :errors keys."
  [namespaces-str]
  (t/trace! {:id :reload/started}
            (let [ns-syms (parse-namespaces namespaces-str)]
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
   :description  "Reloads specified Clojure namespaces"
   :structured-description
   {:claude
    {:description "Tool for reloading Clojure namespaces"}}
   :parameters
   [{:name        "namespaces"
     :type        :string
     :description "Namespaces to reload, in Updated Namespaces List format"
     :required?   true}]
   :returns
   {:type        :map
    :description "Results map with :reloaded and :errors keys"}
   :prompt-fn    make-prompt
   :recognize-fn recognize-reload-request?
   :execute-fn   reload-namespaces})
