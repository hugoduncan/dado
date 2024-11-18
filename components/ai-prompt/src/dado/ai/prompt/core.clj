(ns dado.ai.prompt.core
  "Core implementation of the AI Prompt component."
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [selmer.parser :as selmer]
   [selmer.util :as selmer-util]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have]]))

(defn missing-value-fn [tag context-map]
  (throw
   (ex-info
    "Missing data for prompt substitution"
    {:tag tag :context-map context-map})))

(selmer-util/set-missing-value-formatter! missing-value-fn)

(defn- project-prompt-dir [project-config]
  (fs/path (have (:dev-dir project-config)) "ai" "prompts"))

(defn- resource-template-path [template-name]
  (str "dado/ai/prompts/" template-name ".md"))

(defn- read-template
  "Reads template first from project directory, falling back to resources.
  Throws ex-info with :error/template-not-found if template not found
  in either location."
  [project-config template-name]
  (let [project-path  (fs/path
                       (project-prompt-dir project-config)
                       (str template-name ".md"))
        resource-path (resource-template-path template-name)]
    (t/trace!
     {:id   ::template-search-project
      :data {:template      template-name
             :project-path  project-path
             :resource-path resource-path}}
     (or
      (when(fs/exists? project-path)
        (t/event! :template-load-from-project)
        (slurp (fs/file project-path)))

      ;; Try resource path
      (when-let [resource (io/resource resource-path)]
        (t/event! :template-load-from-resources)
        (slurp resource))

      (do
        (t/event! :template-not-found)
        (throw (ex-info
                "Template not found in project or resources"
                {:type    :error/template-not-found
                 :context {:template      template-name
                           :project-path  project-path
                           :resource-path resource-path}})))))))

(defn- compose-templates
  [templates]
  (t/trace!
   {:id ::compose-templates}
   (str/join "\n" templates)))

(defn- substitute-data
  [composed-template data]
  (t/trace!
   {:id ::substitute-data :data {:keys (keys data)}}
   (selmer/render composed-template data)))

(defn construct-prompt
  [project-config template-names data]
  (t/trace!
   {:id ::construct-prompt :data {:templates template-names :keys (keys data)}}
   (let [templates   (mapv #(read-template project-config %) template-names)
         composed    (compose-templates templates)
         substituted (substitute-data composed data)]
     (if-not (re-find #"\{\{.+?\}\}" substituted)
       substituted
       (throw (ex-info "Missing data for prompt substitution"
               {:type :error/missing-data
                :data data
                :unresolved-template substituted}))))))
