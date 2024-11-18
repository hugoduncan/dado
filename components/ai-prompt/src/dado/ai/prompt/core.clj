(ns dado.ai.prompt.core
  "Core implementation of the AI Prompt component."
  (:require
   [babashka.fs :as fs]
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

(defn- prompt-dir [project-config]
  (fs/path (have (:dev-dir project-config)) "ai" "prompts"))

(defn- read-template
  [prompt-dir template-name]
  (let [template-path (fs/path prompt-dir (str template-name ".md"))]
    (t/trace!
     {:id ::read-template :data {:template template-name}}
     (if (fs/exists? template-path)
       (slurp (fs/file template-path))
       (throw (ex-info
               "Missing template file"
               {:type    :error/missing-template
                :context {:template   template-name
                          :prompt-dir prompt-dir}}))))))

(defn- compose-templates
  [parsed-templates]
  (t/trace!
   {:id ::compose-templates}
   (str/join "\n" parsed-templates)))

(defn- substitute-data
  [composed-template data]
  (t/trace!
    {:id ::substitute-data :data {:keys (keys data)}}
    (selmer/render composed-template data)))

(defn construct-prompt
  [project-config template-names data]
  (t/trace!
   {:id ::construct-prompt :data {:templates template-names :keys (keys data)}}
   (let [templates   (map (fn [template-name]
                            (-> (read-template
                                 (prompt-dir project-config)
                                 template-name)))
                          template-names)
         composed    (compose-templates templates)
         substituted (substitute-data composed data)]
     (if-not (re-find #"\{\{.+?\}\}" substituted)
       substituted
       (throw (ex-info "Missing data for prompt substitution"
                       {:type                :error/missing-data
                        :data                data
                        :unresolved-template substituted}))))))
