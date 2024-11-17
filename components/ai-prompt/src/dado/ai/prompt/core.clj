(ns dado.ai.prompt.core
  "Core implementation of the AI Prompt component."
  (:require
   [clojure.java.io :as io]
   [selmer.parser :as selmer]
   [taoensso.telemere :as t]))

(def ^:private prompt-dir "dev/ai/prompts/")

(defn- read-template
  [template-name]
  (let [template-path (io/file prompt-dir (str template-name ".md"))]
    (t/trace!
      {:id ::read-template :data {:template template-name}}
      (if (.exists template-path)
        (slurp template-path)
        (throw (ex-info "Missing template file" {:type :error/missing-template
                                                 :template template-name}))))))

(defn- parse-template
  [template-name template-content]
  (t/trace!
    {:id ::parse-template :data {:template template-name}}
    (selmer/parse template-content)))

(defn- compose-templates
  [parsed-templates]
  (t/trace!
    {:id ::compose-templates}
    (apply str parsed-templates)))

(defn- substitute-data
  [composed-template data]
  (t/trace!
    {:id ::substitute-data :data {:keys (keys data)}}
    (selmer/render composed-template data)))

(defn construct-prompt
  [template-names data]
  (t/trace!
    {:id ::construct-prompt :data {:templates template-names :keys (keys data)}}
    (let [templates (map (fn [template-name]
                           (-> template-name
                               read-template
                               (parse-template template-name)))
                         template-names)
          composed (compose-templates templates)
          substituted (substitute-data composed data)]
      (if-not (re-find #"\{\{.+?\}\}" substituted)
        substituted
        (throw (ex-info "Missing data for prompt substitution"
                        {:type :error/missing-data
                         :data data
                         :unresolved-template substituted}))))))
