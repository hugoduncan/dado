(ns dado.actions.implement-namespace.core
  "Core implementation of namespace creation from ADR"
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [dado.actions.implement-namespace.model :as model]
   [dado.ai.agent.interface :as agent]
   [dado.ai.agents.refactoring.interface :as refactoring]
   [dado.ai.message.interface :as msg]
   [dado.ai.prompt.interface :as prompt]
   [dado.project-config.interface :as project-config]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(def options? (m/validator model/Options))
(def component-info? (m/validator model/ComponentInfo))
(def implementation-result? (m/validator model/ImplementationResult))

(def default-options
  {:mode :interactive
   :allow-overwrite false})

(defn- read-adr
  "Reads ADR file content. Returns string content or throws."
  [config adr-name]
  (let [adr-dir (project-config/get-directory config :dado/adr)
        adr-path (fs/path adr-dir adr-name)]
    (if (fs/exists? adr-path)
      (slurp adr-path)
      (throw (ex-info "ADR file not found"
                     {:type :error/implement-namespace
                      :adr adr-name})))))

(defn- validate-adr-completeness
  "Validates ADR has required sections. Returns validation result."
  [adr-content]
  (let [required-sections #{"## Status"
                           "## Context"
                           "## Decision"
                           "## Consequences"}
        found-sections (into #{}
                            (comp
                             (map str/trim)
                             (filter #(.startsWith % "##")))
                            (str/split-lines adr-content))
        missing (set/difference required-sections found-sections)]
    {:valid? (empty? missing)
     :errors (when (seq missing)
               [(str "ADR missing required sections: "
                     (str/join ", " missing))])}))

(defn- extract-component-info
  "Extracts component information from ADR content"
  [adr-content]
  (t/trace! {:id :implement/extract-info}
            (try
              ;; Implementation note: This is a stub - the actual logic would need to
              ;; parse the ADR to extract the component name, namespace, and dependencies
              nil
              (catch Exception e
                (throw (ex-info "Failed to extract component info from ADR"
                              {:type :error/implement-namespace
                               :cause e}))))))

(defn- interactive-implementation
  "Handles interactive implementation using AI agent"
  [project-config adr-name]
  (t/trace! {:id :implement/interactive}
            (try
              (let [requested-file-paths (atom [])
                    additonal-context
                    (fn  []
                      [(prompt/implementation-paths "ai-agent")
                       (prompt/implementation-paths "ai-refactoring-agent")
                       (prompt/interface-paths "repl-message-loop")
                       (prompt/implementation-paths "implement-namespace")
                       ["deps.edn"]
                       @requested-file-paths])
                    refactoring-agent
                    (refactoring/create-agent
                     (project-config/load-config)
                     #'additonal-context)]
                (agent/message-loop
                 (project-config/load-config)
                 refactoring-agent
                 (msg/create-message-thread "claude-3-5-sonnet-20241022")))
              nil
              (catch Exception e
                (throw (ex-info "Interactive implementation failed"
                                {:type  :error/implement-namespace
                                 :cause e}))))))

(defn- non-interactive-implementation
  "Handles non-interactive implementation from ADR spec"
  [config adr-content component-info]
  (t/trace! {:id :implement/non-interactive}
            (try
              ;; Implementation note: This would directly implement from ADR spec
              nil
              (catch Exception e
                (throw (ex-info "Non-interactive implementation failed"
                              {:type :error/implement-namespace
                               :cause e}))))))

(defn execute
  "Implements namespace from ADR specification"
  [config adr-name options]
  {:pre  [(have? options? (merge default-options options)
                 :data (me/humanize (m/explain model/Options options)))]
   :post [(have? implementation-result? %
                 :data (me/humanize (m/explain model/ImplementationResult %)))]}
  (t/trace!
   {:id :implement/started}
   (let [options (merge default-options options)]
     (if (= :interactive (:mode options))
       (interactive-implementation config adr-name)
       (non-interactive-implementation config adr-name)))))
