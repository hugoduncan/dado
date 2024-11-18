(ns dado.ai.prompt.interface
  "Public interface for the AI Prompt component."
  (:require
   [dado.ai.prompt.core :as core]))

(defn construct-prompt
  "Constructs an AI prompt string from the given template names and data.
   Templates will be composed in the specified order.

   project-config - the project configuration map, containing :dev-dir entry
   template-names - vector of template names to compose
   data           - map of data for template substitution

   Returns the constructed prompt string.
   Throws exceptions for missing templates or data."
  [project-config template-names data]
  (core/construct-prompt project-config template-names data))
