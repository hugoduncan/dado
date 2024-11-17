(ns dado.ai.prompt.interface
  "Public interface for the AI Prompt component."
  (:require [dado.ai.prompt.core :as core]))

(defn construct-prompt
  "Constructs an AI prompt string from the given template names and data.
   Templates will be composed in the specified order.
  
   Returns the constructed prompt string.
   Throws exceptions for missing templates or data."
  [template-names data]
  (core/construct-prompt template-names data))
