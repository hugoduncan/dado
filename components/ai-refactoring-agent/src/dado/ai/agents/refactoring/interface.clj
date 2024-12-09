(ns dado.ai.agents.refactoring.interface
  (:require [dado.ai.agents.refactoring.core :as core]))

(defn create-agent
  "Creates a refactoring agent for code modifications.
   Returns an agent map compatible with the AI Agent interface.

   The agent specializes in:
   - Code refactoring suggestions
   - Safe code modifications using the file-operation tool
   - Context-aware refactoring

   Throws :error/agent-creation on validation failure."
  [project-config additional-context-fn]
  (core/create-agent project-config additional-context-fn))
