(ns dado.ai.agents.scope.interface
  "Public interface for the AI Scope Agent component.
   Provides agent construction for scope-focused AI interactions."
  (:require [dado.ai.agents.scope.core :as core]))

(defn create-agent
  "Creates a scope agent with the given configuration.
   Returns an agent map that implements the AI Agent interface.

   The scope agent specializes in scope management tasks including:
   - Scope definition and refinement
   - Feature analysis and management
   - Requirements clarification
   - Project boundaries and constraints
   - Feature interaction analysis"
  [project-config additional-context-fn]
  (core/create-agent project-config additional-context-fn))
