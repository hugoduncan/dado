(ns dado.ai.agents.implementation.interface
  "Public interface for the AI Implementation Agent component.
   Provides agent construction for implementation-focused AI interactions."
  (:require [dado.ai.agents.implementation.core :as core]))

(defn create-agent
  "Creates an implementation agent with the given configuration.
   Returns an agent map that implements the AI Agent interface.

   The implementation agent specializes in implementation tasks including:
   - Code generation and review
   - API design and implementation
   - Testing strategy and test creation
   - Performance optimization
   - Code organization and structure"
  [project-config additional-context-fn]
  (core/create-agent project-config additional-context-fn))
