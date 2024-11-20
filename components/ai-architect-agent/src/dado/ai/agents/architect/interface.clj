(ns dado.ai.agents.architect.interface
  "Public interface for the AI Architect Agent component.
   Provides agent construction for architecture-focused AI interactions."
  (:require [dado.ai.agents.architect.core :as core]))

(defn create-agent
  "Creates an architect agent with the given configuration.
   Returns an agent map that implements the AI Agent interface.

   The architect agent specializes in architecture-related tasks including:
   - Architecture design and review
   - Component decomposition
   - Technical decision making
   - System integration patterns
   - Technology stack selection"
  [project-config additional-context-fn]
  (core/create-agent project-config additional-context-fn))
