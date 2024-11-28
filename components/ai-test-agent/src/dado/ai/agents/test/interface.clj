(ns dado.ai.agents.test.interface
  "Public interface for the AI Test Agent component.
   Provides agent construction for testing-focused AI interactions."
  (:require [dado.ai.agents.test.core :as core]))

(defn create-agent
  "Creates a testing agent with the given configuration.
   Returns an agent map that implements the AI Agent interface.

   The agent specializes in:
   - Minimizing token usage in test interactions
   - Providing focused test-specific prompts
   - Managing test-relevant context
   - Supporting test scenario optimization
   - Handling test-related documentation

   Arguments:
   - project-config: Project configuration map
   - additional-context-fn: Function that returns additional context documents

   Throws :error/agent-creation on validation failure."
  [project-config additional-context-fn]
  (core/create-agent project-config additional-context-fn))
