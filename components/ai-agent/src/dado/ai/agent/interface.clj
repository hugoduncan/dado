(ns dado.ai.agent.interface
  "Interface for AI agent component"
  (:require [dado.ai.agent.core :as core]
            [dado.ai.agent.model :as model]))

(defn validate-agent
  "Validates agent map structure. Returns agent if valid.
   Throws :error/agent-validation if invalid.

   The agent map must contain:
   - :name - keyword identifier for the agent
   - :prompt-fn - function that returns prompt string
   - :context-fn - function that returns context data
   - :process-response-fn - function to process AI responses"
  [agent]
  (core/validate-agent agent))

(defn load-agent-document
  "Loads document content for given name following fallback path.
   Returns map with :content and :path keys.
   Throws :error/document-not-found if docs missing.

   Search paths (in order):
   1. <dev-dir>/dado/ai/agents/<agent-name>/
   2. <dev-dir>/dado/ai/agents/common/
   3. dado/ai/agents/<agent-name>/ in resources
   4. dado/ai/agents/common/ in resources"
  [project-config agent doc-name]
  (core/load-agent-document project-config agent doc-name))

(defn message-loop
  "Runs interactive message loop with given agent and message thread.
   Returns final message thread when user enters 'EXIT'.

   Arguments:
   - agent: Valid agent configuration
   - msg-thread: Initial message thread"
  [ai-port-fn agent msg-thread]
  (core/message-loop ai-port-fn agent msg-thread))

(defn agent-schema
  "Returns the malli schema for AI agents.

   The schema defines the required structure for valid agent configurations
   including the function specifications for prompt, context and response processing."
  []
  model/Agent)

(defn agent?
  "Predicate for an AI agent."
  [x]
  (model/agent? x))
