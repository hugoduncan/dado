(ns dado.ai.tool.interface
  "Interface for AI tool management"
  (:require [dado.ai.tool.core :as core]))

(defn register-tool!
  "Registers a tool. Tool map must conform to Tool schema.
   Returns registered tool map if successful.
   Throws :error/tool-validation if validation fails.

   Tool map must contain:
   - :id keyword identifier
   - :name string name
   - :description string description
   - :structured-description map of provider-specific descriptions
   - :parameters vector of parameter specifications
   - :returns map specifying return type and description
   - :prompt-fn function for tool description prompt
   - :recognize-fn function to recognize tool invocation
   - :execute-fn function to execute tool"
  [tool-map]
  (core/register-tool! tool-map))

(defn lookup-tool
  "Looks up tool by keyword identifier.
   Returns tool map if found, nil if not found."
  [tool-id]
  (core/lookup-tool tool-id))

(defn execute-tool!
  "Executes tool with given parameters.
   Returns result map containing:
   {:result any?      ; Tool execution result
    :error map?       ; Error details if execution failed
    :metrics map?}    ; Execution metrics (duration etc)
   Throws :error/tool-execution for validation/execution errors."
  [tool-id params]
  (core/execute-tool! tool-id params))

(defn validate-tool
  "Validates tool map structure.
   Returns tool map if valid.
   Throws :error/tool-validation if invalid."
  [tool-map]
  (core/validate-tool tool-map))
