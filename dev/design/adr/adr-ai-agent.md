# ADR: AI Agent Component

## Status
Proposed

## Context
- Need to define consistent structure for AI Agents
- Agents need configurable prompts and context documents
- Agents may need to process responses
- Document loading should follow consistent fallback pattern
- Need clear interface for creating and using agents
- Must integrate with existing components

## Decision
We will create an AI Agent component that:
- Defines the structure and interface for AI Agents
- Provides an agent validation function
- Implements document loading with fallback behavior

### Agent Structure
An agent is represented by a map with the following schema:

```clojure
(def AgentFn [:=> [:cat :any] :any])

(def Agent
  [:map
   [:name keyword?]
   [:prompt-fn AgentFn]
   [:context-fn AgentFn]
   [:process-response-fn AgentFn]])
```

### Document Loading Paths
Documents are searched in this order:
1. `<dev-dir>/ai/agents/<agent-name>/` in project
2. `<dev-dir>/ai/agents/common/` in project
3. `ai/agents/<agent-name>/` in resources
4. `ai/agents/common/` in resources

### Component Interface
```clojure
(validate-agent
  [agent]
  "Validates agent map structure.
   Returns agent if valid.
   Throws :error/agent-validation if invalid.")

(load-agent-document
  [project-config agent doc-name]
  "Loads document content for given name following fallback path.
   Return the document content.
   Throws :error/document-not-found if any docs missing.")
```

### Implementation Requirements
- Use pure functions
- Validate all inputs
- Load documents lazily when needed
- Cache document content appropriately
- Log all operations at trace level
- Generate clear error messages
- Support composition of agents
- Support inheritance of documents

## Error Types
- :error/agent-validation
- :error/document-not-found
- :error/prompt-generation
- :error/context-loading
- :error/response-processing

## Event Taxonomy
- :agent/created
- :agent/validated
- :agent/documents-loaded
- :agent/prompt-generated
- :agent/context-loaded
- :agent/response-processed

## Consequences

### Positive
- Clear agent structure definition
- Consistent document loading behavior
- Flexible response processing
- Pure functional interface
- Strong validation
- Support for agent composition

### Negative
- Must manage document loading performance
- Need to handle missing documents gracefully
- Must maintain document path hierarchy
- Have to manage function call errors

## Dependencies
- Use `dado.ai.agent` namespace
- Logging system for events
- Error handling for exceptions

## Validation
- Agent maps must conform to schema
- Document paths must be valid
- Functions must have correct signatures
- All required documents must exist
- Response processing must maintain format

## Notes
- Consider future support for:
  - Agent composition patterns
  - Document inheritance rules
  - Response processing chains
  - Document content validation
