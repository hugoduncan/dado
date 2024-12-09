# ADR: AI Tool Component

## Status
Accepted

## Context
- Need consistent structure for AI tools across providers
- Must support both structured and unstructured tool handling
- Tools need clear input/output specifications
- Tool registration and lookup required
- Must be provider-neutral but support provider-specific formats
- Tools must be independently executable
- Need validation of tool specifications

## Decision
We will create an AI Tool component that:

### Tool Structure
- Tools are registered with structured specifications
- Tool IDs can be any keyword (namespaced or not)
- Tools are held in a global in-memory registry (not persisted)
- Each tool provides its own error handling

### Tool Schema
```clojure
(def Tool
  [:map
   [:id keyword?]
   [:name string?]
   [:description string?]
   [:structured-description map?]
   [:parameters [:sequential Parameter]]
   [:returns ToolReturn]
   [:prompt-fn fn?]
   [:recognize-fn fn?]
   [:execute-fn fn?]])

(def Parameter
  [:map
   [:name string?]
   [:type keyword?]
   [:description string?]
   [:required? boolean?]
   [:default {:optional true} any?]])

(def ToolReturn
  [:map
   [:type keyword?]
   [:description string?]])
```

### Component Interface
- `register-tool!` - Registers a tool specification
- `lookup-tool` - Finds tool by ID
- `execute-tool!` - Runs tool with parameters
- `validate-tool` - Validates tool specification

### Implementation Requirements
- Use global atom for tool registry
- Validate tools on registration
- Wrap tool execution with telemere trace
- Tools handle their own specific error cases
- Basic metrics collection via telemere tracing

### Error Types
- :error/tool-validation
- :error/tool-execution

### Event Taxonomy
- :tool/registered
- :tool/validated
- :tool/lookup
- :tool/executed

## Consequences

### Positive
- Consistent tool structure
- Strong validation
- Clear error handling
- Provider-neutral interface
- Flexible tool specifications

### Negative
- Global mutable state
- Must manage tool registry
- Tools must handle own errors
- In-memory only persistence

## Notes
- Tool IDs do not require namespacing
- Registry is in-memory only
- Error translation left to tool implementations
- Basic metrics via telemere, detailed metrics up to tools
