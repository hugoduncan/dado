# ADR: Test Agent Component

## Status
Proposed

## Context
- Need dedicated agent for testing scenarios that minimizes API costs
- Must be compatible with AI Agent component interface
- Must provide specialized prompts for test scenarios
- Need to minimize token usage in test interactions
- Must integrate with existing test infrastructure
- Should follow similar pattern to Refactoring Agent

## Decision
We will create a Testing Agent component that:

### Component Interface
```clojure
(defn create-agent
  [project-config additional-context-fn]
  "Creates a testing agent optimized for test scenarios.
   Returns an agent map compatible with the AI Agent interface.

   The agent specializes in:
   - Minimizing token usage in test interactions
   - Providing focused test-specific prompts
   - Managing test-relevant context

   Arguments:
   - project-config: Project configuration map
   - additional-context-fn: Function that returns additional context documents

   Throws :error/agent-creation on validation failure.")
```

### Implementation Requirements

1. Agent Configuration:
   - Name: `:testing`
   - Uses test-specific prompt templates
   - Loads context from test-related directories
   - Processes responses for test scenarios

2. Token Usage Optimization:
   - Truncate non-essential context
   - Use focused, minimal prompts
   - Strip unnecessary metadata from responses
   - Only include directly relevant test files in context

3. Context Management:
   - Load test-related files from project
   - Support additional test context via function
   - Focus on test-relevant documentation
   - Exclude implementation details unless needed

4. Response Processing:
   - Strip unnecessary formatting
   - Remove redundant content
   - Optimize response format for tests
   - Remove any non-test-related content

5. Prompt Management:
   - Use specialized test prompts
   - Include only necessary context
   - Optimize prompt length
   - Focus on test-specific instructions

### Required Validation
- Agent must conform to AI Agent component schema
- Project config must be valid
- Additional context function must return sequence
- All prompt templates must exist
- Response processing must maintain format

## Error Types
- :error/agent-creation
- :error/prompt-loading
- :error/context-loading
- :error/response-processing

## Event Taxonomy
- :testing/agent-created
- :testing/prompt-loaded
- :testing/context-loaded
- :testing/response-processed

## Dependencies
- AI Agent component for base functionality
- AI Prompt component for template handling
- Project Config component for paths

## Consequences

### Positive
- Reduced API costs in test scenarios
- Focused test interactions
- Consistent with existing agent pattern
- Clear optimization strategy
- Compatible with test infrastructure

### Negative
- Must maintain separate prompt templates
- Need to carefully manage context reduction
- Have to balance completeness vs optimization
- Must validate optimization doesn't impact quality

## Notes
- Uses dado.ai.agents.testing namespace
- Follows existing agent patterns
- Focuses on test-specific optimizations
- Maintains AI Agent interface compatibility
