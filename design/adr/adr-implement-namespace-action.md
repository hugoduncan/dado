# ADR: Implement Namespace Action Component

## Status
Proposed

## Context
- Need to generate polylith component implementation from ADR
- Must handle both interactive and non-interactive modes
- Must validate ADR completeness
- Must generate consistent component structure
- Must handle component dependencies
- Must follow polylith component conventions
- Must use AI for implementation details
- Must maintain project structure integrity

## Decision
We will create an Implement Namespace Action component that:

### Component Interface
```clojure
(execute
  [project-config adr-name options]
  "Implements a polylith component from an ADR specification.
   Returns map of implementation results.

   Arguments:
   - config: Project configuration map
   - adr-path: Path to ADR file
   - options: Map of implementation options:
     :mode - :interactive or :non-interactive (default :interactive)

   Throws :error/implement-namespace for implementation failures.")
```

### Implementation Requirements

1. Component Structure Generation:
   - Create standard polylith component directory structure
   - Generate interface namespace
   - Generate core namespace
   - Generate test namespaces
   - Create deps.edn with dependencies

2. ADR Validation:
   - Verify ADR exists and is readable
   - Check ADR completeness
   - Validate component name
   - Verify target location
   - Check for required sections
   - Extract interface definition
   - Identify dependencies

3. Interactive Mode:
   - Use AI agent for implementation guidance
   - Allow clarifying questions
   - Support incremental implementation
   - Use message loop for interaction
   - Maintain implementation context

4. Non-Interactive Mode:
   - Fail fast on incomplete ADR
   - No user prompts allowed
   - Must use explicit interface definition
   - Must have complete implementation specification
   - Skip optional features not explicitly defined

5. Generated Component Structure:
   - follow the standard polylith component structure

6. Implementation Process:
   a. Validate inputs and ADR
   b. Create component directory structure
   c. Generate component files
   d. Create test files
   e. Create the component's deps.edn
   f. Add the component to the project's deps.edn
   g. Return results map

7. Required Validation Rules:
   - Component name must be valid
   - Target path must be writable
   - ADR must exist and be readable
   - Interface must be defined
   - Dependencies must be specified
   - Implementation details must be complete for non-interactive mode

8. Return Value Format:
```clojure
{:component-name     string?
 :component-path     string?
 :files-created      [string?]
 :namespaces-created [string?]
 :dependencies       [{:lib symbol? :version string?}]
 :interface-ns       symbol?}
```

### Error Types
- :error/invalid-adr
- :error/invalid-component-name
- :error/invalid-target
- :error/incomplete-specification
- :error/implementation-failed
- :error/file-creation
- :error/validation

### Event Taxonomy
- :implement/started
- :implement/validated
- :implement/structure-created
- :implement/files-generated
- :implement/tests-created
- :implement/completed
- :implement/failed

## Dependencies
- AI Agent component for interactive mode
- AI Implementation Agent component for AI interaction
- Project Config component for paths
- REPL Message Loop for interactive mode
- File Operation Directives for outputting files

## Consequences

### Positive
- Consistent component generation
- Flexible implementation modes
- Strong validation
- Clear implementation process
- AI-assisted implementation
- Maintains project structure

### Negative
- Complex ADR validation required
- Must handle partial implementations
- Need to maintain AI prompts
- Must coordinate multiple components
- Have to handle implementation variations

### Neutral
- Projects must provide complete ADRs
- May need to extend validation rules
- Could require prompt tuning

## Notes
- Uses dado.actions.implement-namespace namespace
- Both modes use same core implementation logic
- Interactive mode allows refinement
- Non-interactive requires complete specification
- Generated files use standard templates
- Implementation follows polylith conventions
