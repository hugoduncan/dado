# Component Architectural Decision Records

## ADR 1: Project Configuration Component
**Status**: Accepted

**Context**:
- Need to manage project-specific settings and locations
- Must support multiple AI providers
- Configuration should be version-controlled
- Need to separate configuration from AI context

**Decision**:
- Create dedicated Project Configuration component
- Store configuration in EDN format in project directory
- Include API keys, project directories, and storage locations
- Make component directly accessible from REPL

**Consequences**:
- Positive:
  - Clear separation of configuration from AI context
  - Project-specific settings easily version controlled
  - Configuration accessible to all components
- Negative:
  - Need to handle sensitive data (API keys) carefully
  - Must maintain backward compatibility for config format

## ADR 2: AI Session Manager
**Status**: Accepted

**Context**:
- Need central coordination of AI interactions
- Must manage state across multiple components
- Direct REPL interaction required
- Must coordinate context and thread management

**Decision**:
- Create AI Session Manager as primary coordinator
- Handle session state and command interface
- Coordinate between Thread Manager and Context Manager
- Make directly accessible from REPL

**Consequences**:
- Positive:
  - Single point of coordination for AI interactions
  - Clear session lifecycle management
  - Simplified REPL interaction
- Negative:
  - Potential bottleneck for concurrent operations
  - Must maintain session state consistency

## ADR 3: AI Message Thread Manager
**Status**: Accepted

**Context**:
- Need to maintain conversation history
- Must support resuming previous threads
- Requires persistent storage
- Must integrate with AI provider interface

**Decision**:
- Create dedicated Thread Manager component
- Store threads as EDN files
- Support thread operations (create, resume, modify)
- Integrate with Storage Port for persistence

**Consequences**:
- Positive:
  - Clear thread lifecycle management
  - Persistent conversation history
  - Support for thread operations
- Negative:
  - Must handle large thread histories efficiently
  - Need to manage thread storage growth

## ADR 4: AI Context Manager
**Status**: Accepted

**Context**:
- Need to manage system and project prompts
- Must handle document selection for context
- Requires template management
- Context switches should be explicit

**Decision**:
- Create AI Context Manager for prompt and template handling
- Support composable templates and prompts
- Include document selection capabilities
- Require explicit context switching
- Handle template composition within manager

**Consequences**:
- Positive:
  - Centralized prompt and template management
  - Support for template composition
  - Clear context switching control
- Negative:
  - Must handle complex template composition
  - Need to manage context size limitations

## ADR 5: Storage Port
**Status**: Accepted

**Context**:
- Need persistent storage for all components
- Must support version control
- Should be human-readable
- Must handle multiple data types

**Decision**:
- Create Storage Port interface
- Use EDN format for all data
- Implement File System adapter initially
- Support project and system-level storage

**Consequences**:
- Positive:
  - Consistent data format across system
  - Human-readable storage
  - Version control friendly
- Negative:
  - Performance overhead of EDN format
  - Must maintain EDN schema compatibility

## ADR 6: AI Port
**Status**: Accepted

**Context**:
- Need to support multiple AI providers
- Must handle async operations
- Requires provider-agnostic interface
- Initial support for Claude

**Decision**:
- Create provider-agnostic AI Port interface
- Implement Claude adapter using core.async
- Support standard message format
- Handle provider-specific authentication

**Consequences**:
- Positive:
  - Easy addition of new providers
  - Consistent interface across providers
  - Async operation support
- Negative:
  - Must handle provider-specific features
  - Need to manage different provider capabilities