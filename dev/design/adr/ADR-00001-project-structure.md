# 1. Initial Project Structure Decisions

Date: 2024-11-12
Status: Proposed

## Context
Bootstrapping a new developer tool that will assist with AI-driven development requires 
foundational decisions about project structure, build tools, and namespace organization. 
These choices will impact development workflow, maintainability, and future extensibility.

## Decision
We will:
1. Use Polylith architecture for project structure
   - Provides flexibility through component isolation
   - Enables clear separation of concerns
   - Facilitates testing and reuse of components

2. Use deps.edn as the build tool
   - Modern, flexible dependency management
   - Direct Clojure integration
   - Simple, data-driven configuration

3. Use org.hugoduncan.dado as the root namespace
   - Clear ownership and organization
   - Follows Java package naming conventions
   - Provides namespace hierarchy for project components

## Consequences

### Positive
- Polylith structure enables independent development of components
- deps.edn provides straightforward dependency management
- Clear namespace structure aids organization
- Components can be developed and tested in isolation
- Project structure supports future growth

### Negative
- Additional setup complexity with Polylith
- Need to maintain careful component boundaries
- Team needs familiarity with Polylith conventions

### Neutral
- Will need to establish conventions for component organization
- May need to document Polylith patterns for contributors

## Notes
Initial components likely to include:
- Claude interaction base
- Project management
- Emacs interface
- Thread modeling
- Dev directory management

