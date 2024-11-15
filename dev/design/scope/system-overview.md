# DADO - AI-Assisted Development Tool

Dado is a tool to help developers create, update and maintain a system.

It is based on a conceptual model of development that consists of three parts
 - the system overview
 - the architecture to be used
 - the implementation

It strives to use AI to help develop design documents, documentation and
implementation around each of these three parts.

It is in no way a linear progression through these parts.  It is
intended to be an iterative process that spans across them all.

At the beginning of the system the design is vague.  As the project
progresses the level of detail and completeness increases.  This means
that documents and code will change over time.

As decisions are made, they are recorded in an ADR, an Architectural
Decision Record.  Despite the name this will be used for scope,
features, architecture and implementation.

The ADR's are one of the main vehicles for interacting with the AI.
They can be used as both input and output.

The changes over time are recorded in version control.

AI interactions will recorded, so that at any time they can be resumed
and changed.


The aim is to be able to use dado from the command line, from a clojure
REPL, or from emacs.




## Key Features

### 1. AI Interaction
- Iterative refinement workflow
- Context-aware responses
- Project standard compliance
- Code and documentation updates

### 2. Documentation
- Automatic ADR generation from AI threads
- Doc string/comment updates
- Context document management
- Documentation consistency checking

### 3. Change Management
- Preview changes before application
- Edit suggestions before applying
- Atomic change application
- Different preview modes for different change types

### 4. Project Context
- Use of existing documentation for context
- Standards enforcement
- Project overview integration
- Template management

## Open Questions

### 1. Change Management
- How to structure different preview modes?
- Should partial edits be saveable?
- How to integrate with Emacs undo system?

### 2. Context Management
- How to prioritize different context documents?
- How to specify relevant context for different query types?
- How to weight different types of context?

### 3. Documentation
- How to handle automatic documentation updates?
- Different strategies for function vs namespace documentation?
- Documentation preview system design?

### 4. Thread Management
- How to structure thread persistence?
- Multi-thread analysis strategies?
- Thread to ADR synthesis approach?

## Current Decisions

### Accepted
1. Project Structure (ADR 0001)
   - Polylith architecture
   - deps.edn build tool
   - org.hugoduncan.dado namespace

2. Claude API Component (ADR 0002)
   - Minimal wrapper design
   - hato HTTP client
   - Separation of concerns

### Pending
- Documentation update workflow
- Change application process
- Thread persistence format

## Next Steps
1. Implement core Claude API wrapper
2. Design thread management system
3. Create basic Emacs interface
4. Establish dev directory management

## Initial Bootstrap Requirements
1. Claude interaction namespace
2. Emacs interaction buffer
3. Dev directory management
4. Thread modeling system

Git integration is deferred for initial implementation.
