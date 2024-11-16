# ADR: Storage Port

## Status
Accepted

## Context
- Persistent storage needed for all components
- Must support version control
- Should be human-readable
- Must handle multiple data types
- Need consistent storage interface

## Decision
We will:
- Create Storage Port interface
- Use EDN format for all data
- Implement File System adapter initially
- Support project and system-level storage
- Define clear storage hierarchy
- Use atomic file operations where possible

## Consequences
### Positive
- Consistent data format across system
- Human-readable storage
- Version control friendly
- Clear storage structure

### Negative
- Performance overhead of EDN format
- Must maintain EDN schema compatibility
- Need to handle concurrent file access