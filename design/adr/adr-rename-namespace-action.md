# ADR: Rename Namespace Action Component

## Status
Proposed

## Context
- Need to safely rename project namespaces including their sub-namespaces
- Must update all references to old namespace in implementation code
- Must update namespace aliases to match new names
- Must move files on disk to match new namespace structure
- Action requires source and target namespace names
- Must validate namespace names and file paths
- Must handle namespace dependencies
- Must maintain project integrity during rename

## Decision
We will create an Implement Namespace Action component that:

- Exposes a single function to perform namespace renaming:
```clojure
(rename-namespace [config source-ns target-ns]
  "Renames source namespace and sub-namespaces to target namespace.
   Updates all code references and moves files.
   Returns map of renamed namespaces.
   Throws :error/namespace-rename if validation or rename fails.")
```

### Implementation Requirements

1. Input Validation:
   - Source namespace must exist
   - Target namespace must not exist
   - Both namespaces must be valid Clojure namespace names
   - Source and target must be in same project
   - Target path must not exist

2. Namespace Analysis:
   - Find all sub-namespaces of source namespace
   - Create mapping of old to new namespace names
   - Find all references to source namespace and sub-namespaces
   - Generate list of required file moves

3. File Operations:
   - Move source namespace file to target location
   - Move all sub-namespace files to new locations
   - Update namespace declarations in moved files
   - Update all references to renamed namespaces
   - Update namespace aliases

4. Operations must be performed in this order:
   1. Validate inputs
   2. Analyze namespaces and build change map
   3. Validate all file operations are possible
   4. Move files to new locations
   5. Update namespace references
   6. Update namespace declarations

5. All file operations must use the file-operation tool.

6. The returned map should contain:
```clojure
{:renamed {"old.ns" "new.ns"
           "old.ns.sub" "new.ns.sub"}
 :moved   ["old/ns.clj" "new/ns.clj"
           "old/ns/sub.clj" "new/ns/sub.clj"]
 :updated ["src/other.clj"
           "test/other_test.clj"]}
```

### Error Conditions
- :error/invalid-source-ns - Source namespace doesn't exist
- :error/invalid-target-ns - Target namespace exists or is invalid
- :error/namespace-conflict - Namespace name conflict
- :error/file-conflict - Target path exists
- :error/file-permission - File system permission error
- :error/rename-failed - Rename operation failed

### Event Taxonomy
- :namespace/validated - Input validation complete
- :namespace/analyzed - Namespace analysis complete
- :namespace/moved - Files moved to new location
- :namespace/updated - References updated
- :namespace/renamed - Rename operation complete

### Implementation Validation
- All namespace names must be valid
- All file paths must be relative
- All required files must exist
- All target paths must be available
- All namespace references must be updated

### Error Recovery
- File operations must be atomic
- Failed operations must be rolled back
- Original state must be restored on error
- Error context must include:
  - Source and target namespaces
  - Failed operation details
  - File operation state

## Dependencies
- Patch component for file operations
- Project Config component for paths
- AI Message component for validations

## Consequences

### Positive
- Safe namespace renaming
- Complete sub-namespace handling
- Atomic operations
- Clear error reporting
- Maintains project integrity

### Negative
- Complex implementation
- Must handle file system failures
- Need careful dependency analysis
- Must maintain rollback capability

## Notes
- should use the `dado.actions.rename-namespace` namespace
