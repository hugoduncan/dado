# ADR: Patch Component

## Status
Proposed

## Context
- Need to apply both simplified unified diffs and search-replace edit format
- Component will be used to modify files based on AI responses
- Must handle text file modifications reliably
- Must validate patches before applying
- Must handle file system operations safely and atomically

## Decision
We will:
- Create a dedicated Patch component responsible for:
  - Supporting both simplified unified diff and search-replace edit formats
  - Validating patch structure and target files
  - Applying changes to files on disk
  - Providing pure functions for patch operations

- Support both formats through separate interface functions:
  ```clojure
  (apply-simplified-diff-patch! [patch-content]
    "Applies a simplified unified diff to files.
     Returns map of file paths to change statistics.
     Throws ex-info with all validation errors if any occur.")

  (apply-search-replace-diff-patch! [patch-content]
    "Applies a search-replace edit format patch.
     Returns sequence of operation result maps.
     Throws ex-info with all validation errors if any occur.")
  ```

- Return value formats:
  For simplified diffs:
  ```clojure
  {"path/to/file1" {:lines-added 10
                    :lines-removed 5}
   "path/to/file2" {:lines-added 3
                    :lines-removed 2}}
  ```

  For search-replace edits:
  ```clojure
  [{:operation :edit
    :files ["path/to/file1"]}
   {:operation :create
    :files ["path/to/newfile"]}
   {:operation :move
    :files ["source/path" "target/path"]}]
  ```

- File operations:
  - Before any modifications:
    - Parse and validate complete patch
    - Apply changes in memory
    - Collect all validation errors
    - Only proceed with file system changes if validation succeeds
  - Common operations:
    - Write changes to temporary file
    - Use default system permissions
    - Move temporary file to target atomically
  - Operation-specific:
    - EDIT: Verify target exists and SEARCH blocks match exactly once
    - CREATE: Verify target doesn't exist
    - DELETE: Verify target exists
    - MOVE/COPY: Verify source exists

- Validation rules:
  For simplified diffs:
    - Must follow unified diff format
    - At least two context lines per hunk
    - Target files must exist for modifications
    - All paths must be relative
    - Context lines must match target file

  For search-replace format:
    - Must follow search-replace edit format
    - SEARCH blocks must match target file content exactly once
    - Source files must exist for MOVE/COPY
    - All paths must be relative
    - Target paths must be writable
    - Parent directories must be creatable

- Error handling:
  - Collect all errors during validation phase
  - Throw single exception containing all errors
  - Operation-specific errors:
    - EDIT: No match or multiple matches for SEARCH block
    - CREATE: Target already exists
    - DELETE: Target doesn't exist
    - MOVE/COPY: Source doesn't exist or target exists

## Error Types
- :error/patch-validation
- :error/file-access
- :error/patch-application
- :error/context-mismatch
- :error/file-exists
- :error/search-block-match
- :error/source-missing
- :error/multiple-matches
- :error/invalid-operation

## Event Taxonomy
- :patch/validated
- :patch/invalid
- :patch/applied
- :patch/failed
- :patch/file-created
- :patch/file-deleted
- :patch/file-moved
- :patch/file-copied
- :patch/search-matched
- :patch/operation-complete

## Logging and Metrics
- Use trace level logging for all interface functions
- Generate metrics for:
  - Operations by type
  - Files affected
  - Lines modified
  - Validation time
  - Application time
  - Error counts by type

## Consequences
### Positive
- Supports both patch formats cleanly
- Clear separation of concerns
- Strong validation rules
- Safe file operations
- Pure functional interface
- Complete error collection
- Format-specific return values

### Negative
- Must handle two distinct formats
- More complex validation logic
- Must handle additional file operations
- Need to maintain format-specific validation

## Validation
- All operations must be validated before any files are modified
- Patches must follow their respective format rules
- File paths must be relative
- Source files must exist for MOVE/COPY
- SEARCH blocks must match exactly once
- File operations must be atomic

## Dependencies
- File system access
- Use `dado.patch` namespace
- babashka.fs for filesystem operations
- Logging system (telemere)
- Error handling system

## Notes
Formats are described in:
- "Simplified Diff Format" document
- "Search Replace Edit Format" document
