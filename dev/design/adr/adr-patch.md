# ADR: Patch Component

## Status
Proposed

## Context
- Need to apply simplified unified diffs to text files
- Component will be used to modify files based on AI responses
- Only need to handle text file modifications
- Only need to apply patches, not create them
- Must handle file system operations safely
- Must validate patch format before applying

## Decision
We will:
- Create a dedicated Patch component responsible for:
  - Parsing simplified unified diff format
  - Validating patch structure and target files
  - Applying changes to files on disk
  - Providing pure functions for patch operations

- Use simplified unified diff format as specified in the "Simplified Diff Format" document

- Component Interface:
  ```clojure
  (apply-patch! [patch-content]
    "Applies a patch to files, returns map of results per file.
     The patch-content is a string containing a simplified unified diff.

     First applies all changes in memory, collecting any errors.
     If any errors occur during in-memory application, throws an
     ex-info containing a sequence of all errors encountered.

     Only proceeds with file system changes if no errors occurred
     during in-memory application.

     Returns a map of file paths to change statistics on success.")
  ```

- File operations:
  - Before file modification:
    - Apply complete patch in memory
    - Collect all errors encountered during in-memory application
    - If any errors occurred, throw exception with all errors
    - Otherwise proceed with file system changes
  - Common operations:
    - Write changes to temporary file
    - Use default system permissions for new files/directories
    - Move temporary file to target atomically
  - For modifications:
    - Verify target file exists before starting
  - For new files:
    - Create target directory if it doesn't exist before starting

- Return value format:
  ```clojure
  {"path/to/file1" {:lines-added 10
                    :lines-removed 5}
   "path/to/file2" {:lines-added 3
                    :lines-removed 2}}
  ```

- Multi-file handling:
  - Multiple file patches allowed in single operation
  - Changes to individual files are atomic
  - No cross-file atomicity guaranteed
  - Each file change validated independently during in-memory phase
  - All errors collected during in-memory phase
  - Only proceed with file changes if no errors occurred
  - Return includes results for all processed files

- Validation rules:
  - For modifications:
    - Target file must exist
    - Context lines must match target file
  - For new files:
    - All parent directories must be creatable if they don't exist
  - Patch format requirements:
    - Must follow simplified unified diff format
    - At least two context lines required per hunk
    - All file paths must be relative

- Error handling:
  - Collect all errors during in-memory application
  - Throw single exception containing all collected errors
  - For modifications:
    - Record error if target file doesn't exist
    - Record error if context lines don't match
  - For new files:
    - Record error if target file exists
    - Record error if target directory not writable

## Error Types
- :error/patch-validation
- :error/file-access
- :error/patch-application
- :error/context-mismatch
- :error/file-exists

## Event Taxonomy
- :patch/validated
- :patch/invalid
- :patch/applied
- :patch/failed
- :patch/file-created

## Logging and Metrics
- Use trace level logging for all interface functions
- Generate metrics for:
  - Number of files modified
  - Number of lines added/removed
  - Patch validation time
  - Patch application time
  - Error counts by type

## Consequences
### Positive
- Simple, focused component
- Clear validation rules
- Support for multi-file patches
- Safe file operations
- Pure functional interface
- Easy to test
- No size limitations on patches
- Memory validation before file changes
- Complete error collection before failure

### Negative
- Limited to simplified diff format
- Must handle file system errors carefully
- Cannot handle complex patch scenarios
- Must hold complete patch in memory

## Validation
- Patches must follow simplified unified diff format
- Each hunk must have at least 2 context lines
- All file paths must be relative
- For modifications: target files must exist
- For new files: target files must not exist
- File operations must be atomic

## Dependencies
- File system access
- Use `dado.patch` namespace
- babashka.fs for filesystem operations
- Logging system (telemere)
- Error handling system

## Note
The diff format is described in the "Simplified Diff Format" document.
