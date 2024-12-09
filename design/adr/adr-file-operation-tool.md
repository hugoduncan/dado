# ADR: File Operation Tool

## Status
Proposed

## Context
- Need tool interface for file operations
- Must support all File Operation Directive (FOD) operations
- Must integrate with AI Tool component
- Must integrate with Patch component
- Must validate operation inputs
- Must prevent operations outside project directory
- Must maintain atomic operations with rollback
- Must support all operations in a single call

## Decision
We will create a File Operation Tool that:

### Tool Definition
```clojure
{:id          :dado/file-operation
 :name        "File Operation Tool"
 :description "Executes file operations like create, edit, move, copy and delete"
 :structured-description
 {:claude
  {:description "Tool for performing file operations. All paths must be relative and within project directory."}}
 :parameters
 [:map
  [:operations
   [:vector
    [:map
     [:operation [:enum :create :edit :move :copy :delete]]
     [:path :string]
     [:target-path {:optional true} :string]  ; Required for move/copy
     [:content {:optional true} :string]      ; Required for create/edit
     [:search-blocks                          ; Required for edit
      {:optional true}
      [:vector [:map
                [:search :string]
                [:replace :string]]]]]]]]
 :returns
 {:type        :map
  :description "Map containing operation results and any errors"}
 :prompt-fn    (fn [_] "Use this tool to perform file operations. All paths must be relative.")
 :recognize-fn #(str/includes? % "function_calls")
 :execute-fn   execute-tool!}
```

The tool will:
1. Validate all operations before execution:
   - All paths must be relative and within project directory
   - Required parameters present for each operation type:
     - CREATE: path and content
     - EDIT: path, search-blocks
     - MOVE/COPY: path and target-path
     - DELETE: path
   - Source files must exist for EDIT, MOVE, COPY, DELETE
   - Target paths must not exist for CREATE, MOVE, COPY
   - Parent directories must be creatable

2. Execute operations atomically:
   - Create temporary files for CREATE/EDIT/COPY
   - Move files into place only after successful validation
   - Roll back all changes if any operation fails

3. Return results map:
```clojure
{:operations [{:operation :create
               :path "path/to/file"}
              {:operation :move
               :path "source/path"
               :target "target/path"}]
 :errors [{:operation :edit
           :path "bad/path"
           :reason "File not found"}]}
```

### Implementation Requirements
- Use Patch component for file operations
- Use AI Tool component for registration
- Maintain atomic transaction semantics
- Provide clear error reporting
- Validate before execution
- Roll back on failure

## Error Types
Use standard AI Tool error types:
- :error/tool-validation - Invalid parameters or validation failure
- :error/tool-execution - Operation execution failure

## Event Taxonomy
- :tool/operations-validated
- :tool/operation-started
- :tool/operation-completed
- :tool/operations-rolled-back
- :tool/execution-failed

## Consequences

### Positive
- Clean tool interface for file operations
- Strong validation and safety checks
- Atomic operations with rollback
- Clear error reporting
- Supports all operation types

### Negative
- Complex validation requirements
- Must handle rollback carefully
- Need careful error handling
- Must coordinate with Patch component

## Dependencies
- AI Tool component
- Patch component
- File system access

## Notes
- Uses dado.ai.tools.file-operation namespace
- Maintains same safety guarantees as Patch component
