# ADR: Run Test Namespace Tool

## Status
Proposed

## Context
- Need to execute test namespaces and collect results
- Must support clojure.test initially
- Must capture test failures and exceptions
- Must capture stdout/stderr output
- Must integrate with AI Tool component
- Test results need structured format for AI processing
- Must prevent namespace execution outside project
- Should support both sync and async execution modes

## Decision
We will create a Run Test Namespace Tool that:

### Tool Definition
```clojure
{:id          :dado/run-test-namespace
 :name        "Run Test Namespace Tool"
 :description "Executes test namespace and collects results"
 :structured-description
 {:claude
  {:description "Tool for running Clojure test namespaces and collecting results."}}
 :parameters
 [:map
  [:namespace :string]     ; Namespace to run
  [:async? {:optional true}
   :boolean]               ; Run asynchronously (default false)
  [:timeout {:optional true}
   pos-int?]]             ; Timeout in ms (default 30000)
 :returns
 {:type        :map
  :description "Map containing test results and output"}}
```

The tool will:
1. Execute given test namespace:
   - Load namespace if not loaded
   - Run all tests in namespace
   - Capture all test results
   - Capture stdout/stderr
   - Handle fatal exceptions

2. Return results map:
```clojure
{:namespace    "my.test.ns"
 :summary      {:test 10
               :pass 8
               :fail 1
               :error 1}
 :test-results [{:test-var "test-name"
                :status :pass}
               {:test-var "failed-test"
                :status :fail
                :expected "expected value"
                :actual "actual value"
                :message "failure message"}
               {:test-var "error-test"
                :status :error
                :type "exception type"
                :message "error message"}]
 :output      {:stdout "captured stdout"
               :stderr "captured stderr"}
 :elapsed-ms  127}
```

### Implementation Requirements
- Use clojure.test API for test execution
- Use with-out-str and with-err-str for output capture
- Support both sync and async execution
- Validate namespace exists before execution
- Clear error reporting for fatal errors
- Handle timeouts gracefully
- Provide execution metrics

## Error Types
- :error/namespace-not-found - Test namespace doesn't exist
- :error/namespace-load - Failed to load namespace
- :error/test-execution - Fatal error during execution
- :error/test-timeout - Execution timeout

## Event Taxonomy
- :test/namespace-loaded
- :test/execution-started
- :test/execution-completed
- :test/execution-failed
- :test/results-collected

## Consequences

### Positive
- Clean tool interface for test execution
- Structured result format
- Complete output capture
- Clear error reporting
- Execution metrics included

### Negative
- Must handle namespace loading carefully
- Need timeout management
- Have to capture all output
- Must maintain test isolation

## Dependencies
- AI Tool component
- clojure.test

## Notes
- Uses dado.ai.tools.run-test-namespace namespace
- Initial focus on clojure.test support
- Future expansion could include:
  - Other test frameworks
  - Test selection/filtering
  - Resource cleanup guarantees
  - Test retry options
