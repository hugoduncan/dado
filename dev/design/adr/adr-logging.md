# ADR: Logging System

## Status
Accepted

## Context
- Need consistent logging across system
- System events should be structured
- Must support different log levels
- Should be machine processable
- Need to maintain context
- Must integrate well with Clojure ecosystem
- Should support different output formats and destinations
- Need to handle development and production environments differently
- Must support metrics collection
- Each component needs its own event taxonomy

## Decision
We will:
- Use Telemere directly through its API. A logging component must not
  provide functions for logging.
- Components define their own event taxonomies in their ADRs using namespaced keywords
- Events follow Telemere structure:

```clojure
  {:id        :component/event-name
   :level     :info
   :ns        "dado.component"
   :data      {:relevant "data"}
   :metrics   {:duration 127  ; milliseconds
               :etc      "..."}
   ...}
```

- The sole purpose of a logging compotent is to configure standard sinks
  based on environment:
  - Development:
    - Console with pretty printing
    - Local EDN file
  - Production:
    - Rotating EDN files
    - Metrics extraction
    - Optional remote sink
- Provide logging system configuration for:
  - Log levels per namespace/component
  - Sink configurations
  - Rotation policies
  - Metrics extraction rules
  - Sensitive data redaction
  - Context propagation
  - Buffer sizes
  - Async options

## Consequences
### Positive
- Direct use of Telemere API
- Each component owns its event taxonomy
- Consistent event structure
- Flexible sink configuration
- Built-in metrics support
- Efficient binary format option
- Support for distributed tracing

### Negative
- Must coordinate event taxonomy across components
- Need central registry of event ids
- Must manage sink configurations carefully
- Have to handle remote sink failures
- Need to monitor buffer usage

## Validation
- Event ids must be namespaced keywords
- Components must document their event taxonomies
- Sink configurations must be environment-appropriate
- Buffer sizes must be tuned for production load

## Notes
- Telemere API used directly by components
- Binary format available for production
- Multiple concurrent sinks supported
- Remote sink configuration optional
  - Event taxonomies defined in component ADRs
