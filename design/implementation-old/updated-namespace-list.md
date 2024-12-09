# Updated Namespace List Format

The updated namespace list format is used to specify namespaces in
dependency order. It is specified in a markdown code block with the
`updated-namespaces` language marker.

The code block contains a sequence of namespaces, one per line. The
namespaces are listed in dependency order, with a namespace appearing
before any namespaces that require it.

For example, suppose we have these namespace dependencies:
- `a.core` requires `a.util` and `a.model`
- `a.model` requires `a.util`
- `a.util` requires nothing

A valid updated namespace list would be:

```updated-namespaces
a.util
a.model
a.core
```

Note that `a.util` must come before `a.model` since `a.model` requires
it, and `a.model` must come before `a.core` since `a.core` requires it.

The namespace dependencies can be extracted from the `:require` forms in
the namespaces.

By listing namespaces in dependency order, it becomes clear which
namespaces can be safely modified without impacting dependent code.  This
dependency order is useful for many development tasks, such as
identifying the order in which namespaces should be recompiled, or the
order in which changes should be made.

## Format Details

- Each namespace must be on its own line
- The namespace names must be valid Clojure namespace names
- Empty lines are ignored
- The complete code block must be wrapped in markdown code fences with
  the `updated-namespaces` language marker
- The ordering must respect the dependencies specified in the `:require`
  forms

## Example

```updated-namespaces
my.project.model
my.project.validation
my.project.logic
my.project.api
my.project.core
```

This indicates that:
- `my.project.model` has no dependencies
- `my.project.validation` may depend on `my.project.model`
- `my.project.logic` may depend on `my.project.model` and `my.project.validation`
- `my.project.api` may depend on any of the above
- `my.project.core` may depend on any of the above
