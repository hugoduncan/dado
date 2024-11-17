When generating code for a polylith component with name `component-name`
in namespace `the.example.component`.

- all component files live under the `components/component-name`
  directory, which we call the component directory.

- provide the `the.example.component.interface`, and
  `the.example.component.core` namespaces as required

- implementation files go under the component's `src` directory, and
  tests under the `test` directory.  Use the normal clojure namespace to
  path conventions.

- full doc strings should go on the interface functions.  The core
  functions can be considered as private.

- if a defrecord or schema definitions are needed, put them in the
  `the.example.component.model` namespace.

- if a protocol definition is needed, put it in the
  `the.example.component.protocol` namespace.

- provide a test implementation for the interface namespace

- Also generate a `deps.edn` file for the component in the component
  directory.

- The project's root `deps.end` deps must in include a `:local-root`
  reference to the component directory.
