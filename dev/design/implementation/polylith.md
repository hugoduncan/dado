When generating code for a polylith component:

- provide the `interface`, and `core` namespaces as required

- full doc strings should go on the interface functions.  The core
  functions can be considered as private,

- if a defrecord or schema definitions are needed, put them in the `model` namespace.

- if a protocol definition is needed, put it in the `protocol` namespace.

- provide a test implementation for the interface namespace

- Also generate a `deps.edn` file for the component.
