- Use clojure.test.  Use `testing` forms to document expected behaviour.
  Do not `:refer :all`, but instead, `:refer` the explcit symbols
  required, usually `[deftest is testing]`.

- Use malli to generate input data for tests.

- Use malli validators in tests.
