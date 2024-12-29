- Use clojure.test.  Use `testing` forms to document expected behaviour.
  Do not `:refer :all`, but instead, `:refer` the explcit symbols
  required, usually `[deftest is testing]`.

- Create tests in order, as appropriate
   1 Basic Input Validation
   2 Simple cases
   3 General correctness test
   4 Property tests
   5 Stress tests

- Prefer data driven tests

- Try and write `testing` forms so that the declare the expected
  functionality, as if writing a spec.

- Use malli to generate input data for tests.

- Use malli validators in tests.
