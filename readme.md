* Dado

A dev assistant for clojure and emacs lisp.

Dado - dev assistant with dubious opinions

** Features

 Support the following on clojure and elisp functions:

- suggest implementation alternatives based on the function's doc
  string.

- implement a function based on its doc string

- implement a test for a function

- critique a function

- write a doc string for a function based on its implementation.

** Install

Evaluate `dado-nrepl`.  Note that this will break cider-jack-in if dado
middleware is not on the path.

Put dado's clojure code on the classpath.

At this point it is probably easiest to try this by running a repl from
dado's top level `deps.edn`.

** OpenAI credentials

Set environment veriables:

```
OPENAI_API_KEY
OPENAI_ORGANIZATION
```

** Project Structure

<img src="logo.png" width="30%" alt="Polylith" id="logo">

The Polylith documentation can be found here:

- The [high-level documentation](https://polylith.gitbook.io/polylith)
- The [Polylith Tool documentation](https://polylith.gitbook.io/polylith/poly)
- The [RealWorld example app
  documentation](https://github.com/furkan3ayraktar/clojure-polylith-realworld-example-app)

You can also get in touch with the Polylith Team on
[Slack](https://clojurians.slack.com/archives/C013B7MQHJQ).

<h1>dado</h1>

<p>Add your workspace documentation here...</p>
