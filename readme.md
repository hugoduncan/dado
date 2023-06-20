* Dado

A dev assistant for clojure and emacs lisp.

Dado - dev assistant with dubious opinions

Requires jdk 11 or higher.

** Features

 Support the following on clojure and elisp functions:

- suggest implementation alternatives based on the function's doc
  string.

- implement a function based on its doc string

- implement a test for a function

- critique a function

- write a doc string for a function based on its implementation.

** Install


```
clojure -T:build jar :project middleware
clojure -T:build install :project middleware
```

Add dado to `deps.edn`'s `:aliases`.

```
:dado {:extra-deps
        {org.hugoduncan/dado-middleware {:mvn/version "0.1.2"}}}
```

Evaluate `dado-nrepl.el`.

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
