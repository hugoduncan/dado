# Using telemere to track execution time

```clojure
(:require [taoensso.telemere :as t]

;; Trace (auto interops with OpenTelemetry)
;; Tracks form runtime, return value, and (nested) parent tree
(t/trace!
  {:id ::my-id :data {}}
  (do-some-work))
```

The coordinates for `deps.edn` are:

``` clojure
com.taoensso/telemere {:mvn/version "1.0.0-RC1"}
```
