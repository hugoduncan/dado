- Use hato for http interactions
- Prefer malli over clojure spec.
- Use babashka.fs for filesystem operations, in preference to clojure.java.io
- Use com.taoensso/telemere for logging and metrics
- Use com.taoensso/truss for assertions, including pre and post conditions

- do not use clojure.core.async unless specifically requested
- use promesa in preference to core.async

do not use a library unless the functionality it provides is
specifically called for by the requirements

## Malli

For pre and post conditions using malli and truss:
```clojure
(def X  [:map [:x :boolean]])
(def X? (m/validator X))
(defn a-fn [x]
  {:pre [(have? X? x :data (me/humanize (m/explain X x)))]
	:post [(have? X? % :data (me/humanize (m/explain X x)))]}
  x)
```
The use of `:data` is to provide feedback if the condition fails.

Always define a validator with `m/validator`, and never call `m/validate`.
```clojure
(def X  [:map [:x :boolean]])
(def X? (m/validator X))
(defn some-fn [x]
  {:pre [(have? X? x :data (me/humanize (m/explain X x)))]}
```

### Java Date and Time

Prefer `java.time.Instant` to `java.util.Date`.
