# Search Replace Diff Format

- The search replace diff consists of a markdown code block with the
  `searchreplace` language marker.

- The code block contains a sequence of edit instructions for one or more files.

- Each file starts with:

```
--- target/file/path
+++ target/file/path
```
  If this is a new file it will start with:
```
--- /dev/null
+++ target/file/path
```

This is followed by one or more hunks to be applied to the file.  Each
hunk uses this format:

- A marker for start of the search block `<<<<<<< SEARCH`.
- A sequence of lines that will be searched.
- The dividing line `=======` .
- Lines to be used to replace the lines found.
- A marker for end of the replace block `>>>>>>> REPLACE`.

``` searchreplace
--- target/file/path
+++ target/file/path
<<<<<<< SEARCH
[lines to replace]
=======
[replacement lines]
>>>>>>> REPLACE
<<<<<<< SEARCH
[other lines to replace]
=======
[other replacement lines]
>>>>>>> REPLACE
--- /dev/null
+++ some/new-file-path
<<<<<<< SEARCH
=======
[new lines]
>>>>>>> REPLACE
```

For example, given a file `src/example.clj`:

``` clojure
(defn f
  "Some doc"
  [a]
  (do
     (println 1)
     (println 2)
     (println 3)))
```

A valid diff might be:

``` searchreplace
--- src/example.clj
+++ src/example.clj
<<<<<<< SEARCH
 (defn f
  "Some doc"
  [a]
  (do
     (println 1)
     (println 2)
     (println 3)))
=======
(defn f
  "Some doc"
  [a]
  (do
     (println 1)
     (println 4)
     (println 3)))
>>>>>>> REPLACE
```
