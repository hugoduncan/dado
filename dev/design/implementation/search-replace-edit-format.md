# Search Replace Edit Format

- The search replace edit format consists of a markdown code block with the
  `searchreplace` language marker.

- The code block contains a sequence of edit instructions for one or more files.

- Each file starts with `CREATE` or `EDIT` and the file name, with no
  decoration:

```
EDIT target/file/path
```
  If this is a new file it will start with:
```
CREATE target/file/path
```

This is followed by one or more search/replace edit chunks to be applied
to the file.  Each chunk uses this format:

- A marker for start of the search block `<<<<<<< SEARCH`.
- A sequence of lines that will be searched.
- The dividing line `=======` .
- Lines to be used to replace the lines found.
- A marker for end of the replace block `>>>>>>> REPLACE`.

``` searchreplace
EDIT target/file/path
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
CREATE some/new-file-path
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
EDIT src/example.clj
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
