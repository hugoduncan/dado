# Search Replace Edit Format

The search replace edit format is used to specify file operations in a
safe and atomic way. It consists of a markdown code block with the
`searchreplace` language marker.

The code block contains a sequence of file operations. All file paths
must be relative.

The following operations are supported, with these semantics:

- EDIT: Modify existing file content
  - Target file must exist
  - Operation is atomic - file is changed in a temporary, and atomically
    moved into place
  - Content validation before modification

- CREATE: Create a new file
  - Target file must not exist
  - Parent directories created if needed
  - Operation is atomic via temporary file

- DELETE: Remove a file
  - Target file must exist
  - Operation is atomic

- MOVE: Move/rename a file
  - Source file must exist
  - Target file must not exist
  - Parent directories of target created if needed
  - Operation is atomic

- COPY: Copy a file
  - Source file must exist
  - Target file must not exist
  - Parent directories of target created if needed
  - Operation is atomic via temporary file

File operations use these formats:

```
EDIT target/file/path
[search/replace chunks]

CREATE target/file/path
<<<<<<< CONTENT
[new file content]
>>>>>>> CONTENT

DELETE target/file/path

MOVE source/path target/path

COPY source/path target/path
```

For EDIT operations, one or more search/replace chunks are used to modify
the file content. Each chunk uses this format:

- A marker for start of the search block `<<<<<<< SEARCH`
- A sequence of lines that will be searched
- The dividing line `=======`
- Lines to be used to replace the lines found
- A marker for end of the replace block `>>>>>>> REPLACE`

## Examples:

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

CREATE new/file/path
<<<<<<< CONTENT
This is the complete
content of the new file
>>>>>>> CONTENT

DELETE obsolete/file

MOVE old/path new/path

COPY template/file new/instance
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
