# Describing Changes to File Content

Always describe edits to change or create files using File Operation
Directive (FOD) format!

For each file operation, you MUST write out the instructions using the
File Operation Directive Format. Do NOT use other formats.

The whole FOD MUST be within a markdown code block using
the `fod` language.

All file paths must be relative.

The following operations are supported, with these semantics:

- EDIT path/to/file: Modify existing file's content using search/replace chunks
  - Target file must exist
  - Operation is atomic - file is modified in-place
  - Content validation before modification

- CREATE path/to/file: Create a new file with the specified content
  - Target file must not exist
  - Parent directories created if needed
  - Operation is atomic via temporary file

- DELETE path/to/file: Remove a file
  - Target file must exist
  - Operation is atomic

- MOVE source/path target/path: Move or rename a file
  - Source file must exist
  - Target file must not exist
  - Parent directories of target created if needed
  - Operation is atomic

- COPY source/path target/path: Copy a file to a new location
  - Source file must exist
  - Target file must not exist
  - Parent directories of target created if needed
  - Operation is atomic via temporary file

## EDIT operation

For EDIT operations, generate a search/replace chunk for each part of the
file that needs to be modified.

1 Start each search/replace chunk with a`<<<<<<< SEARCH` marker line.
2 Add the lines to be found. The SEARCH block.
3 Add the `=======` marker line.
4 Add the lines to be inserted to replace the found text. The REPLACE section.
5 The chunk ends with the `>>>>>>> REPLACE` marker line.

Do not forget the marker lines.

<example>

Given the a file at <file-path>some/file/path</file-path> with content
<file-content>
(ns my.ns)

(def c
  "This is the doc string for c"
  "C")

(defn f
  "Some doc"
  [a]
  (let [b (str a a)]
     (println 1)
     (println a)
	 (println b)
	 (println c)))
</file-content>,
then to edit the doc string for `c`, the
<search-content>
(def c
  "This is the doc string for c"
  "C")
</search-content>
could be replaced with
<replace-content>
(def c
  "This is a better doc string for c"
  "C")
</replace-content>

To perform this change you output:
<output>
``` fod
EDIT file-path
<<<<<<< SEARCH
(def c
  "This is the doc string for c"
  "C")
=======
(def c
  "This is a better doc string for c"
  "C")
>>>>>>> REPLACE

</example>

To move code within a file, use 2 chunks: 1 to delete it from its current
location, 1 to insert it in the new location.

You MUST provide enough SEARCH lines to uniquely identify the location of the
edit in the file.

Every SEARCH section must contain a a verbatim copy of the lines to
change in the existing file content.  Check at which line the SEARCH
section contents match the file to be changed.

You MUST provide enough SEARCH lines to uniquely identify the location
of the edit in the file, but SEARCH blocks should be concise. Do not
include large sequences of unchanged lines. Prefer to split SEARCH
blocks into smaller blocks, with only a few unchanged lines to uniquely
identify the edit site.


1. The SEARCH section must contain a verbatim copy of the lines to
   change in the existing file content

2. Must provide enough lines to uniquely identify the location of the edit in the file

3. When editing a function, method, loop, or other code block, include
   the entire code block in the SEARCH section.

4. Check at which line the SEARCH section contents match the file to be changed

5. End of line whitespace is significant and must match exactly

Start a new SEARCH REPLACE chunk for each section of the file that needs
changes.  When editing code, use a new SEARCH REPLACE chunk for each
function or top level form that you want to edit.

Output chunks in whatever order makes the most sense.  Hunks don't need
to be in any particular order.

When editing a block of code containing a function, method, loop,
top-level form, etc, use a SEARCH REPLACE chunk to replace the *entire*
code block. Place the entire existing code block in the SEARCH block,
and the updated version in the REPLACE block. This will help you
generate correct code and correct diffs.  This does do not imply the
anti-pattern of putting the whole file in a single SEARCH REPLACE chunk.

1. Instead of replacing the entire file content in one chunk, create
   separate chunks for distinct changes

2. Each chunk should contain just enough context to uniquely identify
   where the change should be made

3. If modifying multiple parts of a file, create separate chunks for
   each modification

4. When editing a function/method, use a chunk for just that function,
   not the whole file

5. Keep chunks focused on logical units of change


## CREATE operation

For CREATE operations, provide the complete file content after the
`CREATE` line. Parent directories will be created if needed.  The
operation is atomic via a temporary file.
