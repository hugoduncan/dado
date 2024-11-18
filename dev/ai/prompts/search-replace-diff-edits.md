For each file that needs to be changed, you MUST write out the changes using the
Search Replace Diff Format. Do NOT user other formats.

Only ever return code in a Search Replace Diff! Do NOT use Simplified
Diff format.

The whole Simplified diff should be within a markdown code block using
the `searchreplace` language.

For each file, you MUST include the first 2 lines with the file paths.

Start each hunk of changes with a `<<<<<<< SEARCH` line.

Add the lines to be found.  The SEARCH section.

Add the "=======" line.

Add the lines to be inserted to replace the found text. The REPLACE section.

The hunk ends with the ">>>>>>> REPLACE" line.


Every SEARCH section must match the existing file content verbatim, at
the character level, including all comments, docstrings, whitespace,
etc.

You MUST provide enough SEARCH lines to uniquely identify the location of the
edit in the file.

Start a new hunk for each section of the file that needs changes.

Output hunks in whatever order makes the most sense.
Hunks don't need to be in any particular order.

When editing a function, method, loop, etc use a hunk to replace the
*entire* code block.  Place the existing version in the SEARCH block,
and the updated version in the REPLACE block.  This will help you
generate correct code and correct diffs.

To move code within a file, use 2 hunks: 1 to delete it from its current
location, 1 to insert it in the new location.

To make a new file, show a diff from `--- /dev/null` to `+++ path/to/new/file.ext`.

SEARCH blocks should be concise.  Do not include a large sequences of
unchanged lines.  Prefer to split blocks into smaller blocks, with only
a few unchanged lines to uniquely identify the edit site.
