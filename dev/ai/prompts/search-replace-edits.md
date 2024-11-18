# Describing Changes to File Content

Always describe edits to change or create files using Search Replace
Edit format!

For each file that needs to be changed, you MUST write out the changes using the
Search Replace Edit Format. Do NOT user other formats.

The whole Search Replace Edit should be within a markdown code block using
the `searchreplace` language.

For each file to edit, you MUST include the first line with `EDIT` and
the file path.  For each file to create, you MUST include the first line
with `CREATE` and the file path.

for each part of the file that needs to be edited or created, generate a
search/replace chunk

Start each search/replace chunk with a `<<<<<<< SEARCH` marker line.

Add the lines to be found.  The SEARCH block.

Add the `=======` marker line.

Add the lines to be inserted to replace the found text. The REPLACE section.

The chunk ends with the `>>>>>>> REPLACE` marker line.

Do not forget the marker lines.

Every SEARCH section must match the existing file content verbatim, at
the character level, including all comments, docstrings, whitespace,
etc.

You MUST provide enough SEARCH lines to uniquely identify the location of the
edit in the file.

Start a new chunk for each section of the file that needs changes.

Output chunks in whatever order makes the most sense.
Hunks don't need to be in any particular order.

When editing a function, method, loop, etc use a chunk to replace the
*entire* code block.  Place the existing version in the SEARCH block,
and the updated version in the REPLACE block.  This will help you
generate correct code and correct diffs.

To move code within a file, use 2 chunks: 1 to delete it from its current
location, 1 to insert it in the new location.

To make a new file, show an edit from `--- /dev/null` to `+++ path/to/new/file.ext`.

SEARCH blocks should be concise.  Do not include a large sequences of
unchanged lines.  Prefer to split chunks into smaller blocks, with only
a few unchanged lines to uniquely identify the edit site.
