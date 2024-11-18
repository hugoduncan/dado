For each file that needs to be changed, write out the changes similar to a unified diff like `diff -U0` would produce.

Return edits similar to unified diffs that `diff -U0` would produce.

You MUST include the first 2 lines with the file paths.
You MUST NOT include timestamps with the file paths.

Start each hunk of changes with a `@@ ... @@` line.
You MUST NOT include line numbers, unlike what `diff -U0` does.

The patches must be CORRECT patches, that apply cleanly against
the current file contents.

Mark all lines that need to be removed or changed as with `-`.

Mark all new or modified lines with `+`.

Do not leave out any lines or the diff patch won't apply correctly.

The indentation is VERY significant in the diffs.

End of line whitespace is VERY significant in the diffs.

Whitespace after the start of line ` `, `+` or `-` is VERY significant
in the diffs.

You MUST provide enough context to uniquely identify the location of the
edit in the file.

Start a new hunk for each section of the file that needs changes.

Only output hunks that specify changes with `+` or `-` lines.
Skip any hunks that are entirely unchanging ` ` lines.

Output hunks in whatever order makes the most sense.
Hunks don't need to be in any particular order.

When editing a function, method, loop, etc use a hunk to replace the
*entire* code block.  Delete the entire existing version with `-` lines
and then add a new, updated version with `+` lines.  This will help you
generate correct code and correct diffs.

To move code within a file, use 2 hunks: 1 to delete it from its current
location, 1 to insert it in the new location.

To make a new file, show a diff from `--- /dev/null` to `+++ path/to/new/file.ext`.

Before outputting the diff, consider if it could be applied as expected.

For example:

with the following map in the chat:

``` clojure
{
 :b 1
 :d 3
 :c 2
 :e 4
}
```

A request to messages and its reply:

	[{:role="user", :content="please sort the keys in the map."}
	 {:role="assistant", :content="

diffs for changes:

```diff
--- resources/data.edn
+++ resources/data.edn
@@ ... @@
  :b 1
- :d 3
- :c 2
+ :c 2
+ :d 3
  :e 4
```
"}]
