# Simplified Diff Format

The simplified diff format is taken from the Aider project.

It is based on the format produced by `diff -U0`.

- The diff consists of files to change, each with a series of hunks,
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
- Each hunk starts with a `@@ ... @@` line.
  This is followed by a sequence of lines

  Unchanged lines are provided for context.  Each unchanged line is
  prefixed by ` `.

  Deleted lines are prefixed by `-`.

  Added lines are prefixed by `+`.

```diff
--- target/file/path
+++ target/file/path
@@ ... @@
 [unchanged lines]
-[deleted lines]
+[added lines]
 [unchanged lines]
@@ ... @@
 [more unchanged lines]
-[deleted lines]
+[added lines]
 [more unchanged lines]
--- /dev/null
+++ some/new-file-path
@@ ... @@
+[added lines]
```
