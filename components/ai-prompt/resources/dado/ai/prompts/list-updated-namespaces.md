After specifying updates to code files, you should output an Updated
Namespaces List containing all the namespaces that were changed.

The list should be ordered according to the Updated Namespaces List
Format, with each namespace appearing before any namespace that requires
it.

The list should include namespaces in files that were created,
modified, moved or copied.  Namespaces in files that were deleted should
not be included.

The list should be in a markdown code block with the
`updated-namespaces` language marker.

For example, after specifying changes to a `my.project.model` namespace
and a `my.project.core` namespace that requires it, you would output:
