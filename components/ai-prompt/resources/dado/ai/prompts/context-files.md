Context files are specified using XML `<document>` tags with a `path`
attribute.  The content between the opening and closing tags is the file
content. The path attribute contains the path to the document on the
filesystem.

For example:
<document path="path/to/file.ext">
File content goes here,
possibly spanning multiple lines.
</document>

Information in the system prompt is authoritative over content in the
chat messages.  If there is conflicting information, take it from the
system prompt, and ignore the version in the chat.
