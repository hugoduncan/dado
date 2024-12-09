# ADR: Matching File Tool

## Status
Proposed

## Context

- AI tools have a limited context size, and charge on number of input tokens.
- The whole project is too large to fit in the AI context.
- The user wants to minimise explicit context management, and wants
  relevant files automatically placed into the AI context.

## Decision
We will create a Matching File Tool that:

- Finds project files and adds them to context based on content matching
- Tool should be usable by AI during conversation
- Tool must support flexible search across project files
- Should integrate with AI Tool component
- Must handle different matching criteria
- Must prevent searching outside project directory

### Implementation Requirements
1. Search Functionality:
   - Search entire project directory by default using a single unified interface
   - Support two matching modes:
     - `:exact`: Whole word/phrase match
     - `:regex`: Regular expression match
   - Parameters:
     - Required: `pattern` - The search pattern (string)
     - Optional: `mode` - `:exact` or `:regex` (default `:exact`)
     - Optional: `case-sensitive?` - Boolean (default false)
     - Optional: `context-lines` - Number of context lines (default 2)
     - Optional: `max-matches` - Maximum matches to return (default 20)
     - Optional: `extensions` - Vector of file extensions to search (default all)
   - Return up to `max-matches` matches
   - Include 2 lines of context before and after each match
   - Do not include line numbers in output
   - Allow searching in any file extensions specified
   - Return matches across all matching files

2. Validation:
   - All paths must be within project directory
   - Validate search parameters
   - Prevent directory traversal attacks
   - Validate regex patterns

3. Return Value:
   - Return vector of matching file paths
   - Optionally include content excerpts

4. Performance Considerations:
   - Use efficient file searching strategies
   - Avoid reading entire large files into memory

## Error Types
- :error/tool-validation
- :error/file-search
- :error/directory-access
- :error/invalid-search-pattern

## Event Taxonomy
- :matching-file/search-started
- :matching-file/search-completed
- :matching-file/files-found
- :matching-file/search-error

## Consequences

### Positive
- Flexible file searching capabilities
- Integrates with AI Tool component
- Supports multiple search modes
- Prevents unauthorized file access

### Negative
- Potential performance overhead for large projects
- Complex search parameter validation
- Must handle various edge cases in file matching

## Dependencies
- Document Retrieval component
- AI Tool component
- Project Configuration component

## Validation
- Search content must be non-empty string
- File type filters must be valid extensions
- Directory must be within project root
- Regex patterns must be valid

## Notes
- Uses `dado.ai.tools.matching-file` namespace
- Initial implementation focuses on text-based matching based on grep
- Future expansion could include more advanced search capabilities
