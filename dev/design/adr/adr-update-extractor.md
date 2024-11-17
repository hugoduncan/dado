# ADR: Update Extractor Component

## Status
Proposed

## Context
- Need to extract update blocks from text content
- Initially focused on diff updates, designed for future expansion
- Component will be used by AI Message component
- Component should be stateless
- Updates will be contained in markdown code blocks
- Different update types may be added in the future
- Initial implementation specific to diffs

## Decision
We will:
- Create a dedicated Update Extractor component responsible for:
  - Extracting updates from markdown code blocks
  - Initially implementing diff extraction
  - Providing extracted content in a structured format
  - Supporting future update types through new functions

- Implementation Requirements:
  - Use trace level logging for all operations
  - String validation as pre-condition using truss have?
  - No partial extraction on error - fail fast
  - Language identifier from code fence should be discarded

- Component Interface:
  ```clojure
  (extract-diffs [text]
    "Extracts all diff code blocks from text, returns their content as a string.
     Diff blocks are identified by markdown code fence markers.
     Returns empty string if no diff blocks found.
     Throws ex-info with :error/update-extraction if malformed blocks found.")
  ```

- Extraction rules:
  - Look for markdown code blocks marked with 'diff' language
  - Include entire content between opening and closing code fence markers
  - Concatenate multiple diff blocks if present
  - Return empty string if no diff blocks found
  - No inspection or validation of block contents

- Future expansion:
  - Deferred to future ADRs

- Logging Events:
  At trace level:
  - :update/extraction-started - Include first few lines of input text
  - :update/extraction-completed - Include count of blocks found

- Validation:
  - Input text must be a string (pre-condition)

## Error Types
- :error/update-extraction

## Event Taxonomy
- :update/extraction-started
- :update/extraction-completed

## Consequences
### Positive
- Clear, single responsibility functions
- Pure functional interface
- Simple text-based integration
- No state management needed
- Easy to add new update types
- Focused initial implementation

### Negative
- Must handle text processing carefully
- Need consistent approach across update types
- Must maintain extraction pattern documentation

## Dependencies
- Error handling system
- Logging system

## Notes
- Future update types will be added as new functions
- Core extraction logic can be reused internally
- Each update type maintains its own documentation# ADR: Update Extractor Component
