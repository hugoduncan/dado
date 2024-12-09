# ADR Document Retrieval Component


## Context

- Documents exist in the project directories.
- A particular AI interaction requires interaction relevant context.
- Need flexible, powerful document search capabilities
- Must support complex filtering across project documents

## Decision
We will create a Document Retrieval component that:

### Component Interface
```clojure
(search-documents
  [search-params]
  "Searches project documents based on comprehensive criteria.

   search-params map may include:
   - :directories     [string]     ; Specific directories to search
   - :file-types      [:clj :md :edn]  ; File extensions to include
   - :name-patterns   [string]     ; Regex patterns for filename matching
   - :content-search  string       ; Text to search within documents
   - :specificity     [:low :medium :high]  ; Document specificity level
   - :precision       [:low :medium :high]  ; Document precision level
   - :component       string       ; Component name to filter
   - :type            [:interface :implementation :test :adr]
   - :max-results     int          ; Maximum number of results to return
   - :sort-by         [:relevance :specificity :precision :modified-date]

   Returns:
   [{:path string
     :content string
     :metadata {:specificity keyword
                :precision keyword
                :component string
                :type keyword
                :modified-date inst?}}]")
```

### Metadata Management
- Each document will have associated metadata:
  ```clojure
  (def DocumentMetadata
    [:map
     [:specificity [:enum :low :medium :high]]
     [:precision [:enum :low :medium :high]]
     [:component {:optional true} string?]
     [:type [:enum :interface :implementation :test :adr :overview]]
     [:modified-date inst?]
     [:file-size int?]
     [:lines-of-content int?]])
  ```

### Relevance Calculation
- Relevance determined by weighted factors:
  1. Content match percentage
  2. Metadata match
  3. Proximity to current project context
  4. Document type hierarchy

### Search Strategies
- Recursive directory search
- Configurable search depth
- Caching of document metadata
- Lazy loading of document contents

### Error Handling
- Return empty collection if no documents match
- Throw specific exceptions for:
  - Invalid search parameters
  - Inaccessible directories
  - Permission issues

## Error Types
- :error/invalid-search-params
- :error/directory-access
- :error/permission-denied

## Event Taxonomy
- :document/search-initiated
- :document/search-completed
- :document/metadata-indexed
- :document/cache-updated

## Consequences
### Positive
- Flexible document retrieval
- Supports complex search scenarios
- Metadata-driven search
- Performance-aware design

### Negative
- Complex relevance calculation
- Potential performance overhead
- Metadata maintenance required

## Validation
- All search parameters must be validated
- Metadata must conform to defined schema
- Search results limited to project directory
- No access to files outside project scope

## Dependencies
- Babashka filesystem library for traversal
- Malli for schema validation
- Telemere for logging/metrics

## Notes
- Uses `dado.document.retrieval` namespace
- Initial implementation focuses on project design documents
- Future expansion to include more sophisticated search
