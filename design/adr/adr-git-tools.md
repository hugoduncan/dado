# ADR: Git Tools Collection

## Status
Proposed

## Context
- Need to provide Git operations through AI tools interface
- Must support common Git operations safely
- Must prevent destructive operations
- Initially focus on staging and committing changes
- Must integrate with existing AI Tool component
- Repository state changes must be predictable

## Decision
We will create a collection of Git tools that:

### Tools Overview

1. Git Stage Tool
```clojure
{:id          :dado/git-stage
 :name        "Git Stage Tool"
 :description "Stage modified files in git repository"
 :parameters  [:map
              [:paths {:optional true} 
               [:vector :string]]  ; If not provided, stages all modified files
              [:force? {:optional true} :boolean]]  ; Force add untracked files
 :returns     {:type :map
              :description "Map of staged files and any errors"}}
```

2. Git Commit Tool
```clojure
{:id          :dado/git-commit
 :name        "Git Commit Tool"
 :description "Commit staged changes"
 :parameters  [:map
              [:message :string]  ; Required commit message
              [:allow-empty? {:optional true} :boolean]]
 :returns     {:type :map
              :description "Map containing commit result"}}
```

3. Git Status Tool
```clojure
{:id          :dado/git-status
 :name        "Git Status Tool"
 :description "Get repository status"
 :parameters  [:map]  ; No parameters required
 :returns     {:type :map
              :description "Map of modified, staged, and untracked files"}}
```

### Implementation Requirements

1. General Requirements:
   - All tools must validate repository state before execution
   - Tools must be safe to use in automated contexts
   - Clear error reporting for all operations
   - Must prevent operations outside repository
   - Must validate inputs thoroughly

2. Tool-Specific Requirements:

   Git Stage Tool:
   - Support staging specific files or all modified files
   - Validate file paths exist and are in repository
   - Report any files that couldn't be staged
   - Optional force flag for untracked files

   Git Commit Tool:
   - Require non-empty commit message
   - Validate staged changes exist (unless allow-empty flag)
   - Return commit hash and summary
   - Support conventional commit format validation

   Git Status Tool:
   - Fast, non-blocking status check
   - Categorized file listings
   - Include basic branch information
   - Return structured data format

3. Error Handling:
   - :error/not-git-repo - Not in git repository
   - :error/git-operation - Git command failed
   - :error/invalid-paths - Invalid file paths
   - :error/no-changes - No changes to commit
   - :error/validation - Parameter validation failed

4. Return Value Formats:
```clojure
;; Stage Tool
{:staged ["path/to/file1" "path/to/file2"]
 :errors [{:path "bad/path" :reason "File not found"}]}

;; Commit Tool
{:commit {:hash "abc123"
         :message "feat: add new feature"
         :summary {:files 2
                  :insertions 10
                  :deletions 5}}}

;; Status Tool
{:branch "main"
 :modified ["file1" "file2"]
 :staged ["file3"]
 :untracked ["new-file"]
 :ahead 0
 :behind 0}
```

## Event Taxonomy
- :git/stage-started
- :git/stage-completed
- :git/commit-started
- :git/commit-completed
- :git/status-checked
- :git/operation-failed

## Consequences

### Positive
- Safe Git operations through AI interface
- Clear operation boundaries
- Structured return values
- Good error reporting
- Status visibility

### Negative
- Must maintain Git command compatibility
- Need careful error handling
- Have to validate repository state
- Limited to basic operations initially

### Neutral
- May need to add more tools later
- Could extend commit validation
- Might need branch operations

## Dependencies
- AI Tool component for registration
- Git command line tools
- Shell execution capability

## Future Considerations
- Branch management tools
- Pull/push operations
- Conflict resolution
- Interactive staging
- Commit message templates
- Hook integration

## Notes
- Uses dado.git.tools namespace
- Focuses on safe, common operations
- Maintains structured data interface
- Supports automation use cases
