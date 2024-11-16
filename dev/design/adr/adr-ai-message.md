# ADR: AI Message Component

## Status
Proposed

## Context
- Need a consistent AI message for across the system
- Messages must support different roles (system, user, assistant)
- Must handle file attachments and context
- Must support message thread operations
- Code block extraction needed for automation
- Component should be stateless
- AI provider ports will use this format for their input and return values
- Must support message metadata
- Message context needs to be managed consistently

## Decision
We will:
- Create a dedicated AI Message component responsible for:
  - Message thread construction and validation
  - Message construction and validation
  - Message Thread operations (add message, add context, add response).
    These work immutably.
  - File handling (adding file content to context/messages).  Files will
    be added by specifying the path.  The path is used for the context
    `:name`, and the files content is read to pass as the context
    `:content`.  All files should be text files, so the content can be
    read with `slurp`.
  - Response processing
  - File block extraction from a response.  File blocks are identified
    by markdown code fences.  The file blocks can take the form of plain
    text specifying content, or can be unified diffs.  A single extract
    file blocks function should recognise both of these formats.

	The file name is usually specifed as a comment on the first line
    inside the file block.

	The extract-file-blocks function should take a response as its argument.

	It can use the `#"```(\w+)\n[;#]+\s*(.+?)\n([\s\S]*?)```"` regex.
    This may need to be modified to support new other languages.

  - Provide validator functions for use by other components
    in pre and post conditions. The validator functions are not meant to
    be used for external input checks (violations are programming
    errors, not user errors).

  - Provide schema for use by other components in pre and post condition
    explain calls.

- Use immutable data structures for messages and message threads

- validation should be use malli

- The message formats are:
```clojure
;; Request Message format
{:role [:enum [:user :system :assistant]]  ; The role of the message sender
 :content string?               ; The actual message content
 :name (optional) string?      ; Optional name identifier for the role
}

;; Message Thread format
{:id string?                  ; Unique identifier for the message thread
 :created-at inst?           ; When message thread was created
 :messages [{:role :user/:system/:assistant
            :content string?
            :name (optional) string?}]
 :metadata {:model string?   ; The model being used (e.g. "claude-3-opus-20240229")
           :system-prompt (optional) string?  ; The system prompt used for this message thread
           :context (optional) {:files [{:name string?
                                       :content string?}]}}} ; Any context files provided

;; Response Message format
{:role :assistant              ; Always assistant for responses
 :content string?             ; The response content
 :finish-reason :stop/:length/:content-filter  ; Why the response ended
 :usage {:prompt-chars pos-int?   ; Usage statistics
         :completion-chars pos-int?
         :total-chars pos-int?}}
```

```clojure
;; File block  format:
  {:language string?
   :name string?
   :content  string?
   :metadata {:block-type [:enum [:text :unified-diff]]
               :name       string?}}
```

- Make all operations pure functions
- All functions are fully synchronous
- Validate all inputs and outputs

- there should be no configuration required

- Use trace level logging (in core) for each function implementing an
  interface function
- no metrics will be generated

- note: do not use `Thread`as a shema name, as it conflicts with the
  java Thread class.  Use `MessageThread` instead.

- note: do not use `thread`as a variable or argument name, as it
  conflicts with the the clojure `thread` function.  Use
  `message-thread` instead.

## Error Types
- :error/message-validation
- :error/message-thread-validation
- :error/context-validation
- :error/code-extraction

## Consequences
### Positive
- Clear separation of message handling concerns
- Consistent message and message thread structure
- Pure functional interface
- Rich metadata support
- Flexible context handling
- Clean code block extraction

### Negative
- Need to handle large file content efficiently
- Must validate complex nested structures
- Have to manage context size limits

## Event Taxonomy
- :message/created
- :message/invalid
- :message/added-to-message-thread
- :message/context-added
- :message/file-added
- :message/response-added
- :message/code-extracted

## Validation
- Messages must have required fields
- Roles must be valid keywords
- File paths must exist
- Context must be valid
- Code blocks must have language and content
