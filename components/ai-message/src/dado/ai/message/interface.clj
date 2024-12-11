(ns dado.ai.message.interface
  (:require [dado.ai.message.core :as core]
            [dado.ai.message.model :as model]))

(defn text-content
  "Creates a text content map from a string.
   Returns a content map with :type :text and :text containing the string."
  [text]
  (core/text-content text))

(defn tool-result-content
  "Creates a tool result content map.
   Options:
   - :tool-use-id - ID of the tool use (required)
   - :content - Content of the result (required)
   - :is-error - Boolean indicating if this is an error result (optional)
   Returns a content map for a tool result"
  [options]
  (core/tool-result-content options))

(defn create-message
  "Creates a new message with the specified role and no content.
   Optionally accepts a name for the message sender.
   Returns a validated message map with empty content vector.
   Throws if role is invalid."
  [role & {:keys [name]}]
  (core/create-message role :name name))

(defn message-thread-id
  "Return the ID of a message-thread"
  [message-thread]
  (core/message-thread-id message-thread))

(defn add-message-content
  "Adds content to a message.
   Content must be a valid content map (see text-content, tool-result-content).
   Returns updated message with content added.
   Throws if message or content is invalid."
  [message content]
  (core/add-message-content message content))

(defn create-message-thread
  "Creates a new message thread with the specified model and optional system prompt.
   Returns a validated message thread map with a unique ID and creation timestamp."
  [model & {:keys [system-prompt]}]
  (core/create-message-thread model :system-prompt system-prompt))

(defn update-system-prompt
  "Updates the system prompt for a message thread.
   Returns updated message thread with new system prompt.
   Throws if message thread is invalid."
  [message-thread system-prompt]
  (core/update-system-prompt message-thread system-prompt))

(defn add-message
  "Adds a message to a message thread. The message must have :role and :content keys.
   Returns updated message thread with message appended.
   Throws if message thread or message is invalid."
  [message-thread message]
  (core/add-message message-thread message))

(defn add-context-file
  "Adds a file's content to the message thread context.
   File must exist and be readable.
   If there is no existing sequence in the context files, starts a new sequence.
   If there is an existing sequence, adds to the last sequence.
   Returns updated message thread with file content added to context.
   Throws if file does not exist or message thread is invalid."
  [message-thread file-path]
  (core/add-context-file message-thread file-path))

(defn add-context-file-sequence
  "Adds a new sequence of context files.
   Files must exist and be readable.
   Returns updated message thread with the new file sequence in context.
   Throws if files do not exist or message thread is invalid."
  [message-thread file-sequence]
  (core/add-context-file-sequence message-thread file-sequence))

(defn register-tools
  "Registers tools for use in message thread.
   Tools must conform to AI Tool schema.
   Returns updated message thread with tools registered.
   Throws if message thread or tools are invalid."
  [message-thread tools]
  (core/register-tools message-thread tools))

(defn add-tool
  "Adds a tool to the message thread.
   Returns updated message thread with tool added.
   Throws if message thread or tool are invalid."
  [message-thread tool]
  (core/add-tool message-thread tool))

(defn registered-tools
  "Return the registered tools from the message thread. "
  [message-thread]
  (core/registered-tools message-thread))

(defn set-context-files
  "Set the file contexts on the message thread context.
   Takes a sequence of sequences of file paths.
   Each inner sequence becomes a sequence in the context files.
   All files must exist and be readable.
   Returns updated message thread with all context file contents added.
   Throws if any file does not exist or message thread is invalid."
  [message-thread context-file-sequences]
  (core/set-context-files message-thread context-file-sequences))

(defn add-response
  "Adds an AI response message to the message thread.
   Response must be a valid response message with usage statistics.
   Returns updated message thread with response appended.
   Throws if message thread or response is invalid."
  [message-thread response]
  (core/add-response message-thread response))

(defn extract-tool-calls
  "Extracts tool calls from an AI response message.
   Returns sequence of file block maps containing language, name and content.
   Throws if response is invalid."
  [response]
  (core/extract-tool-calls response))

(defn extract-file-blocks
  "Extracts file blocks from an AI response message.
   Returns sequence of file block maps containing language, name and content.
   Throws if response is invalid."
  [response]
  (core/extract-file-blocks response))

(defn extract-simplified-diffs
  "Extracts simplified diffs from an AI response message.
   Returns string containing all extracted diffs.
   Throws if response is invalid."
  [response]
  (core/extract-simplified-diffs response))

(defn extract-file-operation-directives
  "Extracts File Operation Directive (FOD) diffs from an AI response message.
   Returns string containing all extracted diffs.
   Throws if response is invalid."
  [response]
  (core/extract-file-operation-directives response))

(defn extract-updated-namespaces
  "Extracts updated namespaces list from an AI response message.
   Returns sequence of namespace strings in dependency order.
   Empty sequence if no namespace list found.
   Throws if response is invalid."
  [response]
  (core/extract-updated-namespaces response))


#_(defn add-tool-result
    "Adds a tool execution result to a tool call in the message thread.
   Returns updated message thread with tool result added.
   Throws if message thread or tool call ID is invalid."
    [message-thread tool-call-id result]
    (core/add-tool-result message-thread tool-call-id result))

(defn update-ai-managed-context
  "Updates the AI-managed context files in a message thread.
   Takes a sequence of file paths similar.
   Each value in the sequence is included in the AI-managed context files.
   Returns updated message thread with new AI-managed context files.
   Throws if any file does not exist or message thread is invalid."
  [message-thread context-mod]
  (core/update-ai-managed-context message-thread context-mod))

;; Expose validators for use by other components
(def message? model/message?)
(def message-thread? model/message-thread?)
(def response-message? model/response-message?)
(def file-block? model/file-block?)

;; Expose schemas for other components
(def message-schema model/Message)

(def message-thread-schema model/MessageThread)
(def response-message-schema model/ResponseMessage)
(def file-block-schema model/FileBlock)
(def tool-call-schema model/ToolCall)
