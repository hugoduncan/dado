(ns dado.repl.message-loop.interface
  "Component for running interactive REPL-based message loops for AI interactions."
  (:require [dado.repl.message-loop.core :as core]))

(defn message-loop
  "Runs interactive message loop starting with given thread.
   Returns final message thread when user enters 'EXIT'.

   The prompt-fn and context-files-fn are called before each AI interaction
   to get the current prompt and context files.

   Arguments:
   - ai-port: Function that takes a message thread and returns an AI response
   - message-thread: Valid message thread (validated via AI Message component)
   - prompt-fn: Function that returns current system prompt string
   - context-files-fn: Function that returns sequence of context file paths

   The ai-port function should:
   - Accept a message thread as its argument
   - Return a response conforming to AI Message response format
   - Handle any provider-specific configuration internally
   - Throw appropriate exceptions for validation/connection errors

   Returns:
   - Updated message thread containing all interactions

   Throws:
   - ex-info with :error/message-loop-validation for invalid inputs
   - ex-info with :error/message-loop-io for IO errors
   - ex-info with :error/message-loop-interaction for component interaction errors"
  [ai-port message-thread prompt-fn context-files-fn]
  (core/message-loop ai-port message-thread prompt-fn context-files-fn))
