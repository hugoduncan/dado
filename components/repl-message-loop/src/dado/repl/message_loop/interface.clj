(ns dado.repl.message-loop.interface
  "Component for running interactive REPL-based message loops for AI interactions."
  (:require [dado.repl.message-loop.core :as core]))

(defn message-loop
  "Runs interactive message loop starting with given thread.
   Returns final message thread when user enters 'EXIT'.

   Arguments:
   - message-thread: Valid message thread (validated via AI Message component)
   - config: Project configuration map containing required AI provider settings

   Returns:
   - Updated message thread containing all interactions

   Throws:
   - ex-info with :error/message-loop-validation for invalid inputs
   - ex-info with :error/message-loop-io for IO errors
   - ex-info with :error/message-loop-interaction for component interaction errors"
  [message-thread config]
  (core/message-loop message-thread config))
