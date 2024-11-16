(ns org.hugoduncan.dado.ai.interaction.interface
  "Public interface for AI interactions.
   Coordinates between thread management and AI provider APIs."
  (:require [org.hugoduncan.dado.ai.interaction.core :as core]))

(defn start-interaction
  "Start a new AI interaction thread.

   Parameters:
   api-key      - Provider API key
   title        - Thread title
   message      - Initial message content
   options      - Optional map with:
                 :context-docs - Vector of document paths to include
                 :system      - System prompt for the AI
                 :metadata    - Additional thread metadata

   Returns thread record with initial message."
  [api-key title message & {:keys [context-docs system metadata]}]
  (core/start-interaction api-key title message
                          :context-docs context-docs
                          :system system
                          :metadata metadata))

(defn send-message
  "Send a message to the AI and update thread with response.

   Parameters:
   api-key      - Provider API key
   thread-id    - Thread identifier
   message      - Message content
   options      - Optional map with:
                 :context-docs - Additional context documents
                 :system      - Updated system prompt
                 :metadata    - Message metadata

   Returns updated thread record with new messages."
  [api-key thread-id message & {:keys [context-docs system metadata]}]
  (core/send-message api-key thread-id message
                     :context-docs context-docs
                     :system system
                     :metadata metadata))

(defn update-thread-context
  "Update context for a thread.

   Parameters:
   thread-id     - Thread identifier
   context-docs  - Vector of document paths
   options       - Optional map of additional context metadata

   Returns updated thread record."
  [thread-id context-docs & {:keys [metadata]}]
  (core/update-thread-context thread-id context-docs :metadata metadata))
