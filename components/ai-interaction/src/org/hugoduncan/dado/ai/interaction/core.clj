(ns org.hugoduncan.dado.ai.interaction.core
  "Core implementation of AI interaction functionality."
  (:require [org.hugoduncan.dado.ai.claude.interface :as claude]
            [org.hugoduncan.dado.ai.thread.interface :as thread]))

(defn update-thread-context
  "Implementation of context updates.
   See interface ns for documentation."
  [thread-id context-docs & {:keys [metadata]}]
  (thread/update-context
   thread-id
   {:documents context-docs
    :metadata  metadata}))

;; ---- Private Helper Functions ----

(defn- format-messages-for-claude
  "Format thread messages for Claude API.
   Converts internal message format to Claude's expected structure."
  [messages]
  (map (fn [{:keys [role content]}]
         {:role    (case role
                     :human     "user"
                     :assistant "assistant"
                     (name role))
          :content content})
       messages))

(defn- get-claude-context
  "Extract relevant context from thread for Claude API.
   Formats context documents and metadata for Claude consumption."
  [thread]
  (when-let [context (:context thread)]
    ;; Format context for Claude...
    ))

;; ---- Error Handling ----

(defn- throw-interaction-error
  "Throws formatted ex-info for interaction errors"
  [error-type thread-id cause]
  (throw (ex-info "AI interaction error"
                  {:type      error-type
                   :thread-id thread-id
                   :cause     cause})))


(defn start-interaction
  "Implementation of interaction start.
   See interface ns for documentation."
  [api-key title message & {:keys [context-docs system metadata]}]
  (let [thread         (thread/create-thread
                        title
                        :context {:documents context-docs}
                        :metadata (merge metadata
                                         {:system system}))
        message-record (thread/add-message
                        (:id thread)
                        :human
                        message
                        :context-refs context-docs)]
    (assoc thread :messages [message-record])))

(defn send-message
  "Implementation of message sending.
   See interface ns for documentation."
  [api-key thread-id message & {:keys [context-docs system metadata]}]
  (let [thread         (thread/get-thread thread-id)
        ;; Update system prompt if provided
        thread         (if system
                         (thread/update-context
                          thread-id
                          {:metadata (assoc (:metadata thread) :system system)})
                         thread)
        ;; Add human message
        updated-thread (thread/add-message
                        thread-id
                        :human
                        message
                        :context-refs context-docs
                        :metadata metadata)
        ;; Get thread history for context
        messages       (thread/get-messages thread-id)
        ;; Send to Claude
        response       (claude/create-message
                        api-key
                        {:messages (format-messages-for-claude messages)
                         :system   (get-in thread [:metadata :system])
                         :context  (get-claude-context thread)})
        ;; Add AI response to thread
        final-thread   (thread/add-message
                        thread-id
                        :assistant
                        (:content response)
                        :metadata (:metadata response))]
    final-thread))


;; Example usage:
#_
(comment
  (def api-key "my-claude-key")

  ;; Start new interaction
  (def thread
    (start-interaction
     api-key
     "Refactor Auth System"
     "How should we restructure the auth system?"
     :context-docs ["adr/auth-system.md"]
     :system "You are a software architect..."))

  ;; Send follow-up with updated system prompt
  (send-message
   api-key
   (:id thread)
   "What about the session management?"
   :context-docs ["src/auth/session.clj"]
   :system "You are a security expert..."))
