(ns dado.repl.message-loop.core
  (:require [dado.ai.claude.interface :as claude]
            [dado.ai.message.interface :as ai-message]
            [dado.ai.message.interface :as msg]
            [dado.patch.interface :as patch]
            [taoensso.telemere :as t]
            [taoensso.truss :as truss :refer [have have?]]
            [dado.ai.tool.interface :as tool]))

(defn- get-user-input
  "Get input from user, return nil if empty/whitespace-only"
  []
  (let [input (read-line)]
    (when-not (clojure.string/blank? input)
      input)))

(defn- reload-updated-namespaces!
  "Reloads the given sequence of updated namespace names.
   Prints status for each namespace reload attempt.
   Continues even if some reloads fail."
  [updated-namespaces]
  (doseq [ns-name updated-namespaces]
    (try
      (require (symbol ns-name) :reload)
      (println "Reloaded namespace:" ns-name)
      (catch Throwable t
        (println "Failed to reload namespace:" ns-name)
        (println "Error:" (.getMessage t)))))
  (t/event! :message-loop/namespaces-reloaded))

(defn- refresh-thread-context
  "Updates thread with current prompt and context files"
  [thread prompt-fn context-files-fn]
  (-> thread
      (ai-message/update-system-prompt (prompt-fn))
      (ai-message/set-context-files (context-files-fn))))

(defn- execute-tool-calls!
  [msg-thread tool-calls]
  (let [tools (reduce
               (fn [tools tool]
                 (assoc tools (have (:id tool)) tool))
               {}
               (ai-message/registered-tools msg-thread))]
    (doseq [tool-call tool-calls]
      (let [tool (have (tools (have (:id tool-call))))]
        (tool/execute-tool! tool (have (:parameters tool-call)))))))

(defn message-loop
  "Implementation of the interactive message loop.
   See interface ns for docs."
  [config message-thread prompt-fn context-files-fn]
  ;; Validate inputs
  (have? ai-message/message-thread? message-thread
         :data (malli.error/humanize
                (malli.core/explain ai-message/message-thread-schema message-thread)))
  (have? map? config)
  (have? fn? prompt-fn)
  (have? fn? context-files-fn)

  (t/event! :message-loop/started {:config (dissoc config :api-key)})

  (loop [msg-thread message-thread]
    (print "> ")(flush)

    (if-let [input (get-user-input)]
      (if (= input "EXIT")
        (do
          (t/event! :message-loop/exited)
          msg-thread)

        (let [_          (println input)
              ;; Add user message
              msg-thread (ai-message/add-message
                          msg-thread
                          (ai-message/create-message :user input))
              _          (t/event! :message-loop/message-received)

              ;; Refresh context and get AI response
              thread   (refresh-thread-context msg-thread prompt-fn context-files-fn)
              _        (t/event! :message-loop/context-refreshed)
              response (claude/send! (-> config :ai-providers :claude) thread)
              _        (println "-> " (:content response))
              _        (t/event! :message-loop/response-processed)

              ;; Extract and apply tools
              tool-calls         (ai-message/extract-tool-calls response)
              simplified-diffs   (ai-message/extract-simplified-diffs response)
              fods               (ai-message/extract-file-operation-directives response)
              updated-namespaces (ai-message/extract-updated-namespaces response)]

          ;; Reload any updated namespaces
          (execute-tool-calls! msg-thread tool-calls)

          (when (seq simplified-diffs)
            (patch/apply-simplified-diff-patch! simplified-diffs)
            (t/event! :message-loop/diffs-applied))

          (when (seq fods)
            (patch/apply-fod-diff-patch! fods)
            (t/event! :message-loop/diffs-applied))

          ;; Reload any updated namespaces
          (when (seq updated-namespaces)
            (reload-updated-namespaces! updated-namespaces))

          ;; Add response and continue loop
          (recur (ai-message/add-response thread response))))

      ;; Empty input, continue loop
      (recur msg-thread))))
