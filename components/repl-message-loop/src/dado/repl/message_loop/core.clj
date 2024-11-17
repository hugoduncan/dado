(ns dado.repl.message-loop.core
  (:require [dado.ai.claude.interface :as claude]
            [dado.ai.message.interface :as ai-message]
            [dado.patch.interface :as patch]
            [taoensso.telemere :as t]
            [taoensso.truss :as truss :refer [have?]]))

(defn- get-user-input
  "Get input from user, return nil if empty/whitespace-only"
  []
  (let [input (read-line)]
    (when-not (clojure.string/blank? input)
      input)))

(defn message-loop
  "Implementation of the interactive message loop.
   See interface ns for docs."
  [message-thread config]
  ;; Validate inputs
  (have? ai-message/message-thread? message-thread
         :data (malli.error/humanize
                (malli.core/explain ai-message/message-thread-schema message-thread)))
  (have? map? config)

  (t/event! :message-loop/started {:config (dissoc config :api-key)})

  (loop [thread message-thread]
    (print "> ")(flush)

    (if-let [input (get-user-input)]
      (if (= input "EXIT")
        (do
          (t/event! :message-loop/exited)
          thread)

        (let [_      (println input)
              ;; Add user message
              thread (ai-message/add-message thread
                                             (ai-message/create-message :user input))
              _      (t/event! :message-loop/message-received)

              ;; Get AI response
              response (claude/send! (-> config :ai-providers :claude) thread)
              _        (println "-> " (:content response))
              _        (t/event! :message-loop/response-processed)

              ;; Extract and apply any diffs
              diff-content (ai-message/extract-diff-blocks response)]

          (when (seq diff-content)
            (patch/apply-patch! diff-content)
            (t/event! :message-loop/diffs-applied))

          ;; Add response and continue loop
          (recur (ai-message/add-response thread response))))

      ;; Empty input, continue loop
      (recur thread))))
