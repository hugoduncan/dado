(ns dado.repl.message-loop.core
  (:require [clojure.string :as str]
            [dado.ai.message.interface :as message]
            [dado.ai.tool.interface :as tool]
            [dado.patch.interface :as patch]
            [malli.core :as m]
            [malli.error :as me]
            [taoensso.telemere :as t]
            [taoensso.truss :as truss :refer [have have?]]))

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
      (message/update-system-prompt (prompt-fn))
      (message/set-context-files (context-files-fn))))

(defn- execute-tool-call!
  [tools tool-call]
  (t/trace!
   {:id ::tool-call :data {:tool-call tool-call}}
   (let [tool (have (tools (name (have (:tool tool-call)))))
         {:keys [content is-error] :as result}
         (tool/execute-tool!
          tool
          (have (:parameters tool-call)))]
     (when result
       (message/tool-result-content
        {:tool-use-id (have string? (:id tool-call))
         :content     content
         :is-error    is-error})))))

(defn- execute-tool-calls!
  [msg-thread tool-calls]
  (t/trace!
   {:id ::execute-tool-calls!}
   (let [tools (reduce
                (fn [tools tool]
                  ;; tool in Tool format
                  (assoc tools (have (name (:id tool))) tool))
                {}
                (message/registered-tools msg-thread))]
     (t/event! :tools {:level :debug :data {:tools tools}})
     (->> tool-calls
          (keep (partial execute-tool-call! tools))
          vec))))

(defn- print-response-text! [response]
  (doseq [text (->> response
                    :content
                    (keep :text)
                    vec)]
    (println "-> " text)))

(defn complete-with-tools!
  [msg-thread prompt-fn context-files-fn ai-port]
  {:pre [(have? message/message-thread? msg-thread
                :data (me/humanize
                       (m/explain message/message-thread-schema msg-thread)))]}
  (loop [msg-thread msg-thread]
    (t/event! :message-loop/inner-loop-body)
    (let [_ (t/event! :message-loop/sending-request)

          ;; Refresh context and get AI response
          msg-thread (refresh-thread-context
                      msg-thread
                      prompt-fn
                      context-files-fn)
          _          (t/event! :message-loop/context-refreshed)
          response   (ai-port msg-thread)
          _          (t/event!
                      :message-loop/response-received
                      {:level :debug
                       :data  {:response response}})
          _          (print-response-text! response)
          msg-thread (message/add-response msg-thread response)

          ;; Extract and apply tools
          tool-calls         (message/extract-tool-calls response)
          result-contents    (execute-tool-calls! msg-thread tool-calls)
          simplified-diffs   (message/extract-simplified-diffs response)
          fods               (message/extract-file-operation-directives response)
          updated-namespaces (message/extract-updated-namespaces response) ]

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
      (if (seq result-contents)
        (recur (-> msg-thread
                   (message/add-message
                    (t/trace!
                     {:id        ::add-tool-response-message
                      #_#_:level :warn
                      :data      {:result-contents result-contents}}
                     (reduce
                      (fn [msg message-map]
                        (message/add-message-content msg message-map))
                      (message/create-message :user)
                      result-contents)))))
        msg-thread))))

(defn message-loop
  "Implementation of the interactive message loop.
   See interface ns for docs."
  [ai-port message-thread prompt-fn context-files-fn]
  ;; Validate inputs
  (have? message/message-thread? message-thread
         :data (malli.error/humanize
                (malli.core/explain message/message-thread-schema message-thread)))
  (have? fn? ai-port)
  (have? fn? prompt-fn)
  (have? fn? context-files-fn)

  (t/event! :message-loop/started)

  (loop [msg-thread message-thread]
    (t/event! :message-loop/start-loop-body)
    (let [[action msg-thread]
          (do
            (print "> ")(flush)
            (let [input (get-user-input)]
              (t/event! :message-loop/input {:data {:input input}})
              (cond
                (= input "EXIT")   [:exit msg-thread]
                (str/blank? input) [:skip msg-thread]
                :else
                (do
                  (println input)
                  [:process (message/add-message
                             msg-thread
                             (-> (message/create-message :user)
                                 (message/add-message-content
                                  (message/text-content input))))])))) ]

      (cond
        (= :skip action)
        (recur msg-thread)
        (= :exit action)
        (do
          (t/event! :message-loop/exited)
          msg-thread)
        :else
        (->
         (complete-with-tools! msg-thread prompt-fn context-files-fn ai-port)
         (recur))))))
