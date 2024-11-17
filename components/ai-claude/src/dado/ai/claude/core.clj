(ns dado.ai.claude.core
  (:require [hato.client :as http]
            [jsonista.core :as j]
            [taoensso.telemere :as t]
            [taoensso.truss :refer [have?]]
            [malli.core :as m]
            [malli.error :as me]
            [dado.ai.claude.model :as model]))

(def ^:private default-api-url "https://api.anthropic.com/v1/messages")
(def ^:private default-model-name "claude-3-opus-20240229")
(def ^:private default-max-tokens 4096)

(defn- to-claude-role [role]
  (name role))

(defn- to-claude-message [{:keys [role content name]}]
  (cond-> {:role (to-claude-role role)
           :content content}
    name (assoc :name name)))

(defn- to-claude-request [message-thread config]
  (t/trace!
   {:id :dado.ai.claude/request-translation}
   (let [{:keys [model-name max-tokens]} config
         {:keys [system-prompt context]} (:metadata message-thread)
         system-content                  (if (seq (:files context))
                                           (vec
                                            (concat
                                             ;; System prompt if present
                                             (when system-prompt
                                               [{:type "text"
                                                 :text system-prompt}])
                                             ;; Each context file
                                             (for [{:keys [name content]} (:files context)]
                                               {:type "text"
                                                :text
                                                (str
                                                 (when name (str"FILE: " name "\n"))
                                                 content)
                                                ;; at most 4 cache-controls per request
                                                ;; :cache_control {:type "ephemeral"}
                                                })))
                                           ;; Just system prompt as string if no files
                                           system-prompt)]
     (cond-> {:model      (or model-name default-model-name)
              :messages   (mapv to-claude-message (:messages message-thread))
              :max_tokens (or max-tokens default-max-tokens)}
       system-content (assoc :system system-content)))))

(defn- from-claude-response [response]
  (t/log! :warn {:response response})
  (let [content (get-in response [:content 0 :text])]
    {:role          :assistant
     :content       content
     :finish-reason :stop ;; TODO: map actual finish reason
     :usage         {:prompt-chars     (get-in response ["usage" "input_tokens"])
                     :completion-chars (get-in response ["usage" "output_tokens"])
                     :total-chars      (+ (get-in response ["usage" "input_tokens"] 0)
                                          (get-in response ["usage" "output_tokens"] 0))}}))

(defn send! [config message-thread]
  ;; Pre-condition for message-thread format - this is internal validation
  {:pre [(have? model/claude-request? (to-claude-request message-thread config)
                :data (me/humanize
                       (m/explain
                        model/ClaudeRequest
                        (to-claude-request message-thread config))))]}
  ;; Validate config first - this is user input
  (when-let [config-errors (m/explain model/ClaudeConfig config)]
    (throw (ex-info "Invalid Claude configuration"
                    {:type    :error/claude-validation
                     :context {:component "dado.ai.claude"
                               :errors    (me/humanize config-errors)}})))

  (let [{:keys [api-key api-url]} config
        request-body              (to-claude-request message-thread config)
        url                       (or api-url default-api-url)]
    (t/trace!
     {:id :dado.ai.claude/api-call}
     (let [response (-> (http/post
                         url
                         {:headers
                          {"x-api-key"         api-key
                           "anthropic-version" "2023-06-01"
                           "content-type"      "application/json"
                           "anthropic-beta"    "prompt-caching-2024-07-31" }
                          :body (j/write-value-as-string request-body)})
                        :body
                        (j/read-value j/keyword-keys-object-mapper))]
       (if (:error response)
         (throw (ex-info "Claude API error"
                         {:type    :error/claude-response
                          :context {:component "dado.ai.claude"
                                    :error     (:error response)}}))
         (from-claude-response response))))))
