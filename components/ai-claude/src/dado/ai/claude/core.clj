(ns dado.ai.claude.core
  (:require
   [dado.ai.message.interface :as message]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]
   [malli.json-schema :as json-schema]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have have?]]
   [dado.ai.claude.model :as model]))

(def ^:private default-api-url "https://api.anthropic.com/v1/messages")
(def  default-model-name "claude-3-5-sonnet-20241022")
#_(def  default-model-name "claude-3-5-haiku-20241022")
(def ^:private default-max-tokens 8192)

(defn- to-claude-role [role]
  (name role))

(declare to-claude-content)

(defn- to-claude-content-map [content]
  (t/trace!
   {:id :claude/content-map :data {:content content}}
   (cond
     (string? content)
     content

     (vector? content)
     (mapv to-claude-content-map content)

     (= :tool-call (:type content))
     (->  content
          (assoc :type "tool_use"
                 :name (name (:tool content))
                 :input (:parameters content))
          (dissoc :tool :parameters))

     (= :tool-result  (:type content))
     (cond->  (->  content
                   (assoc :type "tool_result"
                          :tool_use_id (:tool-use-id content))
                   (update :content to-claude-content)
                   (dissoc :is-error :tool-use-id))
       (contains? content :is-error) (assoc :is_error (:is-error content) ))

     :else
     (update content :type (fnil name "text")))))

(defn- to-claude-content [content]
  (t/trace!
   {:id :claude/content :data {:content content}}
   (have
    model/claude-message-content?
    (if (string? content)
      content
      (mapv to-claude-content-map content)))))

(defn- to-claude-message [{:keys [role content name] :as message}]
  (t/trace!
   {:id :claude/message :data {:message message}}
   (let [msg (cond-> {:role (to-claude-role role)
                      :content (to-claude-content
                                (have content :data {:message message}))}
               name (assoc :name name))]
     (have model/claude-message?
           msg :data (me/humanize (m/explain model/ClaudeMessage msg))))))

(defn- file-sequence->content-maps
  [add-cache? file-sequence]
  (let [all-but-last (butlast file-sequence)
        last-file    (last file-sequence)]
    (concat
     ;; Convert all but last file in sequence
     (for [{:keys [name content]} all-but-last]
       {:type "text"
        :text (str
               "<document path=\"" name "\">\n"
               content
               "\n</document>")})
     ;; Handle last file, conditionally adding cache control
     (when last-file
       (let [base-content
             {:type "text"
              :text (str
                     "<document path=\"" (:name last-file) "\">\n"
                     (:content last-file)
                     "\n</document>")}]
         [(if add-cache?
            (assoc base-content
                   :cache_control {:type "ephemeral"})
            base-content)])))))

(defn- ->system-content [file-sequences]
  (let [cached-sequences (take 3 file-sequences)
        remaining        (drop 3 file-sequences)
        normal-sequences (butlast remaining)
        last-sequences   [(last remaining)]]
    (->> (concat
          (mapcat #(file-sequence->content-maps true %) cached-sequences)
          (mapcat #(file-sequence->content-maps false %) normal-sequences)
          (mapcat #(file-sequence->content-maps true %) last-sequences))
         vec)))

(defn- to-claude-tool [{:keys [id description parameters]}]
  (t/trace!
   {:id ::to-claude-tool}
   {:name         (name id)
    :description  description
    :input_schema (json-schema/transform parameters)}))

(defn- to-claude-request [message-thread config]
  {:post [(have? model/claude-request? %
                 :data (me/humanize (m/explain model/ClaudeRequest %)))]}
  (t/trace!
   {:id   :dado.ai.claude/request-translation
    :data {:message-thread message-thread}}
   (let [{:keys [model-name max-tokens]} config
         {:keys [system-prompt context tools ai-managed-context]}
         (:metadata message-thread)
         system-content
         (if (seq (:files context))
           (vec
            (concat
             (when system-prompt
               [{:type "text"
                 :text system-prompt}])
             (->system-content
              (cond-> (:files context)
                (seq (:files ai-managed-context))
                (conj (:files context) (:files ai-managed-context))))))
           ;; Just system prompt as string if no files
           system-prompt)]
     (cond-> {:model      (or model-name default-model-name)
              :messages   (mapv to-claude-message (:messages message-thread))
              :max_tokens (or max-tokens default-max-tokens)}
       system-content (assoc :system system-content)
       (seq tools)    (assoc :tools (mapv to-claude-tool tools))))))

(defn- from-claude-content-map [content-map]
  (condp = (:type content-map)
    nil        {:type :text :text (:text content-map)}
    "text"     {:type :text :text (:text content-map)}
    "tool_use" {:type       :tool-call
                :id         (:id content-map)
                :tool       (keyword (:name content-map))
                :parameters (:input content-map)}))

(defn- from-claude-content [content]
  (cond
    (string? content)
    [{:type :text :text content}]
    :else
    (mapv from-claude-content-map content)))

(defn- from-claude-response [response]
  (t/log! :debug {:response response})
  (let [content (from-claude-content (:content response))
        usage   (:usage response)]
    (t/log! {:data {:usage usage}} "Claude token usage")
    (cond-> {:role          :assistant
             :content       content
             :finish-reason (case (:stop_reason response)
                              "tool_use" :tool-call
                              "end_turn" :end-turn
                              "stop" :stop
                              "length" :length
                              "content_filter" :content-filter)
             :usage
             {:prompt-chars     (:input_tokens usage)
              :completion-chars (:output_tokens usage)
              :cache-creation-input-tokens (:cache_creation_input_tokens usage)
              :cache-read-input-tokens (:cache_read_input_tokens usage)
              :total-chars      (+ (:input_tokens usage 0)
                                   (:output_tokens usage 0))}}
      #_#_ (seq tool-calls) (assoc :tool-calls tool-calls))))

(defn- send-request! [http-request-fn request]
  (-> (http-request-fn request)
      :body
      (j/read-value j/keyword-keys-object-mapper)))

(defn send! [config http-request-fn message-thread]
  ;; Pre-condition for message-thread format - this is internal validation
  {:pre [(have? message/message-thread? message-thread
                :data (me/humanize
                       (m/explain
                        message/message-thread-schema
                        message-thread)))]}
  ;; Validate config first - this is user input
  (when-let [config-errors (m/explain model/ClaudeConfig config)]
    (throw (ex-info "Invalid Claude configuration"
                    {:type    :error/claude-validation
                     :context {:component "dado.ai.claude"
                               :errors    (me/humanize config-errors)}})))

  (let [{:keys [api-key api-url]} config
        request-body              (to-claude-request message-thread config)
        url                       (or api-url default-api-url)
        request                   {:url    url
                                   :method :post
                                   :headers
                                   {"x-api-key"         api-key
                                    "anthropic-version" "2023-06-01"
                                    "content-type"      "application/json"
                                    "anthropic-beta"    "prompt-caching-2024-07-31" }
                                   :body   (j/write-value-as-string request-body)}]
    (t/trace!
     {:id :dado.ai.claude/api-call :data {:request request}}
     (let [response (send-request! http-request-fn request)]
       (if (:error response)
         (throw (ex-info "Claude API error"
                         {:type    :error/claude-response
                          :context {:component "dado.ai.claude"
                                    :error     (:error response)}}))
         (from-claude-response response))))))
