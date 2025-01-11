(ns dado.ai.openai.core
  (:require
   [clojure.string :as str]
   [dado.ai.openai.model :as model]
   [dado.ai.message.interface :as message]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]
   [malli.json-schema :as json-schema]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]   ))

(def default-api-url "https://api.openai.com/v1/chat/completions")
(def default-model-name "gpt-4o-mini")

(defn- message-content [s]
  {:text s :type "text"})

(defn- ->chatgpt-tool-call
  [tool-call]
  {:type     "function"
   :id       (:id tool-call)
   :function {:name      (name (:tool tool-call))
              :arguments (:parameters tool-call)}})

(defn- ->chatgpt-content
  [s-or-v]
  (if (string? s-or-v)
    (message-content s-or-v)
    (mapv (comp message-content :text) s-or-v)))

(defn- ->chatgpt-tool-result [tool-result]
  (->chatgpt-content (:content tool-result)))

(defn- convert-message [{:keys [role content]}]
  (let [tool-calls   (->> content
                          (filterv #(= :tool-call (:type %)))
                          ;; remove psuedo calls from <tool_call> tags
                          (filterv #(not= "missing" (:id %)))
                          seq)
        tool-results (seq (filterv #(= :tool-result (:type %)) content))
        text-maps    (seq (filterv
                           #((some-fn (partial = :text) nil?) (:type %))
                           content))]
    (t/trace!
     {:id   ::convert-message
      :data {:tool-calls   tool-calls
             :tool-results tool-results
             :text-maps    text-maps}}
     (cond
       (seq tool-calls)
       {:role       (name role)
        :content    (:text (first text-maps))
        :tool_calls (mapv ->chatgpt-tool-call tool-calls)}
       (seq tool-results)
       {:role         "tool"
        :tool_call_id (:tool-use-id (first tool-results))
        :content      (str/join
                       ", "
                       (mapv :text (:content (first tool-results))))
        }
       :else
       (cond-> {:role    (name role)
                :content []}
         (string? content)
         (update :content conj (message-content content))
         (seq content)
         (update :content into (mapv (comp  message-content :text) text-maps)))))))

(defn- context-content [file]
  (let [{:keys [name content]} file]
    (message-content
     (str "<document>\n<source>" name "</source>\n"
          content "\n</document>"))))

(defn- to-chatgpt-tool [{:keys [id description parameters]}]
  (t/trace!
   {:id ::to-claude-tool}
   {:type "function"
    :function
    {:name        (name id)
     :description description
     :parameters  (json-schema/transform parameters)
     :strict      true}}))

(defn- to-chatgpt-request [message-thread config]
  {:post [(have? model/completion-message? %
                 :data (me/humanize (m/explain model/CompletionMessage %)))]}
  (t/trace!
   {:id   :dado.ai.chatgpt/request-translation
    :data {:message-thread message-thread}}
   (let [{:keys [model-name]}        config
         {:keys [messages metadata]} message-thread
         context-files               (get-in metadata [:context :files])
         supports-tools?             (:supports-tools? config true)]
     {:model    (or model-name default-model-name)
      :messages (vec
                 (concat
                  (when-let [prompt (:system-prompt metadata)]
                    [{:role    "system"
                      :content (vec
                                (concat
                                 [{:text prompt :type "text"}]
                                 (mapcat
                                  #(mapv context-content %)
                                  context-files)))}])
                  (mapv convert-message messages)))
      :tools    (mapv to-chatgpt-tool (:tools metadata))})))

(defn- convert-finish-reason [reason]
  (case reason
    "stop"           :stop
    "length"         :length
    "content_filter" :content-filter
    "function_call"  :function-call))

(defn- read-chatgpt-tool-call [{:keys [id _type function]}]
  {:type       :tool-call
   :id         id
   :tool       (keyword (:name function))
   :parameters (:arguments function)})

(defn- parse-tool-call
  [s]
  (when (string? s)
    (let [re          #"(?s)<tool_call>(.*?)</tool_call>"
          json-string (-> (re-matches re s) second)]
      (t/event! ::parse-tool-call
                {:data {:s           s
                        :json-string json-string}})
      (when json-string
        (let [tool-call (try
                          (t/trace!
                           {:id   ::parse-tool-call
                            :data {:json-string json-string}}
                           (j/read-value
                            json-string
                            j/keyword-keys-object-mapper))
                          (catch Exception _
                            nil))]
          (t/event! ::parse-tool-call {:data {:tool-call tool-call}})
          (when tool-call
            (if (:function tool-call)
              (read-chatgpt-tool-call tool-call)
              {:type       :tool-call
               :id         "missing"
               :tool       (keyword (:name tool-call))
               :parameters (:arguments tool-call)})))))))

(defn- choice->content-maps
  [{:keys [message] :as choice}]
  (let [message-as-tool-call (parse-tool-call (:content message))]
    (t/event! ::choice->content-maps
              {:data {:choice               choice
                      :message-as-tool-call message-as-tool-call}})
    (cond-> []
      (not (str/blank? (:content message)))
      (conj {:type :text :text (:content message)})
      (seq (:tool_calls message))
      (into (mapv read-chatgpt-tool-call (:tool_calls message)))
      message-as-tool-call
      (conj message-as-tool-call))))

(defn- from-chatgpt-response [response]
  (t/trace!
   {:id   ::from-chatgpt-response
    :data {:response response}}
   (let [{:keys [choices usage]} response
         choice                  (first choices)]
     {:role          :assistant
      :content       (choice->content-maps choice)
      :finish-reason (convert-finish-reason (:finish_reason choice))
      :usage         {:prompt-chars     (:prompt_tokens usage)
                      :completion-chars (:completion_tokens usage)
                      :total-chars      (:total_tokens usage)}})))

(defn send! [config http-request-fn message-thread]
  ;; Pre-condition for message-thread format - this is internal validation
  {:pre [(have? message/message-thread? message-thread
                :data (me/humanize
                       (m/explain
                        message/message-thread-schema
                        message-thread)))]}
  ;; Validate config first - this is user input
  (when-not  (model/chatgpt-config? config)
    (throw (ex-info
            "Invalid ChatGPT configuration"
            {:type    :error/chatgpt-validation
             :context {:component "dado.ai.chatgpt"
                       :errors    (me/humanize
                                   (m/explain model/ChatGPTConfig config))}})))

  (let [{:keys [api-key api-url]} config
        request-body              (to-chatgpt-request message-thread config)
        url                       (or api-url default-api-url)]
    (t/trace!
     {:id :dado.ai.chatgpt/api-call}
     (let [response (-> (http-request-fn
                         {:url     url
                          :method  :post
                          :headers {"Content-Type"  "application/json"
                                    "Authorization" (str "Bearer " api-key)}
                          :body    (j/write-value-as-string request-body)})
                        :body
                        (j/read-value j/keyword-keys-object-mapper))]
       (if (:error response)
         (throw (ex-info "ChatGPT API error"
                         {:type    :error/chatgpt-response
                          :context {:component    "dado.ai.chatgpt"
                                    :response     response
                                    :request-body request-body}}))
         (from-chatgpt-response response))))))
