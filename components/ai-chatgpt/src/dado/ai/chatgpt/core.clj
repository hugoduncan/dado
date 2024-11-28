(ns dado.ai.chatgpt.core
  (:require
   [clojure.string :as str]
   [dado.ai.chatgpt.model :as model]
   [dado.ai.message.interface :as message]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(def default-api-url "https://api.openai.com/v1/chat/completions")
(def default-model-name "gpt-4o-mini")

(defn- message-content [s]
  {:text s :type "text"})

(defn- convert-message [{:keys [role content]}]
  {:role    (name role)
   :content (if (string? content)
              [{:text content}]
              (when (seq content)
                (mapv (comp  message-content :text) content)))})

(defn- context-content [file]
  (let [{:keys [name content]} file]
    (message-content
     (str "<document>\n<source>" name "</source>\n"
          content "\n</document>"))))

(defn- to-chatgpt-request [message-thread config]
  {:post [(have? model/completion-message? %
                 :data (me/humanize (m/explain model/CompletionMessage %)))]}
  (t/trace!
   {:id :dado.ai.chatgpt/request-translation}
   (let [{:keys [model-name]}        config
         {:keys [messages metadata]} message-thread
         context-files               (get-in metadata [:context :files])]
     {:model    (or model-name default-model-name)
      :messages (vec
                 (concat
                  (when-let [prompt (:system-prompt metadata)]
                    [{:role    "system"
                      :content (vec
                                (concat
                                 [{:text prompt :type "text"}]
                                 (mapv context-content context-files)))}])
                  (mapv convert-message messages)
                  ))})))

(defn- convert-finish-reason [reason]
  (case reason
    "stop"           :stop
    "length"         :length
    "content_filter" :content-filter
    "function_call"  :function-call))

(defn- from-chatgpt-response [response]
  (let [{:keys [choices usage]} response
        choice                  (first choices)]
    {:role          :assistant
     :content       [{:type :text
                      :text (get-in choice [:message :content])}]
     :finish-reason (convert-finish-reason (:finish_reason choice))
     :usage         {:prompt-chars     (:prompt_tokens usage)
                     :completion-chars (:completion_tokens usage)
                     :total-chars      (:total_tokens usage)}}))

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
                          :headers {"content-type"  "application/json"
                                    "Authorization" (str "Bearer " api-key)}
                          :body    (j/write-value-as-string request-body)})
                        :body
                        (j/read-value j/keyword-keys-object-mapper))]
       (if (:error response)
         (throw (ex-info "ChatGPT API error"
                         {:type    :error/chatgpt-response
                          :context {:component "dado.ai.chatgpt"
                                    :error     (:error response)}}))
         (from-chatgpt-response response))))))
