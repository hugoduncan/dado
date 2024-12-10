(ns dado.ai.ollama.core
  (:require
   [clojure.string :as str]
   [dado.ai.message.interface :as message]
   [dado.ai.ollama.model :as model]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have?]]))

(def ^:private default-api-url "http://localhost:11434/api/chat")
(def ^:private default-model-name "llama3.2:latest")

(defn- to-ollama-role [role]
  (name role))

(defn- to-ollama-message [{:keys [role content]}]
  {:role (to-ollama-role role)
   :content (if (string? content)
              content
              (->> content
                   (map :text)
                   (clojure.string/join "\n")))})

(defn- context-files->prompt-string
  "Convert context file sequences to a string"
  [file-sequences]
  (str/join
   "\n"
   (for [files                  file-sequences
         {:keys [name content]} files]
     (t/trace!
      {:id   :ollama/context-files->messages
       :data {:file (str name)}}
      (str "<document path=\"" name "\">\n" content "\n</document>")))))

(defn- to-ollama-request [message-thread config]
  {:post [(have? model/ollama-request?
                 % :data (me/humanize (m/explain model/OllamaRequest %)))]}
  (t/trace!
   {:id   :dado.ai.ollama/request-translation
    :data {:message-thread message-thread}}
   (let [{:keys [model-name]}        config
         {:keys [messages metadata]} message-thread
         system-prompt               (get metadata :system-prompt)
         context-files               (get-in metadata [:context :files])

         messages
         (cond-> []
           ;; Add system prompt as first message if present
           system-prompt (conj
                          {:role "system"
                           :content
                           (str
                            system-prompt
                            "\n Use this data in constructing your reply:"
                            (context-files->prompt-string context-files))})
           ;; Add context files as user messages

           ;; Add conversation messages
           true (into (mapv to-ollama-message messages)))]
     {:model    (or model-name default-model-name)
      :messages messages
      :stream   false})))

(defn- from-ollama-response [response]
  (t/trace!
   {:id :ollama/response :data {:response response}}
   {:role          :assistant
    :content       [{:type :text
                     :text (get-in response [:message :content])}]
    :finish-reason :end-turn
    :usage         {:prompt-chars     (:prompt_eval_count response)
                    :completion-chars (:eval_count response)
                    :total-chars      (+ (:prompt_eval_count response 0)
                                         (:eval_count response 0))}}))

(defn send! [config http-request-fn message-thread]
  ;; Pre-condition for message-thread format - this is internal validation
  {:pre [(have? message/message-thread? message-thread
                :data (me/humanize
                       (m/explain
                        message/message-thread-schema
                        message-thread)))]}
  ;; Validate config first - this is user input
  (when-not  (model/ollama-config? config)
    (throw (ex-info
            "Invalid Ollama configuration"
            {:type    :error/ollama-validation
             :context {:component "dado.ai.ollama"
                       :errors    (me/humanize
                                   (m/explain model/OllamaConfig config))}})))

  (let [{:keys [api-url]} config
        request-body      (to-ollama-request message-thread config)
        url               (or api-url default-api-url)]
    (t/trace!
     {:id :dado.ai.ollama/api-call}
     (let [response (-> (http-request-fn
                         {:url     url
                          :method  :post
                          :headers {"content-type" "application/json"}
                          :body    (j/write-value-as-string request-body)})
                        :body
                        (j/read-value j/keyword-keys-object-mapper))]
       (if (:error response)
         (throw (ex-info "Ollama API error"
                         {:type    :error/ollama-response
                          :context {:component "dado.ai.ollama"
                                    :error     (:error response)}}))
         (from-ollama-response response))))))
