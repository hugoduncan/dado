(ns dado.ai.ollama.core
  (:require
   [dado.ai.message.interface :as message]
   [dado.ai.ollama.model :as model]
   [hato.client :as http]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.error :as me]
   [taoensso.telemere :as t]
   [taoensso.truss :refer [have have?]]))

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

(defn- to-ollama-request [message-thread config]
  {:post [(have? model/ollama-request?
                 % :data (me/humanize (m/explain model/OllamaRequest %)))]}
  (t/trace!
   {:id   :dado.ai.ollama/request-translation
    :data {:message-thread message-thread}}
   (let [{:keys [model-name]} config
         messages             (mapv to-ollama-message (:messages message-thread))]
     {:model    (or model-name default-model-name)
      :messages messages
      :stream   false})))

(defn- from-ollama-response [response]
  (t/trace!
   {:id :ollama/response :level :warn :data {:response response}}
   {:role          :assistant
    :content       [{:type :text
                     :text (get-in response [:message :content])}]
    :finish-reason :end-turn
    :usage         {:prompt-chars     (:prompt_eval_count response)
                    :completion-chars (:eval_count response)
                    :total-chars      (+ (:prompt_eval_count response 0)
                                         (:eval_count response 0))}}))

(defn send! [config message-thread]
  ;; Pre-condition for message-thread format - this is internal validation
  {:pre [(have? message/message-thread? message-thread
                :data (me/humanize
                       (m/explain
                        message/message-thread-schema
                        message-thread)))]}
  ;; Validate config first - this is user input
  (when-let [config-errors (m/explain model/OllamaConfig config)]
    (throw (ex-info "Invalid Ollama configuration"
                    {:type    :error/ollama-validation
                     :context {:component "dado.ai.ollama"
                             :errors    (me/humanize config-errors)}})))

  (let [{:keys [api-url]} config
        request-body      (to-ollama-request message-thread config)
        url              (or api-url default-api-url)]
    (t/trace!
     {:id :dado.ai.ollama/api-call}
     (let [response (-> (http/post
                        url
                        {:headers {"content-type" "application/json"}
                         :body    (j/write-value-as-string request-body)})
                       :body
                       (j/read-value j/keyword-keys-object-mapper))]
       (if (:error response)
         (throw (ex-info "Ollama API error"
                         {:type    :error/ollama-response
                          :context {:component "dado.ai.ollama"
                                  :error     (:error response)}}))
         (from-ollama-response response))))))
