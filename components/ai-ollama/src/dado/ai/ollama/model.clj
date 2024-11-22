(ns dado.ai.ollama.model
  (:require [malli.core :as m]))

(def OllamaRole
  [:enum "user" "assistant" "system"])

(def OllamaMessage
  [:map
   [:role OllamaRole]
   [:content :string]])

(def OllamaRequest
  [:map
   [:model :string]
   [:messages [:vector OllamaMessage]]])

(def OllamaConfig
  [:map
   [:model-name {:optional true} :string]
   [:api-url {:optional true} :string]])

(def OllamaResponse
  [:map
   [:message [:map
             [:role [:= "assistant"]]
             [:content :string]]]
   [:done :boolean]
   [:model :string]
   [:created_at :string]
   [:total_duration :int]
   [:load_duration :int]
   [:prompt_eval_count :int]
   [:eval_count :int]
   [:eval_duration :int]])

;; Create validators
(def ollama-config? (m/validator OllamaConfig))
(def ollama-request? (m/validator OllamaRequest))
