(ns dado.ai.chatgpt.model
  (:require
   [malli.core :as m]
   [malli.json-schema :as mj]))

(def FunctionCall
  [:map
   [:name {:description "The name of the function to call."} string?]
   [:arguments
    {:description "Arguments to pass to the function, serialized as a JSON string."}
    string?]])

(def ^:private Content
  [:or
   :string
   [:vector
    [:map
     [:type [:enum "text"]]
     [:text :string]]]])

(def Message
  [:map
   [:role {:description "The role of the message author."}
    [:enum "system" "user" "assistant" "tool"]]
   [:content
    {:description "The content of the message (null for function call messages)."}
    Content]
   [:function_call
    {:optional    true
     :description "Optional function call information if the message involves invoking a tool."}
    FunctionCall]])

(def Function
  [:map
   [:name
    {:description "Name of the function."}
    string?]
   [:description
    {:description "Description of what the function does."}
    string?]
   [:parameters
    {:description "Parameters for the function, specified as a JSON Schema object."}
    map?]])

(def CompletionMessage
  [:map
   {:title "ChatGPT Completions API Schema"}
   [:model
    {:description
     "The identifier of the model to use (e.g., 'gpt-4', 'gpt-3.5-turbo')."}
    string?]
   [:messages
    {:description "A list of messages in the conversation."}
    [:vector Message]]
   [:temperature
    {:optional    true
     :description "Sampling temperature to control randomness."}
    [:and number? [:>= 0] [:<= 2]]]
   [:top_p
    {:optional    true
     :description "Probability mass for nucleus sampling."}
    [:and number? [:>= 0] [:<= 1]]]
   [:n
    {:optional    true
     :description "The number of completions to generate for each input."}
    [:and int? [:>= 1]]]
   [:stop
    {:optional    true
     :description "A stop sequence or list of stop sequences where the model should stop generating further tokens."}
    [:maybe [:or
             string?
             [:vector string?]]]]
   [:max_tokens
    {:optional    true
     :description "The maximum number of tokens allowed in the response."}
    int?]
   [:presence_penalty
    {:optional    true
     :description "Penalty for introducing new topics."}
    [:and number? [:>= -2] [:<= 2]]]
   [:frequency_penalty
    {:optional    true
     :description "Penalty for repeating tokens."}
    [:and number? [:>= -2] [:<= 2]]]
   [:functions
    {:optional    true
     :description "Optional list of functions the model may call."}
    [:vector Function]]
   [:function_call
    {:optional    true
     :description "Specifies how the model should call a function."}
    [:or
     [:enum "auto" "none"]
     [:map
      [:name
       {:description "The name of the function to call."}
       string?]]]]])

(def CompletionResponse
  [:map
   [:id string?]
   [:object [:enum "chat.completion"]]
   [:created int?]
   [:model string?]
   [:choices [:vector
              [:map
               [:index int?]
               [:message [:map
                          [:role [:enum "assistant"]]
                          [:content string?]]]
               [:finish_reason [:enum "stop" "length" "content_filter" "tool_calls"]]]]]
   [:usage [:map
            [:prompt_tokens int?]
            [:completion_tokens int?]
            [:total_tokens int?]]]])

(def ChatGPTConfig
  [:map
   [:api-key :string]
   [:api-url {:optional true} :string]
   [:model-name {:optional true} :string]])

(def chatgpt-config? (m/validator ChatGPTConfig))
(def completion-message? (m/validator CompletionMessage))
