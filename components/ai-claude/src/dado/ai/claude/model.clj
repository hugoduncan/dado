(ns dado.ai.claude.model
  (:require [malli.core :as m]
            [dado.ai.tool.model :as tool]))

(def ClaudeRole
  [:enum "user" "assistant"])

(def ClaudeContent
  [:map
   [:type {:optional true} [:= "text"]]
   [:text :string]])

(def ClaudeToolCall
  [:map
   [:type [:= "tool_use"]]
   [:id :string]
   [:name :string]
   [:input map?]])

(def ClaudeToolResult
  [:map
   [:type [:= "tool_result"]]
   [:tool_use_id :string]
   [:content {:optional true} [:or :string [:vector ClaudeContent] [:= []]]]
   [:is_error {:optional true} :boolean]])

(def ClaudeMessageContent
  [:or
   :string
   [:vector [:or ClaudeContent ClaudeToolCall ClaudeToolResult]]
   [:= []]])

(def ClaudeMessage
  [:map
   [:role ClaudeRole]
   [:content ClaudeMessageContent]
   [:name {:optional true} :string]])

(def ClaudeSystemContent
  [:map
   [:type {:optional true} [:= "text"]]
   [:text :string]
   [:cache_control {:optional true} [:map [:type [:enum "ephemeral"]]]]])

(def ClaudeTool
  [:map
   [:name :string]
   [:description :string]
   [:input_schema any?]])

(def ClaudeRequest
  [:map
   [:model :string]
   [:messages [:vector [:or ClaudeMessage ClaudeToolResult]]]
   [:max_tokens {:optional true} pos-int?]
   [:system {:optional true} [:or :string [:vector ClaudeSystemContent]]]
   [:temperature {:optional true} [:double {:min 0.0 :max 1.0}]]
   [:tools {:optional true} [:vector ClaudeTool]]])

(def ClaudeResponse
  [:map
   [:type [:= "message"]]
   [:role [:= "assistant"]]
   [:content [:or
              :string
              [:vector
               [:or ClaudeContent ClaudeToolCall]]]]
   [:stop_reason :string]
   [:stop_sequence any?]
   [:usage [:map
            [:input_tokens :int]
            [:cache_creation_input_tokens {:optional true} :int]
            [:cache_read_input_tokens {:optional true} :int]
            [:output_tokens :int]]]])

(def ClaudeConfig
  [:map
   [:api-key :string]
   [:api-url {:optional true} :string]
   [:model-name {:optional true} :string]
   [:max-tokens {:optional true} pos-int?]])

;; Validators
(def claude-config? (m/validator ClaudeConfig))
(def claude-request? (m/validator ClaudeRequest))
(def claude-message? (m/validator ClaudeMessage))
(def claude-message-content? (m/validator ClaudeMessageContent))
