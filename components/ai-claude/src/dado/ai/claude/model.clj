(ns dado.ai.claude.model
  (:require [malli.core :as m]))

(def ClaudeRole
  [:enum "user" "assistant"])

(def ClaudeContent
  [:or
   string?
   [:map
    [:type [:= "text"]]
    [:text string?]]])

(def ClaudeSystemContent
  [:map
   [:type [:= "text"]]
   [:text string?]
   [:cache_control {:optional true} [:map [:type [:enum "ephemeral"]]]]])

(def ClaudeMessage
  [:map
   [:role ClaudeRole]
   [:content ClaudeContent]
   [:name {:optional true} string?]])

(def ClaudeRequest
  [:map
   [:model string?]
   [:messages [:vector ClaudeMessage]]
   [:max_tokens {:optional true} pos-int?]
   [:system {:optional true} [:or string? [:vector ClaudeSystemContent]]]
   [:temperature {:optional true} [:double {:min 0.0 :max 1.0}]]])

(def ClaudeConfig
  [:map
   [:api-key string?]
   [:api-url {:optional true} string?]
   [:model-name {:optional true} string?]
   [:max-tokens {:optional true} pos-int?]])

;; Validators
(def claude-config? (m/validator ClaudeConfig))
(def claude-request? (m/validator ClaudeRequest))
