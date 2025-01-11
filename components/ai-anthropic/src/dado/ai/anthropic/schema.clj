(ns org.hugoduncan.dado.ai.claude.schema
  "Malli schemas for Claude API data structures."
  (:require [malli.core :as m]))

(def Role
  [:enum "user" "assistant"])

(def Config
  [:map
   [:api-key string?]
   [:base-url {:optional true} string?]])

(def Message
  [:map
   [:role Role]
   [:content string?]])

(def Messages
  [:vector Message])

(def MessageRequest
  [:map
   [:model string?]
   [:messages Messages]
   [:max-tokens {:optional true} pos-int?]
   [:temperature {:optional true} [:double {:min 0.0 :max 1.0}]]])
