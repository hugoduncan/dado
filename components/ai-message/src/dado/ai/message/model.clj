(ns dado.ai.message.model
  (:require [malli.core :as m]
            [dado.ai.tool.model :as tool]))

(def Role
  [:enum :user :system :assistant])

(def SimpleContentMap
  [:map
   [:type {:optional true} [:enum :text]
    :text :string]])

(def ContentMap
  [:or
   SimpleContentMap
   [:map
    [:type [:= :tool-result]]
    [:tool-use-id :string]
    [:content [:or :string [:vector SimpleContentMap]]]
    [:is-error {:optional true} :boolean]]])

(def Message
  [:map
   [:role Role]
   [:content [:or :string [:vector ContentMap]]]
   [:name {:optional true} string?]])

(def ToolCall
  [:map
   [:type [:= :tool-call]]
   [:id :string] ; TODO rename :call-id
   [:tool :keyword]
   [:parameters [:map-of :keyword any?]]])

(def MessageThread
  [:map
   [:id string?]
   [:created-at inst?]
   [:messages [:vector [:or Message ToolCall]]]
   [:metadata [:map
               [:model string?]
               [:system-prompt {:optional true} string?]
               [:context {:optional true}
                [:map
                 [:files [:vector
                          [:vector
                           [:map
                            [:name string?]
                            [:content string?]]]]]]]
               [:tools {:optional true} [:vector tool/Tool]]
               ]]])

(def ResponseMessage
  [:map
   [:role [:= :assistant]]
   [:content [:vector [:or ContentMap ToolCall]]]
   [:finish-reason [:enum :stop :length :content-filter :tool-call :end-turn]]
   [:tool-calls {:optional true} [:vector ToolCall]]
   #_[:usage [:map
              [:prompt-chars pos-int?]
              [:completion-chars pos-int?]
              [:total-chars pos-int?]]]])

(def FileBlock
  [:map
   [:language string?]
   [:name string?]
   [:content string?]
   [:metadata [:map
               [:block-type [:enum :text :unified-diff]]
               [:name string?]]]])

;; Create validators that will be used in pre/post conditions
(def message? (m/validator Message))
(def message-thread? (m/validator MessageThread))
(def response-message? (m/validator ResponseMessage))
(def file-block? (m/validator FileBlock))
