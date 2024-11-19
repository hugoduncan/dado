(ns dado.ai.message.model
  (:require [malli.core :as m]))

(def Role
  [:enum :user :system :assistant])

(def Message
  [:map
   [:role Role]
   [:content string?]
   [:name {:optional true} string?]])

(def MessageThread
  [:map
   [:id string?]
   [:created-at inst?]
   [:messages [:vector Message]]
   [:metadata [:map
               [:model string?]
               [:system-prompt {:optional true} string?]
               [:context {:optional true}
                [:map
                 [:files [:vector
                          [:vector
                           [:map
                            [:name string?]
                            [:content string?]]]]]]]]]])

(def ResponseMessage
  [:map
   [:role [:= :assistant]]
   [:content string?]
   [:finish-reason [:enum :stop :length :content-filter]]
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
