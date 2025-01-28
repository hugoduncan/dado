(ns dado.ai.tools.git.model
  "Data models for git tools."
  (:require
   [malli.core :as m]))

;; Parameter Schemas
(def StageParameters
  [:map
   [:paths {:optional true} [:vector :string]]
   [:force {:optional true} [:maybe :boolean]]])

(def stage-parameters? (m/validator StageParameters))

(def CommitParameters
  [:map
   [:message :string]
   [:allow-empty {:optional true} [:maybe :boolean]]])

(def commit-parameters? (m/validator CommitParameters))

(def StatusParameters
  [:map])

(def status-parameters? (m/validator StatusParameters))

;; Response Schemas
(def StageResponse
  [:map
   [:staged [:vector :string]]
   [:errors [:vector [:map
                     [:path :string]
                     [:reason :string]]]]])

(def CommitResponse
  [:map
   [:commit [:map
            [:hash :string]
            [:message :string]
            [:summary [:map
                      [:files pos-int?]
                      [:insertions pos-int?]
                      [:deletions pos-int?]]]]]])

(def StatusResponse
  [:map
   [:branch :string]
   [:modified [:vector :string]]
   [:staged [:vector :string]]
   [:untracked [:vector :string]]
   [:ahead pos-int?]
   [:behind pos-int?]])
