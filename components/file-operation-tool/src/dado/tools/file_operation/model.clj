(ns dado.tools.file-operation.model
  "Data models for file operation tool"
  (:require [malli.core :as m]))

(def Operation
  [:enum :create :edit :move :copy :delete])

(def Operation
  [:enum :create :edit :move :copy :delete])

(def SearchBlock
  [:map
   [:search :string]
   [:replace :string]])

(def FileOperation
  [:or
   [:map
    [:operation [:= :create]]
    [:path :string]
    [:content {:optional true} :string]]
   [:map
    [:operation [:= :edit]]
    [:path :string]
    [:search-blocks
     {:optional true}
     [:vector SearchBlock]]]
   [:map
    [:operation [:enum :move :copy]]
    [:path :string]
    [:target-path {:optional true} :string]]
   [:map
    [:operation [:= :delete]]
    [:path :string]]])

(def OperationResult
  [:map
   [:operation Operation]
   [:path :string]
   [:target-path {:optional true} :string]
   [:success :boolean]
   [:error {:optional true} :string]])

;; Create validators
(def operation? (m/validator Operation))
(def search-block? (m/validator SearchBlock))
(def file-operation? (m/validator FileOperation))
(def operation-result? (m/validator OperationResult))
