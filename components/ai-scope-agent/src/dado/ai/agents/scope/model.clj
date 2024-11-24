(ns dado.ai.agents.scope.model
  (:require [malli.core :as m]))

(def Agent
  [:map
   [:name [:= :scope]]
   [:prompt-fn fn?]
   [:context-fn fn?]
   [:process-response-fn fn?]])

(def Agent? (m/validator Agent))
