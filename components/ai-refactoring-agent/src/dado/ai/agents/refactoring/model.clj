(ns dado.ai.agents.refactoring.model
  (:require [malli.core :as m]))

(def Agent
  [:map
   [:name [:= :refactoring]]
   [:prompt-fn fn?]
   [:context-fn fn?]
   [:process-response-fn fn?]])

(def Agent? (m/validator Agent))
