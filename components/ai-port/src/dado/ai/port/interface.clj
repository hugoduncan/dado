(ns dado.ai.port.interface
  (:require
   [dado.ai.port.core :as core]))

(defn lookup-send!
  [port-name]
  (core/lookup-send! port-name))

(defn lookup-default-model-name
  [port-name]
  (core/lookup-default-model-name port-name))
