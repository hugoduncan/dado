(ns dado.ai.port.core)

(defn lookup-send!
  [port-name]
  (let [send-fn-sym (symbol (str "dado.ai." port-name ".interface/send!"))]
    (some-> (requiring-resolve  send-fn-sym) deref)))

(defn lookup-default-model-name
  [port-name]
  (let [fn-sym
        (symbol (str "dado.ai." port-name ".interface/default-model-name"))]
    (some-> (requiring-resolve fn-sym) deref)))
