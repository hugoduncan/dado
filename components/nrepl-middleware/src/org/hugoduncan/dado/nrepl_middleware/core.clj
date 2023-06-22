(ns org.hugoduncan.dado.nrepl-middleware.core
  "Middleware of using dado dev chat assistant."
  (:require
   [nrepl.misc :refer [response-for] :as misc]
   [nrepl.transport :as t]
   [org.hugoduncan.dado.chat-gpt.interface :as chat-gpt]
   [org.hugoduncan.dado.operation.interface :as operation])
  (:import
   [nrepl.transport
    Transport]))

(defn- dado-reply
  [{:keys [session ns language action args] :as msg}]
  (let [ns (if ns (symbol ns) (symbol (str (@session #'*ns*))))]
    (try
      (response-for
       msg
       {:status   :done
        :response (operation/op language action args ns)})
      (catch Exception e
        (do ;; binding [*out* (java.io.OutputStreamWriter. System/out)]
          (prn e))
        (response-for
         msg
         {:status #{:done :dado-error}})))))

;; the handler
(defn wrap-dado
  "Middleware that provides dado assistant.
  It understands the following params:

  * `system` - a symbol that determines the action to undertake
  * `ns`- the namespace in which the action occurs. Defaults to `*ns*`.
  * `args` – a map of arguments to pass to the action."
  [h]
  (fn [{:keys [op ^Transport transport] :as msg}]
    (if (= op "dado")
      (t/send transport (dado-reply msg))
      (h msg))))

(defn- input->message-maps [messages]
  (mapv
   #(hash-map "role" (str (first %)) "content" (str (last %)))
   messages))

(defn- dado-chat-reply
  [{:keys [messages] :as msg}]
  (try
    (response-for
     msg
     {:status   :done
      :response (chat-gpt/generate-chat-completion
                 (input->message-maps messages))})
    (catch Exception e
      (do
        (prn e))
      (response-for
       msg
       {:status #{:done :dado-chat-error}}))))

(defn wrap-dado-chat [h]
  "Middleware that provides dado chat.
  It understands the following params:

  * `messages` - a list of messages"
  [h]
  (fn [{:keys [op ^Transport transport] :as msg}]
    (if (= op "dado/chat")
      (t/send transport (dado-chat-reply msg))
      (h msg))))
