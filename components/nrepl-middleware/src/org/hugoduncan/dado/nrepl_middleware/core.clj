(ns org.hugoduncan.dado.nrepl-middleware.core
  "Middleware of using dado dev chat assistant."
  (:require
   [org.hugoduncan.dado.operation.interface :as operation]
   [nrepl.misc :refer [response-for] :as misc]
   [nrepl.transport :as t])
  (:import
   [nrepl.transport
    Transport]))

(def capture (volatile! []))

(defn- dado-reply
  [{:keys [session ns language action args] :as msg}]
  (vswap! capture conj {:msg (dissoc msg :session)})
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
