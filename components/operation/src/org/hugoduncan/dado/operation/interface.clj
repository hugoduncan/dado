(ns org.hugoduncan.dado.operation.interface)

(defmulti op (fn [language action args ns] [language action]))

(defmethod op :default
  [languege action args ns]
  (throw
   (ex-info
    "Unknown operation"
    {:ns       ns
     :language languege
     :action   action
     :args     args
     :status   #{:unkown-dado-action}})))
