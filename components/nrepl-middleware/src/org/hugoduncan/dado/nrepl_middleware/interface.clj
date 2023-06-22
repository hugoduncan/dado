(ns org.hugoduncan.dado.nrepl-middleware.interface
  "Middleware of using dado code assistant."
  (:require
   [org.hugoduncan.dado.nrepl-middleware.core :as nrepl-midleware-core]
   [org.hugoduncan.dado.clojure-fns.interface]
   [org.hugoduncan.dado.elisp-funs.interface]
   [nrepl.middleware :as middleware :refer [set-descriptor!]]))

(defn wrap-dado
  "Middleware that provides `dado` nrepl op.

  It understands the following params:

  * `action` - a symbol that determines the action to undertake
  * `ns`- the namespace in which the action occurs. Defaults to `*ns*`.
  * `args` – a map of arguments to pass to the action."
  [h]
  (nrepl-midleware-core/wrap-dado h))

(set-descriptor!
 #'wrap-dado
 {:requires #{}
  :expects  #{}
  :handles
  {"dado"
   {:doc      "Provides dado code assistant actions."
    :requires {"action"   "The type of action to undertake."
               "args"     "A map of arguments for the action."
               "language" "The target coding language"}
    :optional {"ns"
               "The namespace in which to run the action. Defaults to `*ns*`."}
    :returns  {"response" "An action specific map"}}}})

(defn wrap-dado-chat
  "Middleware that provides `dado/chat` nrepl op.

  It understands the following params:

  * `messages` - a list of messages."
  [h]
  (nrepl-midleware-core/wrap-dado-chat h))

(set-descriptor!
 #'wrap-dado-chat
 {:requires #{}
  :expects  #{}
  :handles
  {"dado/chat"
   {:doc      "Provides dado code assistant actions."
    :requires {"messages" "A list of messages"}
    :optional {}
    :returns  {"choices" "Completions"}}}})

(defn- middleware-symbol [op-name]
  ;; It is safe to use *ns* as this is called only at compile time
  (symbol (str (ns-name *ns*)) (name op-name)))

(def dado-middleware
  "A var that can be added to cider-jack-in-nrepl-middlewares"
  [(middleware-symbol 'wrap-dado)
   (middleware-symbol 'wrap-dado-chat)])
