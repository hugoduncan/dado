(ns dado.nrepl-middleware.interface
  "Middleware of using dado code assistant."
  (:require
   [dado.nrepl-middleware.core :as nrepl-midleware-core]
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

  * `message` - a messages.
  * `agent-name` - the name of the agent to talk to
  * `ai-port-name` - the name of the AI to talk with
  * `conversation-id` - the ID for the conversation"
  [h]
  (nrepl-midleware-core/wrap-dado-chat h))

(set-descriptor!
 #'wrap-dado-chat
 {:requires #{}
  :expects  #{}
  :handles
  {"dado/chat"
   {:doc      "Provides dado code assistant actions."
    :requires {"message"      "A message"
               "agent-name"   "The name of the chat agent"
               "ai-port-name" "The name of the AI provider"
               }
    :optional {"conversation-id" "The ID for the conversation"}
    :returns  {"choices" "Completions"}}}})

(defn- middleware-symbol [op-name]
  ;; It is safe to use *ns* as this is called only at compile time
  (symbol (str (ns-name *ns*)) (name op-name)))

(def dado-middleware
  "A var that can be added to cider-jack-in-nrepl-middlewares"
  [(middleware-symbol 'wrap-dado)
   (middleware-symbol 'wrap-dado-chat)])
