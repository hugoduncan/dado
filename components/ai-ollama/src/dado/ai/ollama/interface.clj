(ns dado.ai.ollama.interface
  (:require [dado.ai.ollama.core :as core]))

(defn send!
  "Send message thread to Ollama AI and return response.

   Parameters:
     config - Map containing Ollama configuration:
              :model-name - Optional model name (defaults to \"llama2:3.2\")
              :api-url - Optional API URL (defaults to \"http://localhost:11434\")

     message-thread - Message thread conforming to AI Message format

   Returns:
     Response message conforming to AI Message response format

   Throws:
     ex-info with :type :error/ollama-connection for connection errors
     ex-info with :type :error/ollama-response for API errors
     ex-info with :type :error/ollama-validation for validation errors"
  [config message-thread]
  (core/send! config message-thread))
