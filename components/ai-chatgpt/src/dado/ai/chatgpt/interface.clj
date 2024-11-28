(ns dado.ai.chatgpt.interface
  (:require
   [dado.ai.chatgpt.core :as core]))

(defn send!
  "Send message thread to ChatGPT AI and return response.

   Parameters:
     config - Map containing ChatGPT configuration:
              :model-name - Optional model name (defaults to \"llama2:3.2\")
              :api-url - Optional API URL (defaults to \"http://localhost:11434\")

     message-thread - Message thread conforming to AI Message format

   Returns:
     Response message conforming to AI Message response format

   Throws:
     ex-info with :type :error/chatgpt-connection for connection errors
     ex-info with :type :error/chatgpt-response for API errors
     ex-info with :type :error/chatgpt-validation for validation errors"
  [config http-request-fn message-thread]
  (core/send! config http-request-fn message-thread))
