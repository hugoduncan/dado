(ns dado.ai.claude.interface
  (:require [dado.ai.claude.core :as core]))

(defn send!
  "Send message thread to Claude AI and return response.

   Parameters:
     config - Map containing Claude configuration:
              :api-key - Required API key for authentication
              :api-url - Optional API URL (defaults to Claude v1 messages endpoint)
              :model-name - Optional model name (defaults to claude-3-opus-20240229)
              :max-tokens - Optional max tokens (defaults to 4096)

     message-thread - Message thread conforming to AI Message format

   Returns:
     Response message conforming to AI Message response format

   Throws:
     ex-info with :type :error/claude-connection for connection errors
     ex-info with :type :error/claude-response for API errors
     ex-info with :type :error/claude-validation for validation errors"
  [config message-thread]
  (core/send! config message-thread))
