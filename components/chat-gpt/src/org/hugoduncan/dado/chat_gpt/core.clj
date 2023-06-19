(ns org.hugoduncan.dado.chat-gpt.core
  "A wrapper for the openapi-clojure library, that handles credentials."
  (:require
   [clojure.string :as string]
   [wkok.openai-clojure.api :as api]))

(def options
  ;; for the organization id see:
  ;;   https://platform.openai.com/account/org-settings
  {:api-key      (System/getenv "OPENAI_API_KEY")
   :organization (System/getenv "OPENAI_ORGANIZATION")})

(defn generate-chat-completion
  "Generate a completion using ChatGPT."
  [system content]
  (let [result (api/create-chat-completion
                {:model    "gpt-3.5-turbo" ;; "gpt-4"
                 :messages [{:role "system" :content system}
                            {:role "user" :content content}]}
                options)]
    (reduce str (mapv (comp :content :message)(:choices result)))))

(defn parse-completion-string
  "Parse a completion string returned by ChatGPT.

  The completion-string string contains alternating explanation text and
  code blocks.  Code blocks in the completion string are delimited with
  ```.  Explanation text is not delimited.

  Each code block and each explanation text is put into an element of
  the returned sequence.  A code block element is a map with a `:code` key
  that contains the code stripped of its ``` delimiters.  An explanation
  text element is a map with a `:text` key containing the explanation
  text.

  Returns the sequence."
  [completion-string]

  (let [completion-string (string/trim completion-string)
        pred              (if (string/starts-with? completion-string  "```")
                            odd?
                            even?)
        parts             (string/split
                           completion-string
                           #"```(?:clojure)?(?:elisp)?(?:emacs-lisp)?")]
    (into
     []
     (map-indexed
      (fn [index part]
        (if (pred index)
          {:text part}
          {:code part})))
     (->> parts
          (map (comp string/trim str))
          (filter (complement string/blank?))))))
;; (hd.environment.generate-fn/generate-test 'hd.environment.chat-gpt/parse-completion-string)


;; (hd.environment.generate-fn/generate-fn-body 'hd.environment.chat-gpt/parse-completion-string)
;; (hd.environment.generate-fn/generate-fn-body 'hd.environment.chat-gpt/format-parsed-completion)
;; (hd.environment.generate-fn/generate-test 'hd.environment.chat-gpt/word-wrap)
;; (hd.environment.generate-fn/generate-test 'hd.environment.chat-gpt/parse-completion-string)

;; (hd.environment.generate-fn/critique-fn 'hd.environment.chat-gpt/word-wrap)
;; (hd.environment.generate-fn/docstring-fn 'hd.environment.chat-gpt/word-wrap)
;; (hd.environment.generate-fn/suggest-fn-impl 'hd.environment.chat-gpt/word-wrap)
