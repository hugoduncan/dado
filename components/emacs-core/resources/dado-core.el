;;; /Users/duncan/projects/hugoduncan/dado/components/emacs-core/resources/dado-core.el ---           -*- lexical-binding: t; -*-

(require 'cider)

(defun dado-generate-completion (system-sym message)
  "Call cider-nrepl to invoke the clojure function
`hd.environment.generate-fun/generate-chat-completion` with the
provided `string` argument and return the result."
  (let* ((cmd
	  (concat
	   "(do
 (require '[org.hugoduncan.dado.chat-gpt.interface :as chat-gpt])
 (chat-gpt/generate-completion :" system-sym " \"" message "\"))")))
    (cider-nrepl-sync-request:eval cmd)))

;;; /Users/duncan/projects/hugoduncan/dado/components/emacs-core/resources/dado-core.el ends here
