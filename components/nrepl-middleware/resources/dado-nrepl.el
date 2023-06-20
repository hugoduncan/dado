;;; dado-nrepl.el --- -*- lexical-binding: t; -*-

(require 'cider)

(defconst dado-result-buffer "*dado-result*")

(defun dado--buffer-coding-language (&optional buffer)
  "Return a symbol representing the coding language of BUFFER.
  BUFFER defaults to the current buffer.
  Recognises emacs lisp and clojure.
  Return 'elisp for emacs lisp, 'clojure for clojure, or nil otherwise."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (let ((mode-name (symbol-name major-mode)))
        (cond ((string-equal mode-name "emacs-lisp-mode") 'elisp)
              ((string-equal mode-name "clojure-mode") 'clojure)
	      ((string-equal mode-name "cider-mode") 'clojure)
              (t nil))))))

(defun dado--elisp-docstring-for-symbol (symbol)
  "Return doc string for SYMBOL."
  (when symbol
    (if (fboundp symbol)
        (documentation symbol)
      (get symbol 'doc-string))))

(defun dado--elisp-arguments-for-symbol (symbol)
  "Return the arguments for SYMBOL that refers to a function."
  (let ((arglist (help-function-arglist symbol)))
    (cdr (cdr arglist))))

(defun dado--elisp-function-source-for-symbol (function-symbol)
  "Return the source code for FUNCTION-SYMBOL that refers to a function.
When no function definition is found, return NIL.

Obtains the function's definition as a closure, converts to a
string, and then reads this to get a form.  Converts the form
representing the closure to a defun form using the function's
name."
  (let ((function (symbol-function function-symbol)))
    (unless (subrp function)		; elisp primitive
      (let* ((form (cdr function))
	     (name (intern
		    (substring-no-properties
		     (symbol-name function-symbol)))))
        (prin1-to-string (cons 'defun (cons name form)) )))))

(defun dado--popup-buffer ()
  (cider-popup-buffer dado-result-buffer t nil 'ancillary))

(defun dado--popup-result (response)
  (let ((response-buffer (dado--popup-buffer)))
    (cider-emit-into-color-buffer response-buffer response)))

(defun dado-request (callback action args)
  "Send \"dado\" op with parameters ACTION, ARGS and NS."
  (thread-first
    `("op" "dado"
      "action" ,action
      "language" ,(symbol-name (dado--buffer-coding-language))
      "ns" ,(cider-current-ns)
      "args" ,args)
    (cider-nrepl-send-request
     callback
     (cider-current-repl)
     'tooling)))

(defun dado--default-reply-handler (reply)
  (when reply
    ;; (message "reply %s" reply)
    (nrepl-dbind-response reply (response)
      (when response
	(nrepl-dbind-response response (suggestion)
	  (when suggestion
	    (dado--popup-result suggestion)))))))

(defun dado-op (callback action args)
  (when (cider-nrepl-op-supported-p "dado")
    (dado-request callback action args))
  nil)

;; (dado-op "docstring" '(dict) nil)
;; (dado-op "suggest-fn-impl" '(dict "fn-name" "org.hugoduncan.dado.util.interface/word-wrap"))

(defun dado-suggest-fn-impl (&optional arg)
  "Suggest possible ways of implementing a clojure function.

Prompts for the name of the clojure function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (cider-symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving suggestions for implementation of %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "suggest-fn-impl"
     `(dict "fn-name" ,symbol))))

(defun dado-implement-fn (&optional arg)
  "Implement a clojure function.

Prompts for the name of the clojure function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (cider-symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving an implementation of %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "implement-fn"
     `(dict "fn-name" ,symbol))))

(defun dado-implement-fn-test (&optional arg)
  "Implement a test for a clojure function.

Prompts for the name of the clojure function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (cider-symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving an test implementation for %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "implement-fn-test"
     `(dict "fn-name" ,symbol))))

(defun dado-critique-fn (&optional arg)
  "Critique a clojure function.

Prompts for the name of the clojure function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (cider-symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving a critique of %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "critique-fn"
     `(dict "fn-name" ,symbol))))

(defun dado-generate-fn-docstring (&optional arg)
  "Generate a doc string for a clojure function.

Prompts for the name of the clojure function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (cider-symbol-at-point) (cider-read-symbol-name))))
    (message "Generating a docstring for %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "generate-fn-docstring"
     `(dict "fn-name" ,symbol))))

;;; Elisp

(defun dado-elisp-suggest-fn-impl (&optional arg)
  "Suggest possible ways of implementing an elisp function.

Prompts for the name of the elisp function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving suggestions for implementation of %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "suggest-fn-impl"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)))))

(defun dado-elisp-implement-fun (&optional arg)
  "Implement an elisp function.

Prompts for the name of the function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving an implementation of %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "implement-fn"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)))))

(defun dado-elisp-implement-fn-test (&optional arg)
  "Implement a test for an elisp function.

Prompts for the name of the elisp function, or uses the
function at point."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (elisp-symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving an test implementation for %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "implement-fn-test"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)
	    "fn-definition" ,(dado--elisp-function-source-for-symbol symbol)))))

(defun dado-elisp-critique-fn (&optional arg)
  "Critique an elisp function.

Prompts for the name of the elisp function, or uses the function
at point."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving a critique of %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "critique-fn"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)
	    "fn-definition" ,(dado--elisp-function-source-for-symbol symbol)))))

(defun dado-elisp-generate-fn-docstring (&optional arg)
  "Generate a doc string for an elisp function.

Prompts for the name of the elisp function, or uses the function
at point."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (symbol-at-point) (cider-read-symbol-name))))
    (message "Generating a docstring for %s" symbol)
    (dado-op
     #'dado--default-reply-handler
     "generate-fn-docstring"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)
	    "fn-definition" ,(dado--elisp-function-source-for-symbol symbol)))))


(defconst dado-middleware-name
  "org.hugoduncan.dado.nrepl-middleware.interface/dado-middleware")

(defmacro dado--cider-alias-pred (alias)
  "Return a predicate for ALIAS being in use."
  `(lambda (&rest _)
     (or
      (and cider-clojure-cli-aliases
           (s-contains? ,alias cider-clojure-cli-aliases))
      (and cider-clojure-cli-global-options
           (s-contains? ,alias cider-clojure-cli-global-options)))))

;; Inject dado middleware when using the `:dado' alias
(add-to-list
 'cider-jack-in-nrepl-middlewares
 `(,dado-middleware-name :predicate ,(dado--cider-alias-pred ":dado")))

(provide 'dado-nrepl)

;;; dado-nrepl.el ends here
