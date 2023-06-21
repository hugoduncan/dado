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

(defundado--elisp-arguments-for-symbol (symbol)
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

(defun dado--popup-buffer (&optional mode)
  (cider-popup-buffer dado-result-buffer t mode 'ancillary))

(defun dado--popup-result (response &optional mode)
  (let ((response-buffer (dado--popup-buffer mode)))
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

(defun dado--default-reply-handler (&optional mode)
  (lambda (reply)
    (when reply
      ;; (message "reply %s" reply)
      (nrepl-dbind-response reply (response)
	(when response
	  (nrepl-dbind-response response (suggestion)
	    (when suggestion
	      (dado--popup-result suggestion mode))))))))

(defun dado-op (callback action args)
  (when (cider-nrepl-op-supported-p "dado")
    (dado-request callback action args))
  nil)

(provide 'dado-nrepl)

;;; dado-nrepl.el ends here
