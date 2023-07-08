;;; dado-mode.el --- DADO mode          -*- lexical-binding: t; -*-

(require 'dado-nrepl)

;;;###autoload
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

;;;###autoload
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
     (dado--default-reply-handler 'clojure-mode)
     "implement-fn"
     `(dict "fn-name" ,symbol))))

;;;###autoload
(defun dado-implement-fn-test (&optional arg)
  "Implement a test for a clojure function.

Prompts for the name of the clojure function, or uses the
function at point, depending on the value of
`cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (cider-symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving a test implementation for %s" symbol)
    (dado-op
     (dado--default-reply-handler 'clojure-mode)
     "implement-fn-test"
     `(dict "fn-name" ,symbol))))

;;;###autoload
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
     (dado--default-reply-handler 'clojure-mode)
     "critique-fn"
     `(dict "fn-name" ,symbol))))

;;;###autoload
(defun dado-suggest-ns-impl ()
  "Provide suggestions to implement a clojure namespace.

Uses the docstring of the current namespace"
  (interactive)
  (cider-ensure-connected)
  (message "Retrieving suggestions for namespace")
  (dado-op
   (dado--default-reply-handler 'clojure-mode)
   "suggest-ns-impl"
   `(dict)))

;;;###autoload
(defun dado-critique-ns (&optional buffer)
  "Critique a clojure namespace.

Uses the current buffer if no buffer given."
  (interactive (list (current-buffer)))
  (cider-ensure-connected)
  (let ((buffer (or buffer (current-buffer)))
	(source (with-current-buffer buffer
		  (buffer-substring-no-properties (point-min) (point-max)))))
    (message "Retrieving a critique of namespace")
    (dado-op
     (dado--default-reply-handler 'clojure-mode)
     "critique-ns"
     `(dict "ns-source" ,source))))

;;;###autoload
(defun dado-implement-ns ()
  "Implement a clojure namespace.

Uses the docstring of the current namespace"
  (interactive)
  (cider-ensure-connected)
  (message "Retrieving an implementation of namespace")
  (dado-op
   (dado--default-reply-handler 'clojure-mode)
   "implement-ns"
   `(dict)))

;;;###autoload
(defun dado-generate-ns-docstring (&optional buffer)
  "Generate a docstring for the clojure namespace.

Uses the current buffer if no buffer given."
  (interactive (list (current-buffer)))
  (cider-ensure-connected)
  (let ((buffer (or buffer (current-buffer)))
	(source (with-current-buffer buffer
		  (buffer-substring-no-properties (point-min) (point-max)))))
    (message "Generating a doc string for the namespace")
    (dado-op
     (dado--default-reply-handler 'clojure-mode)
     "generate-ns-docstring"
     `(dict "ns-source" ,source))))

;;;###autoload
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
     (dado--default-reply-handler 'clojure-mode)
     "generate-fn-docstring"
     `(dict "fn-name" ,symbol))))

;;; Elisp

;;;###autoload
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
     (dado--default-reply-handler 'emacs-lisp-mode)
     "suggest-fn-impl"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)))))

;;;###autoload
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
     (dado--default-reply-handler 'emacs-lisp-mode)
     "implement-fn"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)))))

;;;###autoload
(defun dado-elisp-implement-fun-test (&optional arg)
  "Implement a test for an elisp function.

Prompts for the name of the elisp function, or uses the
function at point."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (elisp-symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving a test implementation for %s" symbol)
    (dado-op
     (dado--default-reply-handler 'emacs-lisp-mode)
     "implement-fn-test"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)
	    "fn-definition" ,(dado--elisp-function-source-for-symbol symbol)))))

;;;###autoload
(defun dado-elisp-critique-fn (&optional arg)
  "Critique an elisp function.

Prompts for the name of the elisp function, or uses the function
at point."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (symbol-at-point) (cider-read-symbol-name))))
    (message "Retrieving a critique of %s" symbol)
    (dado-op
     (dado--default-reply-handler 'emacs-lisp-mode)
     "critique-fn"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)
	    "fn-definition" ,(dado--elisp-function-source-for-symbol symbol)))))

;;;###autoload
(defun dado-elisp-generate-fn-docstring (&optional arg)
  "Generate a doc string for an elisp function.

Prompts for the name of the elisp function, or uses the function
at point."
  (interactive "P")
  (cider-ensure-connected)
  (let ((symbol (or (symbol-at-point) (cider-read-symbol-name))))
    (message "Generating a docstring for %s" symbol)
    (dado-op
     (dado--default-reply-handler 'emacs-lisp-mode)
     "generate-fn-docstring"
     `(dict "fn-name" ,(symbol-name symbol)
	    "docstring" ,(dado--elisp-docstring-for-symbol symbol)
	    "arguments" ,(dado--elisp-arguments-for-symbol symbol)
	    "fn-definition" ,(dado--elisp-function-source-for-symbol symbol)))))

;;;###autoload
(defun dado-elisp-critique-feature ()
  "Critique the given feature code.

Prompts for the name of the elisp function, or uses the function
at point."
  (interactive)
  (cider-ensure-connected)
  (let ((source (buffer-substring-no-properties (point-min) (point-max))))
    (message "Critiquing feature")
    (dado-op
     (dado--default-reply-handler 'emacs-lisp-mode)
     "critique-ns"
     `(dict
       "feature-source" ,source))))

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

(defcustom dado-keymap-prefix "C-c v"
  "Dado keymap prefix."
  :group 'dado
  :type 'string
  :package-version '(dado . "0.0"))

(defvar dado-clojure-command-map
  (-doto (make-sparse-keymap)
    (lsp-define-conditional-key
      ;; namespaces
      "ns" dado-suggest-ns-impl "suggest implementation" t
      "nc" dado-critique-ns "critique" t
      "ni" dado-implement-ns "implement" t
      "nd" dado-generate-ns-docstring "generate docstring" t

      ;; functions
      "fs" dado-suggest-fn-impl "suggest implementation" t
      "fc" dado-critique-fn "critique" t
      "fi" dado-implement-ns "implement" t
      "fd" dado-generate-ns-docstring "generate docstring" t)))

(defvar dado-clojure-map
  (let ((map (make-sparse-keymap)))
    (when dado-keymap-prefix
      (define-key map (kbd dado-keymap-prefix) dado-clojure-command-map))
    map)
  "Keymap for `dado'.")

;;;###autoload
(define-minor-mode dado-mode "Mode for DADO assistant."
  :keymap dado-clojure-map
  :lighter
  (" DADO["
   (dado--buffer-coding-language)
   "]")
  :group 'dado-mode
  :interactive '(clojure-mode emacs-lisp-mode))

(provide 'dado-mode)

;;; dado-mode.el ends here
