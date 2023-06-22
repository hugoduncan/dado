;;; dado-chat-mode.el --- DADO chat mode          -*- lexical-binding: t; -*-

(require 'dado-nrepl)
(require 'ansi-color)

(defgroup dado-chat nil
  "Dado Chat Mode"
  :group 'convenience
  :prefix "dado-chat-")

(defcustom dado-chat-prompt "> "
  "Prompt string for the user input in dado-chat-mode."
  :type 'string
  :group 'dado-chat)

(defcustom dado-chat-first-prompt "purpose> "
  "Prompt string for the first user input in dado-chat-mode."
  :type 'string
  :group 'dado-chat)

(defface dado-chat-user-message-face
  '((t (:foreground "blue")))
  "Face for user messages in dado-chat-mode."
  :group 'dado-chat)

(defface dado-chat-completion-face
  '((t (:foreground "green")))
  "Face for completions in dado-chat-mode."
  :group 'dado-chat)

(defface dado-chat-prompt-face
  '((t (:foreground "green")))
  "Face for prompts in dado-chat-mode."
  :group 'dado-chat)

(defvar-local dado-chat--completions nil)
(defvar-local dado-chat--prompt-overlay nil)

(defun dado-chat--insert-prompt (prompt)
  (let ((prompt (propertize prompt 'face 'dado-chat-prompt-face))
	(start (point)))
    ;;(insert prompt)
    (setq dado-chat--prompt-overlay (make-overlay start (point)))
    (overlay-put dado-chat--prompt-overlay 'before-string prompt)
    (goto-char (overlay-end dado-chat--prompt-overlay))))

(defun dado-chat--insert-message (msg prefix role)
  (let ((start (point)))
    (insert msg)
    (insert "\n")
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'before-string prefix)
      (overlay-put overlay 'category (intern role)))))

(defun dado-chat--insert-user-message (role message)
  (let ((prefix (propertize
		 (if (string-equal "system" role) "Purpose: " "User: ")
		 'face 'dado-chat-user-message-face)))
    (dado-chat--insert-message message prefix role)))

(defun dado-chat--insert-ai-message (msg)
  (let* ((plain-prefix "AI: ")
	 (prefix (propertize plain-prefix 'face 'dado-chat-completion-face)))
    (dado-chat--insert-message msg prefix "assistant")))

(defun dado-chat--get-input ()
  (save-excursion
    (let* ((ostart (overlay-start dado-chat--prompt-overlay))
	   (oend (overlay-end dado-chat--prompt-overlay))
	   (prompt (overlay-get dado-chat--prompt-overlay 'before-string))
	   (msg (buffer-substring-no-properties oend (+ 1 (buffer-size))))
	   (role (if (string-equal prompt dado-chat-first-prompt)
		     "system"
		   "user")))
      (delete-region ostart (+ 1 (buffer-size)))
      (delete-overlay dado-chat--prompt-overlay)
      (list role msg))))

(defun dado-chat--parse-messages ()
  "Scans the current buffer.  At each overlay it constructs a tuple
of the overlay's 'before-string property value and all the text
in the buffer up to the next overlay.  Return a list of all these
tuples."
  (save-excursion
    (let* ((result '())
	   (overlay nil)
	   (next-pos (point-min)))
      (while (not (eq next-pos (point-max)))
	(let ((overlay (car (overlays-at next-pos))))
	  (if overlay
	      (let* ((category (overlay-get overlay 'category))
		     (message-text (string-trim
				    (buffer-substring-no-properties
				     next-pos
				     (overlay-end overlay)))))
		(when message-text
		  (push (list (symbol-name category) message-text) result))
		(setq next-pos (overlay-end overlay)))
	    (setq next-overlay (next-overlay-change next-pos)))))
      (nreverse result))))

(defun dado-chat--process-input ()
  (interactive)
  (let ((input (dado-chat--get-input)))
    (seq-let [role message] input
      (when (not (string-blank-p message))
	(dado-chat--insert-user-message role message)
	(let* ((messages (dado-chat--parse-messages))
	       (completions (dado-chat--chat-completion messages)))
          (setq dado-chat--completions completions))))))

(defun dado-chat--chat-completion (messages)
  (let ((buffer (current-buffer)))
    (dado-chat-op
     (lambda (messages)
       (with-current-buffer buffer
         (dolist (response messages)
           (seq-let [role message] response
             (dado-chat--insert-ai-message message)))
	 (dado-chat--insert-prompt dado-chat-prompt)))
     messages)))

(defvar dado-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'dado-chat--process-input)
    map))

(define-derived-mode dado-chat-mode fundamental-mode "dado-chat"
  (setq-local font-lock-defaults '(nil t))
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (setq-local dado-chat--completions nil)
  (setq-local dado-chat--prompt-overlay nil)
  (use-local-map dado-chat-mode-map)
  (dado-chat--insert-prompt dado-chat-first-prompt))

(defun dado-chat ()
  "Start a new chat session with the dado-chat-mode enabled."
  (interactive)
  (switch-to-buffer (get-buffer-create "*dado-chat*"))
  (dado-chat-mode))

(provide 'dado-chat-mode)

;;; dado-chat-mode.el ends here
