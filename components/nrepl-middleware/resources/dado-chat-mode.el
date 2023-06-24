;;; dado-chat-mode.el --- DADO chat mode          -*- lexical-binding: t; -*-

;; Copyright © 2023 Hugo Duncan
;; Author: Hugo Duncan <hugo@hugoduncan.org>
;; Maintainer: Hugo Duncan <hugo@hugoduncan.org>
;; URL: http://www.github.com/hugoduncan/dado
;; Version: 0.0.0
;; Package-Requires: ((emacs "26") (cider "1.6.0"))
;; Keywords: ai, chat-gpt

;;; Commentary

;; A feature that provides a chat using the ChatGPT completion API.

;; Use chat-mode to create a chat buffer, and either add a message
;; interactively, or call the dado-chat-input function.

(require 'dado-nrepl)
(eval-when-compile (require 'cl-lib))

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

(defvar-local dado-chat--prompt-overlay nil)

(defun dado-chat--insert-prompt (prompt)
  (let ((prompt (propertize prompt 'face 'dado-chat-prompt-face))
	(start (point)))
    (setq dado-chat--prompt-overlay (make-overlay start (point)))
    (overlay-put dado-chat--prompt-overlay 'before-string prompt)
    (goto-char (overlay-end dado-chat--prompt-overlay))))

(defun dado-chat--insert-message (role content prefix)
  (let ((start (point)))
    (insert content)
    (insert "\n")
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'before-string prefix)
      (overlay-put overlay 'category (intern role)))))

(defun dado-chat--insert-user-message (role content)
  (let ((prefix (propertize
		 (if (string-equal "system" role) "Purpose: " "User: ")
		 'face 'dado-chat-user-message-face)))
    (dado-chat--insert-message role content prefix)))

(defun dado-chat--insert-ai-message (content)
  (let* ((plain-prefix "AI: ")
	 (prefix (propertize plain-prefix 'face 'dado-chat-completion-face)))
    (dado-chat--insert-message "assistant" content prefix)))

(defun dado-chat--remove-prompt ()
  (when dado-chat--prompt-overlay
    (delete-overlay dado-chat--prompt-overlay)
    (setq dado-chat--prompt-overlay nil)))

(defun dado-chat--get-input ()
  (save-excursion
    (let* ((ostart (overlay-start dado-chat--prompt-overlay))
	   (oend (overlay-end dado-chat--prompt-overlay))
	   (prompt (overlay-get dado-chat--prompt-overlay 'before-string))
	   (content (buffer-substring-no-properties oend (+ 1 (buffer-size))))
	   (role (if (string-equal prompt dado-chat-first-prompt)
		     "system"
		   "user")))
      (delete-region ostart (+ 1 (buffer-size)))
      (list role content))))

(defun dado-chat--parse-message (overlay)
  "Parse a message from OVERLAY in the current chat buffer."
  (let* ((category (overlay-get overlay 'category))
	 (content (string-trim
		   (buffer-substring-no-properties
		    (overlay-start overlay)
		    (overlay-end overlay)))))
    (when content
      (list (symbol-name category) content))))

(defun dado-chat--parse-messages ()
  "Scans the current buffer.  At each overlay it constructs a tuple
of the overlay's 'before-string property value and all the text
in the buffer up to the next overlay.  Return a list of all these
tuples."
  (save-excursion
    (let* ((messages nil)
           (current-pos (point-min)))
      (while (not (eq current-pos (point-max)))
	(let ((overlay (car (overlays-at current-pos))))
	  (if overlay
	      (when-let ((message (dado-chat--parse-message overlay)))
		(push message messages)
		(setq current-pos (overlay-end overlay)))
	    (setq overlay (next-overlay-change current-pos)))))
      (nreverse messages))))

(defun dado-chat-input (role content &optional callback)
  "Add CONTENT with ROLE to the current chat buffer.
ROLE is a string and is either \"user\" or \"system\".  The
\"system\" role is only allowed for an empty chat buffer.
The optional CALLBACK will be called with a list of completions."
  (cl-assert
   (or (string-equal role "user")
       (and (= 0 (buffer-size))
	    (string-equal role "system"))))
  (when (not (string-blank-p content))
    (dado-chat--remove-prompt)
    (dado-chat--insert-user-message role content)
    (let* ((messages (dado-chat--parse-messages)))
      (dado-chat--chat-completion messages callback))))

(defun dado-chat--process-input ()
  (interactive)
  (let ((input (dado-chat--get-input)))
    (seq-let [role content] input
      (dado-chat-input role content))))

(defun dado-chat--chat-completion (messages &optional callback)
  (let ((buffer (current-buffer)))
    (dado-chat-op
     (lambda (messages)
       (with-current-buffer buffer
         (dolist (response messages)
           (seq-let [role content] response
             (dado-chat--insert-ai-message content)))
	 (dado-chat--insert-prompt dado-chat-prompt)
	 (when callback
	   (funcall callback messages))))
     messages)))

(defvar dado-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'dado-chat--process-input)
    map))

;;;###autoload
(define-derived-mode dado-chat-mode fundamental-mode "dado-chat"
  (setq-local font-lock-defaults '(nil t))
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local dado-chat--prompt-overlay nil)
  (use-local-map dado-chat-mode-map)
  (dado-chat--insert-prompt dado-chat-first-prompt))

;;;###autoload
(defun dado-chat ()
  "Start a new chat session with the dado-chat-mode enabled.
Create a chat buffer.  By default the buffer is named
*dado-chat*, but when invoked with a prefix, this name is made
unique.  The buffer's major mode is dado-chat-mode.
Return the chat buffer."
  (interactive)
  (let* ((buffer-name (if current-prefix-arg
			  (generate-new-buffer-name "*dado-chat*")
			"*dado-chat*"))
	 (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'dado-chat-mode)
      (dado-chat-mode))
    buffer))

(provide 'dado-chat-mode)

;;; dado-chat-mode.el ends here
