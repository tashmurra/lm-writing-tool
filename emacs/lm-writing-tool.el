;;; lm-writing-tool.el --- Minor mode for LM Writing Tool -*- lexical-binding: t; -*-

;; Author: LM Writing Tool contributors
;; URL: https://github.com/peteole/lm-writing-tool
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Keywords: convenience, writing

;;; Commentary:

;; This package provides `lm-writing-tool-mode`, a minor mode that integrates
;; the LM Writing Tool with Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;;;; Customization

(defgroup lm-writing-tool nil
  "Customization group for the LM Writing Tool." :group 'applications)

(defcustom lmwt-provider 'ollama
  "Backend used by `lmwt-request`.
Possible values are `ollama` for a local Ollama server, `github` for
GitHub Copilot and `openai` for OpenAI's API."
  :type '(choice (const :tag "Ollama" ollama)
                 (const :tag "GitHub Copilot" github)
                 (const :tag "OpenAI" openai))
  :group 'lm-writing-tool)

(defcustom lmwt-ollama-url "http://localhost:11434/api/generate"
  "Endpoint of the local Ollama server."
  :type 'string
  :group 'lm-writing-tool)

(defcustom lmwt-ollama-model "llama3"
  "Model to use when talking to an Ollama server."
  :type 'string
  :group 'lm-writing-tool)

(defcustom lmwt-openai-model "gpt-3.5-turbo"
  "Model to use for OpenAI requests."
  :type 'string
  :group 'lm-writing-tool)

(defcustom lmwt-openai-token nil
  "Token used to authenticate against the OpenAI API."
  :type '(choice (const :tag "None" nil) string)
  :group 'lm-writing-tool)

(defcustom lmwt-github-token nil
  "Token used to authenticate against GitHub Copilot."
  :type '(choice (const :tag "None" nil) string)
  :group 'lm-writing-tool)

;;;; Helpers

(defun lmwt--request-ollama (text prompt callback)
  "Send TEXT and PROMPT to a local Ollama server.
CALLBACK is called with each partial response chunk.  When the process
finishes CALLBACK is invoked with nil."
  (let* ((data (json-encode
                `((model . ,lmwt-ollama-model)
                  (prompt . ,(concat prompt "\n" text))
                  (stream . t))))
         (process
          (make-process
           :name "lmwt-ollama"
           :command `("curl" "-sS" "-N" "-X" "POST" ,lmwt-ollama-url
                              "-H" "Content-Type: application/json" "-d" ,data)
           :buffer (generate-new-buffer " *lmwt-ollama*"))))
    (set-process-filter
     process
     (lambda (proc chunk)
       (let ((partial (process-get proc 'partial)))
         (setq partial (concat partial chunk))
         (let ((lines (split-string partial "\n")))
           (process-put proc 'partial (car (last lines)))
           (dolist (line (butlast lines))
             (let* ((json-object-type 'alist)
                    (json-array-type 'list)
                    (payload (ignore-errors (json-read-from-string line)))
                    (resp (alist-get 'response payload)))
                (when resp
                  (funcall callback resp))))))))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (when callback (funcall callback nil))
         (kill-buffer (process-buffer proc)))))
    process))

(defun lmwt--remote-headers ()
  "Return headers for the current remote provider."
  (pcase lmwt-provider
    ('openai `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " lmwt-openai-token))))
    ('github `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Bearer " lmwt-github-token))
               ("Accept" . "text/event-stream")))
    (_ (error "Unsupported provider %S" lmwt-provider))))

(defun lmwt--remote-url ()
  "Return endpoint URL for the current remote provider."
  (pcase lmwt-provider
    ('openai "https://api.openai.com/v1/chat/completions")
    ('github "https://api.githubcopilot.com/chat/completions")
    (_ (error "Unsupported provider %S" lmwt-provider))))

(defun lmwt--request-remote (text prompt callback)
  "Send TEXT and PROMPT to a remote provider.
CALLBACK is called with streamed chunks of data.  When the request
finishes CALLBACK is called with nil."
  (let* ((url-request-method "POST")
         (url-request-extra-headers (lmwt--remote-headers))
         (body `((model . ,(if (eq lmwt-provider 'openai)
                               lmwt-openai-model
                             "gpt-4"))
                 (stream . t)
                 (messages . [((role . "system") (content . ,prompt))
                              ((role . "user") (content . ,text))])))
         (url-request-data (json-encode body))
         (url (lmwt--remote-url)))
    (url-retrieve
     url
     (lambda (_status)
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (while (re-search-forward "^data: \(.*\)$" nil t)
         (let ((data (match-string 1)))
           (unless (string= data "[DONE]")
             (let* ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json (json-read-from-string data))
                    (delta (aref (alist-get 'choices json) 0))
                    (content (alist-get 'content (alist-get 'delta delta))))
               (when content
                 (funcall callback content))))))
       (when callback (funcall callback nil))
       (kill-buffer (current-buffer))))))

;;;###autoload
(defun lmwt-request (text prompt callback)
  "Send TEXT to the configured LLM with PROMPT.
CALLBACK receives chunks of the response as they arrive.  When the
request is finished, CALLBACK is called with nil."
  (pcase lmwt-provider
    ('ollama (lmwt--request-ollama text prompt callback))
    ((or 'github 'openai) (lmwt--request-remote text prompt callback))
    (_ (error "Unknown provider %S" lmwt-provider))))

;;;###autoload
(define-minor-mode lm-writing-tool-mode
  "Minor mode for the LM Writing Tool.

When enabled, provides integration helpers for the LM Writing Tool." 
  :lighter " LM-WT"
  :group 'lm-writing-tool)

(provide 'lm-writing-tool)

;;; lm-writing-tool.el ends here
