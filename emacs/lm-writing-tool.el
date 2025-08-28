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

;;;###autoload
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

;;;; Proofreading and corrections

;;;###autoload
(defcustom lmwt-proofreading-prompt
  "Proofread the following message in American English. If it is gramatically correct, just respond with the word \"Correct\". If it is gramatically incorrect or has spelling mistakes, respond with \"Correction: \" followed by the corrected version."
  "Prompt template used for proofreading.  The snippet to proofread is appended to this string."
  :type 'string
  :group 'lm-writing-tool)

;;;###autoload
(defcustom lmwt-rewrite-prompt
  "Rewrite the following text for clarity in American English. Do not change special commands, code, escape characters, or mathematical formulas. Respond just with the rewritten version of the text, no extra explanation."
  "Prompt template used for rewriting text.  The snippet to rewrite is appended to this string."
  :type 'string
  :group 'lm-writing-tool)

;;;###autoload
(defcustom lmwt-synonyms-prompt
  "Give up to 5 synonyms for the following expression. Just respond with the synonyms, separated by newlines. No extra explanation or context needed."
  "Prompt template used for finding synonyms.  The expression to inspect is appended to this string."
  :type 'string
  :group 'lm-writing-tool)

(defvar lmwt--diagnostics-cache (make-hash-table :test 'equal)
  "Cache mapping text snippets to diagnostics.")

(cl-defstruct lmwt-snippet text beg end)

(defun lmwt-split-buffer-by-line ()
  "Split current buffer into line snippets.
Return a list of `lmwt-snippet` structures containing text and
buffer positions."
  (save-excursion
    (goto-char (point-min))
    (let ((snippets '())
          (line-beg (point-min)))
      (while (< (point) (point-max))
        (let* ((beg (point))
               (end (line-end-position))
               (text (buffer-substring-no-properties beg end)))
          (push (make-lmwt-snippet :text text :beg beg :end end) snippets)
          (setq line-beg (1+ end))
          (goto-char line-beg)))
      ;; Trailing newline results in empty snippet like VS Code implementation
      (when (= (char-before (point-max)) ?\n)
        (push (make-lmwt-snippet :text "" :beg (point-max) :end (point-max)) snippets))
      (nreverse snippets))))

(defun lmwt-request-sync (text prompt)
  "Synchronously send TEXT with PROMPT to the configured LLM.
Returns the complete response as a string."
  (let ((result "")
        (done nil))
    (lmwt-request text prompt
                  (lambda (chunk)
                    (if chunk
                        (setq result (concat result chunk))
                      (setq done t))))
    (while (not done)
      (accept-process-output nil 0.1))
    result))

(defun lmwt-get-text-snippet-diagnostic (text)
  "Return diagnostic information for TEXT, using cache when possible.
The diagnostic is an alist containing `:corrected-version` when a
correction was suggested."
  (or (gethash text lmwt--diagnostics-cache)
      (let* ((resp (lmwt-request-sync text lmwt-proofreading-prompt))
             (diag (when (and resp (string-prefix-p "Correction: " resp))
                     (list :corrected-version (substring resp (length "Correction: "))))))
        (puthash text diag lmwt--diagnostics-cache)
        diag)))

(defun lmwt--diff-chars (a b)
  "Return character diff between strings A and B.
Result is a list of plists with keys :value, :added and :removed."
  (let* ((n (length a))
         (m (length b))
         (matrix (make-vector (1+ n) nil)))
    (dotimes (i (1+ n))
      (aset matrix i (make-vector (1+ m) 0)))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref (aref matrix (1+ i)) (1+ j))
              (if (eq (aref a i) (aref b j))
                  (1+ (aref (aref matrix i) j))
                (max (aref (aref matrix i) (1+ j))
                     (aref (aref matrix (1+ i)) j))))))
    (let ((i n) (j m) (diff '()))
      (while (and (> i 0) (> j 0))
        (cond
         ((eq (aref a (1- i)) (aref b (1- j)))
          (push (list :value (string (aref a (1- i)))) diff)
          (setq i (1- i) j (1- j)))
         ((> (aref (aref matrix (1- i)) j) (aref (aref matrix i) (1- j)))
          (push (list :value (string (aref a (1- i))) :removed t) diff)
          (setq i (1- i)))
         (t
          (push (list :value (string (aref b (1- j))) :added t) diff)
          (setq j (1- j)))))
      (while (> i 0)
        (push (list :value (string (aref a (1- i))) :removed t) diff)
        (setq i (1- i)))
      (while (> j 0)
        (push (list :value (string (aref b (1- j))) :added t) diff)
        (setq j (1- j)))
      ;; merge adjacent segments of same type
      (let ((merged '()))
        (dolist (d diff)
          (let ((prev (car merged)))
            (if (and prev
                     (eq (plist-get prev :added) (plist-get d :added))
                     (eq (plist-get prev :removed) (plist-get d :removed)))
                (setf (plist-get prev :value)
                      (concat (plist-get prev :value) (plist-get d :value)))
              (push (copy-sequence d) merged))))
        (nreverse merged)))))

(defun lmwt-apply-corrections (text corrections)
  "Apply CORRECTIONS to TEXT and return the result.
CORRECTIONS is a list of plists with :start, :end and :to-insert."
  (let ((result text) (offset 0))
    (dolist (c corrections)
      (let ((s (+ (plist-get c :start) offset))
            (e (+ (plist-get c :end) offset))
            (ins (plist-get c :to-insert)))
        (setq result (concat (substring result 0 s) ins (substring result e)))
        (setq offset (+ offset (- (length ins) (- (plist-get c :end)
                                             (plist-get c :start)))))))
    result))

(defun lmwt-calculate-corrections (original corrected)
  "Calculate corrections to transform ORIGINAL into CORRECTED.
Returns a list of plists with :start, :end and :to-insert."
  (let* ((diffs (lmwt--diff-chars original corrected))
         (corrections '())
         (current 0)
         (pending nil))
    (dolist (d diffs)
      (cond
       ((plist-get d :added)
        (if pending
            (progn
              (push (list :start (plist-get pending :start)
                          :end (plist-get pending :end)
                          :to-insert (plist-get d :value)) corrections)
              (setq pending nil))
          (push (list :start current :end current :to-insert (plist-get d :value)) corrections)))
       ((plist-get d :removed)
        (let ((len (length (plist-get d :value))))
          (if pending
              (setf (plist-get pending :end) (+ (plist-get pending :end) len))
            (setq pending (list :start current :end (+ current len) :to-insert "")))
          (setq current (+ current len))))
       (t
        (when pending
          (push pending corrections)
          (setq pending nil))
        (setq current (+ current (length (plist-get d :value)))))))
    (when pending (push pending corrections))
    ;; collapse corrections to word boundaries similar to TypeScript version
    (let ((words (split-string original "\\s+" t))
          (idx 0)
          (result '()))
      (dolist (w words)
        (let* ((start idx)
               (end (+ idx (length w)))
               (changes (cl-remove-if-not (lambda (c)
                                            (and (>= (plist-get c :start) start)
                                                 (<= (plist-get c :end) end)))
                                          corrections))
               (corr-word (lmwt-apply-corrections (substring original start end)
                                                  (mapcar (lambda (c)
                                                            (list :start (- (plist-get c :start) start)
                                                                  :end (- (plist-get c :end) start)
                                                                  :to-insert (plist-get c :to-insert)))
                                                          changes))))
          (unless (string= corr-word (substring original start end))
            (push (list :start start :end end :to-insert corr-word) result))
          (setq idx (+ end 1)))
      (nreverse result))))

(defun lmwt-clear-overlays ()
  "Remove all LM Writing Tool overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'lmwt-correction t))

(defun lmwt-highlight-corrections ()
  "Fetch diagnostics for the current buffer and highlight corrections."
  (interactive)
  (lmwt-clear-overlays)
  (dolist (snippet (lmwt-split-buffer-by-line))
    (let* ((text (lmwt-snippet-text snippet))
           (diag (lmwt-get-text-snippet-diagnostic text))
           (corrected (plist-get diag :corrected-version)))
      (when (and corrected (not (string= text corrected)))
        (dolist (c (lmwt-calculate-corrections text corrected))
          (let* ((beg (+ (lmwt-snippet-beg snippet) (plist-get c :start)))
                 (end (+ (lmwt-snippet-beg snippet) (plist-get c :end)))
                 (ov (make-overlay beg end)))
            (overlay-put ov 'face 'flymake-error)
            (overlay-put ov 'lmwt-correction (plist-get c :to-insert))
            (overlay-put ov 'help-echo (format "Replace with: %s" (plist-get c :to-insert))))))))

;;;###autoload
(defun lmwt-apply-correction ()
  "Apply the correction at point, if any."
  (interactive)
  (let ((ov (cl-find-if (lambda (o) (overlay-get o 'lmwt-correction))
                        (overlays-at (point)))))
    (if ov
        (let ((replacement (overlay-get ov 'lmwt-correction)))
          (delete-region (overlay-start ov) (overlay-end ov))
          (insert replacement)
          (delete-overlay ov))
      (message "No correction at point"))))

;;;###autoload
(defun lmwt-check-buffer ()
  "Check current buffer for issues and highlight suggested corrections."
  (interactive)
  (lmwt-highlight-corrections))

;;;; Rewriting and synonyms

;;;###autoload
(defun lmwt-rewrite-region (beg end)
  "Rewrite the region between BEG and END using the configured LLM.
Displays the rewritten text in a temporary buffer and replaces the
original region after confirmation."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((text (buffer-substring-no-properties beg end))
         (resp (lmwt-request-sync text lmwt-rewrite-prompt))
         (buf (get-buffer-create "*lmwt-rewrite*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert resp)
      (goto-char (point-min)))
    (display-buffer buf)
    (when (yes-or-no-p "Insert rewrite? ")
      (delete-region beg end)
      (insert resp))
    (kill-buffer buf)))

;;;###autoload
(defun lmwt-synonyms (beg end)
  "Offer synonyms for the region between BEG and END and replace it.
Synonyms are fetched from the configured LLM and presented via
`completing-read'."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((expr (buffer-substring-no-properties beg end))
         (resp (lmwt-request-sync expr lmwt-synonyms-prompt))
         (options (split-string resp "\n" t)))
    (if (null options)
        (message "No synonyms found")
      (let ((choice (completing-read "Choose synonym: " options nil t expr)))
        (delete-region beg end)
        (insert choice)))))

;;;; Model selection

;;;###autoload
(defun lmwt-select-model ()
  "Interactively select the LLM provider and model.
The choice is persisted via `customize-save-variable'."
  (interactive)
  (let* ((providers '(("Local Ollama" . ollama)
                      ("OpenAI" . openai)
                      ("GitHub Copilot" . github)))
         (current (car (rassoc lmwt-provider providers)))
         (selection (completing-read "Select provider: " (mapcar #'car providers)
                                     nil t nil nil current))
         (provider (cdr (assoc selection providers))))
    (pcase provider
      ('ollama
       (let ((model (read-string (format "Ollama model (current %s): " lmwt-ollama-model)
                                   nil nil lmwt-ollama-model)))
         (setq lmwt-ollama-model model)
         (customize-save-variable 'lmwt-ollama-model model)))
      ('openai
       (let ((model (read-string (format "OpenAI model (current %s): " lmwt-openai-model)
                                   nil nil lmwt-openai-model)))
         (setq lmwt-openai-model model)
         (customize-save-variable 'lmwt-openai-model model))))
    (setq lmwt-provider provider)
    (customize-save-variable 'lmwt-provider provider)
    (message "LM Writing Tool provider set to %s" provider)))

;;;###autoload
(define-minor-mode lm-writing-tool-mode
  "Minor mode for the LM Writing Tool.

When enabled, provides integration helpers for the LM Writing Tool." 
  :lighter " LM-WT"
  :group 'lm-writing-tool)

(provide 'lm-writing-tool)

;;; lm-writing-tool.el ends here
