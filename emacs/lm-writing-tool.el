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

;;;###autoload
(define-minor-mode lm-writing-tool-mode
  "Minor mode for the LM Writing Tool.

When enabled, provides integration helpers for the LM Writing Tool." 
  :lighter " LM-WT"
  :group 'lm-writing-tool)

(provide 'lm-writing-tool)

;;; lm-writing-tool.el ends here
