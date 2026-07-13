;;; init-markdown.el --- Markdown mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Markdown mode configuration
;;

;;; Code:

(use-package markdown-ts-mode
  :ensure nil
  :config
  (setq markdown-ts-unordered-list-marker
        '(("• " . "- ") ("– " . "• ")))
  :custom-face
  (markdown-ts-list-marker ((t (:family "Maple Mono Normal NF CN")))))

;; npm install -g git+https://gitlab.com/matsievskiysv/math-preview
(use-package math-preview
  :hook (markdown-ts-view-mode . math-preview-all))

(provide 'init-markdown)
;;; init-markdown.el ends here
