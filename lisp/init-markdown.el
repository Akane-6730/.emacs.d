;;; init-markdown.el --- Markdown mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Markdown mode configuration
;;

;;; Code:

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-hide-urls t)
  (markdown-hide-markup t)
  (markdown-enable-wiki-links t)
  (markdown-enable-highlighting-syntax t))


(provide 'init-markdown)
;;; init-markdown.el ends here
