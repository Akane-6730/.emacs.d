;;; init-snippet.el --- Code snippet expansion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures Yasnippet, the standard snippet-expansion system
;; for Emacs. It allows you to type a short keyword and expand it into a
;; larger, boilerplate code structure.
;;
;;; Code:

;; This is the core engine for snippet expansion.
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  ;; A quality-of-life setting that makes navigating snippet fields feel
  ;; more natural and less intrusive.
  (setq yas-prompt-functions '(yas-no-prompt)))

;; This package is a large collection of ready-to-use snippets
;; for dozens of languages. It provides the actual content for
;; Yasnippet to use.
(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-snippet)
;;; init-snippet.el ends here
