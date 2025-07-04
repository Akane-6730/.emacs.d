;;; init-snippet.el --- Code snippet expansion configuration -*- lexical-binding: t; -*-

;;
;; This file configures Yasnippet, the standard snippet-expansion system
;; for Emacs. It allows you to type a short keyword and expand it into a
;; larger, boilerplate code structure.
;;

;;
;; Package: yasnippet
;; This is the core engine for snippet expansion. We enable it globally.
;;
(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  ;; A quality-of-life setting that makes navigating snippet fields feel
  ;; more natural and less intrusive.
  (setq yas-prompt-functions '(yas-no-prompt)))

;;
;; Package: yasnippet-snippets
;; This package is a large, community-maintained collection of ready-to-use
;; snippets for dozens of languages. It provides the actual content for
;; Yasnippet to use. Without it, Yasnippet is just an empty engine.
;;
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


(provide 'init-snippet)
;;; init-snippet.el ends here
