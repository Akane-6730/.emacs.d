;;; init-snippet.el --- Code snippet expansion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures yasnippet for code snippet expansion in Emacs.
;;
;;; Code:

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt)))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-snippet)
;;; init-snippet.el ends here
