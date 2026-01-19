;;; init-shell.el --- Shell and terminal emulator configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; eshell and vterm configurations for an enhanced shell experience in Emacs.

;;; Code:

(use-package eshell
  :ensure nil
  :config
  ;; Sensible defaults for a better user experience.
  (setq eshell-history-size 10000
        eshell-hist-ignoredups t ; Don't save duplicate commands in history
        eshell-directory-name-completion-ignore-case t
        eshell-list-files-after-cd t) ; Automatically list files after `cd`
  :hook (eshell-mode . (lambda () (completion-preview-mode 1))))

(use-package vterm
  :config (setq vterm-disable-bold t))

(provide 'init-shell)
;;; init-shell.el ends here
