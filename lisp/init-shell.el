;;; init-shell.el --- Shell and terminal emulator configurations -*- lexical-binding: t; -*-

;;
;; This file configures Eshell, the powerful shell written entirely in
;; Emacs Lisp. This setup is dependency-free and integrates with popper.el
;; for a popup terminal experience.
;;

(use-package eshell
  :ensure nil ; This is a built-in package.
  :config
  ;; Sensible defaults for a better user experience.
  (setq eshell-history-size 10000
        eshell-hist-ignoredups t ; Don't save duplicate commands in history
        eshell-directory-name-completion-ignore-case t
        eshell-list-files-after-cd t)) ; Automatically list files after `cd`

;;
;; We configure `popper.el` (from `init-window.el`) to treat Eshell buffers
;; as popups. This is much simpler and more robust than custom popup solutions.
;;
(with-eval-after-load 'popper
  (add-to-list 'popper-reference-buffers 'eshell-mode))

(provide 'init-shell)
;;; init-shell.el ends here