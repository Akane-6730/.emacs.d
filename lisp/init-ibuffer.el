;;; init-ibuffer.el --- Ibuffer configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures Ibuffer, the powerful, built-in replacement for the
;; default buffer list.
;;
;;; Code:

;; Replaces the default `list-buffers` command with the superior `ibuffer`.
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Do not show buffers whose names start with a space. These are typically
  ;; internal or temporary buffers that we don't need to see.
  (setq ibuffer-show-empty-filter-groups nil)
  ;; This is the core configuration that makes ibuffer so powerful.
  ;; It sets up predefined filters to group buffers by their type.
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Emacs" (or (mode . lisp-interaction-mode)
                              (mode . emacs-lisp-mode)))
                 ("Dired" (mode . dired-mode))
                 ("Org"   (mode . org-mode))
                 ("Shell"  (or (mode . shell-mode) (mode . eshell-mode)
                               (mode . term-mode)))
                 ("Magit" (name . "^magit:"))))))
  ;; We add a hook to `ibuffer-mode` to automatically apply our saved filter
  ;; groups every time we open ibuffer.
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
