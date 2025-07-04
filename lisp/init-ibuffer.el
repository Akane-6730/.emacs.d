;;; init-ibuffer.el --- Ibuffer (the buffer list) configuration -*- lexical-binding: t; -*-

;;
;; This file configures Ibuffer, the powerful, built-in replacement for the
;; default buffer list. We enable it and set it up to group buffers by their
;; major mode, making it much easier to navigate many open buffers.
;;

;;
;; Package: ibuffer
;; Replaces the default `list-buffers` command with the superior `ibuffer`.
;;
(use-package ibuffer
  :ensure nil ; This is a built-in package.
  ;; We replace the standard `C-x C-b` keybinding to always use ibuffer.
  :bind (("C-x C-b" . ibuffer))
  :config
  ;; Do not show buffers whose names start with a space. These are typically
  ;; internal or temporary buffers that we don't need to see.
  (setq ibuffer-show-empty-filter-groups nil))

;;
;; This is the core configuration that makes ibuffer so powerful.
;; It sets up predefined filters to group buffers by their type.
;;
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Emacs" (or (mode . lisp-interaction-mode)
                              (mode . helpful-mode)
                              (mode . emacs-lisp-mode)))
               ("Dired" (mode . dired-mode))
               ("Org"   (mode . org-mode))
               ("Magit" (name . "^magit:"))))))

;; We add a hook to `ibuffer-mode` to automatically apply our saved filter
;; groups every time we open ibuffer.
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here