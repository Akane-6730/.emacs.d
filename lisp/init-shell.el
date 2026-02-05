;;; init-shell.el --- Shell and terminal emulator configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; eshell and vterm configurations for an enhanced shell experience in Emacs.

;;; Code:

(use-package eshell
  :ensure nil
  :config
  (setq eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-directory-name-completion-ignore-case t
        eshell-list-files-after-cd t)

  ;; Enable image support in `eshell/cat`
  (defun adviced:eshell/cat (orig-fun &rest args)
    "Like `eshell/cat' but with image support."
    (if (seq-every-p (lambda (arg)
                       (and (stringp arg)
                            (file-exists-p arg)
                            (image-supported-file-p arg)))
                     args)
        (with-temp-buffer
          (insert "\n")
          (dolist (path args)
            (let ((spec (create-image
                         (expand-file-name path)
                         (image-type-from-file-name path)
                         nil :max-width 350
                         :conversion (lambda (data) data))))
              (image-flush spec)
              (insert-image spec))
            (insert "\n"))
          (insert "\n")
          (buffer-string))
      (apply orig-fun args)))

  (advice-add #'eshell/cat :around #'adviced:eshell/cat)

  (defun my-eshell-setup ()
    "Configure eshell: Ghost text on, Corfu off."
    ;; Enable Ghost Text (Completion Preview)
    (setq-local completion-preview-minimum-symbol-length 1)
    (completion-preview-mode 1)
    ;; Disable Corfu Popup Menu
    (corfu-mode -1))

  :hook (eshell-mode . my-eshell-setup))

(use-package vterm
  :config (setq vterm-disable-bold t))

;; Bind C-e to accept completion-preview (ghost text) when active
(use-package completion-preview
  :ensure nil
  :bind (:map completion-preview-active-mode-map
              ("C-e" . completion-preview-insert)))

(provide 'init-shell)
;;; init-shell.el ends here
