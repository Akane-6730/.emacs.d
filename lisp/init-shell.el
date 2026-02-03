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
            (Let ((spec (create-image
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
  :hook (eshell-mode . completion-preview-mode))

(use-package vterm
  :config (setq vterm-disable-bold t))

(provide 'init-shell)
;;; init-shell.el ends here
