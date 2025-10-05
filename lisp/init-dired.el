;;; init-dired.el --- Dired (the file manager) configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures Dired, the built-in Emacs file manager.
;; We enhance it with icons, font-locking (colors), and modern behaviors.
;;
;;; Code:

;; Core Dired Behavior
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;; On macOS, dired needs `gls' (GNU ls) for compatibility; without it, dired may break.
  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-alh"))))

  (add-hook 'dired-mode-hook #'auto-revert-mode))

;; Dired Colorful Highlighting
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Dired Icons
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
