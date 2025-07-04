;;; init-dired.el --- Dired (the file manager) configuration -*- lexical-binding: t; -*-

;;
;; This file configures Dired, the built-in Emacs file manager.
;; We enhance it with icons, font-locking (colors), and modern behaviors.
;;

;;----------------------------------------------------------------------------
;; Core Dired Behavior
;;----------------------------------------------------------------------------

(use-package dired
  :ensure nil ; This is a built-in package.
  :config
  ;; When copying or deleting, `dired-dwim-target` allows you to select a
  ;; window with a Dired buffer as the target, which is very convenient.
  (setq dired-dwim-target t)

  ;; A modern and readable default set of switches for the `ls` command.
  ;; `--group-directories-first` is especially useful.
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; This ensures that dired buffers are automatically refreshed when the
  ;; underlying directory changes on disk.
  (add-hook 'dired-mode-hook #'auto-revert-mode))

;;----------------------------------------------------------------------------
;; Dired Colorful Highlighting
;;----------------------------------------------------------------------------

;;
;; Package: diredfl
;; This package provides "font-locking" (i.e., syntax highlighting) for
;; Dired buffers. It is what adds color to the permissions, file sizes,
;; dates, and filenames, making the buffer much easier to read at a glance.
;;

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;;----------------------------------------------------------------------------
;; Dired Icons
;;----------------------------------------------------------------------------

;;
;; Package: nerd-icons-dired
;; This package integrates nerd-icons into Dired, displaying an appropriate
;; icon next to each file and directory. This is a major visual enhancement.
;;
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
