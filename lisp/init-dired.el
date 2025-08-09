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

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; --- macOS Compatibility Fix ---
  ;; The default `ls` on macOS is BSD-based and lacks features of GNU `ls`,
  ;; which can cause errors with dired. This setup checks for `gls` (GNU ls,
  ;; typically installed via Homebrew's `coreutils`) and uses it if available.
  ;; If not, it falls back to a compatible mode for the native BSD `ls`.
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
