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
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames))
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-Alh --group-directories-first")
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
        (setq dired-listing-switches "-Alh"))))

  (setq-local mouse-1-click-follows-link nil)
  (auto-revert-mode))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (concat dired-omit-files
          "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))

;; Colorful dired
(use-package diredfl
  :hook dired-mode)

(use-package nerd-icons-dired
  :hook dired-mode)

(use-package speedbar
  :ensure nil
  :bind (("M-0" . my/speedbar-window-toggle)
         :map speedbar-mode-map
         ("q"   . my/speedbar-window-quit)
         ("TAB" . speedbar-toggle-line-expansion)
         ("d"   . speedbar-item-delete)
         ("c"   . speedbar-item-copy)
         ("r"   . speedbar-item-rename)
         ([remap isearch-forward] . isearch-forward))
  :custom
  (speedbar-show-unknown-files t)
  (speedbar-vc-do-check nil)
  (speedbar-obj-do-check nil)
  :custom-face
  (speedbar-directory-face ((t (:inherit dired-directory))))
  (speedbar-selected-face  ((t (:inherit dired-marked))))
  :config
  (setq speedbar-window-default-width 30)
  (defun my/speedbar-window-toggle ()
    "Toggle the speedbar side window; select it when opening."
    (interactive)
    (let ((was-live (and (fboundp 'speedbar-window--live-p)
                         (speedbar-window--live-p))))
      (speedbar-window)
      (when (and (not was-live)
                 (boundp 'speedbar--window)
                 (window-live-p speedbar--window))
        (select-window speedbar--window))))

  (defun my/speedbar-window-quit ()
    "Close the speedbar side window."
    (interactive)
    (when (and (fboundp 'speedbar-window--live-p)
               (speedbar-window--live-p))
      (speedbar-window))))

(use-package nerd-icons-speedbar
  :vc (:url "https://github.com/Akane-6730/nerd-icons-speedbar")
  :hook (speedbar-mode . nerd-icons-speedbar-mode))

(provide 'init-dired)
;;; init-dired.el ends here
