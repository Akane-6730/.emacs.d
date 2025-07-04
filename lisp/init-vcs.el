;;; init-vcs.el --- Version Control System configuration -*- lexical-binding: t; -*-

;;
;; This file configures packages related to version control systems,
;; with a primary focus on Git and the Magit interface.
;;

;;----------------------------------------------------------------------------
;; The Core Git Interface: Magit
;;----------------------------------------------------------------------------

;;
;; Package: magit
;; The definitive Git client for Emacs. Magit provides a complete, text-based
;; user interface for Git, allowing you to perform virtually any Git operation
;; quickly and efficiently without ever leaving your editor. It is the
;; cornerstone of our VCS setup.
;;
(use-package magit
  :ensure t
  ;; `C-x g` is the universal, standard keybinding for Magit's status window.
  ;; This is a critical binding for any Git user in Emacs.
  :bind (("C-x g" . magit-status))
  :config
  ;; This setting makes diffs more granular and easier to read by showing
  ;; changes within each hunk at the word or character level.
  (setq magit-diff-refine-hunk t))


;;----------------------------------------------------------------------------
;; Git-related File Modes
;;----------------------------------------------------------------------------

;;
;; Package: git-modes
;; This package provides major modes for editing Git-related files, such as
;; `.gitignore`, `.gitattributes`, and `.git/config`. It adds proper syntax
;; highlighting, indentation, and commenting support for these file types.
;;
(use-package git-modes
  :ensure t)


(provide 'init-vcs)
;;; init-vcs.el ends here