;;; init-vcs.el --- Version Control System configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures packages related to version control systems,
;; with a primary focus on Git and the Magit interface.
;;

;;; Code:

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  ;; This setting makes diffs more granular and easier to read by showing
  ;; changes within each hunk at the word or character level.
  (setq magit-diff-refine-hunk t)

  ;; Optimize Magit performance on macOS by using pipes instead of PTYs.
  ;; This avoids the slow PTY allocation issue on macOS.
  ;; Reference: https://irreal.org/blog/?p=13567
  (when (eq system-type 'darwin)
    (setq magit-process-connection-type nil)))

;; provides major modes for editing Git-related files
(use-package git-modes)

(provide 'init-vcs)
;;; init-vcs.el ends here
