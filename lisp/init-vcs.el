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
  (setq magit-diff-refine-hunk t))

;; provides major modes for editing Git-related files
(use-package git-modes)

(provide 'init-vcs)
;;; init-vcs.el ends here
