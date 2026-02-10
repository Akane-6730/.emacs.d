;;; init-project.el --- Initialize project settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file configures project management in Emacs using the built-in `project` package.
;; It customizes project commands, integrates with Magit for version control,
;; and provides a convenient way to open a VTerm in the project's root directory.

;;; Code:

(use-package project
  :init (setq project-switch-commands
              '((?f "Find file" project-find-file)
                (?d "Dired" project-dired)
                (?g "Find regexp" project-find-regexp)
                (?q "Query replace" project-query-replace-regexp)
                (?v "Magit" project-magit-status)
                (?t "Term" eat-project)
                (?e "Eshell" project-eshell)
                ;; (?k "Kill buffers" project-kill-buffers)
                ;; (?! "Shell command" project-shell-command)
                ;; (?b "Buffer" project-switch-to-buffer)
                ))
  :config
  (defun project-magit-status ()
    "Run magit-status in the current project's root."
    (interactive)
    (magit-status-setup-buffer (project-root (project-current t))))

  (defun my/project-remove-project ()
    "Remove project from `project--list' using completion."
    (interactive)
    (project--ensure-read-project-list)
    (let* ((projects project--list)
           (dir (completing-read "REMOVE project from list: " projects nil t)))
      (setq project--list (delete (assoc dir projects) projects))))

  (setq project-vc-merge-submodules nil)

  :bind (("C-x p q" . project-query-replace-regexp) ; C-x p is `project-prefix-map'
         ("C-x p <delete>" . my/project-remove-project)
         ("C-x p DEL" . my/project-remove-project)
         ("C-x p t" . eat-project)))

(provide 'init-project)

;;; init-project.el ends here
