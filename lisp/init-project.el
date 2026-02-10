;;; init-project.el --- Initialize project settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file configures project management in Emacs using the built-in `project` package.
;; It customizes project commands, integrates with Magit for version control,
;; and provides a convenient way to open a VTerm in the project's root directory.

;;; Code:

(use-package project
  :init (setq project-switch-commands
              '((?f "Find file" project-find-file)
                (?g "Find regexp" project-find-regexp)
                (?d "Dired" project-dired)
                (?q "Query replace" project-query-replace-regexp)
                (?v "magit" project-magit-status)
                (?t "Vterm" my/project-vterm)
                (?e "Eshell" project-eshell)
                (?! "Shell command" project-shell-command)
                (?k "Kill buffers" project-kill-buffers)
                (?b "Buffer" project-switch-to-buffer)))
  :config
  (defun project-magit-status ()
    "Run magit-status in the current project's root."
    (interactive)
    (magit-status-setup-buffer (project-root (project-current t))))

  (defun my/project-vterm ()
    "Open or switch to a vterm buffer in the current project's root."
    (interactive)
    (let* ((project (project-current t))
           (root (project-root project))
           (default-directory root)
           (buf-name (format "*vterm-%s*"
                             (file-name-nondirectory
                              (directory-file-name root))))
           (buf (get-buffer buf-name)))
      (if (and buf (buffer-live-p buf))
          (pop-to-buffer buf)
        (vterm buf-name))))

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
         ("C-x p t" . my/project-vterm)))

(provide 'init-project)

;;; init-project.el ends here
