;;; init-workspace.el --- Initialize workspace configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Workspace configurations.
;;

;;; Code:

(use-package tabspaces
  :diminish
  :bind ("C-c b" . tabspaces-switch-or-create-workspace)
  :hook (on-first-input . my/tabspaces-enable)
  :init
  (defun my/tabspaces-enable ()
    "Enable tabspaces; rename initial tab to `tabspaces-default-tab'."
    (tabspaces-mode 1)
    (tab-bar-history-mode 1)
    (unless (member tabspaces-default-tab (tabspaces--list-tabspaces))
      (tab-bar-rename-tab tabspaces-default-tab)))
  :custom
  (tab-bar-show nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-exclude-buffers '("*eat*" "*vterm*" "*shell*" "*eshell*" "*dashboard*" "*Ibuffer*"))
  (tab-bar-new-tab-choice "*scratch*")
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session nil)
  (tabspaces-session-auto-restore nil)
  :config
  (setq consult-buffer-list-function #'consult--frame-buffer-list)

  (defvar my/tabspaces--auto-switch-inhibit nil)

  (defun my/tabspaces-auto-switch-workspace ()
    "On find-file, switch/create the project workspace (else Default)."
    (when (and tabspaces-mode (not my/tabspaces--auto-switch-inhibit)
               buffer-file-name (not (minibufferp)))
      (let* ((my/tabspaces--auto-switch-inhibit t)
             (buf (current-buffer))
             (root (when-let ((p (project-current)))
                     (expand-file-name (project-root p))))
             (name (if root
                       (or (cdr (assoc root tabspaces-project-tab-map))
                           (tabspaces-generate-descriptive-tab-name
                            root (tabspaces--list-tabspaces)))
                     tabspaces-default-tab)))
        (unless (string= name (tabspaces--current-tab-name))
          (tabspaces-switch-or-create-workspace name)
          (switch-to-buffer buf)))))

  (add-hook 'find-file-hook #'my/tabspaces-auto-switch-workspace))

(provide 'init-workspace)
;;; init-workspace.el ends here
