;;; init-ibuffer.el --- Ibuffer configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures Ibuffer, the powerful, built-in replacement for the
;; default buffer list.
;;
;;; Code:

;; Replaces the default `list-buffers` command with the superior `ibuffer`.
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;; Group ibuffer's list by project
(use-package ibuffer-project
  :autoload (ibuffer-project-generate-filter-groups ibuffer-do-sort-by-project-file-relative)
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                       (ibuffer-do-sort-by-project-file-relative)))
  :init (setq ibuffer-project-use-cache t)
  :config
  (with-no-warnings
    (defun my-ibuffer-project-group-name (root type)
      "Return group name for project ROOT and TYPE."
      (if (and (stringp type) (> (length type) 0))
          (format "%s %s" type root)
        (format "%s" root)))

    (progn
      (advice-add #'ibuffer-project-group-name :override #'my-ibuffer-project-group-name)
      (setq ibuffer-project-root-functions
            `((ibuffer-project-project-root . ,(nerd-icons-octicon "nf-oct-repo" :face ibuffer-filter-group-name-face))
              (file-remote-p . ,(nerd-icons-codicon "nf-cod-radio_tower" :face ibuffer-filter-group-name-face)))))))


(use-package nerd-icons-ibuffer
  :hook ibuffer-mode)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
