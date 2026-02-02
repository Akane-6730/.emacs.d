;;; init-dashboard.el --- A custom startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the `dashboard` package to display a personalized
;; startup screen, featuring a custom banner, recent files, and projects.
;;

;;; Code:

(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :bind (("<f2>" . dashboard-open))
  :custom-face
  (dashboard-items-face    ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :config
    ;;; --- Appearance & Theming ---
  (setq dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory)
        dashboard-banner-logo-title "泣くのは弱いからじやない 耐えられるのは強いからじやない"
        dashboard-center-content t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 10)
                          (projects . 5)
                          (bookmarks . 5)))

  (when (find-font (font-spec :family "Maple Mono NF CN"))
    (set-face-attribute 'dashboard-text-banner nil
                        :family "Maple Mono NF CN")))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
