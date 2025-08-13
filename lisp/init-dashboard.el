;;; init-dashboard.el --- A custom startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the `dashboard` package to display a personalized
;; startup screen, featuring a custom banner, recent files, and projects.
;;

;;; Code:

(use-package dashboard
  :init
  ;; This hook correctly sets up the dashboard to appear at startup.
  (dashboard-setup-startup-hook)
  :bind (("<f2>" . dashboard-open))
  :config
  ;;; --- Banner & Logo ---
  ;; Define a customizable variable for the banner file path.
  (defcustom logo (expand-file-name "banner.txt" user-emacs-directory)
    "The path to the startup banner file for the dashboard."
    :type 'file
    :group 'dashboard)
  (setq dashboard-banner-logo-title "泣くのは弱いからじやない 耐えられるのは強いからじやない")
  (setq dashboard-startup-banner logo)

  ;;; --- Appearance & Theming ---
  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-show-shortcuts nil)

  (set-face-attribute 'dashboard-items-face nil :weight 'normal)

  ;;; --- Content & Items ---
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
                          (bookmarks . 5))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
