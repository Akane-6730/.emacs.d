;;; init-dashboard.el --- A custom startup dashboard configuration -*- lexical-binding: t; -*-

;;
;; This file configures a personalized startup dashboard. It now uses the
;; canonical methods to ensure content is centered and the ASCII banner is
;; correctly aligned in both GUI and TUI.
;;

(use-package dashboard
  :ensure t
  :init
  ;; This hook correctly sets up the dashboard to appear at startup.
  (dashboard-setup-startup-hook)
  :bind (("<f2>" . dashboard-open))
  :config
  (defvar my-dashboard-banner-file (expand-file-name
                                    (if (display-graphic-p) "" "banner.txt")
                                    user-emacs-directory))

  (setq dashboard-startup-banner my-dashboard-banner-file)

  ;; We use `set-face-attribute` for a more direct and forceful override
  ;; of any theme-provided bold fonts.
  (set-face-attribute 'dashboard-banner-logo-title nil :weight 'normal)
  (set-face-attribute 'dashboard-heading nil :weight 'normal)
  (set-face-attribute 'dashboard-items-face nil :weight 'normal)

  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)))
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-banner-logo-title "泣くのは弱いからじやない 耐えられるのは強いからじやない"))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
