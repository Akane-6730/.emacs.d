;;; init-check.el --- On-the-fly syntax checking configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; This file configures the framework for on-the-fly syntax checking.
;; We use Flymake, the modern, built-in checker available in Emacs 29+.
;; It automatically checks your code as you type and highlights errors.
;;

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (("C-c f" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   '((error "»" compilation-error)
     (warning "»" compilation-warning)
     (note "»" compilation-info)))
  :config
  (setq flymake-no-changes-timeout nil)
  (setq flymake-show-diagnostics-at-end-of-line 'short)
  (add-hook 'flymake-mode-hook
            (lambda ()
              (remove-hook 'eldoc-documentation-functions
                           #'flymake-eldoc-function t)))
  (add-to-list 'display-buffer-alist
               '("Flymake Diagnostics" nil (post-command-select-window . t))))


(provide 'init-check)
;;; init-check.el ends here
