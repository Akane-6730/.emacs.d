;;; init-check.el --- On-the-fly syntax checking configuration -*- lexical-binding: t; -*-

;;
;; This file configures the framework for on-the-fly syntax checking.
;; We use Flymake, the modern, built-in checker available in Emacs 29+.
;; It automatically checks your code as you type and highlights errors.
;;

(use-package flymake
  :ensure nil ; This is a built-in package.
  ;; Enable Flymake automatically for all programming modes.
  :hook (prog-mode . flymake-mode)
  :bind (("C-c f" . consult-flymake))
  :config
  ;; By default, Flymake uses a fringe indicator (a symbol in the left margin)
  ;; to show errors, which is clean and unobtrusive.
  (setq flymake-fringe-indicator-position 'right-fringe)
  (add-to-list 'display-buffer-alist '("Flymake Diagnostics" nil (post-command-select-window .t))))


(provide 'init-check)
;;; init-check.el ends here
