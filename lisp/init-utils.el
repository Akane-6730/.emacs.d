;;; init-utils.el --- Initialize utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Some useful Utilities.
;;
;;; Code:

;;;----------------------------------------------------------------------------
;;; Keybinding Hints
;;;----------------------------------------------------------------------------

;; (use-package which-key
;;   :ensure nil
;;   :hook (after-init . which-key-mode))

;;;----------------------------------------------------------------------------
;;; Search Tool Enhancement
;;;----------------------------------------------------------------------------

;; Replace the standard `grep` command with the much faster `ripgrep` (rg)
;; if it is available on the system.
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :config
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :init (setq rg-show-columns t)
  :config (add-to-list 'rg-custom-type-aliases '("tmpl" . "*.tmpl")))


;;----------------------------------------------------------------------------
;; Ediff Configuration
;;----------------------------------------------------------------------------

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;;----------------------------------------------------------------------------
;; Helper Functions
;;----------------------------------------------------------------------------

;; Compile packages in elpa directory.
(defun byte-compile-elpa ()
  "Compile packages in elpa directory."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(provide 'init-utils)
;;; init-utils.el ends here
