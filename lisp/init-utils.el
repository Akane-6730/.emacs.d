;;; init-utils.el --- Initialize utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Some useful Utilities.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Keybinding Hints
;;----------------------------------------------------------------------------

(use-package which-key
  :ensure nil
  :diminish
  :hook on-first-input
  :config
  (setq which-key-idle-delay 2))

(use-package which-key-posframe
  :diminish
  :defines posframe-border-width
  :hook which-key-mode
  :config
  (setq which-key-posframe-border-width posframe-border-width
        which-key-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8))))


;;----------------------------------------------------------------------------
;; Search Tool Enhancement
;;----------------------------------------------------------------------------

(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :config
  (grep-apply-setting
   'grep-command "rg --color=auto --null -nH --no-heading -e ")
  (grep-apply-setting
   'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
  (grep-apply-setting
   'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
  (grep-apply-setting
   'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))

;; Writable `grep' buffer
(use-package wgrep
  :config (setq wgrep-auto-save-buffer t
                wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :bind (("C-c /" . rg-menu)
         :map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :config
  (add-to-list 'rg-custom-type-aliases '("tmpl" . "*.tmpl"))
  (setq rg-show-columns t))


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
;; Input Method
;;----------------------------------------------------------------------------

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(rime-predicate-after-alphabet-char-p ; Switch to English after alphabet characters
     rime-predicate-prog-in-code-p        ; Force English in code (except comments/strings)
     rime-predicate-space-after-cc-p      ; Support mixed English/Chinese (space after Chinese)
     )))

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
