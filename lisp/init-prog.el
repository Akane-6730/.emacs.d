;;; init-prog.el --- Base configurations for programming -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Tree-sitter Foundation
;;----------------------------------------------------------------------------

;;
;; Package: treesit-auto
;; automatically download and install the required Tree-sitter grammar
;; for any major mode that supports it, upon your confirmation.
;;

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :config
  ;; When a new grammar is needed, always ask for confirmation before installing.
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4))


;;----------------------------------------------------------------------------
;; Code Insight and Navigation
;;----------------------------------------------------------------------------

;;
;; Feature: Eldoc
;; Eldoc is a built-in minor mode that displays information about the function
;; or variable at point in the echo area (the bottom-most line of Emacs).
;; For example, it will show the argument list of the function you are
;; currently calling, which is extremely helpful.
;;

(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode))

;;
;; Feature: Xref
;; Xref is the built-in framework for cross-referencing (e.g., "find
;; definitions", "find references"). We have already integrated it with
;; `consult` in `completion.el` for a better UI. Here, we just set a keybinding
;; for quickly jumping back from a definition to where you were before.
;;

(use-package xref
  :ensure nil
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use ripgrep for faster text-based searches within xref.
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

;;----------------------------------------------------------------------------
;; Quick Code Execution
;;----------------------------------------------------------------------------

(use-package quickrun
  :bind (("C-c x" . quickrun)
         ("C-c s" . quickrun-shell)
         ("<f5>". quickrun)))

;;----------------------------------------------------------------------------
;; Project-specific Settings
;;----------------------------------------------------------------------------

;;
;; Package: editorconfig
;; EditorConfig helps maintain consistent coding styles for multiple developers
;; working on the same project across various editors and IDEs. It automatically
;; applies settings (like indent style, tab width) from a `.editorconfig`
;; file found in the project root.
;;

(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(provide 'init-prog)
;;; init-prog.el ends here
