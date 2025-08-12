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


;;----------------------------------------------------------------------------
;; Format on Save
;;----------------------------------------------------------------------------
;;
;; This section enables the "format on save" feature. It uses the powerful
;; `my-format-buffer-smart` command (defined in `init-kbd.el`) to automatically
;; format the buffer right before it's saved.
;;

(defun my-prog-mode-setup-format-on-save ()
  "A helper function to enable format-on-save for the current buffer."
  ;; Add the smart formatting function to the `before-save-hook`.
  ;; This hook runs just before a buffer is written to its file.
  ;; We make the hook buffer-local to ensure this behavior only applies
  ;; to the current programming buffer and doesn't affect other modes.
  (add-hook 'before-save-hook #'my-format-buffer-smart nil 'local))

;;
;; We add our setup function to `prog-mode-hook`. This means that
;; whenever you enter a major mode for programming (like `c-mode`, `python-mode`, etc.),
;; the format-on-save behavior will be automatically activated for that buffer.
;;
(add-hook 'prog-mode-hook #'my-prog-mode-setup-format-on-save)

(provide 'init-prog)
;;; init-prog.el ends here
