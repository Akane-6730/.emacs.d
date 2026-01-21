;;; init-lsp.el --- Language Server Protocol (LSP) base setup -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file sets up the Language Server Protocol (LSP) support in Emacs
;; using Eglot, the built-in LSP client.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Core Eglot Configuration
;;----------------------------------------------------------------------------

;; When using tramp, don't start eglot
(defun my-eglot-conditional-ensure ()
  "Run `eglot-ensure` only if the current file is not remote."
  (unless (file-remote-p default-directory)
    (eglot-ensure)))

(use-package eglot
  :ensure nil
  :hook (prog-mode . my-eglot-conditional-ensure)
  :config
  (setq eglot-autoshutdown t)
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-events-buffer-config '(:size 0 :format full))
  ;; Emacs 31 specific: restrict semantic tokens to avoid excessive highlighting
  (setq eglot-semantic-token-types
        '("type" "class" "struct" "interface" "enum" "enumMember" "function" "method" "namespace" "decorator" "parameter" "variable"))
  (setq eglot-semantic-token-modifiers
        '("static" "deprecated" "abstract" "async" "modification" "documentation"))) ; "readonly"

;;----------------------------------------------------------------------------
;; Integration with our Completion System
;;----------------------------------------------------------------------------

;; This package provides Consult integration for Eglot, offering a much
;; nicer UI for Browse things like document symbols (functions, variables).
(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(provide 'init-lsp)
;;; init-lsp.el ends here
