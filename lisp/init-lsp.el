;;; init-lsp.el --- Language Server Protocol (LSP) base setup -*- lexical-binding: t; -*-

;;
;; This file configures the base for our Language Server Protocol support
;; using the built-in `eglot` client. This module itself only sets up the
;; framework; specific language servers (like clangd for C, or pyright for
;; Python) will be configured in their own respective files.
;;

;;----------------------------------------------------------------------------
;; Core Eglot Configuration
;;----------------------------------------------------------------------------

;;
;; Package: eglot
;; Eglot is the built-in LSP client in Emacs 29+. It acts as the bridge
;; between Emacs and external language servers, enabling features like
;; intelligent code completion, go-to-definition, diagnostics, and more.
;;
(use-package eglot
  :ensure nil ; This is a built-in package.
  ;; We hook `eglot-ensure` to `prog-mode-hook`. This function will
  ;; automatically try to start a configured LSP server whenever you open
  ;; a file in any programming mode.
  :hook (prog-mode . eglot-ensure)
  :config
  ;; Automatically shut down the LSP server when the last buffer for a
  ;; project is closed. This helps conserve system resources.
  (setq eglot-autoshutdown t)

  ;; Increase the amount of data Emacs can read from the LSP server at once.
  ;; This can help prevent communication errors with verbose servers.
  (setq read-process-output-max (* 1024 1024)) ; 1MB

  ;; Do not create a buffer for server events/errors. We can check the
  ;; *Messages* buffer if needed. This keeps the buffer list clean.
  (setq eglot-events-buffer-size 0))


;;----------------------------------------------------------------------------
;; Integration with our Completion System
;;----------------------------------------------------------------------------

;;
;; Package: consult-eglot
;; This package provides Consult integration for Eglot, offering a much
;; nicer UI for Browse things like document symbols (functions, variables).
;;
(use-package consult-eglot
  :ensure t
  :after (consult eglot) ; Ensure it loads after consult and eglot are ready.
  :bind (:map eglot-mode-map
              ;; This provides a powerful way to search for symbols in the
              ;; current document using our Vertico/Consult UI.
              ("C-M-." . consult-eglot-symbols)))


(provide 'init-lsp)
;;; init-lsp.el ends here
