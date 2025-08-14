;;; init-kbd.el --- Global keybinding configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file defines a set of global and mode-specific keybindings to
;; enhance productivity and provide a more intuitive user experience.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Vim-like j/k Navigation in Read-only Buffers
;;----------------------------------------------------------------------------

;;
;; To safely implement j/k navigation without breaking normal typing, we
;; define a dedicated minor mode. This is the most robust and idiomatic
;; Emacs way to add mode-specific keybindings without side effects.
;;
(define-minor-mode my-jk-navigation-mode
  "A minor mode to enable `j` and `k` for line-wise navigation."
  :init-value nil
  :lighter " jk-nav" ; A short string to show in the mode-line
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "j") #'next-line)
            (define-key map (kbd "k") #'previous-line)
            map))

;;
;; We activate our new minor mode automatically in `help-mode`.
;; You can add this hook to any other read-only mode where you
;; want this behavior (e.g., `apropos-mode-hook`).
;;

(add-hook 'help-mode-hook #'my-jk-navigation-mode)
(add-hook 'Info-mode-hook #'my-jk-navigation-mode)
(add-hook 'fundamental-mode-hook #'my-jk-navigation-mode)
(add-hook 'flymake-diagnostics-buffer-mode-hook #'my-jk-navigation-mode)

;;----------------------------------------------------------------------------
;; Super-key based Bindings (macOS-like & Text Scaling)
;;----------------------------------------------------------------------------

;;
;; macOS-like editing commands.
;;

(global-set-key (kbd "s-a") #'mark-whole-buffer)  ; Cmd-a -> Select All
(global-set-key (kbd "s-c") #'kill-ring-save)     ; Cmd-c -> Copy
;; (global-set-key (kbd "s-v") #'yank)               ; Cmd-v -> Paste


;;----------------------------------------------------------------------------
;; Super Enter (Open line below)
;;----------------------------------------------------------------------------

;;
;; This function implements the "super enter" feature you requested.
;; It allows you to create a new, correctly-indented line below the current
;; one, without having to move the cursor to the end of the line first.
;;
(defun my-kbd-open-line-below ()
  "Open a new line below the current one and move the cursor to it."
  (interactive)
  ;; 1. Move cursor to the end of the current line.
  (end-of-line)
  ;; 2. Execute `newline-and-indent` to create a new, indented line.
  (newline-and-indent))

(global-set-key (kbd "C-<return>") #'my-kbd-open-line-below)

;;----------------------------------------------------------------------------
;; The Ultimate "Smart Format" Command
;;----------------------------------------------------------------------------

(defun my-format-buffer-smart ()
  "The ultimate smart formatting command.
If an eglot LSP server is active, it uses `eglot-format-buffer`.
Otherwise, it falls back to a comprehensive cleanup: re-indenting the
entire buffer and cleaning up all extraneous whitespace."
  (interactive)
  ;; We check the buffer-local variable `eglot--managed-mode`. This is the
  ;; canonical and most robust way to determine if Eglot is active in the
  ;; current buffer, as recommended by its documentation.
  (if (and (bound-and-true-p eglot--managed-mode)
           (fboundp 'eglot-format-buffer))
      ;; Action if TRUE: Use the precise LSP formatter.
      (progn
        (message "Formatting with eglot LSP...")
        (eglot-format-buffer))
    ;; Action if FALSE: Fallback to a robust, general cleanup.
    (progn
      (message "No LSP formatter found. Performing basic cleanup...")
      ;; 1. Re-indent the entire buffer.
      (indent-region (point-min) (point-max))
      ;; 2. Clean up all extraneous whitespace.
      (whitespace-cleanup)
      (message "Basic cleanup complete."))))


;;----------------------------------------------------------------------------
;; Global F-Key Bindings
;;----------------------------------------------------------------------------

;; F6: Smart Format Buffer
(global-set-key (kbd "<f6>") #'my-format-buffer-smart)

;;----------------------------------------------------------------------------
;; Enable Built-in Commands
;;----------------------------------------------------------------------------

;; Ensure the `narrow-to-region` command is always available.
;; Emacs sometimes disables it by default.
(put 'narrow-to-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Smart Save with Whitespace Cleanup
;;----------------------------------------------------------------------------
(defun my-cleanup-and-save ()
  "Run `whitespace-cleanup` and then save the buffer."
  (interactive)
  (whitespace-cleanup)
  (save-buffer))

;; Remap the default save command to our new, smarter version.
(use-package simple
  :ensure nil
  :bind ([remap save-buffer] . my-cleanup-and-save))

(provide 'init-kbd)
;;; init-kbd.el ends here
