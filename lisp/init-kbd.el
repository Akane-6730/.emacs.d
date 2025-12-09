;;; init-kbd.el --- Global keybinding configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Global and mode-specific keybindings.
;;

;;; Code:

;;; Keybindings

;; macOS-like bindings
(global-set-key (kbd "s-a") #'mark-whole-buffer)
(global-set-key (kbd "s-c") #'kill-ring-save)

;;----------------------------------------------------------------------------
;; Kitty Keyboard Protocol (TUI only)
;;----------------------------------------------------------------------------

;; Enable extended key combinations in TUI
(use-package kkp
  :if (not (display-graphic-p))
  :hook (after-init . global-kkp-mode))

(provide 'init-kbd)
;;; init-kbd.el ends here
