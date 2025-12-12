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

;;---------------------------------------------------------------------
;; Text Scaling
;;---------------------------------------------------------------------

;; Buffer Text Scaling
(global-set-key (kbd "s-=") #'text-scale-increase)
(global-set-key (kbd "s--") #'text-scale-decrease)

;; Global Text Scaling
(defun my/global-text-scale-adjust (inc)
  "Adjust the global font size by INC factor."
  (let* ((old-face-attribute (face-attribute 'default :height))
         (new-face-attribute (+ old-face-attribute inc)))
    (set-face-attribute 'default nil :height new-face-attribute)
    (message "Global font size set to %d" new-face-attribute)))

(defun my/global-text-scale-increase ()
  "Increase global font size."
  (interactive)
  (my/global-text-scale-adjust 10))

(defun my/global-text-scale-decrease ()
  "Decrease global font size."
  (interactive)
  (my/global-text-scale-adjust -10))

(global-set-key (kbd "C-+") #'my/global-text-scale-increase)
(global-set-key (kbd "C-_") #'my/global-text-scale-decrease)

;;---------------------------------------------------------------------
;; Kitty Keyboard Protocol (TUI only)
;;---------------------------------------------------------------------

;; Enable extended key combinations in TUI
(use-package kkp
  :if (not (display-graphic-p))
  :hook (after-init . global-kkp-mode))

(provide 'init-kbd)
;;; init-kbd.el ends here
