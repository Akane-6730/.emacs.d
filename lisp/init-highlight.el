;;; init-highlight.el --- Highlighting configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures various visual highlighting features to improve code
;; readability and editing efficiency.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Current Line Highlighting
;;----------------------------------------------------------------------------

(use-package hl-line
  :hook prog-mode)

;;----------------------------------------------------------------------------
;; Matching Parentheses Highlighting
;;----------------------------------------------------------------------------

;; `show-paren-mode` is a built-in mode that highlights matching delimiters
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  ;; These settings improve the highlighting behavior, making it more intuitive.
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        blink-matching-paren-highlight-offscreen t
        show-paren-context-when-offscreen 'overlay))


;;----------------------------------------------------------------------------
;; Symbol Occurrences Highlighting
;;----------------------------------------------------------------------------

;; `symbol-overlay` highlights all occurrences of the symbol at the current
;; cursor position.
;; (use-package symbol-overlay
;;   :diminish
;;   :hook (prog-mode . symbol-overlay-mode)
;;   :bind (:map symbol-overlay-mode-map
;;               ("M-i" . symbol-overlay-put)
;;               ("M-n" . symbol-overlay-jump-next)
;;               ("M-p" . symbol-overlay-jump-prev)))

;;----------------------------------------------------------------------------
;; Rainbow Delimiters
;;----------------------------------------------------------------------------

;; `rainbow-delimiters` assigns different colors to parentheses, brackets,
;; and braces at different nesting levels.

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))


;;----------------------------------------------------------------------------
;; VCS Changes Highlighting
;;----------------------------------------------------------------------------

(use-package diff-hl
  :autoload diff-hl-flydiff-mode
  :hook ((prog-mode . global-diff-hl-mode)
         (prog-mode . global-diff-hl-show-hunk-mouse-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :custom-face
  (diff-hl-margin-change ((t (:inherit diff-indicator-changed :background unspecified))))
  (diff-hl-margin-insert ((t (:inherit diff-indicator-added   :background unspecified))))
  (diff-hl-margin-delete ((t (:inherit diff-indicator-removed :background unspecified))))
  :config
  ;; Highlight on-the-fly
  (setq diff-hl-update-async t)
  (diff-hl-flydiff-mode 1))

;;----------------------------------------------------------------------------
;; Doxygen Highlighting
;;----------------------------------------------------------------------------

(use-package highlight-doxygen
  :hook prog-mode
  :custom-face
  (highlight-doxygen-comment ((t (:background unspecified)))))

(provide 'init-highlight)
;;; init-highlight.el ends here
