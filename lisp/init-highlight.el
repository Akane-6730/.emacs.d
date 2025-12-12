;;; init-highlight.el --- Highlighting configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures various visual highlighting features to improve code
;; readability and editing efficiency.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Matching Parentheses Highlighting
;;----------------------------------------------------------------------------

;; `show-paren-mode` is a built-in mode that highlights matching delimiters
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((((class color) (background light))
                      (:box (:line-width (-1 . -1) :color "gray73")))
                     (((class color) (background dark))
                      (:box (:line-width (-1 . -1) :color "gray56")))))
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
(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
  :init (setq symbol-overlay-idle-time 0.3))

;;----------------------------------------------------------------------------
;; Rainbow Delimiters
;;----------------------------------------------------------------------------

;; `rainbow-delimiters` assigns different colors to parentheses, brackets,
;; and braces at different nesting levels.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;----------------------------------------------------------------------------
;; VCS Changes Highlighting
;;----------------------------------------------------------------------------

(use-package diff-hl
  :autoload diff-hl-flydiff-mode
  ;; Enable diff-hl globally after startup, and also in Dired buffers.
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :custom (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:inherit diff-indicator-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-indicator-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-indicator-removed :background unspecified))))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  ;; Set fringe style
  (setq-default fringes-outside-margins t)
  ;; By default, the fringe marker can be quite thick. We define our own
  ;; fringe bitmap that is only 2 pixels wide for a more subtle appearance.
  (define-fringe-bitmap 'my-diff-hl-thin-bar [2r11000000])
  (setq diff-hl-fringe-bmp-function
        (lambda (_type _pos) 'my-diff-hl-thin-bar))
  (with-no-warnings
    (defun my-diff-hl--fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        ;; Make the fringe bitmap 3 pixels wide as requested
        (vector #b11100000)
        1 8 '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl--fringe-bmp-function)

    ;; Integrate with Magit for seamless updates. This ensures that diff
    ;; markers stay in sync when you interact with Magit.
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))
  (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil)))))

;;----------------------------------------------------------------------------
;; Doxygen Highlighting
;;----------------------------------------------------------------------------

(use-package highlight-doxygen
  :hook (prog-mode . highlight-doxygen-mode)
  :custom-face
  (highlight-doxygen-comment ((t (:background unspecified)))))

;;----------------------------------------------------------------------------
;; Indentation Guides Highlighting
;;----------------------------------------------------------------------------
;;
;; `indent-bars` provides visual vertical lines to indicate indentation
;; levels, making it easier to follow code structure in languages like Python.
;;
;; (use-package indent-bars
;;   :hook (python-ts-mode . indent-bars-mode)
;;   :custom
;;   (indent-bars-color '(highlight :face-bg t :blend 0.225))
;;   (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
;;   (indent-bars-treesit-support t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   (indent-bars-prefer-character t)
;;   ;; Add other languages as needed
;;   (indent-bars-treesit-scope '((python function_definition class_definition for_statement
;;                                        if_statement with_statement while_statement))))

(provide 'init-highlight)
;;; init-highlight.el ends here
