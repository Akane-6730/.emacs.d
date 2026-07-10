;;; init-highlight.el --- Highlighting configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures various visual highlighting features to improve code
;; readability and editing efficiency.
;;

;;; Code:

;; Current Line Highlighting
(use-package hl-line
  :hook prog-mode LaTeX-mode)

;; Matching Parentheses Highlighting
(use-package paren
  :ensure nil
  :hook ((prog-mode text-mode) . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        blink-matching-paren-highlight-offscreen t
        show-paren-context-when-offscreen 'overlay))

;; Rainbow Delimiters for lisp parentheses highlighting
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode scheme-mode) . rainbow-delimiters-mode))

;; VCS Changes Highlighting
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

;; Doxygen Highlighting
(use-package highlight-doxygen
  :hook prog-mode
  :custom-face
  (highlight-doxygen-comment ((t (:background unspecified))))
  (highlight-doxygen-variable ((t (:inherit eglot-semantic-parameter))))
  :config
  ;; Fix doxygen regex to match identifiers with underscores in C/C++ modes.
  (defun my/highlight-doxygen--patch-sw (tree)
    "Replace \\sw+ with \\(?:\\sw\\|\\s_\\)+ in TREE recursively."
    (cond
     ((stringp tree)
      (replace-regexp-in-string
       (regexp-quote "\\sw+")
       "\\(?:\\sw\\|\\s_\\)+"
       tree t t))
     ((consp tree)
      (cons (my/highlight-doxygen--patch-sw (car tree))
            (my/highlight-doxygen--patch-sw (cdr tree))))
     (t tree)))
  (define-advice highlight-doxygen-anchored-keywords-template
      (:filter-return (keywords) fix-underscore)
    (my/highlight-doxygen--patch-sw keywords)))

;; Highlight TODO, FIXME, and BUG keywords in code comments
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-require-punctuation t
        hl-todo-keyword-faces
        '(("TODO"  . ((:weight bold :family "Maple Mono NF CN") font-lock-type-face))
          ("FIXME" . ((:weight bold :family "Maple Mono NF CN") error))
          ("BUG"   . ((:weight bold :family "Maple Mono NF CN") error))))
  ;; Apply face to keyword only (group 2), not the trailing colon (group 1).
  (setq hl-todo--keywords
        `((,(lambda (bound) (hl-todo--search nil bound))
           (2 (hl-todo--get-face) prepend t)))))

(provide 'init-highlight)
;;; init-highlight.el ends here
