;;; init-prog.el --- Base configurations for programming -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Tree-sitter Foundation
;;----------------------------------------------------------------------------

;; Automatically install Tree-sitter grammars

(use-package treesit-auto
  :hook prog-mode
  :config
  ;; When a new grammar is needed, always ask for confirmation before installing.
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  ;; Disable markdown treesit-auto prompts
  (setq treesit-auto-recipe-list
        (cl-remove-if (lambda (recipe)
                        (member (treesit-auto-recipe-lang recipe) '(markdown markdown-inline)))
                      treesit-auto-recipe-list)))


;;----------------------------------------------------------------------------
;; Code Insight and Navigation
;;----------------------------------------------------------------------------

;; Show function arglist or variable docstring in echo area

(use-package eldoc
  :ensure nil
  :diminish
  :hook prog-mode)

(use-package eldoc-mouse
  :diminish
  :bind (:map eldoc-mouse-mode-map
              ("C-h ." . eldoc-mouse-pop-doc-at-cursor))
  :hook ((eglot-managed-mode emacs-lisp-mode)
         (after-load-theme . eldoc-mouse-set-appearance))
  :init
  (defun eldoc-mouse-set-appearance ()
    "Set appearance of eldoc-mouse."
    (let ((border-color (if (facep 'posframe-border)
                            (face-background 'posframe-border nil t)
                          (face-foreground 'default nil t))))
      (setq eldoc-mouse-posframe-override-parameters
            `((internal-border-width . 1)
              (drag-internal-border . t)
              (timeout . nil)
              (foreground-color . ,(face-foreground 'tooltip nil t))
              (background-color . ,(face-background 'tooltip nil t)))
            eldoc-mouse-posframe-border-color border-color)))
  (eldoc-mouse-set-appearance))


;; Go to definition and finding references
(use-package xref
  :ensure nil
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  (setq xref-search-program 'ripgrep))

;;----------------------------------------------------------------------------
;; Quick Code Execution
;;----------------------------------------------------------------------------

(use-package quickrun
  :bind (("C-c x" . quickrun)
         ("C-c s" . quickrun-shell)
         ("<f5>". quickrun)))

;;----------------------------------------------------------------------------
;; Code Folding
;;----------------------------------------------------------------------------

;; Fold code blocks based on syntax
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-<tab>" . hs-cycle)
              ("C-S-<tab>" . hs-toggle-all)
              ("C-<iso-lefttab>" . hs-toggle-all)
              ("C-c C-h" . hs-hide-block)
              ("C-c C-s" . hs-show-block))
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))

  (setq hs-set-up-overlay #'hs-display-code-line-counts))


;;----------------------------------------------------------------------------
;; Project-specific Settings
;;----------------------------------------------------------------------------

;; Maintain consistent coding styles

(use-package editorconfig
  :diminish
  :hook ((prog-mode text-mode conf-mode) . editorconfig-mode))

(use-package yaml-ts-mode)

(provide 'init-prog)
;;; init-prog.el ends here
