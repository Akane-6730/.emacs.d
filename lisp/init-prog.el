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
  :hook (after-init . global-treesit-auto-mode)
  :config
  ;; When a new grammar is needed, always ask for confirmation before installing.
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4))


;;----------------------------------------------------------------------------
;; Code Insight and Navigation
;;----------------------------------------------------------------------------

;; Show function arglist or variable docstring in echo area

(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :bind (:map prog-mode-map ("C-h ." . eldoc)))

;; Go to definition and finding references

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
  :hook (after-init . editorconfig-mode))

(provide 'init-prog)
;;; init-prog.el ends here
