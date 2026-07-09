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
  :hook (on-first-file . global-treesit-auto-mode)
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  ;; WORKAROUND: Emacs 31 bug - treesit-install-language-grammar prompts for
  ;; directory even when called non-interactively from treesit-auto, causing
  ;; the 'y' from y-or-n-p to leak into the directory prompt.
  ;; Fix: Always pass the correct output directory explicitly.
  (when (and (>= emacs-major-version 31) (eq system-type 'gnu/linux))
    (advice-add 'treesit-install-language-grammar :around
                (lambda (orig-fn lang &optional out-dir)
                  (funcall orig-fn lang
                           (or out-dir (locate-user-emacs-file "tree-sitter"))))
                '((name . treesit-auto-fix-out-dir)))))


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
  :hook (eglot-managed-mode emacs-lisp-mode)
  :config
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
              (background-color . ,(face-background 'tooltip nil t))))
      (set-face-background 'eldoc-mouse-border border-color)
      (posframe-delete eldoc-mouse-posframe-buffer-name)))
  (add-hook 'after-load-theme-hook #'eldoc-mouse-set-appearance)
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
              ("C-c C-s" . hs-show-block)))

;;----------------------------------------------------------------------------
;; Project-specific Settings
;;----------------------------------------------------------------------------

;; Maintain consistent coding styles

(use-package editorconfig
  :diminish
  :hook ((prog-mode text-mode conf-mode) . editorconfig-mode))


;;----------------------------------------------------------------------------
;; Visual Enhancements
;;----------------------------------------------------------------------------

;; Prettify Symbols (e.g., display "lambda" as "λ")
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode))

;; ----------------------------------------------------------------------------
;; Comint
;; ----------------------------------------------------------------------------

;; Scroll comint buffer (shell/REPL) to bottom on new output, but only in the
;; active window.
(setq comint-scroll-to-bottom-on-output 'this)

(provide 'init-prog)
;;; init-prog.el ends here
