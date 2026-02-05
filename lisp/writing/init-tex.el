;;; init-tex.el --- Initial configuration for LaTeX editing  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for super-fast Latex input using AucTeX, CDLatex and a bit of YaSnippet.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Math Environment Detection (shared by LaTeX-mode and org-mode)
;;----------------------------------------------------------------------------

;; Theorem-like block types that should be treated as LaTeX environments
(defvar my/org-latex-block-types '("definition" "theorem" "exmp" "proof" "lemma" "corollary" "proposition" "remark")
  "List of org special block types that should be treated as LaTeX environments.
Inside these blocks, LaTeX snippets will auto-expand and cdlatex will work.")

(defun my/org-inside-latex-block-p ()
  "Return non-nil if point is inside a theorem-like special block in org-mode."
  (when (derived-mode-p 'org-mode)
    (let ((element (org-element-at-point)))
      ;; Walk up the element tree to find if we're in a special block
      (while (and element
                  (not (eq (org-element-type element) 'special-block)))
        (setq element (org-element-property :parent element)))
      (when (eq (org-element-type element) 'special-block)
        (member (org-element-property :type element) my/org-latex-block-types)))))

(defun my/in-math-p ()
  "Return non-nil if point is in a math environment.
Works in both LaTeX-mode and org-mode."
  (cond
   ;; In LaTeX-mode, use texmathp from AUCTeX
   ((derived-mode-p 'latex-mode 'LaTeX-mode)
    (and (fboundp 'texmathp) (texmathp)))
   ;; In org-mode, check LaTeX fragment OR theorem-like blocks
   ((derived-mode-p 'org-mode)
    (or (and (fboundp 'org-inside-LaTeX-fragment-p)
             (org-inside-LaTeX-fragment-p))
        (my/org-inside-latex-block-p)))
   ;; Default: not in math
   (t nil)))

;; Make `texmathp' work in org-mode by advising it.
;; This allows snippets with `condition: (and (texmathp) 'auto)' to work
;; in org-mode without modification.
(defun my/texmathp-in-org (orig-fun &rest args)
  "Advice for `texmathp' to work in org-mode.
In org-mode, delegate to `my/in-math-p' which checks both LaTeX fragments
and theorem-like special blocks.
Otherwise, call the original ORIG-FUN with ARGS."
  (if (derived-mode-p 'org-mode)
      (my/in-math-p)
    (apply orig-fun args)))

(with-eval-after-load 'texmathp
  (advice-add 'texmathp :around #'my/texmathp-in-org))

;;----------------------------------------------------------------------------
;; Auto-expanding YaSnippet Function
;;----------------------------------------------------------------------------

(defun my/yas-try-expanding-auto-snippets ()
  "Try to expand auto-snippets after self-insert.
Only expands snippets with condition returning symbol `auto'."
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    ;; Only attempt expansion in LaTeX-mode or in org-mode math environments
    (when (or (derived-mode-p 'latex-mode 'LaTeX-mode)
              (and (derived-mode-p 'org-mode) (my/in-math-p)))
      ;; The double quoting is NOT a typo! This requires snippet condition
      ;; to evaluate to the symbol `auto'.
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;;----------------------------------------------------------------------------
;; AUCTeX Configuration
;;----------------------------------------------------------------------------

(use-package latex
  :ensure auctex
  :init (setq prettify-symbols-unprettify-at-point t)
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . my/setup-latex-auto-snippets))
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  ;; Setup auto-expanding snippets for LaTeX-mode
  (defun my/setup-latex-auto-snippets ()
    "Enable auto-expanding snippets in LaTeX-mode."
    (add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets nil t))

  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

;; CDLatex settings
(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind
  (:map cdlatex-mode-map ("<tab>" . cdlatex-tab)))

;; Yasnippet settings for LaTeX
(use-package yasnippet
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t))

;;----------------------------------------------------------------------------
;; CDLatex + YaSnippet Integration
;;----------------------------------------------------------------------------

;; Allow cdlatex tab to work inside Yas fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;;----------------------------------------------------------------------------
;; Org-mode CDLatex + YaSnippet Integration
;;----------------------------------------------------------------------------

(use-package org
  :ensure nil
  :hook (org-mode . org-cdlatex-mode)
  :bind (:map org-cdlatex-mode-map
              ("$" . my/insert-inline-OCDL))
  :config
  ;; Setup yasnippet for org-mode using hack-local-variables-hook
  ;; This runs AFTER major mode hooks and local variables are processed
  (defun my/org-setup-yasnippet-late ()
    "Enable yasnippet and configure auto-expansion for Org mode (late setup)."
    ;; yas-minor-mode is already enabled by yas-global-mode from init-snippet.el
    ;; Just add the auto-expand hook buffer-locally
    (add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets nil t))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'hack-local-variables-hook #'my/org-setup-yasnippet-late nil t)))

  (defun my/insert-inline-OCDL ()
    "Insert inline math environment using CDLatex in org mode."
    (interactive)
    (insert "\\(")
    (save-excursion (insert "\\)"))))

(provide 'init-tex)

;;; init-tex.el ends here
