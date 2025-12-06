;;; init-org.el --- Org Mode and Org Babel configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configures Org Mode, with a focus on Org Babel and LaTeX exporting.
;;

;;; Code:

(defvar org-setup-dir (expand-file-name "org/setup/" user-emacs-directory))

(use-package org
  :ensure nil
  :hook (org-mode . my-org-mode-setup-emphasis-keys)
  (org-mode . org-indent-mode)
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-highlight-latex-and-related '(native))
  (require 'org-tempo)
  (require 'ox-beamer)
  (setq org-beamer-frame-default-options "fragile")
  (setq org-structure-template-alist
        '(("s"  . "src")
          ("sc" . "src scheme")
          ("e"  . "src emacs-lisp")
          ("p"  . "src python")
          ("r"  . "src ruby")
          ("sh" . "src shell")
          ("j"  . "src java")
          ("q"  . "quote")
          ("ex" . "example")))
  (setq org-babel-python-command "python3")
  :init
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (scheme     . t)
          (python     . t)
          (ruby       . t)
          (shell      . t)
          (java       . t)
          (C          . t)))
  :bind (("C-c a" . org-agenda)))


;;----------------------------------------------------------------------------
;; Fine-grained Electric Pair Configuration for Org Mode
;;----------------------------------------------------------------------------
;; This ensures other pairs like `()` and `""` continue to work,
;; while only disabling auto-pairing for `<` to not conflict with
;; org-tempo structure templates (e.g., `<s` + TAB).

(defun my-org-mode-pair-predicate (c)
  "A custom predicate for `electric-pair-mode` in Org buffers.
It returns t (inhibit pairing) if the character is `<`, otherwise
it falls back to the default conservative behavior."
  (if (char-equal c ?<)
      t  ; Inhibit pairing for '<'
    ;; For all other characters, delegate to the global conservative rule.
    (electric-pair-conservative-inhibit c)))

;; We apply our custom predicate locally to Org mode buffers.
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate #'my-org-mode-pair-predicate)))

;;----------------------------------------------------------------------------
;; Helper function for smart emphasis
;;----------------------------------------------------------------------------
(defun my-smart-org-emphasize (char)
  "Apply emphasis CHAR to region or word at point."
  (if (use-region-p)
      (org-emphasize char)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds
          (progn
            (goto-char (car bounds))
            (set-mark (cdr bounds))
            (org-emphasize char))
        (org-emphasize char)))))

(defun my-org-mode-setup-emphasis-keys ()
  "Setup Super-key bindings for emphasis in Org mode."
  (define-key org-mode-map (kbd "s-b") (lambda () (interactive) (my-smart-org-emphasize ?*)))
  (define-key org-mode-map (kbd "s-i") (lambda () (interactive) (my-smart-org-emphasize ?/)))
  (define-key org-mode-map (kbd "s-u") (lambda () (interactive) (my-smart-org-emphasize ?_)))
  (define-key org-mode-map (kbd "s-c") (lambda () (interactive) (my-smart-org-emphasize ?~)))
  (define-key org-mode-map (kbd "s-d") (lambda () (interactive) (my-smart-org-emphasize ?+))))

;; ----------------------------------------------------------------------------
;; Visual Enhancements for Org Mode
;; ----------------------------------------------------------------------------

;; Automatically show/hide emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom (org-appear-autosubmarkers t)
  (org-appear-autolinks t))

;; Modern heading and list styles
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config (setq org-modern-star '("●" "○" "✸" "✿")
                org-modern-replace-stars "●○✿◉✸"
                org-modern-list '((45 . "➤")  ;; '-' -> '➤'
                                  (43 . "•") )  ;; '+' -> '•'
                org-modern-tag nil
                org-modern-priority nil
                org-modern-timestamp nil
                org-modern-block-name nil
                org-modern-todo nil
                org-modern-table nil))

;; Better alignment of tables
(use-package valign
  :hook (org-mode . valign-mode))

;; Real-time LaTeX Fragment Preview
(use-package org-fragtog
  :diminish
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.6)) ;; MacOS :scale 1.8
  (setq org-preview-latex-default-process 'dvisvgm))

;;----------------------------------------------------------------------------
;; Export Configuration
;;----------------------------------------------------------------------------

;; latex export configuration
(use-package ox-latex
  :ensure nil
  :config
  (setq org-latex-compiler "xelatex")
  (setq org-latex-tables-booktabs t)
  (setq org-latex-tables-centered t)
  (setq org-latex-src-block-backend 'minted)
  ;; minted configurations
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("framesep" "2mm")
          ("breaklines" "true")
          ("autogobble" "true")))

  (setq org-latex-default-class "cn-article")



  (setq org-latex-pdf-process
        '("latexmk -xelatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

  (add-to-list 'org-latex-classes
               `("en-article"
                 ,(concat "\\documentclass{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
\\input{" org-setup-dir "latex/style-en.tex}
[EXTRA]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               `("cn-article"
                 ,(concat "\\documentclass{ctexart}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
\\input{" org-setup-dir "latex/style-cn.tex}
[EXTRA]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               `("zju-beamer"
                 ,(concat "\\documentclass[10pt,aspectratio=169,mathserif]{beamer}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
\\def\\ZjuRoot{" org-setup-dir "beamer/zju/}
\\input{" org-setup-dir "beamer/zju/style-zju.tex}
[EXTRA]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;----------------------------------------------------------------------------
;; Reveal.js Export Configuration
;;----------------------------------------------------------------------------
(use-package org-re-reveal
  :init
  ;; Load org-re-reveal when ox is loaded to register the export backend
  (with-eval-after-load 'ox
    (require 'org-re-reveal))
  :config
  ;; Use a reliable CDN for Reveal.js so it works out of the box without local installation
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-re-reveal-revealjs-version "4")
  ;; Aesthetic defaults
  (setq org-re-reveal-theme "simple")
  (setq org-re-reveal-transition "slide")
  ;; Use a light theme for code highlighting to match the template
  (setq org-re-reveal-highlight-css "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css")
  ;; Set slide number format to current/total
  (setq org-re-reveal-slide-number "c/t")
  ;; Default extra CSS for ZJU theme
  (setq org-re-reveal-extra-css (concat org-setup-dir "reveal/zju/zju-reveal.css"))
  ;; Default plugins
  (setq org-re-reveal-plugins '(highlight))
  ;; Default Org export options for cleaner presentations
  (setq org-export-with-toc nil              ; No table of contents by default
        ;; org-export-with-section-numbers nil   ; No section numbers
        org-export-time-stamp-file nil)      ; No timestamp
  )


(provide 'init-org)
;;; init-org.el ends here
