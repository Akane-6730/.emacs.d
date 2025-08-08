;;; init-org.el --- Org Mode and Org Babel configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configures Org Mode, with a focus on Org Babel and LaTeX exporting.
;;

;;; Code:

(use-package org
  :ensure nil
  :hook (org-mode . my-org-mode-setup-emphasis-keys)
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-hide-emphasis-markers t)
  (require 'org-tempo)
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


;;----------------------------------------------------------------------------
;; LaTeX Export Configuration
;;----------------------------------------------------------------------------
(use-package ox-latex
  :ensure nil
  :config
  (setq org-latex-compiler "xelatex")
  (setq org-latex-listings 'minted)
  ;; minted configurations
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("framesep" "2mm")
          ("breaklines" "true")
          ("fontsize" "\\footnotesize")
          ("framerule" "0.8pt")
          ("autogobble" "true")
          ("linenos" "true")
          ("fontfamily" "MonacoFamily")))
  ;; (setq org-latex-pdf-process
  ;;       '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-default-class "cn-article")
  (add-to-list 'org-latex-classes
               '("cn-article"
                 "\\documentclass[11pt, a4paper]{article}
[NO-DEFAULT-PACKAGES]

% --- basic macro packages ---
\\usepackage{amsmath, amssymb, graphicx, longtable}
\\usepackage[normalem]{ulem}
\\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}
\\usepackage{xcolor}
\\definecolor{solBlue}{HTML}{268bd2}
\\usepackage[bottom]{footmisc}
\\usepackage[colorlinks=true, allcolors=solBlue]{hyperref}
% Math
\\usepackage{mathtools}
\\usepackage{bm}

% --- fonts configurations ---
\\usepackage{fontspec}
\\setmainfont{Latin Modern Roman}
\\setsansfont{Latin Modern Sans}
\\setmonofont{Latin Modern Mono}
\\usepackage{xeCJK}
\\setCJKmainfont{Songti SC}
\\setCJKsansfont{PingFang SC}
\\setCJKmonofont{PingFang SC}
\\newfontfamily\\monaco{Monaco}[NFSSFamily=MonacoFamily]

% --- code block syntax highlighting configures ---
\\usepackage{minted}
\\usemintedstyle{solarized-light}

[EXTRA]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'init-org)
;;; init-org.el ends here
