;;; init-org.el --- Org Mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configures Org Mode, organized into the following sections:
;; 1. Core Configuration
;; 2. UI & Visuals
;; 3. UX Enhancements
;; 4. Code Execution
;; 5. Export Configuration
;; 6. Agenda & Capture
;; 7. Hacks & Chinese Support
;;

;;; Code:

;; Custom Org setup directory
(defvar org-setup-dir (expand-file-name "org/setup/" user-emacs-directory))

(unless (package-installed-p 'org-mode)
  (package-vc-install
   '(org-mode :url "https://git.tecosaur.net/tec/org-mode.git"
              :branch "dev")))

;;----------------------------------------------------------------------------
;; 1. Core Org Mode Configuration
;;----------------------------------------------------------------------------
(use-package org
  :init (setq org-modules-loaded t)
  :load-path "~/.emacs.d/elpa/org-mode/lisp/"
  :hook ((org-mode . my-org-mode-setup-emphasis-keys)
         (org-mode . org-indent-mode))
  :config
  (setq org-directory "~/Documents/org/")
  (setq org-export-with-smart-quotes t) ;; Global smart quotes for all exports
  (setq org-hide-emphasis-markers t)
  (setq org-highlight-latex-and-related '(native latex))
  (setq org-ellipsis " ") ; ⤵ ▾ ▼ ↴
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)
  ;; Prettify checkbox symbols using display property
  (font-lock-add-keywords
   'org-mode
   '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(\\[ \\]\\)"
      1 '(face nil display " "))
     ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(\\[-\\]\\)"
      1 '(face nil display " "))
     ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(\\[X\\]\\)"
      1 '(face nil display " ")))
   'append))


;; ----------------------------------------------------------------------------
;; 2. UI Visual Enhancements
;; ----------------------------------------------------------------------------

;; Automatically show/hide emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autolinks t)
  (setq org-appear-autoentities t)       ; Show original \alpha when cursor is on α
  (setq org-appear-autokeywords t)       ; Show #+KEYWORD markers
  (setq org-appear-inside-latex t))      ; Work inside LaTeX fragments

;; Better CJK table alignment
(when (display-graphic-p)
  (use-package valign
    :hook (org-mode . valign-mode)))

;; Asynchronous LaTeX preview
(use-package org-latex-preview
  :ensure nil
  :init (setq org-startup-with-latex-preview t)
  :hook (org-mode . org-latex-preview-mode)
  :config
  (setq org-latex-precompile nil)
  (setq org-latex-preview-process-precompile nil)
  ;; Increase preview width
  ;; (plist-put org-latex-preview-appearance-options :page-width 0.8)
  (setq org-latex-preview-mode-display-live t)
  (setq org-latex-preview-mode-update-delay 0.25))

;; Better headline bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-hide-leading-stars t)
  (setq org-superstar-headline-bullets-list '(9673 9675 9679)) ;10047
  (setq org-superstar-item-bullet-alist
        '((43 . 8226) (42 . 8226) (45 . 8211)))
  :custom-face
  (org-superstar-header-bullet ((t (:family "JetBrains Mono"))))
  (org-superstar-item ((t (:family "JetBrains Mono")))))


;; SVG Tag Mode
(when window-system
  ;; 1. Define Helper Functions FIRST to avoid void-function errors
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun my/svg-tag-mode-refresh (&rest _)
    (require 'svg-lib)
    (setq svg-lib-style-default (svg-lib-style-compute-default))
    (clear-image-cache)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-mode)
          (svg-tag-mode -1)
          (svg-tag-mode 1)))))

  (defun svg-progress-percent (value)
    (save-match-data
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                        nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11 :height 0.8)
                  (svg-lib-tag (concat value "%")
                               nil :stroke 0 :margin 0 :radius 3 :font-weight 'bold :font-family "Roboto Mono" :height 0.8)) :ascent 'center)))

  (defun svg-progress-count (value)
    (save-match-data
      (let* ((seq (split-string value "/"))
             (count (if (stringp (car seq))
                        (float (string-to-number (car seq)))
                      0))
             (total (if (stringp (cadr seq))
                        (float (string-to-number (cadr seq)))
                      1000)))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :width 11 :height 0.8)
                    (svg-lib-tag value nil
                                 :stroke 0 :margin 0 :radius 3 :font-weight 'bold :font-family "Roboto Mono" :height 0.8)) :ascent 'center))))

  ;; 2. Load Libraries
  (use-package svg-lib
    :config
    ;; Ensure default styles are correct
    (setq svg-lib-style-default (svg-lib-style-compute-default)))

  (use-package svg-tag-mode
    :hook (org-mode . svg-tag-mode)
    :config
    (setq svg-tag-action-at-point 'edit)

    (advice-add 'load-theme :after #'my/svg-tag-mode-refresh)
    (advice-add 'consult-theme :after #'my/svg-tag-mode-refresh)
    (advice-add 'text-scale-adjust :after #'my/svg-tag-mode-refresh)

    ;; Define a custom face for headlines that is visible in both light and dark modes
    (defface my/svg-headline-face
      '((((background dark)) :foreground "#8b9798")  ; Lighter grey for dark mode
        (((background light)) :foreground "#505050")) ; Darker grey for light mode
      "Face for SVG headlines.")

    (setq svg-tag-tags
          `(
            ;; 1. Org Tags (Pill shape)
            ;; Matches :TAG: patterns
            ("\\(:[A-Za-z0-9]+:\\)$" . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :height 0.8))))

            ;; 2. Priorities
            ("\\(\\[#[A-Z]\\]\\)" . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :face 'org-priority :inverse nil :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :stroke 1.5 :height 0.8))))

            ;; 3. Keywords (Rounded Box)
            ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :height 0.8))))
            ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :inverse nil :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :stroke 1.5 :height 0.8))))
            ("NOTE" . ((lambda (tag) (svg-tag-make "NOTE" :face 'org-note :inverse nil :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :stroke 1.5 :height 0.8))))
            ("COMMENT" . ((lambda (tag) (svg-tag-make "COMMENT" :face 'org-note :inverse nil :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :stroke 1.5 :height 0.8))))

            ;; 4. Active Date (Split-Pill Style)
            ;; Date only: <2023-01-01>
            (,(format "\\(<%s>\\)" date-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :end -1 :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :height 0.8))))

            ;; Date + Time/Day: <2023-01-01 Fri 10:00>
            ;; Left part (Date): Light/Outlined, cropped right
            (,(format "\\(<%s \\)%s>" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :stroke 2 :height 0.8))))

            ;; Right part (Day/Time): Dark/Inverse, cropped left
            (,(format "<%s \\(%s>\\)" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :height 0.8))))

            ;; 5. Inactive Date (Same split-pill style)
            ;; Date only: [2023-01-01]
            (,(format "\\(\\[%s\\]\\)" date-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :height 0.8))))

            ;; Date + Time/Day: [2023-01-01 Fri 10:00]
            ;; Left part
            (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :stroke 2 :height 0.8))))

            ;; Right part
            (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date :radius 4 :font-family "Roboto Mono" :font-weight 'bold :padding 1 :height 0.8))))

            ;; 6. Progress
            ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                                (svg-progress-percent (substring tag 1 -2)))))
            ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                              (svg-progress-count (substring tag 1 -1))))))))

  ;; Add refresh hook
  (add-hook 'org-mode-hook #'my/svg-tag-mode-refresh)


  (defun org-agenda-show-svg ()
    (let* ((case-fold-search nil)
           (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
           (keyword (car keywords)))
      (while keyword
        (save-excursion
          (while (re-search-forward (nth 0 keyword) nil t)
            (overlay-put (make-overlay
                          (match-beginning 0) (match-end 0))
                         'display  (nth 3 (eval (nth 2 keyword)))) ))
        (pop keywords)
        (setq keyword (car keywords)))))
  (add-hook 'org-agenda-finalize-hook #'org-agenda-show-svg)
  )


;;----------------------------------------------------------------------------
;; 3. UX Enhancements
;;----------------------------------------------------------------------------

;; Structure templates (e.g., <s TAB)
(use-package org-tempo
  :ensure nil
  :after org
  :demand t
  :custom
  (org-structure-template-alist
   '(("s"  . "src")
     ("sc" . "src scheme")
     ("e"  . "src emacs-lisp")
     ("l"  . "export latex")
     ("p"  . "src python")
     ("r" . "src ruby")
     ("sh" . "src sh")
     ("j"  . "src java")
     ("q"  . "quote")
     ("ex" . "example")
     ;; Theorem environment templates (using Org special blocks)
     ;; Use names that don't conflict with LaTeX snippets
     ("def" . "definition")
     ("thm" . "theorem")
     ("exm" . "exmp")     ; Use "exmp" to avoid conflict with org's built-in "example"
     ("prf" . "proof"))))

(defun my-org-mode-pair-predicate (c)
  "A custom predicate for `electric-pair-mode` in Org buffers.
It returns t (inhibit pairing) if the character C is `<`, otherwise
it falls back to the default conservative behavior."
  (if (char-equal c ?<)
      t  ; Inhibit pairing for '<'
    ;; For all other characters, delegate to the global conservative rule.
    (electric-pair-conservative-inhibit c)))

;; We apply our custom predicate locally to Org mode buffers.
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate #'my-org-mode-pair-predicate)))

;; Smart Emphasis
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
;; 4. Code Evaluation
;;----------------------------------------------------------------------------

;; Org Babel Configuration
(use-package ob
  :ensure nil
  :after org
  :config
  (setq org-confirm-babel-evaluate nil
        org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (scheme     . t)
     (python     . t)
     (ruby       . t)
     (shell      . t)
     (java       . t)
     (C          . t))))


;;----------------------------------------------------------------------------
;; 5. Export Configuration: LaTeX, Beamer and Reveal.js
;;----------------------------------------------------------------------------

;; Latex Article Export
(use-package ox-latex
  :ensure nil
  :config
  (setq org-latex-compiler "xelatex")
  (setq org-latex-tables-booktabs t)
  (setq org-latex-tables-centered t)
  ;; Use minted for syntax highlighting
  (setq org-latex-src-block-backend 'minted)
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("framesep" "2mm")
          ("breaklines" "true")
          ("autogobble" "true")))

  (setq org-latex-pdf-process
        '("latexmk -xelatex -shell-escape -file-line-error -interaction=nonstopmode -output-directory=%o %f"))

  (setq org-latex-default-class "cn-article")

  ;; Define custom LaTeX classes

  ;; 1. Standard English Article
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

  ;; 2. Chinese Article (using ctexart)
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

  ;; 3. ZJU Beamer Presentation
  (add-to-list 'org-latex-classes
               `("zju-beamer"
                 ,(concat "\\documentclass[10pt,aspectratio=169,mathserif,fontset=none]{ctexbeamer}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
\\def\\ZjuRoot{" org-setup-dir "beamer/zju/}
\\input{" org-setup-dir "beamer/zju/style-zju.tex}
[EXTRA]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; Beamer Export
(use-package ox-beamer
  :ensure nil
  :init
  (with-eval-after-load 'ox (require 'ox-beamer))
  :config
  ;; Allow using 'fragile' frames by default (useful for code blocks)
  (setq org-beamer-frame-default-options "fragile"))

;; ----------------------------------------------------------------------------
;; Custom Org Special Blocks for Theorem Environments
;; ----------------------------------------------------------------------------

(with-eval-after-load 'ox-latex
  ;; Save the original function before overriding
  (defvar my/org-latex-special-block-original
    (symbol-function 'org-latex-special-block)
    "Original org-latex-special-block function.")

  ;; List of theorem-like environments we handle
  (defvar my/theorem-environments '("definition" "theorem" "exmp" "proof")
    "List of special block types that should be treated as theorem environments.")

  (defun my/parse-theorem-block-args (special-block)
    "Parse title and label from SPECIAL-BLOCK's raw parameters.
Supports syntax: #+BEGIN_theorem Title Here :label mylabel
Returns (title . label) cons cell."
    (let* ((raw-params (org-element-property :parameters special-block))
           (title "")
           (label ""))
      (when raw-params
        ;; Extract :label if present
        (if (string-match "\\(.*?\\)\\s-*:label\\s-+\\(\\S-+\\)\\s-*$" raw-params)
            (setq title (string-trim (match-string 1 raw-params))
                  label (match-string 2 raw-params))
          ;; No :label, entire string is title
          (setq title (string-trim raw-params))))
      (cons title label)))

  ;; Add custom export for special blocks to handle theorem environments
  (defun my/org-latex-special-block (special-block contents info)
    "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
    (let* ((type (org-element-property :type special-block))
           ;; Try to get title/label from block parameters first
           (parsed (my/parse-theorem-block-args special-block))
           ;; Fall back to ATTR_LATEX if not in parameters
           (attr (org-export-read-attribute :attr_latex special-block))
           (title (if (string-empty-p (car parsed))
                      (or (plist-get attr :title) "")
                    (car parsed)))
           (label (if (string-empty-p (cdr parsed))
                      (or (plist-get attr :label) "")
                    (cdr parsed))))
      (cond
       ;; Handle definition environment
       ((string= type "definition")
        (format "\\begin{definition}{%s}{%s}\n%s\\end{definition}"
                title label (or contents "")))
       ;; Handle theorem environment
       ((string= type "theorem")
        (format "\\begin{theorem}{%s}{%s}\n%s\\end{theorem}"
                title label (or contents "")))
       ;; Handle example environment (use "exmp" to avoid conflict with org's built-in example block)
       ((string= type "exmp")
        (format "\\begin{example}{%s}{%s}\n%s\\end{example}"
                title label (or contents "")))
       ;; Handle proof environment (no title/label needed)
       ((string= type "proof")
        (format "\\begin{proof}\n%s\\end{proof}" (or contents "")))
       ;; Default: use original special block export
       (t (funcall my/org-latex-special-block-original special-block contents info)))))

  ;; Override the default special block export function
  (advice-add 'org-latex-special-block :override #'my/org-latex-special-block))


;; Reveal.js Export (HTML Slides)
(use-package org-re-reveal
  :init
  (with-eval-after-load 'ox (require 'org-re-reveal))
  :config
  ;; Use a reliable CDN for Reveal.js (v4) to avoid local installation
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-re-reveal-revealjs-version "4")
  (setq org-re-reveal-theme "simple")
  (setq org-re-reveal-transition "slide")
  (setq org-re-reveal-highlight-css "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css")
  (setq org-re-reveal-slide-number "c/t")
  (setq org-re-reveal-extra-css (concat org-setup-dir "reveal/zju/zju-reveal.css"))
  (setq org-re-reveal-history t)
  (setq org-re-reveal-plugins '(highlight))
  ;; Default Org export options for cleaner presentations
  (setq org-export-with-toc nil              ; No table of contents by default
        ;; org-export-with-section-numbers nil   ; No section numbers
        org-export-time-stamp-file nil))      ; No timestamp

;; Automation: Auto-export on Save
(defun my/org-auto-export-on-save ()
  "Export logic to run when saving an Org file.
Only export when LATEX_CLASS is explicitly defined in the header.
If LATEX_CLASS is set to 'zju-beamer', export as Beamer PDF.
Otherwise, export as standard LaTeX PDF."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let* ((keywords (org-collect-keywords '("LATEX_CLASS")))
           (latex-class (if keywords (cadr (car keywords)) nil)))
      (when latex-class
        (if (string-equal latex-class "zju-beamer")
            (org-beamer-export-to-pdf t)
          (org-latex-export-to-pdf t))))))

(add-hook 'after-save-hook #'my/org-auto-export-on-save)

;;----------------------------------------------------------------------------
;; 6. Agenda & Capture
;;----------------------------------------------------------------------------

(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
  (setq org-default-notes-file (concat org-directory "todo.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "todo.org" "Inbox")
           "* TODO %?\n")
          ("p" "Project Task" entry (file+headline "projects.org" "Tasks")
           "* TODO %?\n  Source: %a\n  %i")
          ("e" "Emacs Config" entry (file+headline "config.org" "Emacs Tasks")
           "* TODO %?\n"))))

(use-package org-agenda
  :ensure nil
  :bind
  ("C-c a" . org-agenda)
  (:map org-agenda-mode-map
        ("C-." . consult-org-agenda))
  :custom
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  :config
  (setq org-agenda-files (append (list (expand-file-name "todo.org" org-directory)
                                       (expand-file-name "projects.org" org-directory)
                                       (expand-file-name "config.org" org-directory))
                                 (directory-files-recursively (expand-file-name "notes/" org-directory) "\\.org$")))
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Unscheduled Tasks")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting"))))))))



;;----------------------------------------------------------------------------
;; 7. Hacks & Chinese Support
;;----------------------------------------------------------------------------

(with-eval-after-load 'org
  ;; [https://emacs-china.org/t/org-mode/22313/5]
  ;; 修正 org-mode 内渲染中文标记时，中文词左右无需空格
  (setq org-emphasis-regexp-components
        '("-[:space:]('\"{[:nonascii:][:alpha:]"
          "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
          "[:space:]"
          "."
          1))
  ;; Re-calculate Org emphasis regexps to apply the changes above
  (when (fboundp 'org-set-emph-re)
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

  ;; [https://emacs-china.org/t/org-mode/22313/4]
  ;; 修正 org-mode 内 *粗体*连*粗体* 的渲染
  (defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\).*?[~=*/_+]"
                            (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
          (let* ((marker (match-string 2))
                 (verbatim? (member marker '("~" "="))))
            (when (save-excursion
                    (goto-char (match-beginning 0))
                    (and
                     ;; Do not match table hlines.
                     (not (and (equal marker "+")
                               (org-match-line
                                "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
                     ;; Do not match headline stars.  Do not consider
                     ;; stars of a headline as closing marker for bold
                     ;; markup either.
                     (not (and (equal marker "*")
                               (save-excursion
                                 (forward-char)
                                 (skip-chars-backward "*")
                                 (looking-at-p org-outline-regexp-bol))))
                     ;; Do not treat subscript as underline (require non-alphanum before _)
                     (not (and (equal marker "_")
                               (let ((pre-char (char-before (match-beginning 2))))
                                 (and pre-char
                                      (string-match-p "[a-zA-Z0-9]" (char-to-string pre-char))))))
                     ;; Match full emphasis markup regexp.
                     (looking-at (if verbatim? org-verbatim-re org-emph-re))
                     ;; Do not span over paragraph boundaries.
                     (not (string-match-p org-element-paragraph-separate
                                          (match-string 2)))
                     ;; Do not span over cells in table rows.
                     (not (and (save-match-data (org-match-line "[ \t]*|"))
                               (string-match-p "|" (match-string 4))))))
              (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
                          (m (if org-hide-emphasis-markers 4 2)))
                (font-lock-prepend-text-property
                 (match-beginning m) (match-end m) 'face face)
                (when verbatim?
                  (org-remove-flyspell-overlays-in
                   (match-beginning 0) (match-end 0))
                  (remove-text-properties (match-beginning 2) (match-end 2)
                                          '(display t invisible t intangible t)))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(font-lock-multiline t org-emphasis t))
                (when (and org-hide-emphasis-markers
                           (not (org-at-comment-p)))
                  (add-text-properties (match-end 4) (match-beginning 5)
                                       '(invisible t))
                  (add-text-properties (match-beginning 3) (match-end 3)
                                       '(invisible t)))
                (throw :exit t))))))))

  ;; [https://emacs-china.org/t/org-emphasis-regexp-components-org/25600]
  ;; 使 org-mode 导出 Latex 时中文标记能够正确导出
  (defun eli/org-element--parse-generic-emphasis (mark type)
    "Parse emphasis object at point, if any.

MARK is the delimiter string used.  TYPE is a symbol among
`bold', `code', `italic', `strike-through', `underline', and
`verbatim'.

Assume point is at first MARK."
    (save-excursion
      (let ((origin (point)))
        (unless (bolp) (forward-char -1))
        (let ((opening-re
               (rx-to-string
                `(seq (or line-start (any space ?- ?\( ?' ?\" ?\{ nonascii))
                      ,mark
                      (not space)))))
          (when (looking-at opening-re)
            (goto-char (1+ origin))
            (let ((closing-re
                   (rx-to-string
                    `(seq
                      (not space)
                      (group ,mark)
                      (or (any space ?- ?. ?, ?\; ?: ?! ?? ?' ?\" ?\) ?\} ?\\ ?\[
                               nonascii)
                          line-end)))))
              (when (re-search-forward closing-re nil t)
                (let ((closing (match-end 1)))
                  (goto-char closing)
                  (let* ((post-blank (skip-chars-forward " \t"))
                         (contents-begin (1+ origin))
                         (contents-end (1- closing)))
                    (list type
                          (append
                           (list :begin origin
                                 :end (point)
                                 :post-blank post-blank)
                           (if (memq type '(code verbatim))
                               (list :value
                                     (and (memq type '(code verbatim))
                                          (buffer-substring
                                           contents-begin contents-end)))
                             (list :contents-begin contents-begin
                                   :contents-end contents-end)))))))))))))

  (advice-add #'org-element--parse-generic-emphasis :override #'eli/org-element--parse-generic-emphasis))


(provide 'init-org)

;;; init-org.el ends here
