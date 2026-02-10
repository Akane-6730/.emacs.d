;;; init-spell.el --- Spell checking configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configures on-the-fly spell checking using the built-in `flyspell`.
;;
;; In document modes (Org/Markdown), checks the whole buffer.
;; In programming modes, checks only comments and strings.
;;
;; Requires external program `aspell` to be installed.

;;; Code:

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :bind (:map flyspell-mode-map
         ("C-;" . nil) ("C-," . nil) ("C-." . nil))
  :hook ((prog-mode . my-spell--flyspell-prog-maybe)
         ((markdown-mode org-mode) . my-spell--flyspell-maybe))
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  (flyspell-issue-message-flag nil)
  :config
  (defvar my-spell-buffer-size-limit (* 512 1024)
    "Skip flyspell in buffers larger than this (default 512KB).")

  (defun my-spell--flyspell-maybe ()
    "Enable `flyspell-mode' unless buffer is too large."
    (unless (> (buffer-size) my-spell-buffer-size-limit)
      (flyspell-mode 1)))

  (defun my-spell--flyspell-prog-maybe ()
    "Enable `flyspell-prog-mode' unless buffer is too large."
    (unless (> (buffer-size) my-spell-buffer-size-limit)
      (flyspell-prog-mode)))

  ;; Helper to setup skip regions
  (defun my-spell--add-skip-regions (regions)
    "Add REGIONS to `ispell-skip-region-alist' locally."
    (setq-local ispell-skip-region-alist
                (append regions ispell-skip-region-alist)))

  ;; Common LaTeX-style skip patterns
  (defconst my-spell--latex-skip-regions
    '(("\\\\(" "\\\\)") ("\\[" "\\]")
      ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}"))
    "Skip regions for LaTeX math environments.")

  ;; Org-mode: skip properties, verbatim, code blocks, and LaTeX
  (add-hook 'org-mode-hook
            (lambda ()
              (my-spell--add-skip-regions
               `((,org-property-drawer-re)
                 ("~" "~") ("=" "=")
                 ("^#\\+BEGIN_SRC" "^#\\+END_SRC")
                 ,@my-spell--latex-skip-regions))))

  ;; Markdown: skip inline code, fenced blocks, templates, and LaTeX
  (add-hook 'markdown-mode-hook
            (lambda ()
              (my-spell--add-skip-regions
               `(("`" "`")
                 ("^```" "^```")
                 ("{{" "}}")
                 ,@my-spell--latex-skip-regions)))))

(provide 'init-spell)
;;; init-spell.el ends here
