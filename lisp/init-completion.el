;;; init-completion.el --- The completion framework -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the entire completion system. It uses
;; Vertico for the minibuffer UI, Corfu for in-buffer completion,
;; and Consult for powerful search and navigation commands.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Minibuffer Completion UI
;;----------------------------------------------------------------------------

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom (vertico-count 15)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Adds rich, contextual annotations to completion candidates in the minibuffer.
(use-package marginalia
  :after vertico
  :hook (vertico-mode . marginalia-mode))

;;----------------------------------------------------------------------------
;; In-Buffer Completion
;;----------------------------------------------------------------------------

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.05)
  (corfu-cycle t)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
              ("TAB" . my-corfu-smart-tab)
              ;; Custom binding to quit Corfu and move to the next line
              ("C-j" . my-corfu-quit-and-next-line))
  :config
  ;; helper functions
  (defun my-corfu-smart-tab ()
    "Smarter TAB for Corfu. Expands yasnippet, or inserts Corfu completion."
    (interactive)
    (or (yas-expand)
        (corfu-insert)))
  (defun my-corfu-quit-and-next-line ()
    "Quit Corfu and then move to the next line."
    (interactive)
    (corfu-quit)
    (forward-line))
  ;; In eshell, disable auto-completion to prevent triggering on every space.
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local corfu-auto nil)
                                (corfu-mode))))

(when (< emacs-major-version 31)
  (use-package corfu-terminal
    :if (display-graphic-p)
    :ensure t
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; Provides various "Completion At Point Extensions" (backends) for Corfu.
(use-package cape
  :commands (cape-file cape-elisp-block cape-keyword)
  :autoload (cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
  :autoload (cape-wrap-silent cape-wrap-purify)
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)

  ;; Make these capfs composable.
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))


;;----------------------------------------------------------------------------
;; Enhanced Commands & Search
;;----------------------------------------------------------------------------

;; A suite of enhanced commands for search, navigation, and narrowing.
(use-package consult
  :bind ( ;; C-x prefix bindings
         ("C-x b"   . consult-buffer)                ;; Better `switch-to-buffer`
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; M-g (goto) prefix bindings
         ("M-g g" . consult-goto-line)              ;; Better `goto-line`
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g f" . consult-flymake)                ;; For built-in error checking
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s (search) prefix bindings
         ("M-s s" . consult-line)                   ;; Better `isearch` (replaces C-s)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)                   ;; Alternative for `consult-line`
         ("M-s L" . consult-line-multi)
         ;; C-c prefix bindings (user-level commands)
         ("C-."   . consult-imenu))
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c r" . consult-ripgrep)                ;; Alternative for ripgrep
  ("C-c T" . consult-theme)
  ("C-c m"   . consult-man)

  ([remap Info-search]        . consult-info)
  ([remap isearch-forward]    . consult-line)
  ([remap recentf-open-files] . consult-recent-file)
  :init
  ;; Use Consult's richer UI for `xref` (e.g., find references/definitions).
  ;; This is a major improvement over the default xref display.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   :preview-key '(:debounce 0.4 any))
  ;; Set the key for "narrowing" results, e.g., searching only within a
  ;; specific directory during a file search.
  (setq consult-narrow-key "<"))

;;----------------------------------------------------------------------------
;; Configuration for Built-in Emacs Completion Settings
;;----------------------------------------------------------------------------

(use-package emacs
  :ensure nil
  :custom
  ;; (setq-default tab-always-indent 'complete)
  ;; Clean up M-x by hiding commands not meant for direct user interaction.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil))


(provide 'init-completion)
;;; init-completion.el ends here
