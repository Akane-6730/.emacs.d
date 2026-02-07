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
  :hook (on-first-input . vertico-mode)
  :custom (vertico-count 13)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  ;; Fix "Selecting deleted buffer" error when switching buffers with consult-buffer
  ;; This happens when a buffer (like *Org Export Process*) dies while the menu is open.
  (defun my/nerd-icons-completion-safe-get-icon (orig-fun &rest args)
    "Safe wrapper for `nerd-icons-completion-get-icon' that handles dead buffers."
    (condition-case nil
        (apply orig-fun args)
      (error " ")))
  (advice-add 'nerd-icons-completion-get-icon :around #'my/nerd-icons-completion-safe-get-icon))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-pcm-leading-wildcard t)
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; Adds rich, contextual annotations to completion candidates in the minibuffer.
(use-package marginalia
  :after vertico
  :hook (vertico-mode . marginalia-mode))

;;----------------------------------------------------------------------------
;; In-Buffer Completion
;;----------------------------------------------------------------------------
(use-package corfu
  :hook
  (on-first-input . global-corfu-mode)
  (global-corfu-mode . corfu-popupinfo-mode)
  ;; Use 'orderless-flex' only when using corfu (in-buffer completion)
  (corfu-mode . (lambda ()
                  (setq-local orderless-matching-styles
                              '(orderless-literal orderless-regexp orderless-flex))))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.05)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (global-corfu-modes '((not erc-mode circe-mode help-mode gud-mode vterm-mode) t))
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
  ;;Quit completion before saving
  (add-hook 'before-save-hook #'corfu-quit)
  (advice-add #'persistent-scratch-save :before #'corfu-quit))

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

(use-package yasnippet-capf
  :after cape
  :commands yasnippet-capf
  :functions cape-capf-super eglot-completion-at-point my-eglot-capf-with-yasnippet
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)

  ;; To integrate `yasnippet-capf' with `eglot' completion
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun my-eglot-capf-with-yasnippet ()
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-super
                  #'eglot-completion-at-point
                  #'yasnippet-capf))))
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf-with-yasnippet)

  :config
  ;; Patch yasnippet-capf to avoid errors with key-less snippets
  (defun yasnippet-capf--completions-for-prefix (prefix tables)
    (let ((templates (yas--all-templates tables))
          (requirement (yas--require-template-specific-condition-p)))
      (mapcar (lambda (template)
                (let ((can-expand (yas--template-can-expand-p
                                   (yas--template-condition template) requirement))
                      (name (yas--template-name template))
                      (name-or-key
                       (funcall (intern-soft (format "yas--template-%s" yasnippet-capf-lookup-by)) template)))
                  (when (and can-expand (stringp name-or-key))
                    (propertize name-or-key
                                'yas-annotation name
                                'yas-template template
                                'yas-prefix-offset (- (length name-or-key)
                                                      (length prefix))))))
              templates))))


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
