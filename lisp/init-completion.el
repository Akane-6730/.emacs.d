;;; init-completion.el --- The completion framework -*- lexical-binding: t; -*-

;;
;; This file configures the entire completion system. It uses a modern stack
;; based on Vertico for the minibuffer UI, Corfu for in-buffer completion,
;; and Consult for powerful search and navigation commands.
;;

;;----------------------------------------------------------------------------
;; Minibuffer Completion UI (The Vertico Stack)
;;----------------------------------------------------------------------------

;;
;; Package: vertico
;; Replaces the default completion UI with a performant vertical display.
;; It provides the interactive list of candidates in the minibuffer for
;; commands like M-x and C-x b.
;;
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom (vertico-count 15)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)))

(use-package nerd-icons-completion
  :hook (vertico-mode . nerd-icons-completion-mode))

;;
;; Package: orderless
;; A powerful completion style that allows matching items out of order.
;; Instead of typing a precise substring, you can type space-separated
;; keywords to filter candidates efficiently.
;;
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;;
;; Package: marginalia
;; Adds rich, contextual annotations to completion candidates in the minibuffer.
;; For example, it shows function signatures, variable values, or file details
;; right next to the candidate name.
;;
(use-package marginalia
  :ensure t
  :after vertico
  :hook (vertico-mode . marginalia-mode))


;;----------------------------------------------------------------------------
;; In-Buffer Completion (The Corfu Stack)
;;----------------------------------------------------------------------------

;;
;; Package: corfu
;; Provides a clean, unobtrusive pop-up for in-buffer auto-completion.
;; It displays completion suggestions directly at the cursor as you type,
;; similar to modern IDEs.
;;

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-cycle t) ; Enable cycling for our smart-tab to use
  :config
  (defun my-corfu-smart-tab ()
    "A smart TAB command for Corfu.
     If a yasnippet can be expanded, expand it. Otherwise, cycle
     to the next Corfu completion candidate."
    (interactive)
    ;; We use a special variable to tell yasnippet not to fallback
    ;; to any other completion function, just to try expanding.
    (let ((yas-fallback-behavior 'return-nil))
      (if (yas-expand)
          ;; If yas-expand returns `t` (meaning a snippet was expanded),
          ;; we do nothing further.
          (progn)
        ;; If no snippet was expanded, we fallback to Corfu's first candidate.
        (corfu-insert))))
  ;; We now directly modify `corfu-map` using `define-key`.
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  ;; Bind TAB to our new smart function.
  (define-key corfu-map (kbd "TAB") #'my-corfu-smart-tab))


;;
;; Package: cape
;; Provides various "Completion At Point Extensions" (backends) for Corfu.
;; This package acts as a source for completion candidates, enabling Corfu
;; to complete things like file paths, words from other buffers, and more.
;;
;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


;;----------------------------------------------------------------------------
;; Enhanced Commands & Search (Consult)
;;----------------------------------------------------------------------------

;;
;; Package: consult
;; A suite of enhanced commands for search, navigation, and narrowing.
;; It provides powerful, preview-enabled replacements for built-in commands
;; like `switch-to-buffer` and introduces new ones like `consult-ripgrep`.
;;
(use-package consult
  :ensure t
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

  ;; The :init block runs before the package is loaded.
  :init
  ;; Use Consult's richer UI for `xref` (e.g., find references/definitions).
  ;; This is a major improvement over the default xref display.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; The :config block runs after the package has been loaded.
  :config
  ;; Configure how previews are triggered in Consult.
  ;; This setup enables preview for most commands with a slight debounce
  ;; to avoid being overwhelming.
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

;;
;; Fine-tunes Emacs's built-in completion behavior to better integrate
;; with our modern completion framework.
;;
(use-package emacs
  :ensure nil
  :init
  ;; (setq-default tab-always-indent 'complete)
  ;; Clean up M-x by hiding commands not meant for direct user interaction.
  (setq read-extended-command-predicate #'command-completion-default-include-p))


(provide 'init-completion)
;;; init-completion.el ends here
