;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

;; Python Mode
(use-package python
  :ensure nil
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Use buffer-local setting for Python mode only
  (defun my-python-eglot-setup ()
    (setq-local eglot-semantic-token-types
                '("type"  "struct" "interface" "enum" "enumMember" "function" "method" "namespace" "decorator" "variable" ))) ; "class" "parameter"
  (add-hook 'python-base-mode-hook 'my-python-eglot-setup)

  ;; Type checker & language server: `ty'
  (when (executable-find "ty")
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '((python-mode python-ts-mode)
                     . ("ty" "server")))))
  ;; Linter & formatter: `ruff'
  (when (executable-find "ruff")
    (use-package flymake-ruff
      :hook (python-base-mode . flymake-ruff-load))

    (defun my-python-format-buffer ()
      "Format Python buffer with ruff."
      (interactive)
      (if (buffer-file-name)
          ;; If file is saved, format the file directly
          (progn
            (save-buffer)
            (shell-command (format "ruff format %s" (shell-quote-argument (buffer-file-name))))
            (revert-buffer t t t)
            (message "Formatted with ruff"))
        ;; For unsaved buffers, try stdin (may fail if syntax errors)
        (shell-command-on-region
         (point-min) (point-max)
         "ruff format -"
         (current-buffer) t
         "*ruff errors*" t)))

    ;; Bind F6 in Python mode to use ruff formatter
    (defun my-python-mode-keys ()
      "Set Python-specific keybindings."
      (local-set-key (kbd "<f6>") 'my-python-format-buffer))

    (add-hook 'python-base-mode-hook 'my-python-mode-keys)))

(defun my-python-highlight-setup ()
  "Setup custom highlighting for Python."
  ;; Override 'self' to be orange
  (font-lock-add-keywords
   nil
   '(("\\<self\\>" 0 'eglot-semantic-parameter t))))

(add-hook 'python-base-mode-hook 'my-python-highlight-setup)

(provide 'init-python)

;;; init-python.el ends here
