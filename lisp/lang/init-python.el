;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

;; Python Mode
(use-package python
  :ensure nil
  :bind (:map python-base-mode-map
              ("<f6>" . my-python-format-buffer))
  :hook ((python-base-mode . my-python-eglot-setup)
         (python-base-mode . my-python-highlight-setup)
         (inferior-python-mode . my-python-inferior-cleanup-setup))
  :custom
  ;; Disable readline based native completion
  (python-shell-completion-native-enable nil)
  :config
  (defun my-python-eglot-setup ()
    "Configure buffer-local eglot semantic tokens for Python."
    (setq-local eglot-semantic-token-types
                '("type" "struct" "interface" "enum" "enumMember"
                  "function" "method" "namespace" "decorator" "variable"))) ; "class" "parameter"

  (defun my-python-highlight-setup ()
    "Setup custom highlighting for Python."
    ;; Override 'self' to be orange
    (font-lock-add-keywords nil '(("\\<self\\>" 0 'eglot-semantic-parameter t))))

  (defun my-python-inferior-cleanup-setup ()
    "Auto-kill inferior Python buffer and window on process exit."
    (set-process-sentinel
     (get-buffer-process (current-buffer))
     (lambda (p _e)
       (when (memq (process-status p) '(exit signal))
         (let ((buf (process-buffer p)))
           (when (buffer-live-p buf)
             (when-let ((win (get-buffer-window buf)))
               (ignore-errors (delete-window win)))
             (kill-buffer buf)))))))

  (defun my-python-format-buffer ()
    "Format Python buffer with ruff."
    (interactive)
    (unless (executable-find "ruff")
      (user-error "`ruff' not found in PATH"))
    (if (buffer-file-name)
        ;; If file is saved, format the file directly
        (progn
          (save-buffer)
          (shell-command (format "ruff format %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t)
          (message "Formatted with ruff"))
      ;; For unsaved buffers, try stdin (may fail if syntax errors)
      (shell-command-on-region
       (point-min) (point-max)
       "ruff format -"
       (current-buffer) t
       "*ruff errors*" t)))

  ;; Type checker & language server: `ty'
  (when (executable-find "ty")
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '((python-mode python-ts-mode) . ("ty" "server"))))))

;; Linter: `ruff'
(when (executable-find "ruff")
  (use-package flymake-ruff
    :hook (python-base-mode . flymake-ruff-load)))

;; Python Executable Tracker
(use-package pet)

(provide 'init-python)

;;; init-python.el ends here
