;;; init-snippet.el --- Code snippet expansion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures yasnippet for code snippet expansion in Emacs.
;;
;;; Code:

(use-package yasnippet
  :hook (on-first-input . yas-global-mode)
  :config
  (setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet-capf
  :after cape
  :commands yasnippet-capf
  :init
  (defvar-local my/eglot+yas-capf nil
    "Buffer-local super Capf merging Eglot and Yasnippet.")

  (defun my/yasnippet-capf-setup ()
    "Enable `yasnippet-capf' in the current buffer."
    (add-hook 'completion-at-point-functions #'yasnippet-capf t t))

  ;; Non-LSP buffers: snippets sit alongside the mode Capf.
  (add-hook 'prog-mode-hook #'my/yasnippet-capf-setup)
  (add-hook 'conf-mode-hook #'my/yasnippet-capf-setup)
  (add-hook 'text-mode-hook #'my/yasnippet-capf-setup)

  ;; LSP buffers: merge Eglot + Yasnippet into one candidate list so Corfu
  ;; shows both.  Appending yasnippet alone is not enough — a successful
  ;; exclusive Eglot response would hide snippets.
  (defun my/eglot-capf-setup ()
    "Install or tear down the Eglot+Yasnippet super Capf."
    (if (eglot-managed-p)
        (progn
          (setq my/eglot+yas-capf
                (cape-capf-super #'eglot-completion-at-point
                                 #'yasnippet-capf))
          (setq-local completion-at-point-functions
                      (cons my/eglot+yas-capf
                            (cl-set-difference
                             completion-at-point-functions
                             (list #'eglot-completion-at-point
                                   #'yasnippet-capf)
                             :test #'eq))))
      ;; Eglot just left: drop the super Capf, restore plain yasnippet.
      (when my/eglot+yas-capf
        (remove-hook 'completion-at-point-functions my/eglot+yas-capf 'local)
        (setq my/eglot+yas-capf nil))
      (my/yasnippet-capf-setup)))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf-setup)

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

(provide 'init-snippet)
;;; init-snippet.el ends here
