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
  :commands yasnippet-capf
  :functions cape-capf-super eglot-completion-at-point my-eglot-capf-with-yasnippet
  :init
  ;; Smartly merge yasnippet-capf with the first existing capf in the list.
  ;; This ensures snippets are available alongside other completions (e.g. Org, Elisp).
  (defun my/setup-merged-yasnippet-capf ()
    (let ((head (car (remove t completion-at-point-functions))))
      (when head
        (setq-local completion-at-point-functions
                    (cons (cape-capf-super
                           head
                           #'yasnippet-capf)
                          (cdr (remove t completion-at-point-functions)))))))

  (add-hook 'prog-mode-hook #'my/setup-merged-yasnippet-capf)
  (add-hook 'conf-mode-hook #'my/setup-merged-yasnippet-capf)
  (add-hook 'text-mode-hook #'my/setup-merged-yasnippet-capf)

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

(provide 'init-snippet)
;;; init-snippet.el ends here
