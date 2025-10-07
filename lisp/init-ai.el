;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI configurations.
;;

;;; Code:

;; Interact with ChatGPT or other LLMs
(use-package gptel
  :functions gptel-make-openai
  :bind ("C-c g" . gptel-menu)
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'gpt-4.1)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"
                        :stream t)))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

;; GitHub Copilot for code completion
(use-package copilot
  :hook ((markdown-mode latex-mode) . copilot-mode)
  :init (setq copilot--indent-warning-printed-p t
              copilot-indent-offset-warning-disable t)
  :config
  (with-no-warnings
    (defun my-copilot-get-source-suppress-warning (original-function &rest args)
      "Advice to suppress display-warning in copilot--get-source."
      (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) nil)))
        (apply original-function args))))
  (advice-add 'copilot--get-source :around #'my-copilot-get-source-suppress-warning)
  :bind (:map copilot-mode-map
              ("<tab>" . copilot-accept-completion)
              ("M-<right>" . copilot-accept-completion-by-word)))

(provide 'init-ai)
;;; init-ai.el ends here
