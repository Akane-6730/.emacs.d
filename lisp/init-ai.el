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
  :custom (gptel-default-mode 'org-mode)
  :config
  (setq gptel--known-backends nil) ; Clear default backends (ChatGPT)
  (setq gptel-model 'gemini-2.5-pro
        gptel-backend (gptel-make-gemini "Gemini"
                        :key gptel-api-key
                        :stream t
                        :models '(gemini-2.5-pro gemini-2.5-flash)))

  (gptel-make-gh-copilot "Copilot" :stream t))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :config
  (setq gptel-magit-model "gpt-4.1"
        gptel-magit-backend (gptel-make-gh-copilot "copilot" :stream t)))

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
