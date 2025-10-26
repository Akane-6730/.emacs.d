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
  (setq gptel-model 'gpt-5
        gptel-backend (gptel-make-openai "KXY"
                        :host "api.kkyyxx.xyz"
                        :key gptel-api-key
                        :stream t
                        :models '(gpt-5 gpt-5-codex
                                        claude-4.5-sonnet claude-4-opus claude-4.1-opus)))

  (gptel-make-openai "DoneHub"
    :host "donehub.darstib.cn"
    :protocol "http"
    :key gptel-api-key
    :stream t
    :models '(gpt-5 gpt-5-codex
                    claude-4.5-sonnet claude-opus-4 claude-sonnet-4-think claude-haiku-4-5-20251001
                    gemini-2.5-pro gemini-2.5-flash))

  (gptel-make-gh-copilot "Copilot" :stream t)

  (gptel-make-gemini "Gemini"
    :key gptel-api-key
    :stream t
    :models '(gemini-2.5-pro gemini-2.5-flash))

  (gptel-make-openai "Anannas"
    :host "api.anannas.ai"
    :key gptel-api-key
    :stream t
    :models '(deepseek/deepseek-v3.2-exp
              qwen/qwen3-coder-plus
              qwen/qwen3-coder
              qwen/qwen3-max)))

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
