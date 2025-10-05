;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI configurations.
;;

;;; Code:

;; Interact with ChatGPT or other LLMs
(use-package gptel
  :functions gptel-make-openai
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'gpt-4.1)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"
                        :stream t)))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

(provide 'init-ai)
;;; init-ai.el ends here
