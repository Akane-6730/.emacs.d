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
  (setq gptel--known-backends nil) ; Clear default backends (ChatGPT)
  (setq gptel-track-media t)
  (setq gptel-model 'gemini-3-pro-preview
        gptel-backend (gptel-make-gh-copilot "Copilot" :stream t))
  (gptel-make-gemini "Gemini"
                        :key gptel-api-key
                        :stream t
                        :models '(gemini-2.5-pro gemini-2.5-flash)))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :config
  (setq gptel-magit-model "gemini-3-flash-preview"
        gptel-magit-backend (gptel-make-gh-copilot "copilot" :stream t)))

;; GitHub Copilot for code completion
(use-package copilot
  :hook ((latex-mode text-mode) . copilot-mode)
  :bind (:map copilot-mode-map
              ("TAB" . my/copilot-tab)
              ("M-<right>" . copilot-accept-completion-by-word))
  :config
  (defun my/copilot-tab ()
    "Tab command that handles Copilot, Yasnippet, Corfu, and default Tab."
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             (boundp 'copilot--overlay)
             (overlayp copilot--overlay)
             (overlay-buffer copilot--overlay))
        (copilot-accept-completion)
      (let* ((minor-mode-map-alist
              (assq-delete-all 'copilot-mode (copy-sequence minor-mode-map-alist)))
             (cmd (key-binding (this-command-keys))))
        (if (and cmd (commandp cmd))
            (call-interactively cmd)
          (indent-for-tab-command)))))

  (setq copilot--indent-warning-printed-p t
        copilot-indent-offset-warning-disable t)

  (with-no-warnings
    (defun my-copilot-get-source-suppress-warning (original-function &rest args)
      "Advice to suppress display-warning in copilot--get-source."
      (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) nil)))
        (apply original-function args))))
  (advice-add 'copilot--get-source :around #'my-copilot-get-source-suppress-warning))

(provide 'init-ai)
;;; init-ai.el ends here
