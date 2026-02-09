;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI configurations.
;;

;;; Code:

;; Interact with ChatGPT or other LLMs
(use-package gptel
  :functions gptel-make-openai
  :bind
  ("C-c g" . gptel-menu)
  ("C-c <RET>" . gptel-send)
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel--known-backends nil) ; Clear default backends (ChatGPT)
  (setq gptel-track-media t)
  (setq gptel-model 'gemini-3-pro-preview
        gptel-backend (gptel-make-gh-copilot "Copilot" :stream t))
  (gptel-make-gemini "Gemini"
    :key gptel-api-key
    :stream t
    :models '(gemini-2.5-pro gemini-2.5-flash))
  (gptel-make-openai "KFC"
    :host "kfc-api.sxxe.net"
    :key gptel-api-key
    :stream t
    :models '(cursor2-claude-4.5-sonnet cursor2-claude-4.1-opus)))


;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install)
  :config
  (setq gptel-magit-model "claude-haiku-4.5"
        gptel-magit-backend (gptel-make-gh-copilot "copilot" :stream t))

  ;; Better prompt: structured with bullet points
  (setq gptel-magit-commit-prompt
        "You are an expert at writing Git commits. Write a structured commit message.

Structure:
    <type>(<scope>): <subject>

    - <detail item 1>
    - <detail item 2>
    [optional: more context if complex]

Rules:
1. Subject Line:
   - Max 50 chars. Imperative mood. Capitalized. No period.
   - Focus on the PRIMARY impact (the \"what\" and \"why\").

2. Body:
   - REQUIRED for non-trivial commits.
   - Use a bulleted list (-) for specific changes.
   - Be specific but concise. Group related changes.

Priority:
- FEAT/FIX are the most important.
- Refactorings should be listed in the body, not hidden, but not cluttering the subject.

Output ONLY the commit message.")

  ;; Fix: Don't re-fill the message at all. Trust the LLM's formatting.
  (defun my/gptel-magit--format-commit-message (message)
    "Return message as-is, respecting LLM's formatting."
    message)

  (advice-add 'gptel-magit--format-commit-message
              :override #'my/gptel-magit--format-commit-message))

;; GitHub Copilot for code completion
(use-package copilot
  :hook ((latex-mode markdown-mode) . copilot-mode)
  ;; :hook (org-mode . my/copilot-enable-for-org)
  :bind (:map copilot-mode-map
              ("C-e" . my/copilot-accept-or-end-of-line)
              ("M-f" . my/copilot-accept-word-or-forward-word))
  :config
  (defun my/copilot-enable-for-org ()
    "Enable Copilot mode in Org mode, but not during agenda or capture."
    (unless (or (eq this-command 'org-agenda) (eq this-command 'org-capture))
      (when (eq major-mode 'org-mode)
        (copilot-mode 1))))

  (defun my/copilot-accept-or-end-of-line ()
    "Accept Copilot completion or move to end of line (mwim)."
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             (boundp 'copilot--overlay)
             (overlayp copilot--overlay)
             (overlay-buffer copilot--overlay))
        (copilot-accept-completion)
      (call-interactively #'move-end-of-line)))

  (defun my/copilot-accept-word-or-forward-word ()
    "Accept Copilot word or move forward word."
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             (boundp 'copilot--overlay)
             (overlayp copilot--overlay)
             (overlay-buffer copilot--overlay))
        (copilot-accept-completion-by-word)
      (call-interactively #'forward-word)))

  (setq copilot--indent-warning-printed-p t
        copilot-indent-offset-warning-disable t)

  (with-no-warnings
    (defun my-copilot-get-source-suppress-warning (original-function &rest args)
      "Advice to suppress display-warning in copilot--get-source."
      (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) nil)))
        (apply original-function args))))
  (advice-add 'copilot--get-source :around #'my-copilot-get-source-suppress-warning))

;; A native shell experience to interact with ACP agents
;; (use-package agent-shell
;;   :diminish agent-shell-ui-mode
;;   :hook (agent-shell-mode . (lambda () (activate-input-method "rime")))
;;   :bind (:map agent-shell-mode-map
;;               ("C-<return>" . agent-shell-submit)
;;               ("RET" . nil)))


(provide 'init-ai)
;;; init-ai.el ends here
