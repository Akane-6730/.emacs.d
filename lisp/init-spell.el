;;; init-spell.el --- Spell checking configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configures on-the-fly spell checking using the built-in `flyspell`.
;;
;; This setup enables spell checking for human-readable text.
;; In document modes (like Org or Markdown), it checks the whole buffer.
;; In programming modes, it intelligently checks only comments and strings.
;;
;; To correct a misspelled word, move the cursor over it and use the default
;; command `flyspell-auto-correct-word` (default bound to `C-M-i`).
;;
;; This feature requires the external program `aspell` to be installed.

;;; Code:

;;----------------------------------------------------------------------------
;; Helper functions for Keybinding Management
;;----------------------------------------------------------------------------

(defun my-spell--unbind-all-conflicting-keys ()
  "Globally unbind all potentially conflicting keys from flyspell.
This creates a clean slate, and specific keys can be re-bound locally
in the modes where they are desired."
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-;" flyspell-mode-map)
  (unbind-key "C-," flyspell-mode-map))

(defun my-spell--rebind-correction-keys-for-text ()
  "Re-bind keys for non-programming modes.
This function is hooked into text-centric modes to provide convenient
correction keys, without affecting programming modes."
  ;; We bind `C-,` locally for the buffer.
  (define-key (current-local-map) (kbd "C-,") #'flyspell-goto-next-error))

;;----------------------------------------------------------------------------
;; On-the-fly Spell Checking
;;----------------------------------------------------------------------------

(use-package flyspell
  :ensure nil ; Built-in, no installation needed.
  :if (executable-find "aspell") ; Only load if the `aspell` command exists.
  :hook ((prog-mode . flyspell-prog-mode)
         ;; This hook runs in ALL flyspell modes and removes all conflicting keys.
         (flyspell-mode . my-spell--unbind-all-conflicting-keys))
  :config
  ;; Use `aspell` as the backend and provide args for better suggestions.
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")
        flyspell-issue-message-flag nil)) ; Suppress startup messages.

;; For each text-centric mode, enable `flyspell-mode` and then add back
;; the specific keybindings we want for those modes.
(dolist (hook '(text-mode-hook
                org-mode-hook
                markdown-mode-hook
                git-commit-mode-hook))
  (add-hook hook #'flyspell-mode)
  (add-hook hook #'my-spell--rebind-correction-keys-for-text))

(provide 'init-spell)
;;; init-spell.el ends here
