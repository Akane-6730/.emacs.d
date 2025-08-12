;;; init-utils.el --- Initialize utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Some useful Utilities.
;;

;;; Code:

;;;----------------------------------------------------------------------------
;;; Keybinding Hints
;;;----------------------------------------------------------------------------

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

;;;----------------------------------------------------------------------------
;;; Search Tool Enhancement
;;;----------------------------------------------------------------------------

;; Replace the standard `grep` command with the much faster `ripgrep` (rg)
;; if it is available on the system.
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))


;;;----------------------------------------------------------------------------
;;; Auto-compile Personal Elisp Files on Save
;;;----------------------------------------------------------------------------

(defun auto-recompile-file-maybe ()
  (when (and (fboundp 'vc-root-dir)
             (string= (vc-root-dir) user-emacs-directory))
    (byte-compile-file buffer-file-name)))

(defun add-after-save-hook ()
  (add-hook 'after-save-hook 'auto-recompile-file-maybe nil t))

(add-hook 'emacs-lisp-mode-hook #'add-after-save-hook)

(provide 'init-utils)
;;; init-utils.el ends here
