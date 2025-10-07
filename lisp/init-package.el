;;; init-package.el --- Package management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the Emacs package management system.
;; It defines package repositories and bootstraps `use-package`.
;;

;;; Code:

;;;; Set package archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system.
(package-initialize)

;; Ensure GPG keyring for GNU ELPA is up to date.
(unless package-archive-contents
  (message "First time setup: refreshing package contents...")
  (package-refresh-contents)
  (message "Package contents refreshed."))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)

;; Prevent saving `package-selected-packages` to `custom-file`.
;; @see https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;; Ensures the GPG keyring for GNU ELPA is up-to-date to verify package
;; signatures.
(use-package gnu-elpa-keyring-update)

(provide 'init-package)
;;; init-package.el ends here
