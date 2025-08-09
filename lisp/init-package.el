;;; init-package.el --- Package management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the Emacs package management system.
;; It defines package repositories and bootstraps `use-package`.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Package Archives
;;----------------------------------------------------------------------------
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))


;;----------------------------------------------------------------------------
;; Bootstrap on First Run (Corrected Logic)
;;----------------------------------------------------------------------------

;; 1. Initialize the package system. This defines all the necessary functions
;;    and variables, including `package-archive-contents`.
(package-initialize)

;; 2. Now that the package system is initialized, we can safely check if the
;;    package list has been downloaded. If not, we fetch it.
(unless package-archive-contents
  (message "First time setup: refreshing package contents...")
  (package-refresh-contents)
  (message "Package contents refreshed."))


;;----------------------------------------------------------------------------
;; `use-package` Configuration
;;----------------------------------------------------------------------------
;; `use-package` is built into Emacs 29+, so we just need to configure it.
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)


;;----------------------------------------------------------------------------
;; Essential Security Package
;;----------------------------------------------------------------------------

;; Ensures the GPG keyring for GNU ELPA is up-to-date to verify package
;; signatures. With our corrected bootstrap logic, this will now be
;; successfully installed on the first run without errors.
(use-package gnu-elpa-keyring-update)


(provide 'init-package)
;;; init-package.el ends here
