;;; init-utils.el --- Custom utility functions for our configuration -*- lexical-binding: t; -*-

;;
;; This file is our personal "toolbox". It contains custom helper functions
;; that are used by other modules in our configuration. It should be loaded
;; very early by `init.el`.
;;

;; This is the function definition for `font-installed-p`, taken from
;; the Centaur source and simplified for Emacs 29+. It checks if a font
;; with the given name is available on your system.
(defun font-installed-p (font-name)
  "Check if a font with FONT-NAME is available on the system.
This is a custom utility function."
  (when (stringp font-name)
    ;; `find-font` is the standard way to check for a font's existence.
    (find-font (font-spec :family font-name))))

(defcustom logo (expand-file-name
                 "banner.txt"
                 user-emacs-directory)
  "Set LOGO.."
  :type 'string)

;; This makes the functions in this file available to other files that `require` it.
(provide 'init-utils)
;;; init-utils.el ends here
