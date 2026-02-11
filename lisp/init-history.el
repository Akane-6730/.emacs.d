;;; init-history.el --- History and state persistence -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures package that help Emacs remember state across sessions:
;; - savehist: Minibuffer history
;; - saveplace: Cursor position in files
;; - recentf: Recently opened files
;;
;;; Code:

;; Minibuffer history
(use-package savehist
  :hook (on-first-input . savehist-mode)
  :init (setq
         ;; enable-recursive-minibuffers t ; Allow commands in minibuffers
         history-length 1000
         savehist-additional-variables '(mark-ring
                                         global-mark-ring
                                         search-ring
                                         regexp-search-ring
                                         extended-command-history)
         savehist-autosave-interval 300))

(use-package saveplace
  :hook (on-first-file . save-place-mode))

(use-package recentf
  :hook (on-first-input . recentf-mode)
  :bind ("C-x C-r" . recentf-open-files)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(provide 'init-history)
;;; init-history.el ends here
