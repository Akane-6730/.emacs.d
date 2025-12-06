;;; init-base.el --- Core editor settings and behaviors -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file establishes the fundamental behavior of the editor. It includes
;; personal information, system-specific tweaks (macOS/Linux), encoding,
;; file history, and other essential defaults.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Personal Information
;;----------------------------------------------------------------------------

;; Set your personal details. These are used in file headers, email, etc.
(setq user-full-name    "Akane"
      user-mail-address "710105188@qq.com")

;; Use garbage collector magic hack to improve performance.
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init (setq gcmh-idle-delay 'auto
              gcmh-auto-idle-delay-factor 10
              gcmh-high-cons-threshold #x1000000)) ; 16MB

;;----------------------------------------------------------------------------
;; System-Specific Configuration (macOS & Linux)
;;----------------------------------------------------------------------------

;; macOS Keyboard Configuration
;; This makes the Command key act as Meta and Option key as Super.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier  'super))

;; Manually load PATH from ~/.path for better performance.
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > .path
(when (or (memq window-system '(mac ns x)) (daemonp))
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

;; Disable `exec-path-from-shell` for bad performance on startup.
;; (use-package exec-path-from-shell
;;   :if (or (memq window-system '(mac ns x)) (daemonp))
;;   :commands exec-path-from-shell-initialize
;;   :custom (setq exec-path-from-shell-arguments '("-l"))
;;   :init
;;   (exec-path-from-shell-initialize))

;;----------------------------------------------------------------------------
;; Encoding
;;----------------------------------------------------------------------------

;; Set UTF-8 as the default encoding everywhere. This is the modern standard
;; and prevents a wide range of issues with international characters.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;;----------------------------------------------------------------------------
;; Network / Proxy Settings
;;----------------------------------------------------------------------------

;; Set up HTTP/HTTPS proxy for Emacs and external processes
(defconst my-proxy-host "127.0.0.1:7897" "Host:Port for the proxy.")
(defconst my-proxy-url (concat "http://" my-proxy-host) "Full URL for the proxy.")

(setenv "http_proxy" my-proxy-url)
(setenv "https_proxy" my-proxy-url)
(setenv "no_proxy" "localhost,127.0.0.1,.local")

;; Configure proxy for the internal 'url' package
(setq url-proxy-services
      `(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . ,my-proxy-host)
        ("https" . ,my-proxy-host)))


;;----------------------------------------------------------------------------
;; Core Editor Behavior
;;----------------------------------------------------------------------------

;; Use y/n for prompts instead of the full "yes" or "no". It's faster.
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 ;; Indentation: Use spaces instead of tabs, with a width of 4 spaces.
 indent-tabs-mode nil
 tab-width 4
 ;; Set the default width for wrapping text.
 fill-column 80
 ;; Default to text-mode for new, unrecognized files.
 major-mode 'text-mode)

(setq
 make-backup-files nil
 auto-save-default nil
 delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
 ;; When multiple buffers have the same name, show parts of the file path
 ;; to distinguish them, e.g., <.../project-a/file.txt> and <.../project-b/file.txt>.
 uniquify-buffer-name-style 'post-forward-angle-brackets
 ;; When you press C-g, show a visual "bell" (flash) instead of making a sound.
 visible-bell t
 ;; Kill the entire line, including the newline character when using `kill-line` (C-k).
 inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
 uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
 adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
 adaptive-fill-first-line-regexp "^* *$"
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil
 word-wrap-by-category t)

;;----------------------------------------------------------------------------
;; File & Command History
;;----------------------------------------------------------------------------
;; `savehist-mode` saves your minibuffer history
(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-x C-r" . recentf-open-files))
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

;;----------------------------------------------------------------------------
;; Input Method
;;----------------------------------------------------------------------------

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(rime-predicate-after-alphabet-char-p ; Switch to English after alphabet characters
     rime-predicate-prog-in-code-p        ; Force English in code (except comments/strings)
     rime-predicate-space-after-cc-p      ; Support mixed English/Chinese (space after Chinese)
     )))

(provide 'init-base)
;;; init-base.el ends here
