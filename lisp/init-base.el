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

;;----------------------------------------------------------------------------
;; System & Performance
;;----------------------------------------------------------------------------

;; Use garbage collector magic hack to improve performance.
(use-package gcmh
  :ensure t
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init (setq gcmh-idle-delay 'auto
              gcmh-auto-idle-delay-factor 10
              gcmh-high-cons-threshold #x1000000)) ; 16MB

;; macOS Keyboard Configuration
;; This makes the Command key act as Meta and Option key as Super.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier  'super))

;; Manually load PATH from ~/.path for better performance.
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > .path
(when (or (window-system) (daemonp))
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

;; Async
(use-package async
  :functions (async-bytecomp-package-mode dired-async-mode)
  :config
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))

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
;; Encoding
;;----------------------------------------------------------------------------

;; Set UTF-8 as the default encoding everywhere. This is the modern standard
;; and prevents a wide range of issues with international characters.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

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
 delete-by-moving-to-trash t
 ;; When multiple buffers have the same name, show parts of the file path
 ;; to distinguish them, e.g., <.../project-a/file.txt> and <.../project-b/file.txt>.
 uniquify-buffer-name-style 'post-forward-angle-brackets
 ;; When you press C-g, show a visual "bell" (flash) instead of making a sound.
 visible-bell t
 ;; Kill the entire line, including the newline character when using `kill-line` (C-k).
 kill-whole-line t
 inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
 adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
 adaptive-fill-first-line-regexp "^* *$"
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil
 word-wrap-by-category t)

(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

;;----------------------------------------------------------------------------
;; History & Persistence
;;----------------------------------------------------------------------------

;; `savehist-mode` saves your minibuffer history
(use-package savehist
  :defer 0.01
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300)
  :config (savehist-mode 1))

(use-package saveplace
  :defer 0.01
  :config (save-place-mode 1))

(use-package recentf
  :defer 0.01
  :bind (("C-x C-r" . recentf-open-files))
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (recentf-mode 1)
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))


(defun byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

;; Async
(use-package async
  :functions (async-bytecomp-package-mode dired-async-mode)
  :config
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode))
  :config
  (setq column-number-mode t
        line-number-mode t
        kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        visual-line-mode t
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
  (when (not (display-graphic-p))
    (xterm-mouse-mode 1))
  ;; Prettify the process list
  (with-no-warnings
    (defun my-list-processes--prettify ()
      "Prettify process list."
      (when-let* ((entries tabulated-list-entries))
        (setq tabulated-list-entries nil)
        (dolist (p (process-list))
          (when-let* ((val (cadr (assoc p entries)))
                      (name (aref val 0))
                      (pid (aref val 1))
                      (status (aref val 2))
                      (status (list status
                                    'face
                                    (if (memq status '(stop exit closed failed))
                                        'error
                                      'success)))
                      (buf-label (aref val 3))
                      (tty (list (aref val 4) 'face 'font-lock-doc-face))
                      (thread (list (aref val 5) 'face 'font-lock-doc-face))
                      (cmd (list (aref val 6) 'face 'completions-annotations)))
            (push (list p (vector name pid status buf-label tty thread cmd))
                  tabulated-list-entries)))))
    (advice-add #'list-processes--refresh :after #'my-list-processes--prettify)))

(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

(provide 'init-base)
;;; init-base.el ends here
