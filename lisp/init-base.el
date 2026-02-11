;;; init-base.el --- Core editor settings and behaviors -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the foundational editor experience for Emacs.
;; It includes:
;; - Personal information
;; - Environment setup (PATH, Proxy)
;; - Better Defaults
;; - Scrolling optimizations (pixel-scroll, ultra-scroll)
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Personal Information
;;----------------------------------------------------------------------------

(setq user-full-name    "Akane"
      user-mail-address "710105188@qq.com")

;;----------------------------------------------------------------------------
;; Environment
;;----------------------------------------------------------------------------

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
;; Better Defaults
;;----------------------------------------------------------------------------

;; Encoding
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; Startup
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;; Prompts & Dialogs
(setq use-short-answers t
      use-file-dialog nil                 ; Don't use native file dialog
      use-dialog-box nil)                 ; Don't use dialog boxes for prompts

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Text Wrapping & Fill
(setq-default fill-column 80
              truncate-lines t)           ; Truncate long lines instead of wrapping

(setq word-wrap-by-category t             ; Better word wrap for CJK
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$")

;; Sentence & Paragraph
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)      ; Don't require double space after period

;; File & Backup
(setq make-backup-files nil
      auto-save-default nil
      delete-by-moving-to-trash t)

;; Buffer Management
(setq-default major-mode 'text-mode)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Distinguish buffers with same filename
(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function) ; Don't prompt when killing buffer with running process

;; Flash screen instead of audible bell
(setq visible-bell t)

;; Line Numbers (Modeline)
(setq column-number-mode t
      line-number-mode t)

;; Cursor Display & Movement
(setq-default cursor-in-non-selected-windows nil)  ; Hide cursor in inactive windows

(setq line-move-visual nil                ; Move by logical lines, not visual lines
      track-eol t)                        ; Keep cursor at EOL when moving vertically

;; Basic Editing
(setq kill-whole-line t)                  ; C-k kills entire line including newline
(setq set-mark-command-repeat-pop t)      ; After C-u C-SPC, repeat C-SPC to keep popping

;; Terminal Compatibility
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Line Numbers
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode LaTeX-mode)
  :config
  (setq display-line-numbers-width-start t))

;; Text Mode Hooks
(use-package simple
  :ensure nil
  :hook ((on-first-file . size-indication-mode)   ; Show buffer size in modeline
         (text-mode . visual-line-mode)))         ; Soft wrap in text modes

;; ---------------------------------------------------------------------------
;; Scrolling
;; ---------------------------------------------------------------------------

(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      hscroll-step 1
      hscroll-margin 2
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount-horizontal 1
      mouse-wheel-progressive-speed nil)

(use-package pixel-scroll
  :ensure nil
  :hook (on-first-input . pixel-scroll-precision-mode)
  :bind (("C-v" . pixel-scroll-interpolate-down)
         ("M-v" . pixel-scroll-interpolate-up))
  :config
  (setq pixel-scroll-precision-interpolate-page t))

(use-package ultra-scroll
  :after pixel-scroll
  :demand t
  :config
  (ultra-scroll-mode 1))


;;----------------------------------------------------------------------------
;; Server
;;----------------------------------------------------------------------------

(use-package server
  :ensure nil
  :defer 0.5)

(provide 'init-base)
;;; init-base.el ends here
