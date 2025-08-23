;;; init-base.el --- Core editor settings and behaviors -*- lexical-binding: t; -*-

;;
;; This file establishes the fundamental behavior of the editor. It includes
;; personal information, system-specific tweaks (macOS/Linux), encoding,
;; file history, and other essential defaults.
;;

;;----------------------------------------------------------------------------
;; Personal Information
;;----------------------------------------------------------------------------

;; Set your personal details. These are used in file headers, email, etc.
(setq user-full-name    "Akane"
      user-mail-address "710105188@qq.com")

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
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

;; Ensure Emacs inherits the shell's environment variables (like $PATH).
;; This is crucial for external tools, especially LSP servers, to work correctly.
(when (or (memq window-system '(mac ns x)) (daemonp))
  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :custom (exec-path-from-shell-arguments '("-l"))
    :init (exec-path-from-shell-initialize)))


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
 ;; Don't create backup files (`#foo#`). We use version control (Git) instead.
 make-backup-files nil
 ;; Disable auto-save files (`#foo#`). It can be distracting.
 auto-save-default nil

 ;; When deleting a file, move it to the system trash instead of deleting forever.
 delete-by-moving-to-trash t

 ;; When multiple buffers have the same name, show parts of the file path
 ;; to distinguish them, e.g., <.../project-a/file.txt> and
 ;; <.../project-b/file.txt>.
 uniquify-buffer-name-style 'post-forward-angle-brackets

 ;; When you press C-g, show a visual "bell" (flash) instead of making a sound.
 visible-bell t

 ;; This tells Emacs to kill the entire line, including the newline character,
 ;; when using `kill-line` (C-k).
 kill-whole-line t)


;;----------------------------------------------------------------------------
;; File & Command History
;;----------------------------------------------------------------------------

;; `savehist-mode` saves your minibuffer history (e.g., past commands, search
;; queries) between Emacs sessions.
(use-package savehist
  :ensure nil ; This is a built-in package.
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000
        enable-recursive-minibuffers t))

;; `save-place-mode` remembers the cursor position in files, so when you
;; reopen a file, you're right back where you left off.
(use-package saveplace
  :ensure nil ; This is a built-in package.
  :hook (after-init . save-place-mode))

;; `recentf-mode` keeps a list of recently opened files, making it easy to
;; jump back to them.
(use-package recentf
  :ensure nil ; This is a built-in package.
  :hook (after-init . recentf-mode)
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 50
        recentf-max-saved-items 50
        ;; Exclude temporary files, caches, and package directories.
        recentf-exclude '("/tmp/"
                          "/ssh:"
                          "COMMIT_EDITMSG"
                          "/.emacs.d/elpa/"
                          "/.emacs.d/eln-cache/"
                          "/.emacs.d/auto-save-list/")))


;;----------------------------------------------------------------------------
;; Emacs Server
;;----------------------------------------------------------------------------

;; Start the Emacs server, allowing `emacsclient` to connect to this Emacs
;; instance. This is useful for opening files quickly from the terminal.
(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))


(provide 'init-base)
;;; init-base.el ends here
