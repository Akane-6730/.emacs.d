;;; init-mini.el --- Personal minimal configuration for debugging. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A minimal, modern, and personal configuration for fast startup and debugging.
;; It uses only built-in Emacs features (no external packages) and is
;; targeted for Emacs 29+.
;;
;; To use it, run: emacs -q -l /path/to/your/init-mini.el
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Core Setup & Better Defaults
;;----------------------------------------------------------------------------

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better defaults
;; (setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq kill-whole-line t)                   ; Kill line including '\n'

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; UI
;; (load-theme 'tango t)

(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; (global-hl-line-mode 1)

;; (if (fboundp 'display-line-numbers-mode)
;;     (global-display-line-numbers-mode 1)
;;   (global-linum-mode 1))

;; Basic modes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(when (fboundp 'savehist-mode)
  (savehist-mode 1))
(if (fboundp 'save-place-mode)
    (save-place-mode 1)
  (require 'saveplace)
  (setq-default save-place t))

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Completion
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode 1))

(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))
      (global-set-key (kbd "C-x C-r") 'fido-recentf-open))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-enable-flex-matching t)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to find a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (global-set-key (kbd "C-x C-r") 'ido-recentf-open)))

;; Key Modifiers
(cond
 ((eq system-type 'windows-nt)
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super     ; Left Windows key
        w32-apps-modifier 'hyper)       ; Menu/App key
  (w32-register-hot-key [s-t]))
 ((eq window-system 'mac)
  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super z)] #'undo)))

;; Keybindings
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;; Maximize window to full Screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;----------------------------------------------------------------------------
;; Font Setup
;;----------------------------------------------------------------------------
(when window-system
  (defun my-find-first-available-font (font-list)
    "Find the first installed font from FONT-LIST."
    (cl-find-if (lambda (font-name) (find-font (font-spec :family font-name)))
                font-list))
  ;; 1. Setup Monospace font
  (let* ((preferred-mono-fonts '("Monaco" "Menlo" "Consolas" "SF Mono" "Courier New"))
         (font (my-find-first-available-font preferred-mono-fonts)))
    (when font
      (set-face-attribute 'default nil :family font :height 180)))
  ;; 2. Setup CJK font
  (let* ((preferred-cjk-fonts '("LXGW WenKai Mono GB Screen" "PingFang SC"))
         (font (my-find-first-available-font preferred-cjk-fonts)))
    (when font
      (set-fontset-font t 'han font))))


;; On macOS, use thinner font smoothing for a sharper look.
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier  'super))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-window)

;;----------------------------------------------------------------------------
;; Window Management
;;----------------------------------------------------------------------------
(use-package windmove
  :ensure nil
  ;; We bind the navigation commands to `Super` + arrow keys.
  :hook (after-init . (lambda ()
                        (windmove-default-keybindings 'super))))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind (("C-c <left>"  . winner-undo)
         ("C-c <right>" . winner-redo)))


;; Auto-focus on New Window After Split
(defun my-window--select-new-window-after-split (orig-fn &rest args)
  "Select the new window immediately after a split.
This function is intended as `:around` advice.  It calls the
original split function ORIG-FN with its ARGS, selects the resulting
new window, and then returns it."
  (let ((new-window (apply orig-fn args)))
    (select-window new-window)
    new-window))

(advice-add 'split-window-right :around #'my-window--select-new-window-after-split)
(advice-add 'split-window-below :around #'my-window--select-new-window-after-split)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
