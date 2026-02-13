;;; early-init.el --- Early Initialization for a Fast Startup -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file is loaded before Emacs initializes its UI and package system.
;; Its purpose is to configure settings that speed up the startup process.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Performance Tweaks
;;----------------------------------------------------------------------------

;; Defer Garbage Collection (not needed with IGC/MPS)
(unless (featurep 'mps)
  (setq gc-cons-threshold most-positive-fixnum))

;; Disable Automatic Package Initialization
;; We will manually initialize packages with `use-package` to have full control
;; over lazy-loading. Disabling this is critical for a fast, custom setup.
(setq package-enable-at-startup nil)

;; Native Compilation Settings (for Emacs with native-comp)
;; Disable Just-In-Time (JIT) compilation during startup. Packages should
;; already be compiled ahead-of-time.
(setq native-comp-jit-compilation nil)

;; Prefer Newer Files in Non-interactive Sessions
;; This ensures that in scripts or batch mode, you're not using stale
;; byte-compiled code if the source .el file is newer. For interactive
;; sessions, it prefers the faster, byte-compiled .elc files.
(setq load-prefer-newer noninteractive)

;;----------------------------------------------------------------------------
;; UI Customization
;;----------------------------------------------------------------------------

(setq frame-inhibit-implied-resize t)
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable UI Elements Before They Are Drawn
;; Pushing to `default-frame-alist` here is the most efficient way to disable
;; these UI elements, as it happens before they are ever rendered.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(fullscreen . nil))

(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 32))

;; macOS Specific UI Tweak
;; Use a transparent title bar on macOS for a modern look.
;; `(eq system-type 'darwin)` is the modern way to check for macOS.
(when (eq system-type 'darwin)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Prevent flash of unstyled mode line
(setq-default mode-line-format nil)

;;----------------------------------------------------------------------------
;; Core Behavior
;;----------------------------------------------------------------------------

;; Explicitly set UTF-8 to prevent any potential issues with file encoding.
(prefer-coding-system 'utf-8)

;; Configure `use-package`
;; Since Emacs 29, `use-package` is built-in. This enables its Imenu support,
;; which is useful for navigating our package configurations.
(setq use-package-enable-imenu-support t)

;;; early-init.el ends here
