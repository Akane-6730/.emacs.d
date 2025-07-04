;;; early-init.el --- Early Initialization for a Fast Startup -*- lexical-binding: t; -*-

;;
;; This file is loaded before Emacs initializes its UI and package system.
;; Its purpose is to configure settings that speed up the startup process.
;;

;;----------------------------------------------------------------------------
;; Performance Tweaks
;;----------------------------------------------------------------------------

;; 1. Defer Garbage Collection
;; We set a very high garbage collection threshold during startup.
;; This prevents GC from running, which is a major source of slowness.
;; The threshold will be restored to a normal value after startup is complete.
(setq gc-cons-threshold most-positive-fixnum)

;; 2. Disable Automatic Package Initialization
;; We will manually initialize packages with `use-package` to have full control
;; over lazy-loading. Disabling this is critical for a fast, custom setup.
(setq package-enable-at-startup nil)

;; 3. Native Compilation Settings (for Emacs with native-comp)
;; Disable Just-In-Time (JIT) compilation during startup. Packages should
;; already be compiled ahead-of-time (AOT).
;; The `native-comp-deferred-compilation` variable is obsolete in Emacs 29.
(setq native-comp-jit-compilation nil)

;; 4. Prefer Newer Files in Non-interactive Sessions
;; This ensures that in scripts or batch mode, you're not using stale
;; byte-compiled code if the source .el file is newer. For interactive
;; sessions, it prefers the faster, byte-compiled .elc files.
(setq load-prefer-newer noninteractive)

;;----------------------------------------------------------------------------
;; UI Customization
;;----------------------------------------------------------------------------

;; 1. Inhibit Frame Resizing
;; Prevents the frame from resizing during initialization, which can cause
;; flickering and minor delays.
(setq frame-inhibit-implied-resize t)

;; 2. Disable UI Elements Before They Are Drawn
;; Pushing to `default-frame-alist` here is the most efficient way to disable
;; these UI elements, as it happens before they are ever rendered.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 3. macOS Specific UI Tweak
;; Use a transparent title bar on macOS for a modern look.
;; `(eq system-type 'darwin)` is the modern way to check for macOS.
(when (eq system-type 'darwin)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; 4. Disable the Default Mode-line
;; The default mode-line can be surprisingly expensive to compute. We disable
;; it here and will replace it with an efficient, custom one (like doom-modeline)
;; later in the configuration.
(setq-default mode-line-format nil)


;;----------------------------------------------------------------------------
;; Core Behavior
;;----------------------------------------------------------------------------

;; 1. Set Default Encoding
;; Explicitly set UTF-8 to prevent any potential issues with file encoding.
(prefer-coding-system 'utf-8)

;; 2. Configure `use-package`
;; Since Emacs 29, `use-package` is built-in. This enables its Imenu support,
;; which is useful for navigating our package configurations.
(setq use-package-enable-imenu-support t)


;;; early-init.el ends here
