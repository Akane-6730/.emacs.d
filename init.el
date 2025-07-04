;;; init.el --- The main entry point of the configuration -*- lexical-binding: t; -*-

;;
;; This file orchestrates the loading of all other configuration modules.
;; It is designed to be lean and clean. Its responsibilities are:
;; 1. Setting up the load path for our custom Lisp files.
;; 2. Requiring modules in a logical order.
;; 3. Performing post-startup actions, like restoring garbage collection.
;;

;;----------------------------------------------------------------------------
;; Core Setup
;;----------------------------------------------------------------------------

;; Add our `lisp' directory to the `load-path'. This must be done first,
;; so Emacs knows where to find our custom modules.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; A clever optimization for startup performance.
;; `file-name-handler-alist' can be slow as it's consulted for every file
;; load. We disable it during startup and restore it once everything is loaded.
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist old-file-name-handler-alist))
            ;; Use a priority of 101 to ensure it runs after other startup tasks.
            101))

;;----------------------------------------------------------------------------
;; Module Loading
;;----------------------------------------------------------------------------
;;
;; Load our configuration modules. The order is important:
;; - Start with package management.
;; - Load core editor functionalities and UI.
;; - Load user-facing tools and language-specific settings last.
;;


(require 'init-utils)      ; Our custom helper functions (like font-installed-p).


;; Phase 1: Foundation
;;--------------------------
(require 'init-package)    ; Sets up `package.el` and `use-package`.

;; Phase 2: Core Editor Experience
;;--------------------------
(require 'init-base)        ; Basic editor settings (encoding, backups, etc.).
(require 'init-ui)          ; Theme, fonts, modeline, and other visuals.
(require 'init-edit)        ; Editing enhancements (paredit, highlighting, etc.).
(require 'init-completion)  ; The completion framework (e.g., Vertico).
(require 'init-kbd)

;; Phase 3: Major Tools & Workflows
;;--------------------------
(require 'init-vcs)         ; Version control (Magit).
(require 'init-ibuffer)
(require 'init-dashboard)   ; Startup dashboard.
(require 'init-dired)       ; File manager enhancements.
(require 'init-shell)      ; Eshell configuration.
(require 'init-window)      ; Window management commands.

;; Phase 4: Programming Language Support
;;--------------------------
(require 'init-prog)
(require 'init-lsp)         ; Language Server Protocol base config (eglot/lsp-mode).
(require 'init-snippet)
(require 'init-check)
(require 'init-c)

;; (require 'python)      ; Python-specific setup.
;; (require 'elisp)    ; ... add other languages as needed.
;; (require 'go)
;; (require 'rust)

;;----------------------------------------------------------------------------
;; Final Steps after Startup
;;----------------------------------------------------------------------------

;; After all initialization is done, restore the garbage collection threshold
;; to a sane value for normal interactive use. This is the counterpart
;; to the `most-positive-fixnum` setting in `early-init.el`.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 800 1024)))) ; 800 KB is a good default.


;; Let Emacs know this is a custom file that shouldn't be touched by its
;; own customization tools.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; init.el ends here
