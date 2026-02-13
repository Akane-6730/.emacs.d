;;; init-perf.el --- Performance optimization using on.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module uses `on.el` to provide transient hooks for lazy loading.
;; It also includes general performance optimizations.
;;
;;; Code:


;; Exposes a number of utility hooks and functions ported from Doom Emacs.
(use-package on
  :demand t)

;; Garbage Collector Magic Hack (not needed with IGC/MPS)
(unless (featurep 'mps)
  (use-package gcmh
    :diminish
    :hook (emacs-startup . gcmh-mode)
    :init
    (setq gcmh-idle-delay 'auto
          gcmh-auto-idle-delay-factor 10
          gcmh-high-cons-threshold #x1000000))) ; 16MB

;; Asynchronous processing
(use-package async
  :functions (async-bytecomp-package-mode dired-async-mode)
  :init
  (async-bytecomp-package-mode 1)
  :config
  (with-eval-after-load 'dired
    (dired-async-mode 1)))

;; Large file optimizations
;; [https://emacs-china.org/t/topic/25811/9]
;;
;; WARNING: Do NOT set `bidi-display-reordering' to nil!
;; In Emacs 31+ with HarfBuzz, disabling bidi causes a segfault when
;; `auto-compose-chars' calls `hb_shape_full' during text shaping.
;; This crashes Emacs when marginalia tries to annotate variables
;; containing certain characters (e.g., `C-h v char-property...' triggers it).
;;
;; Instead, we set bidi direction to left-to-right for performance,
;; which skips the expensive reordering logic while keeping HarfBuzz happy.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC

;; macOS Performance: Use pipes instead of PTYs for external processes.
;; This significantly speeds up process creation and data transfer on macOS.
;; Reference: https://irreal.org/blog/?p=13567
(when (eq system-type 'darwin)
  (setq process-connection-type nil))

;; Hanlde minified code
(use-package so-long
  :ensure nil
  :hook (on-first-file . global-so-long-mode))

(provide 'init-perf)
;;; init-perf.el ends here
