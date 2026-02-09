;;; init-fonts.el --- Font configuration and mixed-pitch mode -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides comprehensive font configuration for Emacs,
;; with special attention to CJK (Chinese, Japanese, Korean) support.
;;
;; Features:
;; - Automatic font detection with caching for fast startup
;; - Separate configurations for default, fixed-pitch, and variable-pitch faces
;; - Mixed-pitch minor mode for prose-oriented buffers (Org, Markdown)
;; - Lazy initialization of variable-pitch fonts for faster startup
;;
;; Usage:
;; The module auto-configures fonts on startup.  For prose modes,
;; enable `my-mixed-font-mode' to use variable-pitch for body text
;; while keeping code blocks in fixed-pitch.

;;; Code:

(require 'cl-lib)

;;;; Custom Group

(defgroup my-fonts nil
  "Font configuration for Emacs."
  :group 'faces
  :prefix "my-fonts-")

;;;; Font Preferences
;;
;; Define preferred fonts in order of preference.  The first available
;; font in each list will be used.

(defconst my-fonts-mono-list
  (if (eq system-type 'darwin)
      '("Monaco"
        "Maple Mono Normal NF CN"
        "Maple Mono NF CN"
        "Menlo"
        "Cascadia Code"
        "Consolas"
        "SF Mono"
        "Courier New")
    '("Maple Mono Normal NF CN"
      "Maple Mono NF CN"
      "Monaco"
      "Menlo"
      "Cascadia Code"
      "Consolas"
      "SF Mono"
      "Courier New"))
  "Preferred monospace fonts in order of priority.")

(defconst my-fonts-han-list
  '("LXGW WenKai Mono GB Screen"
    "PingFang SC"
    "Source Han Sans SC")
  "Preferred CJK (Han script) fonts in order of priority.")

(defconst my-fonts-variable-latin-list
  '("Source Serif 4"
    "ETBembo"
    "ET Book"
    "Crimson"
    "Times New Roman")
  "Preferred variable-pitch Latin fonts for prose, in order of priority.")

(defconst my-fonts-variable-han-list
  '("Source Han Serif SC VF"
    "Source Han Serif SC"
    "Songti SC"
    "Noto Serif CJK SC"
    "SimSun")
  "Preferred variable-pitch CJK fonts for prose, in order of priority.")

(defconst my-fonts-italic-han-list
  '("LXGW WenKai Mono GB Screen"
    "LXGW WenKai Mono Screen"
    "LXGW WenKai Mono"
    "LXGW WenKai"
    "Kaiti"
    "STKaiti")
  "Preferred fonts for CJK italic text, in order of priority.")

(defconst my-fonts-symbol-list
  '("Apple Symbols"
    "Symbols Nerd Font Mono"
    "Symbola"
    "Symbol"
    "Segoe UI Symbol")
  "Preferred symbol fonts for special characters.")

(defcustom my-fonts-default-height
  (if (eq system-type 'darwin) 180 140)
  "Default font height in 1/10 pt units."
  :type 'integer
  :group 'my-fonts)

;;;; Internal Utilities

(defun my-fonts--available-p (font-name)
  "Return non-nil if FONT-NAME is available on this system."
  (find-font (font-spec :family font-name)))

(defun my-fonts--first-available (font-list)
  "Return the first available font from FONT-LIST, or nil if none found."
  (cl-find-if #'my-fonts--available-p font-list))

(defun my-fonts--ensure-fontset (name)
  "Ensure a fontset named NAME exists and return its name.
Creates the fontset if it doesn't exist."
  (let* ((fontset-name (concat "fontset-" name))
         (xlfd (format "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-%s" name)))
    (unless (member fontset-name (fontset-list))
      (create-fontset-from-fontset-spec xlfd))
    fontset-name))

;;;; Core Font Setup Functions

(defun my-fonts-setup-default (frame)
  "Setup default face fonts for FRAME.
This configures:
- Default monospace font for the `default' face
- CJK (Han) font for Chinese characters
- Kana font for Japanese
- Symbol fonts for special Unicode ranges"
  (with-selected-frame frame
    ;; 1. Monospace (default face)
    (when-let* ((mono-font (my-fonts--first-available my-fonts-mono-list)))
      (set-face-attribute 'default frame
                          :family mono-font
                          :height my-fonts-default-height))

    ;; 2. CJK Han script
    (when-let* ((han-font (my-fonts--first-available my-fonts-han-list)))
      (set-fontset-font t 'han (font-spec :family han-font)))

    ;; 3. Japanese Kana (reuse Han font list)
    (when-let* ((kana-font (my-fonts--first-available my-fonts-han-list)))
      (set-fontset-font t 'kana (font-spec :family kana-font)))

    ;; 4. Symbol fonts for special Unicode ranges
    ;; These fix inconsistent sizing in doom-modeline window numbers, etc.
    (when-let* ((symbol-font (my-fonts--first-available my-fonts-symbol-list)))
      ;; Enclosed Alphanumerics: ①②③ (U+2460-U+24FF)
      (set-fontset-font t '(#x2460 . #x24FF) (font-spec :family symbol-font))
      ;; Dingbats: ❶❷❸ (U+2700-U+27BF)
      (set-fontset-font t '(#x2700 . #x27BF) (font-spec :family symbol-font)))))

(defvar my-fonts--fixed-pitch-initialized nil
  "Non-nil if fixed-pitch fonts have been set up.")

(defun my-fonts-setup-fixed-pitch ()
  "Setup the `fixed-pitch' face with appropriate fontset.
This is called lazily via `on-init-ui' hook."
  (interactive)
  (unless my-fonts--fixed-pitch-initialized
    (let ((fontset (my-fonts--ensure-fontset "fixed"))
          (mono-font (my-fonts--first-available my-fonts-mono-list))
          (italic-han (my-fonts--first-available my-fonts-italic-han-list)))
      ;; Han script for fixed-pitch context
      (set-fontset-font fontset 'han (font-spec :family "Source Han Sans SC"))

      (when mono-font
        (set-face-attribute 'fixed-pitch nil
                            :family mono-font
                            :weight 'regular
                            :fontset fontset))

      ;; Italic fontset for fixed-pitch CJK
      (when italic-han
        (let ((italic-fontset (my-fonts--ensure-fontset "fixeditalic")))
          (set-fontset-font italic-fontset 'han
                            (font-spec :family italic-han :slant 'italic))
          (when mono-font
            (set-fontset-font italic-fontset 'latin
                              (font-spec :family mono-font :slant 'italic))))))
    (setq my-fonts--fixed-pitch-initialized t)))

(defvar my-fonts--variable-pitch-initialized nil
  "Non-nil if variable-pitch fonts have been set up.
Used to defer setup until first use of `my-mixed-font-mode'.")

(defun my-fonts-setup-variable-pitch ()
  "Setup the `variable-pitch' face with serif fonts for prose.
This function is called lazily on first activation of `my-mixed-font-mode'."
  (interactive)
  (let* ((fontset (my-fonts--ensure-fontset "variable"))
         (latin-font (my-fonts--first-available my-fonts-variable-latin-list))
         (han-font (my-fonts--first-available my-fonts-variable-han-list))
         (italic-han (my-fonts--first-available my-fonts-italic-han-list)))
    ;; Latin serif
    (when latin-font
      (set-fontset-font fontset 'latin (font-spec :family latin-font)))
    ;; CJK serif
    (when han-font
      (set-fontset-font fontset 'han (font-spec :family han-font)))

    (when latin-font
      (set-face-attribute 'variable-pitch nil
                          :family latin-font
                          :weight 'regular
                          :fontset fontset))

    ;; Italic fontset for variable-pitch CJK
    (when (or italic-han latin-font)
      (let ((italic-fontset (my-fonts--ensure-fontset "variableitalic")))
        (when italic-han
          (set-fontset-font italic-fontset 'han
                            (font-spec :family italic-han :slant 'italic)))
        (when latin-font
          (set-fontset-font italic-fontset 'latin
                            (font-spec :family latin-font :slant 'italic)))
        (set-face-attribute 'italic nil :fontset italic-fontset)))))

;;;; Mixed-Pitch Minor Mode
;;
;; A minor mode that uses variable-pitch for prose while keeping
;; code-related faces in fixed-pitch.

(defcustom my-mixed-font-fixed-faces
  '(;; Org mode
    org-block org-code org-verbatim org-table org-table-row
    org-formula org-link org-special-keyword org-meta-line
    org-checkbox org-priority org-todo org-done org-ellipsis
    org-tag org-date org-document-info-keyword org-hide
    ;; Markdown
    markdown-code-face markdown-pre-face markdown-table-face
    ;; General
    line-number font-lock-comment-face corfu-default)
  "Faces that should remain fixed-pitch in `my-mixed-font-mode'.
These are typically code blocks, tables, and other monospace elements."
  :type '(repeat face)
  :group 'my-fonts)

(defvar-local my-mixed-font--cookies nil
  "Face remap cookies for the current buffer.
Used to restore faces when disabling `my-mixed-font-mode'.")

;;;###autoload
(define-minor-mode my-mixed-font-mode
  "Toggle mixed-pitch fonts in the current buffer.

When enabled:
- Body text uses `variable-pitch' (serif) font
- Code, tables, and other technical elements use `fixed-pitch'
- Leading whitespace uses fixed-pitch to preserve indentation

This mode is intended for prose-oriented buffers like Org and Markdown."
  :lighter " Mixed"
  :group 'my-fonts
  (if my-mixed-font-mode
      (progn
        ;; Ensure fixed-pitch is set up first
        (my-fonts-setup-fixed-pitch)

        ;; Lazy initialization of variable-pitch fonts
        (unless my-fonts--variable-pitch-initialized
          (my-fonts-setup-variable-pitch)
          (setq my-fonts--variable-pitch-initialized t))

        ;; Enable variable-pitch as the base
        (variable-pitch-mode 1)

        ;; Remap specified faces to fixed-pitch
        (setq my-mixed-font--cookies nil)
        (dolist (face my-mixed-font-fixed-faces)
          (push (face-remap-add-relative face 'fixed-pitch)
                my-mixed-font--cookies))

        ;; Keep leading whitespace fixed-pitch for proper indentation
        (font-lock-add-keywords nil '(("^[[:space:]]+" 0 'fixed-pitch)) 'append)
        (font-lock-flush))

    ;; Disable: restore original state
    (variable-pitch-mode -1)
    (mapc #'face-remap-remove-relative my-mixed-font--cookies)
    (setq my-mixed-font--cookies nil)
    (font-lock-remove-keywords nil '(("^[[:space:]]+" 0 'fixed-pitch)))
    (font-lock-flush)))

;;;; Italic Faces (Non-macOS)

(defun my-fonts-setup-italic-faces ()
  "Enable italic style for various programming faces.
This is only applied on non-macOS systems where italic rendering
may need explicit configuration."
  (unless (eq system-type 'darwin)
    (dolist (face '(font-lock-preprocessor-face
                    font-lock-type-face
                    font-lock-comment-face
                    font-lock-keyword-face
                    font-lock-builtin-face))
      (set-face-attribute face nil :slant 'italic))))

;;;; Initialization

(defun my-fonts-init ()
  "Initialize font configuration.
Called automatically for GUI frames."
  (when (display-graphic-p)
    ;; Setup default fonts for current or new frames (must be early)
    (if (daemonp)
        (add-hook 'after-make-frame-functions #'my-fonts-setup-default)
      (my-fonts-setup-default (selected-frame)))

    ;; Enable mixed-font-mode in prose buffers
    ;; fixed-pitch and variable-pitch are set up lazily when mode activates
    (add-hook 'org-mode-hook #'my-mixed-font-mode)
    (add-hook 'markdown-mode-hook #'my-mixed-font-mode)))

;; macOS-specific: thinner font smoothing
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t))

;; Auto-initialize for GUI
(when (or window-system (daemonp))
  (my-fonts-init))

(provide 'init-fonts)
;;; init-fonts.el ends here
