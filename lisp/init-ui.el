;;; init-ui.el --- User Interface and Appearance -*- lexical-binding: t; -*-


;;; Commentary:
;;
;; This file configures the visual aspects of Emacs, including the theme,
;; fonts, modeline, line numbers, and icons.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------------

(when window-system
  (defun font-available-p (font-name)
    "Check if font with FONT-NAME is available."
    (find-font (font-spec :family font-name)))

  (defun my--ensure-fontset (name)
    "Ensure fontset NAME exists and return its fontset name string."
    (let* ((fontset (concat "fontset-" name))
           (xlfd (concat "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-" name)))
      (unless (member fontset (fontset-list))
        (create-fontset-from-fontset-spec xlfd))
      fontset))

  (defun my--first-available-font (fonts)
    "Return the first available font in FONTS."
    (cl-find-if #'font-available-p fonts))

  (defconst my--preferred-mono-fonts
    (if (eq system-type 'darwin)
        '("Monaco" "Maple Mono NF CN" "Menlo" "Cascadia code" "Consolas" "SF Mono" "Courier New" "Monego")
      '("Maple Mono NF CN" "Monaco" "Menlo" "Cascadia code" "Consolas" "SF Mono" "Courier New" "Monego"))
    "Preferred monospace fonts in order.")

  (defun my--preferred-mono-font ()
    "Return the first available preferred monospace font."
    (my--first-available-font my--preferred-mono-fonts))

  ;; Setup variable-pitch and fixed-pitch fonts
  (defun my/setup-variable-pitch-fonts ()
    "Setup variable-pitch fonts for generic reading modes (Org, Markdown, Info)."
    (interactive)
    (let ((fontset (my--ensure-fontset "variable")))
      (set-fontset-font fontset 'latin (font-spec :family "Source Serif 4"))
      (set-fontset-font fontset 'han (font-spec :family "Source Han Serif SC VF"))

      (set-face-attribute 'variable-pitch nil
                          :family "Source Serif 4"
                          :weight 'regular
                          :fontset fontset)

      ;; Setup Chinese italics to use LXGW WenKai Mono
      ;; Create an italic fontset for Chinese characters
      (let ((italic-fontset (my--ensure-fontset "variableitalic")))
        ;; Set Chinese italic font to LXGW WenKai Mono
        (set-fontset-font italic-fontset 'han
                          (font-spec :family "LXGW WenKai Mono GB Screen" :slant 'italic))
        ;; Keep English italic font as is
        (set-fontset-font italic-fontset 'latin
                          (font-spec :family "Source Serif 4" :slant 'italic))
        ;; Apply italic fontset to italic faces
        (set-face-attribute 'italic nil :fontset italic-fontset))))

  (defun my/setup-fixed-pitch-fonts ()
    "Setup fixed-pitch face (Mono)."
    (interactive)
    (let ((fontset (my--ensure-fontset "fixed"))
          (mono-font (my--preferred-mono-font)))
      ;; Set Chinese font to Source Han Sans as requested
      (set-fontset-font fontset 'han (font-spec :family "Source Han Sans SC"))

      (when mono-font
        (set-face-attribute 'fixed-pitch nil
                            :family mono-font
                            :weight 'regular
                            :fontset fontset))

      ;; Setup Chinese italics in fixed-pitch context
      (let ((italic-fontset (my--ensure-fontset "fixeditalic")))
        ;; Set Chinese italic font to LXGW WenKai Mono
        (set-fontset-font italic-fontset 'han
                          (font-spec :family "LXGW WenKai Mono GB Screen" :slant 'italic))
        ;; English italic uses same mono font
        (when mono-font
          (set-fontset-font italic-fontset 'latin
                            (font-spec :family mono-font :slant 'italic))))))

  ;; Run basic setup once on init
  (add-hook 'window-setup-hook #'my/setup-fixed-pitch-fonts)
  ;; Defer variable pitch setup until needed
  (add-hook 'org-mode-hook #'my/setup-variable-pitch-fonts)
  (add-hook 'markdown-mode-hook #'my/setup-variable-pitch-fonts)

  (defgroup my-mixed-font nil
    "Configuration for custom mixed-font mode."
    :group 'faces)

  (defcustom my-mixed-font-fixed-faces
    '(org-block org-code org-verbatim org-table org-table-row
                org-formula org-link org-special-keyword org-meta-line
                org-checkbox org-priority org-todo org-done org-ellipsis
                org-tag org-date line-number org-document-info-keyword
                org-hide font-lock-comment-face corfu-default
                markdown-code-face markdown-pre-face markdown-table-face)
    "List of faces that should use fixed-pitch font in `my-mixed-font-mode'."
    :type '(repeat face)
    :group 'my-mixed-font)

  (defvar-local my-mixed-font--cookies nil
    "List of cookies for remapped faces.")

  (define-minor-mode my-mixed-font-mode
    "Toggle mixed font mode (prose in variable-pitch, code in fixed-pitch)."
    :init-value nil
    :lighter " Mixed"
    (if my-mixed-font-mode
        (progn
          ;; Enable variable-pitch-mode
          (variable-pitch-mode 1)
          ;; Remap fixed faces
          (setq my-mixed-font--cookies nil)
          (dolist (face my-mixed-font-fixed-faces)
            (push (face-remap-add-relative face 'fixed-pitch)
                  my-mixed-font--cookies)))
      ;; Disable
      (variable-pitch-mode -1)
      (mapc #'face-remap-remove-relative my-mixed-font--cookies)
      (setq my-mixed-font--cookies nil)))

  (add-hook 'org-mode-hook #'my-mixed-font-mode)
  (add-hook 'markdown-mode-hook #'my-mixed-font-mode)

  ;; Set the global font family and size.

  ;; 1. Setup English / Monospace font with fallback
    (let* ((preferred-mono-fonts my--preferred-mono-fonts)
      (installed-font (my--preferred-mono-font))
      (mono-height (if (eq system-type 'darwin) 180 140)))
    ;; Only set the font if one from our preferred list is found.
    ;; Otherwise, do nothing and let Emacs use its system default.
    (when installed-font
      (set-face-attribute 'default nil :family installed-font :height mono-height)))

  ;; 2. Setup Chinese / Han script font with fallback
    (let* ((preferred-han-fonts '("LXGW WenKai Mono GB Screen" "PingFang SC" "Source Han Sans SC"))
      (installed-font (my--first-available-font preferred-han-fonts)))
    ;; Same logic for Chinese fonts.
    (when installed-font
      (set-fontset-font t 'han (font-spec :family installed-font))))

  ;; 3. Setup Japanese / Kana script font to use the same fallback list
    (let* ((preferred-kana '("LXGW WenKai Mono GB Screen" "PingFang SC"))
      (font (my--first-available-font preferred-kana)))
    (when font
      (set-fontset-font t 'kana (font-spec :family font)))))


;; On macOS, use thinner font smoothing for a sharper look.
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t))


;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------

;; `doom-themes` is a package containing a collection of beautifully crafted themes.
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors to avoid getting the yellow warning triangle on MacOS.
  (doom-themes-visual-bell-config)
  ;; (doom-themes-org-config)
  ;; Load the theme AFTER ensuring the package is loaded
  (load-theme 'my-light t)
  ;; Set specific faces to use italics AFTER loading the theme
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))


;;;----------------------------------------------------------------------------
;;; Robust Theme Loading
;;;----------------------------------------------------------------------------
;; This ensures that whenever a new theme is loaded, all previously enabled
;; themes are completely disabled first. This prevents visual artifacts and
;; "style residue" from the old theme.

(defun my-theme-load-wrapper (original-fn theme &rest args)
  "A wrapper for `load-theme' that disables old themes first."
  ;; 1. Disable all currently enabled themes.
  (mapc #'disable-theme custom-enabled-themes)
  ;; 2. Call the original `load-theme' function with its arguments.
  (apply original-fn theme args))

;; Advise the core `load-theme' function to use our wrapper.
(advice-add 'load-theme :around #'my-theme-load-wrapper)


(defun my-toggle-theme ()
  "Toggle between `my-dark` and `my-light` themes."
  (interactive)

  (if (custom-theme-enabled-p 'my-dark)
      (load-theme 'my-light t)
    (load-theme 'my-dark t)))

(global-set-key (kbd "<f7>") #'my-toggle-theme)


;;----------------------------------------------------------------------------
;; Icons
;;----------------------------------------------------------------------------

(use-package nerd-icons
  :commands nerd-icons-install-fonts
  :functions font-available-p
  :config
  ;; Install nerd fonts automatically only in GUI
  ;; For macOS, may install via "brew install font-symbols-only-nerd-font"
  (when (and (display-graphic-p)
             (not (font-available-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))


;;----------------------------------------------------------------------------
;; Modeline
;;----------------------------------------------------------------------------

(use-package time
  :init (setq display-time-default-load-average nil
              display-time-format "%H:%M"))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (display-time-mode t)
  (display-battery-mode t)
  :config
  (setq doom-modeline-height 18)
  (setq doom-modeline-battery t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time-clock-size 1.0))

;;----------------------------------------------------------------------------
;; Layout
;;----------------------------------------------------------------------------

(use-package spacious-padding
  :hook (after-init . spacious-padding-mode)
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 10   ; Gap between the frame border and the windows
           :header-line-width 0        ; Vertical padding for the header-line
           :mode-line-width 0          ; Vertical padding for the mode-line
           :custom-button-width 0      ; Padding for buttons (e.g. in Custom UI)
           :tab-width 2                ; Horizontal padding for tab-bar tabs
           :right-divider-width 15     ; Width of the divider between side-by-side windows
           :scroll-bar-width 0         ; Width reserved for scrollbars
           :fringe-width 8)))          ; Width of the lateral fringes (git gutters, etc.)

;;----------------------------------------------------------------------------
;; Misc
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Visual & UX
;;----------------------------------------------------------------------------

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode))
  :config
  (setq column-number-mode t
        line-number-mode t
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

;; Display line numbers in prog-mode
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  ;; This makes the line number column adjust its width automatically.
  (setq display-line-numbers-width-start t))

(setq-default cursor-in-non-selected-windows nil)

(add-hook 'window-setup-hook #'window-divider-mode)

;; Automatically sets the titlebar color on macOS to match the theme
(when (and (eq system-type 'darwin) (display-graphic-p))
  (defun my/ns-set-frame-titlebar (frame &rest _)
    "Set transparent titlebar for FRAME to match theme's background mode."
    (when (display-graphic-p frame)
      (let ((mode (frame-parameter frame 'background-mode)))
        (modify-frame-parameters
         frame
         `((ns-transparent-titlebar . t)
           (ns-appearance . ,mode))))))

  (defun my/ns-set-all-titlebars (&rest _)
    "Apply titlebar settings to all existing frames."
    (mapc #'my/ns-set-frame-titlebar (frame-list)))

  (add-hook 'after-init-hook #'my/ns-set-all-titlebars)
  (add-hook 'after-make-frame-functions #'my/ns-set-frame-titlebar)
  (advice-add 'frame-set-background-mode :after #'my/ns-set-frame-titlebar)

  (my/ns-set-frame-titlebar (selected-frame)))

;; set transparency
;; (set-frame-parameter nil 'alpha 99)


;; --- Scrolling Behavior ---

(setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

(use-package pixel-scroll
  :ensure nil
  :bind (("C-v" . pixel-scroll-interpolate-down)
         ("M-v" . pixel-scroll-interpolate-up))
  :custom
  (pixel-scroll-precision-interpolate-page t)
  :init
  (pixel-scroll-precision-mode 1))

(use-package ultra-scroll
  :bind (:map pixel-scroll-precision-mode-map
              ("<wheel-up>" . ultra-scroll)
              ("<wheel-down>" . ultra-scroll))
  :config
  (ultra-scroll-mode 1))


;; --- GUI Behavior & Clean Startup ---
;; Prevent Emacs from using native OS dialogs for files and prompts.
(setq use-file-dialog nil
      use-dialog-box nil
      ;; Disable the splash screen and customize startup messages for a minimal launch.
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;; --- Window Divider Appearance ---
;; Enable and configure visual dividers between split windows.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1 ; Solve the problem of the modeline being too wide
      window-divider-default-right-width 1)

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode)
  :config (dolist (mode '(dashboard-mode emacs-news-mode))
            (add-to-list 'page-break-lines-modes mode)))

(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

(use-package vertico-posframe
  :functions posframe-poshandler-frame-center-near-bottom
  :hook (vertico-mode . vertico-posframe-mode)
  :init (setq vertico-posframe-poshandler
              #'posframe-poshandler-frame-center-near-bottom
              vertico-posframe-parameters
              '((left-fringe  . 8)
                (right-fringe . 8))))

(provide 'init-ui)
;;; init-ui.el ends here
