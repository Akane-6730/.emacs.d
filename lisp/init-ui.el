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

;; Set the global font family and size.
(when window-system
  (defun my-find-first-available-font (font-list)
    "Find the first installed font from FONT-LIST."
    (cl-find-if (lambda (font-name) (find-font (font-spec :family font-name)))
                font-list))
  ;; 1. Setup English / Monospace font with fallback
  (let* ((preferred-mono-fonts '("Monaco" "Menlo" "Consolas" "SF Mono" "Courier New" "Monego"))
         (installed-font (my-find-first-available-font preferred-mono-fonts)))
    ;; Only set the font if one from our preferred list is found.
    ;; Otherwise, do nothing and let Emacs use its system default.
    (when installed-font
      (set-face-attribute 'default nil :family installed-font :height 180)))

  ;; 2. Setup Chinese / Han script font with fallback
  (let* ((preferred-han-fonts '("LXGW WenKai Mono GB Screen" "PingFang SC"))
         (installed-font (my-find-first-available-font preferred-han-fonts)))
    ;; Same logic for Chinese fonts.
    (when installed-font
      (set-fontset-font t 'han (font-spec :family installed-font))))

  ;; 3. Setup Japanese / Kana script font to use the same fallback list
  (let* ((preferred-kana '("LXGW WenKai Mono GB Screen" "PingFang SC"))
         (font (my-find-first-available-font preferred-kana)))
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
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil)
  :config
  ;; Enable flashing mode-line on errors to avoid getting the yellow warning triangle on MacOS.
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :hook (after-init . (lambda ()
                        (load-theme 'my-dark t))))


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


;;----------------------------------------------------------------------------
;; Icons
;;----------------------------------------------------------------------------

;; `nerd-icons` provides a vast collection of icons used by other UI packages
;; like `doom-modeline`.
(use-package nerd-icons
  :demand t)


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


;; `minions` integrates with the modeline to provide a clean menu for minor modes.
;; (use-package minions
;;   :ensure t
;;   :hook (doom-modeline-mode . minions-mode))


;;----------------------------------------------------------------------------
;; Misc
;;----------------------------------------------------------------------------

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
(set-frame-parameter nil 'alpha 95)


;; --- Scrolling Behavior ---
(setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      fast-but-imprecise-scrolling t
      pixel-scroll-precision-interpolate-page t
      ;; make scrolling smoother by avoiding unnecessary fontification.
      redisplay-skip-fontification-on-input t)

(pixel-scroll-precision-mode 1)

(use-package ultra-scroll
  :hook (after-init . ultra-scroll-mode))

(setq pixel-scroll-precision-interpolate-page t)

(global-set-key (kbd "C-v") #'pixel-scroll-interpolate-down)
(global-set-key (kbd "M-v") #'pixel-scroll-interpolate-up)


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

(provide 'init-ui)
;;; init-ui.el ends here
