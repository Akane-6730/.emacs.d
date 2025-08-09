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
  :ensure t
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil)
  :config
  ;; Enable flashing mode-line on errors to avoid getting the yellow warning triangle on MacOS.
  (doom-themes-visual-bell-config)
  :hook (after-init . (lambda ()
                        (if window-system
                            (load-theme 'doom-solarized-light t)
                          (load-theme 'doom-monokai-pro t))
                        ;;; TODO
                        (defun my-enable-fancy-fonts-in-org-mode ()
                          "Enable bold and italic fonts for Org mode."
                          (setq doom-themes-enable-bold t)
                          (setq doom-themes-enable-italic t)
                          (load-theme (car custom-enabled-themes) t))

                        (defun my-disable-fancy-fonts-in-prog-mode ()
                          "Disable bold and italic fonts for programming modes."
                          (setq doom-themes-enable-bold nil)
                          (setq doom-themes-enable-italic nil)
                          (load-theme (car custom-enabled-themes) t))

                        (add-hook 'org-mode-hook #'my-enable-fancy-fonts-in-org-mode)
                        (add-hook 'prog-mode-hook #'my-disable-fancy-fonts-in-prog-mode))))


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

;; `doom-modeline` is a high-performance, good-looking modeline.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))


;; `minions` integrates with the modeline to provide a clean menu for minor modes.
;; (use-package minions
;;   :ensure t
;;   :hook (doom-modeline-mode . minions-mode))


;;----------------------------------------------------------------------------
;; Misc
;;----------------------------------------------------------------------------

;; Display line numbers in prog-mode
(use-package display-line-numbers
  :ensure nil ; Built-in package.
  :hook (prog-mode . display-line-numbers-mode)
  :config
  ;; This makes the line number column adjust its width automatically.
  (setq display-line-numbers-width-start t))

(setq-default cursor-in-non-selected-windows nil)

;; Maximize window to full Screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'window-setup-hook #'window-divider-mode)

;; ;; TODO
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq
 ;; --- Scrolling Behavior ---
 ;; Configure the mouse wheel to scroll one line at a time.
 mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
 mouse-wheel-progressive-speed nil
 scroll-conservatively 101
 fast-but-imprecise-scrolling t
 ;; make scrolling smoother by avoiding unnecessary fontification.
 redisplay-skip-fontification-on-input t


 ;; --- GUI Behavior & Clean Startup ---
 ;; Prevent Emacs from using native OS dialogs for files and prompts.
 use-file-dialog nil
 use-dialog-box nil
 ;; Disable the splash screen and customize startup messages for a minimal launch.
 inhibit-startup-screen t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-scratch-message nil

 ;; --- Window Divider Appearance ---
 ;; Enable and configure visual dividers between split windows.
 window-divider-default-places t
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
