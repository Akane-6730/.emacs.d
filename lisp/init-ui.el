;;; init-ui.el --- User Interface and Appearance -*- lexical-binding: t; -*-

;;
;; This file configures the visual aspects of Emacs, including the theme,
;; fonts, modeline, line numbers, and icons. The goal is a clean, modern,
;; and functional appearance.
;;

;;----------------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------------

;; Set the global font family and size.

;; Helper function to find the first available font from a list.
(defun find-first-available-font (font-list)
  "Return the first installed font from FONT-LIST."
  ;; This now works because `font-installed-p` is pre-loaded from `utils.el`.
  (cl-find-if #'font-installed-p font-list))

;; Set the default font for English and code.
(let ((preferred-english-fonts '("Monaco" "Monego" "Consolas" "SF Mono")))
  (when-let ((font (find-first-available-font preferred-english-fonts)))
    (set-face-attribute 'default nil :family font :height 140)))

;; Set the font for Chinese characters (han script).
(let ((preferred-chinese-fonts '("霞鹜文楷" "LXGW WenKai" "PingFang SC")))
  (when-let ((font (find-first-available-font preferred-chinese-fonts)))
    (set-fontset-font t 'han (font-spec :family font))))
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
  ;; Set your preferred theme.
  (load-theme 'doom-monokai-octagon t)

  ;; Make the visual bell flash the modeline instead of beeping.
  (doom-themes-visual-bell-config)

  :hook (after-init . (lambda ()
                        (load-theme 'doom-monokai-octagon t)
                        ;; For macOS, we also sync the title bar inside the hook.
                        (when (eq system-type 'darwin)
                          (doom-themes-sync-macos-title-bar-theme)))))


;;----------------------------------------------------------------------------
;; Icons
;;----------------------------------------------------------------------------

;; `nerd-icons` provides a vast collection of icons used by other UI packages
;; like `doom-modeline`.
(use-package nerd-icons
  :ensure t
  :demand t)


;;----------------------------------------------------------------------------
;; Modeline
;;----------------------------------------------------------------------------

;; `doom-modeline` is a high-performance, good-looking modeline.
(setq doom-modeline-time-icon t)
(setq find-file-visit-truename t)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))



;; `minions` integrates with the modeline to provide a clean menu for minor modes.
(use-package minions
  :ensure t
  :hook (doom-modeline-mode . minions-mode))


;;----------------------------------------------------------------------------
;; Core UI Elements
;;----------------------------------------------------------------------------

;; Display line numbers in programming and configuration modes.
(use-package display-line-numbers
  :ensure nil ; Built-in package.
  :hook (prog-mode . display-line-numbers-mode)
  :config
  ;; This makes the line number column adjust its width automatically.
  (setq display-line-numbers-width-start t))

;; Highlight the current line to improve focus.
(global-hl-line-mode 1)

;; Make the cursor a steady bar instead of a blinking block.
(setq cursor-type 'bar)
(blink-cursor-mode 0)

;; Add a visual divider between vertical window splits.
(add-hook 'window-setup-hook #'window-divider-mode)
(setq window-divider-default-places t
      window-divider-default-right-width 1)


;;----------------------------------------------------------------------------
;; Scrolling
;;----------------------------------------------------------------------------

;; Configure mouse wheel to scroll smoothly, one line at a time.
(setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101) ; Avoids recentering the screen on scroll.


(provide 'init-ui)
;;; init-ui.el ends here
