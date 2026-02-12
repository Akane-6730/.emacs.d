;;; init-ui.el --- User Interface and Appearance -*- lexical-binding: t; -*-

;;; Commentary:

;; This module configures the visual aspects of Emacs, including:
;; - Theme loading and switching
;; - Modeline (doom-modeline)
;; - Icons (nerd-icons)
;; - Layout (spacious-padding, olivetti)
;; - Posframe-based UI elements
;;
;; Font configuration is handled separately by `init-fonts'.

;;; Code:

(require 'init-fonts)

;;; ----------------------------------------------------------------------------
;;; Theme
;;; ----------------------------------------------------------------------------

;; `doom-themes' is a package containing a collection of beautifully
;; crafted themes.
(use-package doom-themes
  :demand t
  :config
  ;; Bold/italic always enabled at theme level.
  ;; For Monaco/Monego, italic is selectively removed from monospace faces
  ;; AFTER theme load by `my-fonts-setup-italic-faces' in init-fonts.el.
  ;; This preserves italic on variable-pitch/serif faces (org-quote, etc.).
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq custom-theme-directory (expand-file-name "lisp/themes" user-emacs-directory))

  ;; Enable flashing mode-line on errors (avoids yellow warning triangle on macOS)
  (doom-themes-visual-bell-config)

  (defun my/apply-theme (frame)
    "Apply appropriate theme and fonts for FRAME."
    (with-selected-frame frame
      (if (display-graphic-p frame)
          (progn
            (my-fonts-setup-default frame)
            (load-theme 'my-light t))
        (load-theme 'my-dark t))
      (my-fonts-setup-italic-faces)))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my/apply-theme)
    (my/apply-theme (selected-frame))))

;;; Robust Theme Loading
;; Ensure old themes are disabled before loading new ones.

(defun my--theme-load-wrapper (original-fn theme &rest args)
  "Advice wrapper for `load-theme' that disables old themes first.
Calls ORIGINAL-FN with THEME and ARGS after cleanup."
  (mapc #'disable-theme custom-enabled-themes)
  (apply original-fn theme args))

(advice-add 'load-theme :around #'my--theme-load-wrapper)

(defun my-toggle-theme ()
  "Toggle between `my-dark' and `my-light' themes."
  (interactive)
  (if (custom-theme-enabled-p 'my-dark)
      (load-theme 'my-light t)
    (load-theme 'my-dark t))
  (my-fonts-setup-italic-faces))

(global-set-key (kbd "<f7>") #'my-toggle-theme)

;;; macOS Titlebar
;; Automatically set titlebar color to match theme's background mode.

(when (and (eq system-type 'darwin) (display-graphic-p))
  (defun my--ns-set-frame-titlebar (frame &rest _)
    "Set transparent titlebar for FRAME to match theme's background mode."
    (when (display-graphic-p frame)
      (let ((mode (frame-parameter frame 'background-mode)))
        (modify-frame-parameters
         frame
         `((ns-transparent-titlebar . t)
           (ns-appearance . ,mode))))))

  (defun my--ns-set-all-titlebars (&rest _)
    "Apply titlebar settings to all existing frames."
    (mapc #'my--ns-set-frame-titlebar (frame-list)))

  (add-hook 'after-init-hook #'my--ns-set-all-titlebars)
  (add-hook 'after-make-frame-functions #'my--ns-set-frame-titlebar)
  (advice-add 'frame-set-background-mode :after #'my--ns-set-frame-titlebar)
  (my--ns-set-frame-titlebar (selected-frame)))

;;; ----------------------------------------------------------------------------
;;; Icons
;;; ----------------------------------------------------------------------------

(use-package nerd-icons
  :commands nerd-icons-install-fonts
  :config
  (when (and (display-graphic-p)
             (not (my-fonts--available-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

;;; ----------------------------------------------------------------------------
;;; Modeline
;;; ----------------------------------------------------------------------------

(use-package time
  :defer 0.5
  :config
  (setq display-time-default-load-average nil
        display-time-format "%H:%M")
  (display-time-mode 1))

(use-package battery
  :defer 0.5
  :config
  (display-battery-mode 1))

(use-package doom-modeline
  :hook after-init
  :config
  (setq doom-modeline-height 18
        doom-modeline-battery t
        doom-modeline-time-icon t
        doom-modeline-time-clock-size 1.0
        doom-modeline-enable-buffer-position nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-modification-icon nil)
  (unless (display-graphic-p)
    (setq doom-modeline-unicode-number nil))

  ;; Custom segment: buffer name with major-mode icon (no lock)
  (doom-modeline-def-segment my-buffer-name
    "Display buffer name with mode icon, without state icons."
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-mode-icon)
     (doom-modeline-spc)
     (propertize (buffer-name) 'face 'doom-modeline-buffer-file)))

  ;; Custom segment: show git branch in Magit buffers
  (doom-modeline-def-segment my-magit-branch
    "Display current git branch in Magit buffers."
    (when (and (derived-mode-p 'magit-mode)
               (fboundp 'magit-get-current-branch))
      (when-let ((branch (magit-get-current-branch)))
        (concat
         (doom-modeline-spc)
         (nerd-icons-octicon "nf-oct-git_branch" :face 'doom-modeline-info)
         (doom-modeline-spc)
         (propertize branch 'face 'doom-modeline-info)))))

  ;; Magit modeline: no lock icon, branch on right side
  (doom-modeline-def-modeline 'my-magit
    '(bar modals matches my-buffer-name remote-host)
    '(my-magit-branch misc-info major-mode process))
  (add-to-list 'doom-modeline-mode-alist '(magit-mode . my-magit)))

(use-package hide-mode-line
  :hook ((eat-mode
          eshell-mode shell-mode
          term-mode vterm-mode
          pdf-annot-list-mode) . turn-on-hide-mode-line-mode))

;;; ----------------------------------------------------------------------------
;;; Layout
;;; ----------------------------------------------------------------------------

(when (display-graphic-p)
  (use-package spacious-padding
    :hook (on-init-ui . spacious-padding-mode)
    :config
    (setq spacious-padding-widths
          '(:internal-border-width 10
                                   :header-line-width 0
                                   :mode-line-width 0
                                   :custom-button-width 0
                                   :tab-width 2
                                   :right-divider-width 15
                                   :fringe-width 8)))

  ;; Centered text layout for prose modes
  (use-package olivetti
    :commands olivetti-mode
    :hook ((org-mode markdown-mode Info-mode message-mode) . olivetti-mode)
    :config
    (add-to-list 'window-persistent-parameters '(spilt-window . t))
    (advice-add 'window-toggle-side-windows :before #'olivetti-reset-all-windows)
    (advice-add 'olivetti-reset-window :after
                (lambda (window)
                  (set-window-parameter window 'min-margins (cons 0 0))))))

;;; Window Divider Appearance

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;;; ----------------------------------------------------------------------------
;;; Posframe-based UI Elements
;;; ----------------------------------------------------------------------------

(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    '((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (defun my--posframe-prettify-frame (&rest _)
    "Remove fringe background from posframe."
    (set-face-background 'fringe nil posframe--frame))
  (advice-add #'posframe--create-posframe :after #'my--posframe-prettify-frame)

  (defun posframe-poshandler-frame-center-near-bottom (info)
    "Position posframe at center, slightly below middle of frame.
INFO is the posframe position info plist."
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (+ (plist-get info :parent-frame-height)
                (* 2 (plist-get info :font-height)))
             2))))

(when (display-graphic-p)
  (use-package vertico-posframe
    :hook (vertico-mode . vertico-posframe-mode)
    :init
    (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center-near-bottom
          vertico-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8))))

  (use-package transient-posframe
    :diminish
    :hook (on-first-input . transient-posframe-mode)
    :init
    (setq transient-mode-line-format nil
          transient-posframe-border-width posframe-border-width
          transient-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8))))

  (use-package which-key-posframe
    :diminish
    :hook which-key-mode
    :config
    (setq which-key-posframe-border-width posframe-border-width
          which-key-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))))

;;; ----------------------------------------------------------------------------
;;; Misc UI Tweaks
;;; ----------------------------------------------------------------------------

;; Display ^L page breaks as horizontal lines
(use-package page-break-lines
  :diminish
  :hook (on-first-input . global-page-break-lines-mode)
  :config
  (dolist (mode '(dashboard-mode emacs-news-mode))
    (add-to-list 'page-break-lines-modes mode)))

;; Prettify process list
(use-package simple
  :ensure nil
  :config
  (defun my--list-processes-prettify ()
    "Prettify the process list with colored status."
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
  (advice-add #'list-processes--refresh :after #'my--list-processes-prettify))

(provide 'init-ui)
;;; init-ui.el ends here
