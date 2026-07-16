;;; init-ui.el --- User Interface and Appearance -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme, fonts, modeline, icons, layout, and posframe UI.

;;; Code:


;; Font
(when (or (display-graphic-p) (daemonp))
  (use-package typographic
    :vc (:url "https://github.com/Akane-6730/typographic")
    :demand t
    :hook ((org-mode . typographic-serif-mode)
           (markdown-ts-mode . typographic-sans-mode)
           (markdown-ts-view-mode . typographic-sans-mode))
    :custom
    (typographic-serif-latin "Source Serif 4")
    (typographic-serif-cjk "Source Han Serif SC VF")
    (typographic-sans-latin "Noto Sans")
    (typographic-sans-cjk "Source Han Sans SC")
    :config
    (defvar monaco-font (if (eq system-type 'darwin) "Monaco" "Monaco Nerd Font"))
    (set-face-attribute 'default nil :family monaco-font
                        :height (if (eq system-type 'darwin) 180 140))
    (set-face-attribute 'fixed-pitch nil :family monaco-font)
    (set-fontset-font t 'han "LXGW WenKai Mono GB Screen")
    (set-fontset-font t 'kana "LXGW WenKai Mono GB Screen")
    (setq typographic-no-italic-font monaco-font
          typographic-no-bold-font monaco-font)
    (when (eq system-type 'darwin)
      (setq ns-use-thin-smoothing t))))

;;; Theme

;; Built-in theme machinery (`load-theme', `custom-enabled-themes', ...).
(use-package custom
  :ensure nil
  :demand t
  :init
  (setq custom-theme-directory
        (expand-file-name "lisp/themes" user-emacs-directory))
  :bind ("<f7>" . my-toggle-theme)
  :config
  (defvar after-load-theme-hook nil
    "Hook run after a Custom theme has been loaded.")

  (defun my--theme-load-wrapper (original-fn theme &rest args)
    "Around advice for `load-theme': disable old themes first."
    (mapc #'disable-theme custom-enabled-themes)
    (prog1 (apply original-fn theme args)
      (run-hooks 'after-load-theme-hook)))
  (advice-add 'load-theme :around #'my--theme-load-wrapper)

  (defun my-toggle-theme ()
    "Toggle between `my-dark' and `my-light'."
    (interactive)
    (if (custom-theme-enabled-p 'my-dark)
        (load-theme 'my-light t)
      (load-theme 'my-dark t))))

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (doom-themes-visual-bell-config)

  (defun my/apply-theme (frame)
    "Apply theme for FRAME (light GUI / dark TTY)."
    (with-selected-frame frame
      (if (display-graphic-p frame)
          (load-theme 'my-light t)
        (load-theme 'my-dark t))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my/apply-theme)
    (my/apply-theme (selected-frame))))

;; Sync macOS titlebar with theme background mode.
(use-package frame
  :ensure nil
  :if (eq system-type 'darwin)
  :hook ((after-init . my--ns-set-all-titlebars)
         (after-make-frame-functions . my--ns-set-frame-titlebar))
  :config
  (defun my--ns-set-frame-titlebar (frame &rest _)
    "Match FRAME titlebar to theme background mode."
    (when (display-graphic-p frame)
      (let ((mode (frame-parameter frame 'background-mode)))
        (modify-frame-parameters
         frame
         `((ns-transparent-titlebar . t)
           (ns-appearance . ,mode))))))

  (defun my--ns-set-all-titlebars (&rest _)
    "Apply titlebar settings to all frames."
    (mapc #'my--ns-set-frame-titlebar (frame-list)))

  (advice-add 'frame-set-background-mode :after #'my--ns-set-frame-titlebar)
  (when (display-graphic-p)
    (my--ns-set-frame-titlebar (selected-frame))))

;;; Icons

(use-package nerd-icons
  :commands nerd-icons-install-fonts
  :config
  (when (and (display-graphic-p)
             (not (typographic-available-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

;;; Modeline

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

  (doom-modeline-def-segment my-buffer-name
    "Buffer name with mode icon, no state icons."
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-mode-icon)
     (doom-modeline-spc)
     (propertize (buffer-name) 'face 'doom-modeline-buffer-file)))

  (doom-modeline-def-segment my-magit-branch
    "Git branch in Magit buffers."
    (when (and (derived-mode-p 'magit-mode)
               (fboundp 'magit-get-current-branch))
      (when-let ((branch (magit-get-current-branch)))
        (concat
         (doom-modeline-spc)
         (nerd-icons-octicon "nf-oct-git_branch" :face 'doom-modeline-info)
         (doom-modeline-spc)
         (propertize branch 'face 'doom-modeline-info)))))

  (doom-modeline-def-modeline 'my-magit
    '(bar modals matches my-buffer-name remote-host)
    '(my-magit-branch misc-info major-mode process))
  (add-to-list 'doom-modeline-mode-alist '(magit-mode . my-magit)))

(use-package hide-mode-line
  :hook ((eshell-mode shell-mode term-mode ghostel-mode
                      inferior-python-mode
                      flymake-diagnostics-buffer-mode
                      pdf-annot-list-mode) . turn-on-hide-mode-line-mode))

;;; Layout

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
                                   :fringe-width 8
                                   :left-fringe-width 4)))

  (use-package olivetti
    :commands olivetti-mode
    :hook ((org-mode markdown-ts-view-mode Info-mode message-mode) . olivetti-mode)
    :config
    (add-to-list 'window-persistent-parameters '(spilt-window . t))
    (advice-add 'window-toggle-side-windows :before #'olivetti-reset-all-windows)
    (advice-add 'olivetti-reset-window :after
                (lambda (window)
                  (set-window-parameter window 'min-margins (cons 0 0))))))

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;;; Posframe

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

  (defun posframe-poshandler-frame-center (info)
    "Center posframe in the parent frame."
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (- (plist-get info :parent-frame-height)
                (plist-get info :posframe-height))
             2))))

(when (display-graphic-p)
  ;; Multiform: preview-heavy search stays in minibuffer; rest uses posframe.
  (use-package vertico-posframe
    :after vertico
    :hook (vertico-mode . vertico-multiform-mode)
    :init
    (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center
          vertico-posframe-size-function
          (lambda (_buffer)
            (let ((width (round (* (frame-width) (/ 2.0 3))))
                  (height (+ 1 vertico-count)))
              (list :width width :height height
                    :min-width width :min-height height)))
          vertico-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8))
          vertico-multiform-commands
          '((consult-line) (consult-line-multi)
            (consult-ripgrep) (consult-grep) (consult-git-grep)
            (consult-outline)
            (consult-imenu) (consult-imenu-multi)
            (consult-flymake)
            (consult-eglot-symbols)
            ("\\`xref-find-")
            (t posframe))))

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

;;; Misc

(use-package page-break-lines
  :diminish
  :hook (on-first-input . global-page-break-lines-mode)
  :config
  (dolist (mode '(dashboard-mode emacs-news-mode prog-mode))
    (add-to-list 'page-break-lines-modes mode)))

(use-package simple
  :ensure nil
  :config
  (defun my--list-processes-prettify ()
    "Colorize status in the process list."
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
