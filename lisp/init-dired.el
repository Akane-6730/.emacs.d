;;; init-dired.el --- Dired (the file manager) configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures Dired, the built-in Emacs file manager.
;; We enhance it with icons, font-locking (colors), and modern behaviors.
;;
;;; Code:

;; Core Dired Behavior
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-Alh --group-directories-first")
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;; On macOS, dired needs `gls' (GNU ls) for compatibility; without it, dired may break.
  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-Alh"))))

  (setq-local mouse-1-click-follows-link nil)
  (auto-revert-mode))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\.[^.]\\|^go\\|^eln-cache$\\|^auto-save-list$\\|^transient$\\|^tree-sitter$\\|^dirvish$\\|^elpa$\\|^history\\|^places\\|^recentf\\|^screenshots\\|^eshell\\|^snippets\\|^rime\\|^package\\.json$\\|^package-lock\\.json$\\|^node_modules$\\|^_minted-\\|^agent")))

(use-package dirvish
  :init (with-eval-after-load 'dired (dirvish-override-dired-mode))
  :config
  (setq dirvish-side-width 25)
  (setq dirvish-header-line-height 21)
  (setq dirvish-path-separators (list "   " "   " "  "))
  (setq dirvish-subtree-state-style 'nerd)
  (setq dirvish-attributes '(subtree-state nerd-icons git-msg file-time file-size))
  (setq dirvish-side-attributes '(nerd-icons subtree-state))
  (setq dirvish-side-mode-line-format '(:left (sort omit symlink)))
  (setq dirvish-quick-access-entries
   '(("e" "~/.emacs.d/" "Emacs user directory")
     ("c" "~/Documents/code/" "Code")
     ("d" "~/Documents/" "Documents")
     ("h" "~/" "Home")
     ("o" "~/Documents/org/" "Org")))

  (setq dirvish-large-directory-threshold 20000)
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)

  (defun my/dirvish-mouse-find-file (event)
    "Middle click behavior: select window/point and find file."
    (interactive "e")
    (let* ((win (posn-window (event-start event)))
           (pos (posn-point (event-start event))))
      (select-window win)
      (goto-char pos)
      (dired-find-file)))

  (defun my/dirvish--switch-to-path (path)
    "Switch to PATH in the current Dirvish window, handling dedicated status."
    (let ((win (selected-window)))
      (set-window-dedicated-p win nil)
      (set-window-buffer win (dired-noselect path))
      (set-window-dedicated-p win t)))

  (defun my/dirvish-project-root ()
    "Switch to a project root in the current Dirvish window."
    (interactive)
    (let ((project-dir (project-prompt-project-dir)))
      (my/dirvish--switch-to-path project-dir)))

  (defun my/dirvish-next-project ()
    "Switch to the next project."
    (interactive)
    (let* ((projects (project-known-project-roots))
           (norm-projects (mapcar (lambda (p) (file-name-as-directory (expand-file-name p))) projects))
           (current (file-name-as-directory (expand-file-name default-directory)))
           (current-index (cl-position current norm-projects :test #'equal))
           (next-index (if current-index
                           (mod (1+ current-index) (length projects))
                         0))
           (next (nth next-index projects)))
      (my/dirvish--switch-to-path next)))

  (defun my/dirvish-prev-project ()
    "Switch to the previous project."
    (interactive)
    (let* ((projects (project-known-project-roots))
           (norm-projects (mapcar (lambda (p) (file-name-as-directory (expand-file-name p))) projects))
           (current (file-name-as-directory (expand-file-name default-directory)))
           (current-index (cl-position current norm-projects :test #'equal))
           (prev-index (if current-index
                           (mod (1- current-index) (length projects))
                         0))
           (prev (nth prev-index projects)))
      (my/dirvish--switch-to-path prev)))

  (defun dired-goto-dir-or-file (path)
    "Open PATH in Dired.
   If PATH is a directory, open it in the current window.
   If PATH is a file, open its parent directory, move point to the file, and open it."
    (interactive "fGoto (dir or file): ")
    (let* ((expanded (expand-file-name path))
           (dir (if (file-directory-p expanded)
                    expanded
                  (file-name-directory expanded))))
      (my/dirvish--switch-to-path dir)
      (when (file-exists-p expanded)
        (with-selected-window (selected-window)
          (dired-goto-file expanded)
          (unless (file-directory-p expanded)
            (dired-find-file))))))
  :bind
  (("M-o"  . dirvish-side)
   ("<f8>" . dirvish-side)
   ("M-0"  . dirvish-side)
   :map dirvish-mode-map
   ("<mouse-1>" . 'dirvish-subtree-toggle-or-open)
   ("<mouse-2>" . 'my/dirvish-mouse-find-file)
   ("<mouse-3>" . 'dired-up-directory)
   ("j" . dired-goto-dir-or-file)
   ("b" . dired-up-directory)
   ("C-j" . dired-up-directory)
   ("?" . dirvish-dispatch)         ; [?] a helpful cheatsheet
   ("TAB" . dirvish-subtree-toggle)
   ("<backtab>" . dirvish-subtree-remove)
   ("o" . dirvish-quick-access)     ; [o]pen `dirvish-quick-access-entries'
   ("a" . dirvish-setup-menu)       ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("s" . dirvish-quicksort)        ; [s]ort file list
   ("P" . my/dirvish-project-root)  ; [P]roject switching
   ("M-n" . my/dirvish-next-project)
   ("M-p" . my/dirvish-prev-project)
   ("r" . dirvish-history-jump)     ; [r]ecent visited
   ("l" . dirvish-ls-switches-menu) ; [l]s command flags
   ("v" . dirvish-vc-menu)          ; [v]ersion control commands
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)
   ("*" . dirvish-mark-menu)
   ("y" . dirvish-yank-menu)
   ("N" . dirvish-narrow)
   ("^" . dirvish-history-last)
   ("q"   . dirvish-quit)))

(provide 'init-dired)
;;; init-dired.el ends here
