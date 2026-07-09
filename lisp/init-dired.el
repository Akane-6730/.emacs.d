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
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames))
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
  (concat dired-omit-files
          "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))

;; Colorful dired
(use-package diredfl
  :hook dired-mode)

(use-package nerd-icons-dired
  :hook dired-mode)

(use-package speedbar
  :ensure nil
  :bind (("M-0" . my/speedbar-window-toggle)
         :map speedbar-mode-map
         ("q"   . my/speedbar-window-quit)
         ("TAB" . speedbar-toggle-line-expansion)
         ("d"   . speedbar-item-delete)
         ("c"   . speedbar-item-copy)
         ("r"   . speedbar-item-rename)
         ("C-s" . isearch-forward)
         ;; Cancel the global `[remap isearch-forward] -> consult-line' in this
         ;; map so C-s falls through to a real isearch.
         ([remap isearch-forward] . isearch-forward))
  :hook (speedbar-mode . my/speedbar-inherit-dired-faces)
  :config
  (setq speedbar-window-default-width 30)
  (setq speedbar-use-images t)

  ;; `speedbar-window' opens a side window via `display-buffer-in-side-window'
  ;; and then explicitly `select-window's back to the caller, so the generic
  ;; split-window advice in init-window doesn't apply. Wrap it so M-0 focuses
  ;; the speedbar on open, and toggles closed when it's already visible.
  (defun my/speedbar-window-toggle ()
    "Toggle the speedbar side window; select it when opening."
    (interactive)
    (let ((was-live (and (fboundp 'speedbar-window--live-p)
                         (speedbar-window--live-p))))
      (speedbar-window)
      (when (and (not was-live)
                 (boundp 'speedbar--window)
                 (window-live-p speedbar--window))
        (select-window speedbar--window))))

  (defun my/speedbar-window-quit ()
    "Close the speedbar side window."
    (interactive)
    (when (and (fboundp 'speedbar-window--live-p)
               (speedbar-window--live-p))
      (speedbar-window)))

  ;; Buffer-local face remap onto the *built-in* dired faces. Diredfl is
  ;; meant to inherit/extend dired, not the other way around — so we anchor
  ;; on dired here and let the user's theme decide how dired-* look.
  (defun my/speedbar-inherit-dired-faces ()
    (face-remap-add-relative 'speedbar-directory-face 'dired-directory)
    (face-remap-add-relative 'speedbar-selected-face  'dired-marked))

  ;; Stock `speedbar-insert-image-button-maybe' delegates to ezimage and only
  ;; understands image-symbol values. Override to also accept strings (used
  ;; directly as `display') and function symbols (called with the file path).
  (defun speedbar-insert-image-button-maybe (start length &optional label)
    (when speedbar-use-images
      (let* ((text (buffer-substring start (+ length start)))
             (item (assoc text speedbar-expand-image-button-alist)))
        (setq label (cond (label (expand-file-name label))
                          (t (ignore-errors (speedbar-line-file)))))
        (when item
          (let* ((replacement (cdr item))
                 (display
                  (cond
                   ((stringp replacement) replacement)
                   ((and label (symbolp replacement) (fboundp replacement))
                    (funcall replacement label))
                   ((and (symbolp replacement) (boundp replacement)
                         (consp (symbol-value replacement)))
                    (symbol-value replacement)))))
            (when display
              ;; Clear the underlying `speedbar-button-face' on the button text
              ;; so the icon string's own face (from nerd-icons) isn't merged
              ;; with it — otherwise weight/height/etc. bleed through and the
              ;; icon color looks off compared with nerd-icons-dired.
              (add-text-properties start (+ start (length text))
                                   (list 'display display
                                         'face nil
                                         'rear-nonsticky (list 'display 'face)))))))))

  ;; `speedbar-make-button' is called for the [+]/<+>/etc. expander before
  ;; the file-name text has been inserted on the line, so `speedbar-line-file'
  ;; cannot resolve the file at that moment. The fork patches this by
  ;; threading a LABEL through; replicate that here.
  (defun speedbar-make-button (start end face mouse function &optional token label)
    "Override stock to forward an optional LABEL to image-button insertion."
    (unless (eq face t)
      (put-text-property start end 'face face))
    (add-text-properties
     start end `(mouse-face ,mouse invisible nil
                            speedbar-text ,(buffer-substring-no-properties start end)))
    (when speedbar-use-tool-tips-flag
      (put-text-property start end 'help-echo #'dframe-help-echo))
    (when function (put-text-property start end 'speedbar-function function))
    (when token (put-text-property start end 'speedbar-token token))
    (when (<= (- end start) 3)
      (speedbar-insert-image-button-maybe start (- end start) label)))

  ;; Inject `tag-button' as the label when `speedbar-make-tag-line' calls
  ;; `speedbar-make-button'.
  (defun my/speedbar-make-tag-line-pass-label (orig &rest args)
    (let* ((tag-button (nth 4 args))
           (orig-mkbtn (symbol-function 'speedbar-make-button)))
      (cl-letf (((symbol-function 'speedbar-make-button)
                 (lambda (s e f m fn &optional tok _label)
                   (funcall orig-mkbtn s e f m fn tok tag-button))))
        (apply orig args))))
  (advice-add 'speedbar-make-tag-line :around
              #'my/speedbar-make-tag-line-pass-label)

  ;; Directory icons: nerd-icons returns a generic folder glyph with a
  ;; hard-coded foreground. Override it to `dired-directory' so the icon
  ;; tracks the same color as the directory text — one theme knob controls
  ;; both. File icons are left untouched so nerd-icons' per-extension
  ;; colors keep working.
  (defun my/speedbar-icon-for-dir (file)
    (let ((s (copy-sequence (nerd-icons-icon-for-dir file))))
      (add-face-text-property 0 (length s) '(:inherit dired-directory) nil s)
      s))

  (setq speedbar-expand-image-button-alist
        `(("<+>" . my/speedbar-icon-for-dir)
          ("<->" . my/speedbar-icon-for-dir)
          ("< >" . my/speedbar-icon-for-dir)
          ("[+]" . nerd-icons-icon-for-file)
          ("[-]" . nerd-icons-icon-for-file)
          ("[?]" . nerd-icons-icon-for-file)
          ("[ ]" . nerd-icons-icon-for-file)
          ("{+}" . ,(nerd-icons-faicon "nf-fa-tags"))
          ("{-}" . ,(nerd-icons-faicon "nf-fa-tags"))
          ("<M>" . ,(nerd-icons-codicon "nf-cod-mail"))
          ("<d>" . ,(nerd-icons-codicon "nf-cod-book"))
          ("<i>" . ,(nerd-icons-codicon "nf-cod-info"))
          (" =>" . ,(nerd-icons-faicon "nf-fa-tag"))
          (" +>" . ,(nerd-icons-faicon "nf-fa-tag"))
          (" ->" . ,(nerd-icons-faicon "nf-fa-tag"))
          (">"   . ,(nerd-icons-faicon "nf-fa-tag"))
          ("@"   . ,(nerd-icons-faicon "nf-fa-tag"))
          ("  @" . ,(nerd-icons-faicon "nf-fa-tag"))
          ("*"   . ,(nerd-icons-devicon "nf-dev-git_branch"))
          ("#"   . ,(nerd-icons-codicon "nf-cod-file_binary"))
          ("!"   . ,(nerd-icons-mdicon "nf-md-update"))
          ("//"  . ,(format "%s " (nerd-icons-mdicon "nf-md-label_outline")))
          ("%"   . ,(nerd-icons-codicon "nf-cod-lock")))))

(provide 'init-dired)
;;; init-dired.el ends here
