;;; init-window.el --- Advanced window management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Enhanced window and popup management.
;;
;; Features:
;; - Directional window navigation (windmove)
;; - Window layout undo/redo (winner-mode)
;; - Auto-focus new window after split
;; - Popup management (popper)
;; - Enhanced window switching (ace-window)
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Directional Window Navigation (Super + Arrow)
;;----------------------------------------------------------------------------
(use-package windmove
  :ensure nil
  :hook (after-init . (lambda ()
                        (windmove-default-keybindings 'super))))


;;----------------------------------------------------------------------------
;; Window Layout History (C-c <left>/<right>)
;;----------------------------------------------------------------------------

;; `winner-mode` is a global minor mode that records window configuration
;; changes, allowing you to "undo" and "redo" window layouts.
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :bind (("C-c <left>"  . winner-undo)
         ("C-c <right>" . winner-redo))
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;;----------------------------------------------------------------------------
;; Auto-focus on New Window After Split
;;----------------------------------------------------------------------------
(defun my-window--select-new-window-after-split (orig-fn &rest args)
  "Select the new window immediately after a split.
original split function ORIG-FN with its ARGS, selects the resulting
new window, and then returns it."
  (let ((new-window (apply orig-fn args)))
    (select-window new-window)
    new-window))

;; We "advise" the core window splitting functions. This is the modern and
;; robust Emacs way to add behavior to existing functions.
(advice-add 'split-window-right :around #'my-window--select-new-window-after-split)
(advice-add 'split-window-below :around #'my-window--select-new-window-after-split)

;;----------------------------------------------------------------------------
;; Popup Window Manager (Popper)
;;----------------------------------------------------------------------------

;; `popper` is a package for managing "popup" buffers like compilation output,
;; help buffers, etc., often displayed in a side window.
(use-package popper
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle))
  :hook (after-init . popper-mode)
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          help-mode
          compilation-mode
          grep-mode occur-mode rg-mode
          "\\*quickrun\\*$"
          ;; Regex for various terminal-like buffers
          "^\\*.*\\(eat\\|eshell\\|shell\\|terminal\\|vterm\\).*-*\\*.*$"
          ;; REPLs
          "\\*sml\\*$"
          "\\*racket\\*$"
          "\\*python3\\*$"
          "\\*acp error\\*$"
          racket-repl-mode
          inferior-python-mode
          inferior-scheme-mode
          comint-mode
          inf-ruby-mode
          process-menu-mode))

  ;; Set popup window height to 1/3 of frame height.
  (defun my-popper-fit-window-height (win)
    "Adjust the height of popup window WIN to fit the buffer's content."
    (let ((desired-height (floor (/ (frame-height) 3))))
      (fit-window-to-buffer win desired-height desired-height)))
  (setq popper-window-height #'my-popper-fit-window-height)

  ;; Close popper window with `C-g`, unless region is active or in certain modes.
  (defun popper-close-window-hack (&rest _args)
    "Close popper window via `C-g'."
    (when (and popper-open-popup-alist (not (region-active-p)))
      (let ((window (caar popper-open-popup-alist))
            (buffer (cdar popper-open-popup-alist)))
        (when (and (window-live-p window)
                   (buffer-live-p buffer)
                   (not (with-current-buffer buffer
                          (derived-mode-p 'eshell-mode 'shell-mode 'term-mode 'vterm-mode))))
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'popper-close-window-hack))

;; ----------------------------------------------------------------------------
;; Ace Window for Enhanced Window Switching (M-1 ~ M-9)
;; ----------------------------------------------------------------------------
(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :bind ([remap other-window] . ace-window)
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))
      (user-error "`toggle-window-split' only supports two windows")))

  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
        (dolist (win (aw-window-list))
          (when (and (window-live-p win)
                     (eq number
                         (string-to-number
                          (window-parameter win 'ace-window-path))))
            (setq found t)
            (aw-switch-to-window win)))
        (unless found
          (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
              (lambda ()
                (interactive)
                (aw--select-window (1+ n))))))

(provide 'init-window)
;;; init-window.el ends here
