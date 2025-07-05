;;; init-window.el --- Advanced window management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures advanced window and frame management behaviors to
;; improve workflow efficiency.
;;
;; Key Features:
;; - Directional window navigation using `windmove`.
;; - Window layout history navigation with `winner-mode`.
;; - Auto-focusing the newly created window after a split.
;; - Advanced `popper` configuration for managing popup windows, including
;;   allowing them to become the only window on the frame.

;;; Code:

;;----------------------------------------------------------------------------
;; Directional Window Navigation
;;----------------------------------------------------------------------------
;;
;; `windmove` is a built-in package that allows navigating between windows
;; directionally (up, down, left, right).
;;
(use-package windmove
  :ensure nil
  :config
  ;; We bind the navigation commands to `Super` + arrow keys.
  (windmove-default-keybindings 'super))


;;----------------------------------------------------------------------------
;; Window Layout History
;;----------------------------------------------------------------------------
;;
;; `winner-mode` is a global minor mode that records window configuration
;; changes, allowing you to "undo" and "redo" window layouts.
;;
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind (("C-c <left>"  . winner-undo)
         ("C-c <right>" . winner-redo)))


;;----------------------------------------------------------------------------
;; Auto-focus on New Window After Split
;;----------------------------------------------------------------------------
;;
;; This feature automatically selects the new window immediately after a split,
;; saving the extra step of manually switching focus.
;;
(defun my-window--select-new-window-after-split (orig-fn &rest args)
  "Select the new window immediately after a split.
This function is intended as `:around` advice.  It calls the
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
;;
;; `popper` is a package for managing "popup" buffers like compilation output,
;; help buffers, etc., often displayed in a side window.
;;
(use-package popper
  :ensure t
  :bind (("C-<tab>" . popper-toggle)
         ("M-`" . popper-cycle))
  :hook (after-init . popper-mode)
  :config
  ;; Define which buffers should be treated as popups.
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          help-mode
          compilation-mode
          "\\*quickrun\\*$"
          ;; Regex for various terminal-like buffers
          "^\\*.*\\(eat\\|eshell\\|shell\\|terminal\\|vterm\\).*-*\\*.*$"))

  ;; Your custom function to fit window height.
  (defun my-popper-fit-window-height (win)
    "Adjust the height of popup window WIN to fit the buffer's content."
    (let ((desired-height (floor (/ (frame-height) 3))))
      (fit-window-to-buffer win desired-height desired-height)))
  (setq popper-window-height #'my-popper-fit-window-height)

  ;; Your custom hack to close popper with C-g.
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

(provide 'init-window)
;;; init-window.el ends here
