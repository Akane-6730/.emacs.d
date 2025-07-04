;;; init-window.el --- Window management enhancements -*- lexical-binding: t; -*-

;;
;; This file provides configurations to improve window and frame management.
;;

;;----------------------------------------------------------------------------
;; Directional Window Navigation
;;----------------------------------------------------------------------------
(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings 'super))

;;----------------------------------------------------------------------------
;; Window Layout History
;;----------------------------------------------------------------------------
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind (("C-c <left>"  . winner-undo)
         ("C-c <right>" . winner-redo)))


;;----------------------------------------------------------------------------
;; Popup Window Manager with Smart Sizing
;;----------------------------------------------------------------------------

;;
;; Package: popper
;; This is the definitive solution for managing popup windows. We use a more
;; comprehensive list of rules, inspired by Centaur, to ensure that buffers
;; like *compilation* are correctly handled.
;;

(use-package popper
  :ensure t
  :hook (after-init . popper-mode)
  :bind (("C-<tab>" . popper-toggle)
         ("M-`" . popper-cycle))
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
          "\\*quickrun\\*$"
          "^\\*.*eat.*\\*.*$"
          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"))

  (with-no-warnings

    (defun my-popper-fit-window-height (win)
      "Adjust the height of popup window WIN to fit the buffer's content."
      (let ((desired-height (floor (/ (frame-height) 3))))
        (fit-window-to-buffer win desired-height desired-height)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _args)
      "Close popper window via `C-g'."
      (when (and ; (called-interactively-p 'interactive)
             (not (region-active-p))
             popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist))
              (buffer (cdar popper-open-popup-alist)))
          (when (and (window-live-p window)
                     (buffer-live-p buffer)
                     (not (with-current-buffer buffer
                            (derived-mode-p 'eshell-mode
                                            'shell-mode
                                            'term-mode
                                            'vterm-mode))))
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))



(provide 'init-window)
;; ;;; init-window.el ends here
