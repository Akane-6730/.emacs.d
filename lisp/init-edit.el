;;; init-edit.el --- Core editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures packages that enhance the core text editing experience.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Parentheses and Delimiter Management
;;----------------------------------------------------------------------------

;; Automatically pair parentheses
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :config
  ;; Disable auto-pairing if the cursor is right before a non-whitespace character
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


;;----------------------------------------------------------------------------
;; Expanding Selections
;;----------------------------------------------------------------------------

;; Icremental selection expansion
(use-package expand-region
  :bind (("C-=" . er/expand-region))
  :config
  (defun treesit-mark-bigger-node ()
    "Use tree-sitter to mark regions."
    (let* ((root (treesit-buffer-root-node))
           (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let* ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))
  (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node))


;;----------------------------------------------------------------------------
;; Smart, Case-Sensitive Query-Replace at Point
;;----------------------------------------------------------------------------

(defun smart-query-replace-at-point ()
  "Perform a case-sensitive query-replace on the symbol at point."
  (interactive)
  (let ((search-term (thing-at-point 'symbol t)))
    (if (and search-term (not (string-empty-p search-term)))
        (let ((case-fold-search nil))
          (let ((replace-term
                 (read-from-minibuffer
                  (format "Query replace '%s' with: " search-term)
                  nil nil nil nil search-term)))
            (goto-char (point-min))
            (perform-replace search-term replace-term t nil nil)))
      (message "No symbol at point."))))

(global-set-key (kbd "C-;") #'smart-query-replace-at-point)

;;----------------------------------------------------------------------------
;; General Editing Enhancements
;;----------------------------------------------------------------------------

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Auto-reload changed files
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t) ;; Also auto-revert Dired and other buffers
  (auto-revert-verbose nil))

;; Move to beginning/end of code first, then line
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;; Delete all whitespace up to the next non-whitespace character
(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Drag lines/regions around
(use-package drag-stuff
  :diminish
  :autoload drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;;----------------------------------------------------------------------------
;; Undo Tree
;;----------------------------------------------------------------------------

;; Visualize undo history
(use-package vundo
  :bind (("C-x u" . vundo)))

;;----------------------------------------------------------------------------
;; Clipboard Integration
;;----------------------------------------------------------------------------

;; System clipboard integration
(use-package xclip
  :hook (after-init . xclip-mode))

;;----------------------------------------------------------------------------
;; In-Buffer Navigation with Avy
;;----------------------------------------------------------------------------

;; Jump to visible characters
;; (use-package avy
;;   :bind (("C-'" . avy-goto-char-timer)
;;          ("M-g g" . avy-goto-line)))

;;----------------------------------------------------------------------------
;; Multiple Cursors
;;----------------------------------------------------------------------------

;; Edit multiple places at once
;; (use-package multiple-cursors
;;   :ensure t
;;   :bind (("C-S-c C-S-c" . mc/edit-lines)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)
;;          ("C-c C-<" . mc/mark-all-like-this)))

(provide 'init-edit)
;;; init-edit.el ends here
