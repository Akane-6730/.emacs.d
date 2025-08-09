;;; init-edit.el --- Core editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; This file configures packages that enhance the core text editing experience.
;; The focus is on efficient navigation, manipulation of parentheses and other
;; paired delimiters, and smart editing commands.

;;; Code:

;;----------------------------------------------------------------------------
;; Parentheses and Delimiter Management
;;----------------------------------------------------------------------------

;; `electric-pair-mode` is a built-in minor mode that automatically inserts
;; a closing delimiter when you type an opening one (e.g., `(` -> `()`).
;; It's simple, fast, and essential for sanity.
(use-package elec-pair
  :ensure nil ; Built-in package
  :hook (after-init . electric-pair-mode)
  :config
  ;; This predicate makes pairing less aggressive, for example, it won't
  ;; auto-pair a quote if you're on top of another non-whitespace character.
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


;;----------------------------------------------------------------------------
;; In-Buffer Navigation with Avy
;;----------------------------------------------------------------------------

;; `avy` allows you to jump directly to any visible character, word, or line
;; on the screen, which is much faster than repeated key presses.
;; (use-package avy
;;   :bind (("C-'" . avy-goto-char-timer) ; A more accessible binding
;;          ("M-g g" . avy-goto-line)))


;;----------------------------------------------------------------------------
;; Expanding Selections
;;----------------------------------------------------------------------------

;; `expand-region` provides a command to intelligently and incrementally
;; expand the selected region by semantic units (e.g., from word to string,
;; to expression, to function body).
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
;; Multiple Cursors
;;----------------------------------------------------------------------------

;; `multiple-cursors` allows you to edit multiple places in the buffer at once.
;; It's a powerful tool for simultaneous, repetitive edits.
;; (use-package multiple-cursors
;;   :ensure t
;;   :bind (("C-S-c C-S-c" . mc/edit-lines)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)
;;          ("C-c C-<" . mc/mark-all-like-this)))

;;----------------------------------------------------------------------------
;; Smart, Case-Sensitive Query-Replace at Point
;;----------------------------------------------------------------------------
(defun smart-query-replace-at-point ()
  "Perform a case-sensitive query-replace on the symbol at point.
This command operates on the entire accessible buffer, automatically
respecting any narrowing (`C-x n n`)."
  (interactive)
  (let ((search-term (thing-at-point 'symbol t)))
    (if (and search-term (not (string-empty-p search-term)))
        ;; By wrapping the logic in `let`, we temporarily force the
        ;; replacement to be case-sensitive for this command only.
        (let ((case-fold-search nil))
          (let ((replace-term
                 (read-from-minibuffer
                  (format "Query replace '%s' with: " search-term)
                  nil nil nil nil search-term)))
            (goto-char (point-min))
            (perform-replace search-term replace-term
                             t   ; Query: ask for confirmation.
                             nil ; Regexp: treat as plain text.
                             nil ; Delimited: not word-boundary-only.
                             )))
      (message "No symbol at point."))))

;; Bind the definitive, simple command to C-;
(global-set-key (kbd "C-;") #'smart-query-replace-at-point)

;;----------------------------------------------------------------------------
;; General Editing Enhancements
;;----------------------------------------------------------------------------

;; When you have a region selected and you start typing, this mode will
;; delete the selection first, which is the standard behavior in most editors.
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files when they are changed on disk by another program.
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Smarter Home/End keys. `mwim` stands for "Move Where I Mean". Pressing `C-a`
;; once moves to the beginning of indentation, pressing again moves to the
;; absolute beginning of the line.
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;; `hungry-delete` changes backspace to also delete whitespace. For example,
;; if you have "foo    bar" and the cursor is after "bar", one backspace will
;; delete "bar", and the next will delete all the spaces back to "foo".
(use-package hungry-delete
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))


;;----------------------------------------------------------------------------
;; Undo Tree
;;----------------------------------------------------------------------------

;; `vundo` visualizes the undo history as a tree, allowing you to easily
;; navigate and revert to any previous state, never losing work even if you
;; undone and made new changes.
(use-package vundo
  :bind (("C-x u" . vundo)))


(provide 'init-edit)
;;; init-edit.el ends here
