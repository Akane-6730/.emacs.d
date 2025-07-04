;;; init-edit.el --- Core editing enhancements -*- lexical-binding: t; -*-

;;
;; This file configures packages that enhance the core text editing experience.
;; The focus is on efficient navigation, manipulation of parentheses and other
;; paired delimiters, and smart editing commands.
;;

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
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-timer) ; A more accessible binding
         ("M-g g" . avy-goto-line)))


;;----------------------------------------------------------------------------
;; Expanding Selections
;;----------------------------------------------------------------------------

;; `expand-region` provides a command to intelligently and incrementally
;; expand the selected region by semantic units (e.g., from word to string,
;; to expression, to function body).
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region))
  :config
  ;; Add tree-sitter support ONLY if tree-sitter is actually available.
  ;; `(fboundp 'treesit-available-p)` checks if the core tree-sitter function
  ;; exists. This prevents errors if tree-sitter isn't fully set up yet.
  (when (fboundp 'treesit-available-p)
    (defun er/mark-tree-sitter-node ()
      "Expand region to the parent tree-sitter node."
      (when-let ((node (treesit-enclosing-node-at (point))))
        (goto-char (treesit-node-start node))
        (set-mark (treesit-node-end node))))
    ;; Add our function to the list of expansion methods.
    (add-to-list 'er/try-expand-list 'er/mark-tree-sitter-node)))


;;----------------------------------------------------------------------------
;; Multiple Cursors
;;----------------------------------------------------------------------------

;; `multiple-cursors` allows you to edit multiple places in the buffer at once.
;; It's a powerful tool for simultaneous, repetitive edits.
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


;;----------------------------------------------------------------------------
;; General Editing Enhancements
;;----------------------------------------------------------------------------

;; When you have a region selected and you start typing, this mode will
;; delete the selection first, which is the standard behavior in most editors.
(delete-selection-mode 1)

;; Automatically reload files when they are changed on disk by another program.
(global-auto-revert-mode 1)

;; Smarter Home/End keys. `mwim` stands for "Move Where I Mean". Pressing `C-a`
;; once moves to the beginning of indentation, pressing again moves to the
;; absolute beginning of the line.
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

;; `hungry-delete` changes backspace to also delete whitespace. For example,
;; if you have "foo    bar" and the cursor is after "bar", one backspace will
;; delete "bar", and the next will delete all the spaces back to "foo".
(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode))


;;----------------------------------------------------------------------------
;; Undo Tree
;;----------------------------------------------------------------------------

;; `vundo` visualizes the undo history as a tree, allowing you to easily
;; navigate and revert to any previous state, never losing work even if you
;; undone and made new changes.
(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo)))


(provide 'init-edit)
;;; init-edit.el ends here
