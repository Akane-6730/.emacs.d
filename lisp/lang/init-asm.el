;; init-asm.el --- Assembly language support -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Assembly language support for Emacs
;;

;;; Code:

(use-package riscv-mode
  :load-path "~/.emacs.d/site-lisp/riscv-mode"
  :ensure nil
  :mode ("\\.[sS]\\'" . riscv-mode)
  :hook (riscv-mode . riscv--setup-capf)
  :config
  (defun riscv--setup-capf ()
    "Enable snippet completions in `riscv-mode' only."
    (require 'yasnippet-capf)
    (add-to-list 'completion-at-point-functions #'yasnippet-capf)
    (setq-local corfu-auto-prefix 1)))

(provide 'init-asm)
;;; init-asm.el ends here
