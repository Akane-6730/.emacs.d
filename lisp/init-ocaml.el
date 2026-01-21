;;; init-ocaml.el --- ocaml -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Ocaml mode
(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :custom
  (tuareg-match-patterns-aligned t)
  (tuareg-indent-align-with-first-arg t)
  :custom-face
  (tuareg-font-lock-governing-face
   ((t (:bold nil)))))

(use-package ocaml-eglot
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
