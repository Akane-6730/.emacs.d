;;; init-verilog.el --- Verilog/SystemVerilog configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for Verilog and SystemVerilog.
;; Includes tree-sitter support and extra extensions.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Verilog Tree-Sitter Mode
;;----------------------------------------------------------------------------

(use-package verilog-ts-mode
  :mode ("\\.s?vh?\\'" . verilog-ts-mode))

;;----------------------------------------------------------------------------
;; Verilog Extensions
;;----------------------------------------------------------------------------

(use-package verilog-ext
  :hook ((verilog-mode . verilog-ext-mode)
         (verilog-ts-mode . verilog-ext-mode))
  :init
  ;; Feature list configuration
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          navigation
          template
          formatter
          compilation
          imenu
          hideshow
          typedefs
          time-stamp
          block-end-comments
          ports))
  :config
  (verilog-ext-mode-setup)
  (add-to-list 'eglot-server-programs
               `((verilog-mode system-verilog-mode verilog-ts-mode)
                 . ("verible-verilog-ls"
                    "--indentation_spaces=4"
                    "--column_limit=120"
                    "--line_break_penalty=2"

                    "--assignment_statement_alignment=align"
                    "--module_net_variable_alignment=align"
                    "--port_declarations_alignment=align"
                    "--formal_parameters_alignment=align"
                    "--case_items_alignment=align"
                    "--distribution_items_alignment=align"))))

(provide 'init-verilog)
;;; init-verilog.el ends here
