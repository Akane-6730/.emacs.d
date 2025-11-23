;; init-dap.el --- Initialize DAP configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;;
;; Debug Adapter Protocol (DAP) configurations.
;;

;;; Code:

(use-package dape
  :custom (dape-buffer-window-arrangement 'right)
  :config
  (dape-breakpoint-global-mode)
  ;; codelldb
  (add-to-list 'dape-configs
               '(my-codelldb-cc
                 modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                 ensure dape-ensure-command
                 command-cwd dape-command-cwd
                 command (file-name-concat dape-adapter-dir
                                           "codelldb"
                                           "extension"
                                           "adapter"
                                           "codelldb")
                 command-args ("--port" :autoport)
                 port :autoport
                 :type "lldb"
                 :request "launch"
                 :name "Codelldb: Launch current file"
                 :cwd "."
                 :program (lambda ()
                            (let* ((source-file (buffer-file-name))
                                   (dir (file-name-directory source-file))
                                   (name (file-name-sans-extension
                                          (file-name-nondirectory source-file))))
                              (concat dir name)))

                 :args []
                 :stopOnEntry nil))
  ;; gdb
  (add-to-list 'dape-configs
               '(gdb
                 modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                 ensure dape-ensure-command
                 command "gdb"
                 command-args ("--interpreter=dap")
                 :request "launch"
                 :cwd "."
                 :program (lambda ()
                            (file-name-sans-extension (buffer-file-name)))
                 :args []
                 :stopAtBeginningOfMainSubprogram t)))


;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config (repeat-mode))

(provide 'init-dape)
;;; init-dape.el ends here
