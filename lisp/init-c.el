;;; init-c.el --- Configurations for C/C++ development -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures a complete development environment for C and C++.
;; It enforces a consistent indentation style, provides a smart compilation
;; command, and sets up language-specific keybindings.
;;
;;; Code:

;;----------------------------------------------------------------------------
;; Customizable Compilation Variables
;;----------------------------------------------------------------------------
(defvar my-c-common-flags "-Wall -Wextra" "Common compiler flags for C/C++.")
(defvar my-c-std-version "" "The C standard to use (e.g., `c99`, `c11`).")
(defvar my-cpp-std-version "" "The C++ standard to use (e.g., `c++14`, `c++17`).")
(defvar my-c-debug-flags "-g" "Flags for including debug information.")



;;----------------------------------------------------------------------------
;; The Unified C/C++ Tree-sitter Setup Function
;;----------------------------------------------------------------------------

(defun my-c-ts-mode-setup ()
  "Apply all custom settings for C/C++ Tree-sitter modes."
  ;; 1. --- Indentation Style  ---
  ;; This single variable controls indentation for both c-ts-mode and
  ;; c++-ts-mode, as the latter inherits from the former.
  (setq-local c-ts-mode-indent-style 'k&r)
  (setq-local c-ts-mode-indent-offset 4)
  (setq-default c-basic-offset 4)

  ;; 2. Setup Smart Compile Command
  (when buffer-file-name
    (let* ((is-cpp        (or (string-suffix-p ".cpp" buffer-file-name)
                              (derived-mode-p 'c++-ts-mode)))
           (compiler      (if is-cpp "g++" "gcc"))
           (std-version   (if is-cpp my-cpp-std-version my-c-std-version))
           (std-flag      (unless (string-empty-p std-version)
                            (format "-std=%s" std-version)))
           (file-name     (file-name-nondirectory buffer-file-name))
           (file-sans-ext (file-name-sans-extension file-name)))
      (setq-local compile-command
                  (format "%s -o %s %s %s %s %s"
                          compiler file-sans-ext file-name
                          my-c-common-flags my-c-debug-flags (or std-flag "")))))

  ;; 3. Setup Keybinding
  (define-key (current-local-map) (kbd "<f12>") #'compile))

;;----------------------------------------------------------------------------
;; Hooking into Tree-sitter Modes
;;----------------------------------------------------------------------------

;; We explicitly add our setup function to the specific hooks for
;; Tree-sitter modes. This is the most direct and reliable method.
(add-hook 'c-ts-mode-hook #'my-c-ts-mode-setup)
(add-hook 'c++-ts-mode-hook #'my-c-ts-mode-setup)

;; Associate CUDA files with C++ modes
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

;; Associate CMakeLists.txt with cmake-ts-mode
(use-package cmake-ts-mode
  :mode ("CMakeLists\\.txt\\'" . cmake-ts-mode)
  :config
  (add-to-list 'eglot-server-programs
               '((cmake-mode cmake-ts-mode) . ("neocmakelsp" "stdio"))))

(provide 'init-c)
;;; init-c.el ends here
