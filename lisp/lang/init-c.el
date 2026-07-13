;;; init-c.el --- Configurations for C/C++ development -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures a complete development environment for C and C++.
;; It enforces a consistent indentation style, provides a smart compilation
;; command, and sets up language-specific keybindings.
;;
;;; Code:

(use-package c-ts-mode
  :ensure nil
  :preface
  (defvar my-c-common-flags "-Wall -Wextra" "Common compiler flags for C/C++.")
  (defvar my-c-std-version "" "The C standard to use (e.g., `c99`, `c11`).")
  (defvar my-cpp-std-version "" "The C++ standard to use (e.g., `c++14`, `c++17`).")
  (defvar my-c-debug-flags "-g" "Flags for including debug information.")

  (defun my-c-match-symbol-in-code (regexp limit)
    "Search for REGEXP up to LIMIT, skipping strings and comments."
    (catch 'match
      (while (re-search-forward regexp limit t)
        (unless (nth 8 (syntax-ppss))
          (throw 'match t)))))

  (defun my-c-ts-font-lock-matcher (limit)
    "Match C/C++ delimiters and operators up to LIMIT using Tree-sitter."
    (catch 'match
      (while (re-search-forward
              "\\(?:\\.\\|->\\|::\\|\\[\\|\\]\\|;\\|<\\|>\\|[\"']\\)" limit t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (text (match-string 0))
               (node (treesit-node-at beg))
               (type (and node (treesit-node-type node)))
               (parent (and node (treesit-node-parent node))))
          (when
              (cond
               ((equal type "escape_sequence") nil)
               ((member text '("\"" "'"))
                (and node
                     (or (= (- (treesit-node-end node)
                               (treesit-node-start node))
                            1)
                         (and (member type '("string_literal" "char_literal"
                                             "system_lib_string"))
                              (or (= beg (treesit-node-start node))
                                  (= beg (1- (treesit-node-end node))))))))
               ((member text '("<" ">"))
                (or (and parent
                         (member (treesit-node-type parent)
                                 '("template_parameter_list"
                                   "template_argument_list")))
                    (equal type "system_lib_string")))
               (node
                (and (not (member type '("string_content" "comment")))
                     (= (- (treesit-node-end node) (treesit-node-start node))
                        (- end beg)))))
            (throw 'match t))))))

  (defun my-c-ts-mode-setup ()
    "Set up indentation, compilation, keys, and faces for C/C++."
    (setq-local c-ts-mode-indent-style 'k&r
                c-ts-indent-offset 4)
    (setq-default c-basic-offset 4)

    (when buffer-file-name
      (let* ((cpp-p (or (string-suffix-p ".cpp" buffer-file-name)
                        (derived-mode-p 'c++-ts-mode)))
             (compiler (if cpp-p "g++" "gcc"))
             (standard (if cpp-p my-cpp-std-version my-c-std-version))
             (file (file-name-nondirectory buffer-file-name)))
        (setq-local compile-command
                    (format "%s -o %s %s %s %s %s"
                            compiler (file-name-sans-extension file) file
                            my-c-common-flags my-c-debug-flags
                            (if (string-empty-p standard)
                                ""
                              (concat "-std=" standard))))))

    (font-lock-add-keywords
     nil
     `((my-c-ts-font-lock-matcher 0 'font-lock-delimiter-face t)
       ("^\\s-*\\(#\\)" 1 'font-lock-delimiter-face t)
       (,(lambda (limit) (my-c-match-symbol-in-code "\\<namespace\\>" limit))
        0 'font-lock-type-face t)
       (,(lambda (limit) (my-c-match-symbol-in-code "\\<this\\>" limit))
        0 'eglot-semantic-parameter t))))
  :bind (:map c-ts-mode-map ("<f12>" . compile)
              :map c++-ts-mode-map ("<f12>" . compile))
  :mode (("\\.cu\\'" . c++-ts-mode)
         ("\\.cuh\\'" . c++-mode))
  :hook ((c-ts-mode . my-c-ts-mode-setup)
         (c++-ts-mode . my-c-ts-mode-setup)))

;; Associate CMakeLists.txt with cmake-ts-mode
(use-package cmake-ts-mode
  :mode ("CMakeLists\\.txt\\'" . cmake-ts-mode)
  :config
  (add-to-list 'eglot-server-programs
               '((cmake-mode cmake-ts-mode) . ("neocmakelsp" "stdio"))))

(provide 'init-c)
;;; init-c.el ends here
