;;; riscv-mode.el --- Major-mode for RISC-V assembly  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; riscv-mode provides syntax highlighting and syntactic indentation for RISC-V.
;;
;; Code:

(require 'thingatpt)
(require 'font-lock)

(defgroup riscv nil
  "Major mode for editing RISC-V assembly."
  :prefix "riscv-"
  :group 'languages)

;; Use symbol boundaries (\_< and \_>) to prevent partial matches
;; e.g., prevents "ra" matching inside "kerneltrap"
(defconst riscv-registers
  (concat "\\_<\\("
          "zero\\|ra\\|sp\\|gp\\|tp\\|fp\\|"
          "t[0-6]\\|s[0-9]\\|s1[01]\\|a[0-7]\\|"
          "x[0-9]\\|x[12][0-9]\\|x3[01]\\|"
          "ft[0-9]\\|ft1[01]\\|fs[0-9]\\|fs1[01]\\|fa[0-7]\\|"
          "f[0-9]\\|f[12][0-9]\\|f3[01]"
          "\\)\\_>"))

(defconst riscv-keywords
  '("lui" "auipc"
    "jal" "jalr"
    "beq" "bne" "blt" "bge" "bltu" "bgeu"
    "lh" "lb" "lw" "lbu" "lhu"
    "sb" "sh" "sw"
    "add" "sub"
    "addi"
    "sll" "slt" "sltu" "xor" "srl" "sra" "or" "and"
    "slti" "sltiu" "xori" "ori" "andi" "slli" "srli" "srai"
    "fence" "fence.i"
    "scall" "sbreak" "ecall" "ebreak" "uret" "sret" "mret" "wfi" "sfence.vma"
    "rdcycle" "rdcycleh" "rdtime" "rdtimeh" "rdinstret" "rdinstreth"
    "lwu" "ld" "sd"
    "addiw" "slliw" "srliw" "sraiw" "addw" "subw" "sllw" "srlw" "sraw"
    "mul" "mulh" "mulhsu" "mulhu" "div" "divu" "rem" "remu"
    "mulw" "divw" "divuw" "remw" "remuw"
    ;; Atomics
    "lr.w" "sc.w" "amoswap.w" "amoadd.w" "amoxor.w" "amoand.w" "amoor.w"
    "amomin.w" "amomax.w" "amominu.w" "amomaxu.w"
    "lr.d" "sc.d" "amoswap.d" "amoadd.d" "amoxor.d" "amoand.d" "amoor.d"
    "amomin.d" "amomax.d" "amominu.d" "amomaxu.d"
    ;; Floating point
    "flw" "fsw" "fmadd.s" "fmsub.s" "fnmsub.s" "fnmadd.s"
    "fadd.s" "fsub.s" "fmul.s" "fdiv.s" "fsqrt.s"
    "fsgnj.s" "fsnjn.s" "fsnjx.s"
    "fmin.s" "fmax.s" "fcvt.w.s" "fcvt.wu.s" "fmv.x.s"
    "feq.s" "flt.s" "fle.s"
    "fclass.s" "fcvt.s.w" "fcvt.s.wu" "fmv.s.x"
    "frcsr" "frrm" "frflags" "fscsr" "fsrm" "fsflags" "fsrmi" "fsflagsi"
    "fcvt.l.s" "fcvt.l.u.s" "fcvt.s.l" "fcvt.s.lu"
    ;; Double Precision
    "fld" "fsd" "fmadd.d" "fmsub.d" "fnmsub.d" "fnmadd.d"
    "fadd.d" "fsub.d" "fmul.d" "fdiv.d" "fsqrt.d"
    "fsgnj.d" "fsnjn.d" "fsnjx.d"
    "fmin.d" "fmax.d" "fcvt.w.d" "fcvt.wu.d" "fmv.x.d"
    "feq.d" "flt.d" "fle.d"
    "fclass.d" "fcvt.d.w" "fcvt.d.wu" "fmv.d.x"
    "frcsr" "frrm" "frflags" "fscsr" "fsrm" "fsflags" "fsrmi" "fsflagsi"
    "fcvt.l.d" "fcvt.l.u.d" "fcvt.d.l" "fcvt.d.lu"
    "fmv.d.x"
    ;; Pseudoinstructions
    "nop"
    "la" "li"
    "lb" "lh" "lw" "ld"
    "sb" "sh" "sw" "sd"
    "flw" "fld"
    "fsw" "fsd"
    "mv"
    "not" "neg" "negw"
    "sext"
    "seqz" "snez" "sltz" "sgtz"
    "fmv.s" "fmv.d"
    "fabs.s" "fabs.d"
    "fneg.s" "fneg.d"
    "beqz" "bnez" "blez" "bgez" "bltz" "bgtz"
    "j" "jal" "jr" "jalr" "ret" "call" "tail"
    ;; System
    "csrr" "csrw" "csrs" "csrc" "csrrw" "csrrs" "csrrc" "csrrwi" "csrrsi" "csrrci"))

(defconst riscv-defs
  '("align" "ascii" "asciiz" "byte" "data" "double" "extern"
    "float" "globl" "global" "half" "kdata" "ktext" "space" "text" "word"
    "section" "bss" "option" "macro" "endm" "set" "equ" "balign"))

(defconst riscv--number-regexp
  "\\_<-?\\(?:0x[0-9A-Fa-f]+\\|0b[01]+\\|[0-9]+\\(?:\\.[0-9]+\\)?\\)\\_>")

(defconst riscv--string-regexp
  "\"\\(?:\\\\.\\|[^\"\\\\]\\)*\"")

(defconst riscv--label-regexp
  "^[ \t]*[A-Za-z._][A-Za-z0-9._]*:")

(defconst riscv--label-only-regexp
  "^[ \t]*[A-Za-z._][A-Za-z0-9._]*:[ \t]*\\(?:#.*\\)?$")

(defconst riscv--directive-regexp
  "^[ \t]*\\.[A-Za-z][A-Za-z0-9._]*\\b")

(defconst riscv--preprocessor-regexp
  "^[ \t]*#[ \t]*\\(?:include\\|define\\|undef\\|if\\|ifdef\\|ifndef\\|else\\|elif\\|endif\\|error\\|pragma\\|line\\)\\b")

(defconst riscv--preprocessor-regex-tag
  "^[ \t]*\\(#\\)[ \t]*\\(?:include\\|define\\|undef\\|if\\|ifdef\\|ifndef\\|else\\|elif\\|endif\\|error\\|pragma\\|line\\)\\b")

(defconst riscv--section-directive-regexp
  "^[ \t]*\\.\\(section\\|text\\|data\\|bss\\|ktext\\|kdata\\|macro\\|endm\\)\\b")

(defun riscv-syntax-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    (riscv--preprocessor-regex-tag
     (1 ".")))
   start end))

(defconst riscv-font-lock-keywords
  `((,riscv--number-regexp . font-lock-constant-face)                 ;; Numbers
    (,riscv--string-regexp . font-lock-string-face)                   ;; Strings
    (,riscv--preprocessor-regexp . font-lock-preprocessor-face)       ;; Preprocessor
    (,riscv--label-regexp . font-lock-function-name-face)             ;; Labels
    (,(regexp-opt riscv-keywords 'symbols) . font-lock-keyword-face)  ;; Instructions
    (,(concat "\\." (regexp-opt riscv-defs t) "\\_>") . font-lock-preprocessor-face) ;; Directives
    (,riscv-registers . font-lock-variable-name-face)))               ;; Registers

(defcustom riscv-indent-offset 4
  "Indentation offset for RISC-V mode."
  :tag "Indentation Offset"
  :group 'riscv
  :type 'integer
  :safe #'integerp)

(defcustom riscv-interpreter "spike"
  "Interpreter to run RISC-V code in."
  :tag "RISC-V Interpreter"
  :group 'riscv
  :type 'string)

(defvar riscv-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<backtab>") 'riscv-dedent)
    (define-key map (kbd "C-c C-c") 'riscv-run-buffer)
    map)
  "Keymap for riscv-mode.")

(defun riscv--interpreter-buffer-name ()
  "Return a buffer name for the preferred RISC-V interpreter."
  (format "*%s*" riscv-interpreter))

(defun riscv--get-indent-level (&optional line)
  "Return the number of spaces indenting the last label."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (or line (line-number-at-pos))))
    (current-indentation)))

(defun riscv--last-label-line ()
  "Return the line number of the last label."
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward riscv--label-regexp nil t)
        (line-number-at-pos)
      0))) ;; Return 0 if no label found to prevent errors

(defun riscv--previous-code-indentation ()
  "Return indentation of previous non-empty, non-comment line.
Returns nil if no such line exists."
  (save-excursion
    (let ((indent nil)
          (found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        ;; Check if line is empty, comment start, or inside comment
        (unless (or (looking-at "^[ \t]*\\(?:#\\|/\\*\\|$\\)")
                    (nth 4 (syntax-ppss)))
          (setq indent (current-indentation))
          (setq found t)))
      indent)))

(defun riscv-comment-indent ()
  "Return comment indentation for RISC-V assembly."
  (save-excursion
    (back-to-indentation)
    (if (looking-at "\\(#\\|/\\*\\)")
        (let ((prev (riscv--previous-code-indentation)))
          (cond
           ((null prev) 0)                ;; No previous code -> 0
           ((> prev 0) prev)              ;; Previous code indented -> keep indent
           (t riscv-indent-offset)))      ;; Previous was top-level -> indent
      (max comment-column (1+ (current-column))))))

(defun riscv-indent ()
  "Indent current line based on nearby labels and directives."
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent 0))
    (save-excursion
      (back-to-indentation)
      (cond
       ((looking-at riscv--label-regexp)
        (setq indent 0))
       ((looking-at riscv--section-directive-regexp)
        (setq indent 0))
       ((looking-at riscv--preprocessor-regexp)
        (setq indent 0))
       ((looking-at "^[ \t]*\\(#\\|/\\*\\)")
        (setq indent (riscv-comment-indent)))
       ((nth 4 (syntax-ppss))
        ;; Inside a block comment
        (let* ((start-pos (nth 8 (syntax-ppss)))
               (start-indent (save-excursion (goto-char start-pos) (current-column))))
          (if (looking-at "^[ \t]*\\*/")
              (setq indent start-indent)     ;; Align */ with /*
            (if (save-excursion (forward-line -1) (<= (point) start-pos))
                (setq indent (+ start-indent 2)) ;; First line inside: +2 spaces
              (setq indent (save-excursion
                             (forward-line -1)
                             (while (and (looking-at "^[ \t]*$") (> (point) start-pos))
                               (forward-line -1))
                             (if (<= (point) start-pos)
                                 (+ start-indent 2)
                               (current-indentation)))))))) ;; Follow previous content
       (t
        (let ((label-line (riscv--last-label-line)))
          (setq indent (if (> label-line 0)
                           (+ riscv-indent-offset
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- label-line))
                                (current-indentation)))
                         riscv-indent-offset))))))
    (indent-line-to (max 0 indent))
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

(defun riscv-dedent ()
  "Decrease indentation."
  (interactive)
  (indent-line-to (max 0 (- (current-indentation) riscv-indent-offset))))

(defun riscv-run-buffer ()
  "Run the current buffer in a RISC-V interpreter."
  (interactive)
  (let ((tmp-file (make-temp-file "riscv-")))
    (write-region (point-min) (point-max) tmp-file)
    (let ((file tmp-file))
      (when (buffer-live-p (get-buffer (riscv--interpreter-buffer-name)))
        (kill-buffer (riscv--interpreter-buffer-name)))
      (start-process riscv-interpreter
                     (riscv--interpreter-buffer-name)
                     riscv-interpreter file))
    (switch-to-buffer-other-window (riscv--interpreter-buffer-name))
    (read-only-mode t)
    (delete-file tmp-file)))

;;;###autoload
(define-derived-mode riscv-mode prog-mode "RISC-V"
  "Major mode for editing RISC-V assembly."
  (setq-local font-lock-defaults '(riscv-font-lock-keywords))
  (setq-local syntax-propertize-function #'riscv-syntax-propertize)
  (setq-local tab-width riscv-indent-offset)
  (setq-local indent-line-function 'riscv-indent)
  (setq-local comment-indent-function 'riscv-comment-indent)
  (setq-local comment-start "# ")
  (modify-syntax-entry ?# "< b" riscv-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" riscv-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" riscv-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" riscv-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" riscv-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\.[sS]\'" . riscv-mode))

(provide 'riscv-mode)
;;; riscv-mode.el ends here
