;;; my-dark-theme.el --- port of Monokai Classic -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup my-dark-theme nil
  "Options for doom-molokai."
  :group 'doom-themes)

(defcustom my-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'my-dark-theme
  :type 'boolean)

(defcustom my-dark-comment-bg my-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'my-dark-theme
  :type 'boolean)

(defcustom my-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'my-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme my-dark
    "A dark, vibrant theme inspired by Textmate's Monokai."
  :family 'doom-molokai
  :background-mode 'dark

  ;; name        gui       256       16
  ((bg         '("#2c2e34" nil       nil            ))
   (bg-alt     '("#222327" nil       nil            )) ; 1E2528
   (base0      '("#131313" "#121212" "black"        ))
   (base1      '("#161b1e" "#1c1c1c" "black"        ))
   (base2      '("#363944" "#262626" "brightblack"  )) ;2e363b
   (base3      '("#474950" "#3a3a3a" "brightblack"  ))
   (base4      '("#545f62" "#585858" "brightblack"  ))
   (base5      '("#5a6568" "#585858" "brightblack"  ))
   (base6      '("#6b7678" "#6c6c6c" "brightblack"  ))
   (base7      '("#8b9798" "#8a8a8a" "brightblack"  ))
   (base8      '("#b4c1c0" "#bcbcbc" "white"        ))
   (fg         '("#e2e2e3" "color-250" "brightwhite"  ))
   (fg-alt     '("#c6c6c6" "#c6c6c6" "white"        ))
   (grey       '("#7f8490" "color-246" "brightblack"))
   (red        '("#fc5d7c" "color-203" "red"         ))
   (orange     '("#f39660" "color-215" "brightred"   ))
   (yellow     '("#e7c664" "color-179" "yellow"      ))
   (green      '("#9ed072" "color-107" "green"       ))
   (cyan       '("#76cce0" "color-110" "brightcyan"  ))
   (violet     '("#b39df3" "color-176" "magenta"     ))
   (magenta    '("#FF6188" "#FF6188" "violet"       ))
   (blue        cyan)
   (dark-blue   cyan)
   (teal        cyan)
   (dark-cyan   cyan)

   ;; face categories
   (highlight      yellow)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base2)
   (builtin        violet)
   (comments       (if my-dark-brighter-comments violet base6))
   (doc-comments   (if my-dark-brighter-comments (doom-lighten violet 0.1) (doom-lighten base6 0.25)))
   (constants      violet)
   (functions      green)
   (keywords       magenta)
   (methods        green)
   (operators      magenta)
   (type           cyan)
   (strings        yellow)
   (variables      fg)
   (numbers        violet)
   (region         base3)
   (error          magenta)
   (warning        orange)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when my-dark-padded-modeline
      (if (integerp my-dark-padded-modeline) my-dark-padded-modeline 4)))


   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;;;; Base theme face overrides
  ((cursor                                       :background fg)
   ;; I-search
   (match                                        :foreground bg :background yellow)
   (lazy-highlight                               :inherit 'match)
   (isearch-fail                                 :foreground magenta)
   ;; current line
   (hl-line                                      :background base2)
   ;; line-numbers
   ((line-number &override)                      :foreground base4 :distant-foreground nil)
   ((line-number-current-line &override)         :foreground base7 :distant-foreground nil)
   ;; mode-line
   (mode-line                                    :background base3 :foreground fg
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color red)))
   (mode-line-inactive                           :background bg :foreground fg
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color red)))
   ;;;; doom-modeline
   (doom-modeline-bar                            :background yellow)
   (doom-modeline-buffer-file                    :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path                    :inherit 'normal :foreground green)
   (doom-modeline-buffer-project-root            :foreground green :weight 'normal)
   (doom-modeline-buffer-modified                :inherit 'normal :foreground orange)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A                            :background (doom-blend red bg 0.3) :weight 'normal)
   ;;;; markdown-mode
   (markdown-blockquote-face                     :inherit 'italic :foreground dark-blue)
   (markdown-list-face                           :foreground red)
   (markdown-pre-face                            :foreground cyan)
   (markdown-link-face                           :inherit 'normal :foreground blue)
   ((markdown-code-face &override)               :background (doom-lighten base2 0.045))
   ;;;; outline <built-in>
   ((outline-1 &override)                        :foreground "#d3869b")
   ((outline-2 &override)                        :foreground "#81a2be")
   ((outline-3 &override)                        :foreground "#b8bb26")
   ((outline-4 &override)                        :foreground "#c39ac9")
   (outline-5                                    :inherit 'outline-4)
   (outline-6                                    :inherit 'outline-5)
   (outline-7                                    :inherit 'outline-6)
   (outline-8                                    :inherit 'outline-7)
   ;;;; org <built-in>
   (org-drawer :foreground "#bbd9b0")

   (org-document-info                :foreground "#81a2be")
   (org-document-title               :foreground "#c39ac9")
   (org-ellipsis                     :foreground orange)
   (org-tag                          :foreground yellow :weight 'normal)
   ((org-quote &override)            :inherit 'italic :foreground base7 :background org-quote)
   (org-todo                         :foreground yellow)
   (org-list-dt                      :foreground yellow)
   ((org-block &override)            :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   ;;;; org agenda
   (org-agenda-date                  :foreground "#b294bb")
   (org-agenda-date-today            :foreground "#d0bed6")
   (org-agenda-date-weekend          :foreground "#6a5870")
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground  violet)
   (rainbow-delimiters-depth-2-face :foreground  blue)
   (rainbow-delimiters-depth-3-face :foreground  yellow)
   (rainbow-delimiters-depth-4-face :foreground  green)
   (rainbow-delimiters-depth-5-face :foreground  violet)
   (rainbow-delimiters-depth-6-face :foreground  blue)
   (rainbow-delimiters-depth-7-face :foreground  yellow)
   ;;;; show-paren-mode
   (show-paren-match                             :weight 'normal :foreground green)
   (show-paren-mismatch                          :weight 'normal :foreground red)
   ;;;; term <built-in>
   (term-color-black                            :foreground base3)
   (term-color-blue                             :foreground blue)
   (term-color-cyan                             :foreground violet)
   (term-color-green                            :foreground green)
   (term-color-magenta                          :foreground red)
   (term-color-red                              :foreground red)
   (term-color-white                            :foreground fg)
   (term-color-yellow                           :foreground yellow)
   ;;;; treemacs
   (treemacs-git-added-face                     :foreground green)
   (treemacs-git-conflict-face                  :foreground red)
   (treemacs-git-ignored-face                   :foreground base6)
   (treemacs-git-modified-face                  :foreground (doom-darken blue 0.2))
   (treemacs-git-renamed-face                   :foreground orange)
   (treemacs-git-untracked-face                 :foreground (doom-darken yellow 0.2))
   (treemacs-on-failure-pulse-face              :foreground base0 :background red)
   (treemacs-on-success-pulse-face              :foreground base0 :background green)

   ;; Rime
   (rime-highlight-candidate-face               :foreground green)
   ;; Tab bar
   (tab-bar                                     :background bg)
   (tab-bar-tab
    :background bg
    :foreground fg
    :box nil
    :underline `(:color ,blue :position -2))
   (tab-bar-tab-inactive
    :background bg
    :foreground fg-alt
    :box nil
    :underline nil)
   ;; Tab line
   (tab-line                                    :background bg)
   (tab-line-tab
    :background bg
    :foreground fg
    :box nil
    :underline `(:color ,blue :position -2))
   (tab-line-tab-current
    :background bg
    :foreground fg
    :box nil
    :underline `(:color ,blue :position -2))
   (tab-line-tab-inactive
    :background bg
    :foreground fg-alt
    :box nil
    :underline nil)
   ;; Magit
   (magit-header-line
    :background base3
    :foreground fg
    :box nil)
   ;; Info
   (info-menu-star :foreground magenta)
   ;; Override the theme's default color for operators.
   (font-lock-operator-face :foreground magenta)
   (font-lock-property-name-face :foreground orange)
   (font-lock-function-call-face :foreground green)
   )

  ;;;; Base theme variable overrides
  ;; ()
  )

(custom-theme-set-variables 'my-dark
                            `(hl-todo-keyword-faces
                              (list
                               (cons "TODO"       (doom-color 'blue))
                               (cons "FIXME"      (doom-color 'magenta))
                               (cons "BUG"        (doom-color 'magenta))
                               (cons "HACK"       (doom-color 'orange))
                               (cons "NOTE"       (doom-color 'yellow))
                               (cons "DONE"       (doom-color 'green))
                               (cons "DEPRECATED" (doom-color 'grey))))
                            `(ansi-color-names-vector
                              (vector (doom-color 'base0)   ; black
                                      (doom-color 'red)     ; red
                                      (doom-color 'green)   ; green
                                      (doom-color 'yellow)  ; yellow
                                      (doom-color 'blue)    ; blue
                                      (doom-color 'magenta) ; magenta
                                      (doom-color 'cyan)    ; cyan
                                      (doom-color 'fg))))   ; white

(provide 'my-dark-theme)
;;; my-dark-theme.el ends here
