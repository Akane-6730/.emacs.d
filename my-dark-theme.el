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
  ((bg         '("#2c2e33" nil       nil            ))
   (bg-alt     '("#1E2528" nil       nil            ))
   (base0      '("#131313" "#121212" "black"        ))
   (base1      '("#161b1e" "#1c1c1c" "black"        ))
   (base2      '("#2e363b" "#262626" "brightblack"  ))
   (base3      '("#474950" "#3a3a3a" "brightblack"  ))
   (base4      '("#545f62" "#585858" "brightblack"  ))
   (base5      '("#5a6568" "#585858" "brightblack"  ))
   (base6      '("#6b7678" "#6c6c6c" "brightblack"  ))
   (base7      '("#8b9798" "#8a8a8a" "brightblack"  ))
   (base8      '("#b4c1c0" "#bcbcbc" "white"        ))
   (fg         '("#f2fffc" "#ffffff" "brightwhite"  ))
   (fg-alt     '("#c6c6c6" "#c6c6c6" "white"        ))

   (grey       base4)
   (red        '("#ff6d7e" "#ff69bf" "red"          ))
   (orange     '("#ffb270" "#ff7f50" "brightred"    ))
   (green      '("#a2e57b" "#90ee90" "green"        ))
   (yellow     '("#ffed72" "#f0e68c" "yellow"       ))
   (violet     '("#baa0f8" "#9370db" "magenta"      ))
   (cyan       '("#7cd5f1" "#40e0d0" "brightcyan"   ))
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
   (operators      red)
   (type           cyan)
   (strings        yellow)
   (variables      fg)
   (numbers        violet)
   (region         base3)
   (error          red)
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
   (match                                        :foreground fg :background base3)
   (isearch                                      :inherit 'match :box `(:line-width 2 :color ,yellow))
   (lazy-highlight                               :inherit 'match)
   (isearch-fail                                 :foreground red)
   ;; current line
   (hl-line                                      :background base3)
   ;; line-numbers
   ((line-number &override)                      :foreground base4 :distant-foreground nil)
   ((line-number-current-line &override)         :foreground base7 :distant-foreground nil)
   ;; mode-line
   (mode-line                                    :background base3 :foreground fg
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color red)))
   (mode-line-inactive                           :background bg :foreground fg
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color red)))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property                     :foreground keywords)
   ;;;; deadgrep
   (deadgrep-match-face                          :inherit 'match :box `(:line-width 2 :color ,yellow))

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
   ;;;; neotree
   (neo-dir-link-face                            :foreground cyan)
   (neo-expand-btn-face                          :foreground red)
   ;;;; outline <built-in>
   ((outline-1 &override)                        :foreground yellow)
   ((outline-2 &override)                        :foreground blue)
   ((outline-3 &override)                        :foreground green)
   ((outline-4 &override)                        :foreground fg)
   (outline-5                                    :inherit 'outline-4)
   (outline-6                                    :inherit 'outline-5)
   (outline-7                                    :inherit 'outline-6)
   (outline-8                                    :inherit 'outline-7)
   ;;;; org <built-in>
   (org-ellipsis                                 :foreground orange)
   (org-tag                                      :foreground yellow :weight 'normal)
   ((org-quote &override)                        :inherit 'italic :foreground base7 :background org-quote)
   (org-todo                                     :foreground yellow)
   (org-list-dt                                  :foreground yellow)
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   ;;;; show-paren-mode
   (show-paren-match                             :weight 'normal :foreground green)
   (show-paren-mismatch                          :weight 'normal :foreground red)
   ;;;; swiper
   (swiper-background-match-face-1               :inherit 'match :weight 'normal)
   (swiper-background-match-face-2               :inherit 'match)
   (swiper-background-match-face-3               :inherit 'match :foreground green)
   (swiper-background-match-face-4               :inherit 'match :weight 'normal :foreground green)
   (swiper-match-face-1                          :inherit 'isearch :weight 'normal)
   (swiper-match-face-2                          :inherit 'isearch)
   (swiper-match-face-3                          :inherit 'isearch :foreground green)
   (swiper-match-face-4                          :inherit 'isearch :weight 'normal :foreground green)
   (swiper-line-face                             :inherit 'hl-line)
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
   (treemacs-git-modified-face                  :foreground blue)
   (treemacs-git-renamed-face                   :foreground orange)
   (treemacs-git-untracked-face                 :foreground (doom-darken yellow 0.3))
   (treemacs-on-failure-pulse-face              :foreground base0 :background red)
   (treemacs-on-success-pulse-face              :foreground base0 :background green)
   ;;;; web-mode
   (web-mode-html-tag-face                      :foreground red)
   (web-mode-html-tag-bracket-face              :foreground base7)
   (web-mode-html-attr-name-face                :foreground cyan :italic italic)
   (web-mode-html-attr-equal-face               :inherit 'web-mode-html-tag-bracket-face)
   ;; Apparently web-mode has no face for values of css properties.
   (web-mode-css-selector-face                  :foreground green)
   (web-mode-css-property-name-face             :foreground base7)
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

   ;; Override the theme's default color for operators.
   (font-lock-operator-face :foreground magenta)
   )

  ;;;; Base theme variable overrides
  ;; ()
  )

(provide 'my-dark-theme)
;;; my-dark-theme.el ends here
