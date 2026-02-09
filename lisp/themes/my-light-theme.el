;;; my-light-theme.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup my-light-theme nil
  "Options for the `my-light' theme."
  :group 'doom-themes)

(defcustom my-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'my-light-theme
  :type 'boolean)

(defcustom my-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'my-light-theme
  :type 'boolean)

(defcustom my-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'my-light-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme my-light
    "A light theme."
  :family 'my-light
  :background-mode 'light

  ;; name        default   256       16
  ((bg         '("#FFFFFF" "#FBF7EF" "white"        ))
   (fg         '("#4d4d4c" "#3a3a3a" "black"))
   ;; (fg         '("#657377" "#657377" "black"        ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#f2f2f2" nil       nil     ))
   (fg-alt     (doom-darken fg 0.6))
   ;; (fg-alt     '("#5B7279" "#5B7279" "brightwhite"  ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#FBF7EF" "#FBF7EF" "white"        ))
   (base1      '("#f2f2f2" "#f2f2f2" "white"        ))
   (base2      '("#e4e4e4" "#e4e4e4"         ))
   (base3      '("#FCF7E8" "#FCF7E8" "brightblack"  ))
   (base4      '("#8FAAAB" "#8FAAAB" "brightblack"  ))
   (base5      '("#98A8A8" "#98A8A8" "brightblack"  ))
   (base6      '("#657377" "#657377" "brightblack"  ))
   (base7      '("#5B7279" "#5B7279" "brightblack"  ))
   (base8      '("#657377" "#657377" "brightblack"  ))

   (grey       '("#8e908c" "#999999" "silver"))
   ;; (grey       base4)
   (red        '("#E14775" "#E14775" "red"          ))
   (orange     '("#D56500" "#dd8844" "brightred"    ))
   (green      '("#819500" "#99bb66" "green"        ))
   (yellow     '("#AC8300" "#ECBE7B" "yellow"       ))
   ;; (blue       '("#2B90D8" "#51afef" "brightblue"   ))
   (blue  '("#3F88AD" "#2257A0" "blue"         ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (teal       '("#3e999f" "#339999" "brightblue"))
   (magenta    '("#DD459D" "#c678dd" "magenta"      ))
   ;; (violet     '("#7D80D1" "#a9a1e1" "brightmagenta"))
   (violet '("#8959a8" "#996699" "brightmagenta"))
   (dark-violet '("#8959a8" "#996699" "brightmagenta"))
   (cyan       '("#259D94" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#4271ae" "#339999" "brightblue"))

   ;; Custom colors
   (magenta-alt '("#b95291" "#b95291" "magenta"))
   (blue-alt    '("#4881a7" "#4881a7" "blue"))
   (bg-code     '("#fbfaf4" "#fbfaf4" "white"))
   (bg-selected '("#e9f3fb" "#e9f3fb" "white"))
   (border      '("grey80"  "grey80"  "white"))

   ;; face categories -- required for all themes
   (highlight      dark-blue)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        dark-cyan)
   (comments       (if my-light-brighter-comments
                       (doom-lighten teal 0.25)
                     grey))
   ;; (doc-comments   base5)
   (doc-comments   grey)
   (constants      violet)
   (functions      magenta)
   (keywords       green)
   (methods        cyan)
   (operators      fg)
   (type           yellow)
   (strings        cyan)
   (variables      blue)
   (numbers        violet)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    (doom-lighten blue 0.2))
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright my-light-brighter-modeline)
   (-modeline-pad
    (when my-light-padded-modeline
      (if (integerp my-light-padded-modeline) my-light-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-darken bg 0.05)))
   (modeline-bg-alt
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-lighten base3 0.2)))
   (modeline-bg-inactive     (doom-darken bg 0.025))
   (modeline-bg-inactive-alt (doom-darken bg 0.02)))

  ;;;; Base theme face overrides
  ;; --- Base ---
  ((cursor :background dark-blue)
   (hl-line :background base1)
   ((line-number                  &override) :foreground base6)
   ((line-number-current-line     &override) :foreground fg :background base1 :weight 'bold)
   (secondary-selection :background base2)

   ;; --- Font Lock ---
   ((gnus-group-news-low          &override) :inherit nil :foreground base5 :weight 'normal)
   ((gnus-group-news-low-empty    &override) :inherit nil :foreground base5 :weight 'normal)
   ((font-lock-comment-face &override)
    :foreground grey
    :background (if my-light-brighter-comments
                    (doom-blend teal base0 0.07)
                  'unspecified))

   ((font-lock-function-name-face &override) :foreground blue)
   ((font-lock-preprocessor-face &override) :foreground magenta :weight 'normal)
   ((font-lock-variable-name-face &override) :foreground fg)

   (font-lock-property-use-face :foreground blue-alt)

   ;; --- Modeline ---
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   ;; --- Org Mode ---
   (org-level-1 :foreground dark-cyan   :height 1.4 :weight 'medium)
   (org-level-2 :foreground dark-violet :height 1.3 :weight 'medium)
   (org-level-3 :foreground dark-blue   :height 1.2 :weight 'medium)
   (org-level-4 :foreground magenta-alt :height 1.2 :weight 'medium)
   (org-level-5 :foreground base6       :height 1.2 :weight 'medium)
   (org-level-6 :foreground base6       :height 1.2 :weight 'medium)
   (org-level-7 :foreground base6       :height 1.2 :weight 'medium)
   (org-level-8 :foreground base6       :height 1.2 :weight 'medium)

   (org-document-title :foreground dark-cyan :weight 'medium :height 2.0)
   (org-document-info :foreground magenta-alt :height 1.2)
   (org-priority :foreground magenta-alt :height 1.0)
   (org-todo :foreground teal)
   (org-done :inherit 'org-todo :foreground base5)
   (org-table :foreground fg)
   (org-block :background (doom-blend yellow bg 0.04) :extend t)
   (org-verbatim :background bg-code) ; :box `(:line-width -1 :color ,border))
   (org-code     :background bg-code) ; :box `(:line-width -1 :color ,border))
   (org-date-selected :background fg :foreground bg)

   ;; Org Agenda
   (org-agenda-structure :foreground dark-cyan :weight 'bold)
   (org-agenda-date :foreground fg)
   (org-agenda-date-today :foreground dark-blue :weight 'bold)
   (org-agenda-date-weekend :foreground base5)
   (org-agenda-current-time :foreground yellow :weight 'bold)
   (org-imminent-deadline :foreground magenta-alt)

   ;; --- Markdown ---
   (markdown-markup-face :foreground base5)
   (markdown-header-face-1 :foreground dark-cyan   :height 1.4 :weight 'medium)
   (markdown-header-face-2 :foreground dark-violet :height 1.3 :weight 'medium)
   (markdown-header-face-3 :foreground dark-blue   :height 1.2 :weight 'medium)
   (markdown-header-face-4 :foreground magenta-alt :height 1.2 :weight 'medium)
   (markdown-header-face-5 :foreground base6       :height 1.2 :weight 'medium)
   (markdown-header-face-6 :foreground base6       :height 1.2 :weight 'medium)
   (markdown-header-face-7 :foreground base6       :height 1.2 :weight 'medium)
   (markdown-header-face-8 :foreground base6       :height 1.2 :weight 'medium)
   ((markdown-code-face &override) :background bg-code)
   (markdown-list-face :foreground fg)
   (markdown-bold-face :foreground fg :weight 'bold)
   (markdown-pre-face  :background (doom-blend yellow bg 0.04) :extend t)

   ;; --- Outline ---
   ((outline-1 &override) :foreground dark-cyan)
   ((outline-2 &override) :foreground dark-violet)
   ((outline-3 &override) :foreground dark-blue)
   ((outline-4 &override) :foreground magenta-alt)
   ((outline-5 &override) :foreground base6)
   ((outline-6 &override) :foreground base6)
   ((outline-7 &override) :foreground base6)
   ((outline-8 &override) :foreground base6)

   ;; --- Navigation & Tools ---
   ;; Dired
   (dired-directory :foreground base7)
   (dired-mark      :foreground yellow)
   (dired-marked    :background bg-selected)
   (dired-flagged   :foreground red)

   ;; Dirvish
   (dirvish-hl-line          :background base1)
   (dirvish-hl-line-inactive :background bg)

    ;; Magit
    (magit-header-line :background base2)

    ;; Diff
    (diff-refine-added   :background (doom-blend green bg 0.4) :inverse-video nil)
    (diff-refine-removed :background (doom-blend red bg 0.4) :inverse-video nil)

   ;; Rainbow Delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground violet)
   (rainbow-delimiters-depth-5-face :foreground yellow)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground yellow)

   ;; Show Paren
   (show-paren-match     :foreground green :box '(:line-width (-2 . -2) :color "gray"))
   (show-paren-mismatch  :foreground red)

   ;; Misc
   (rime-default-face :background base2)
   (tutorial-warning-face :foreground teal)
   (info-menu-star :foreground yellow)
   )
  )

(provide 'my-light-theme)
;;; my-light-theme.el ends here
