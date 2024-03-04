;; Colors, typography, and iconography -*- lexical-binding: t; -*-

(defvar ngs-mode-line-padding 3)

;; Highly-legible themes
;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :config
  (modus-themes-select 'modus-vivendi-tinted))

;; Colorful and legible themes
;; https://protesilaos.com/emacs/ef-themes
(use-package ef-themes)

;; Highlights color values with the corresponding color
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode)

;; Allows for detailed, on-demand font configurations
;; https://protesilaos.com/emacs/fontaine
(use-package fontaine
  :init
  (setq fontaine-presets
        '((regular)
          (t
           :default-family "Berkeley Mono"
           :default-weight regular
           :default-height 120
           :line-spacing 0.4
           :variable-pitch-family "IBM Plex Sans")))

  :hook
  ((kill-emacs .  fontaine-store-latest-preset)
   (enable-theme-functions  . fontaine-apply-current-preset))

  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

;; Adjusts the font size in all Emacs frames
;; https://github.com/purcell/default-text-scale
(use-package default-text-scale
  :bind ("C-c C-t" . 'hydra-text-scale/body)
  
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("=" default-text-scale-increase "larger")
    ("-" default-text-scale-decrease "smaller")
    ("0" default-text-scale-reset "reset")
    ("q" nil "quit" :exit t)))

;; Font ligatures using HARFBUZZ/Cairo
;; https://github.com/mickeynp/ligature.el
(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  (ligature-set-ligatures
   'prog-mode
   '(; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  (global-ligature-mode t))

;; Icons library
;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons)

;; Icons in dired buffers
;; https://github.com/rainstormstudio/nerd-icons-dired
(use-package nerd-icons-dired
  :diminish
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Icons in completion interfaces
;; https://github.com/rainstormstudio/nerd-icons-completion
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Icons in file explorer
;; https://github.com/rainstormstudio/treemacs-nerd-icons
(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;; Icons for Corfu candidates
;; https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'ngs-theme)
