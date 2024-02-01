;; Colors, typography, and iconography -*- lexical-binding: t -*-

;; Family of highly-legible themes
;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :config
  (defun ngs-modus-themes-custom-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :box (:line-width 4 :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 4 :color ,bg-mode-line-inactive)))))))
  (load-theme 'modus-vivendi-tinted t)
  (ngs-modus-themes-custom-faces))

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
	   :line-spacing 0.3
           :variable-pitch-family "IBM Plex Sans")))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

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
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'ngs-theme)
