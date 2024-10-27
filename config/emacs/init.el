;;; GNU Emacs initialization

;; Add Nix bin directories to `exec-path'
(add-to-list 'exec-path
	     (format "/etc/profiles/per-user/%s/bin/" (getenv "USER")))
(add-to-list 'exec-path "/run/current-system/sw/bin/")

(defun ngs-home-manager-bin (file)
  "Returns the path to the Home Manager bin `file' for the current user."
  (format "/etc/profiles/per-user/%s/bin/%s" (getenv "USER") file))

;; Emacs settings
(use-package emacs
  :init
  (setopt user-full-name "Nicholas Scheurich"
          user-email-address "nick@scheurich.haus"
          history-length 40
          custom-file (locate-user-emacs-file "custom-vars.el")
	  shell-file-name (ngs-home-manager-bin "zsh")
	  sh-shell (ngs-home-manager-bin "zsh")
	  sh-shell-file (ngs-home-manager-bin "zsh"))
    (add-to-list 'default-frame-alist
                 '(font . "MonoLisa-12"))
    (setq default-line-spacing 0.2))

;; Enable package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load configuration modules
(add-to-list 'load-path (locate-user-emacs-file "ngs-lisp"))

(require 'ngs-emacs-core)
(require 'ngs-completion)
(require 'ngs-modality)
(require 'ngs-prog)


;; Buffer-local direnv integration
;; https://github.com/purcell/envrc
(use-package envrc
  :ensure t
  :config
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
  (envrc-global-mode))

;; Highly-legible themes
;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :ensure t
  :config
  (modus-themes-select 'modus-vivendi-tinted))

;; Font ligatures using HARFBUZZ/Cairo
;; https://github.com/mickeynp/ligature.el
(use-package ligature
  :ensure t
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
(use-package nerd-icons
  :ensure t)

;; Icons in dired buffers
;; https://github.com/rainstormstudio/nerd-icons-dired
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Icons in completion interfaces
;; https://github.com/rainstormstudio/nerd-icons-completion
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Icons for Corfu candidates
;; https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :ensure t
  :after corfu
  :init
  (setopt kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Magit; it's like Git but magic
(use-package magit
  :ensure t)

;; Jump to things in Emacs tree-style
;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :bind ("M-j" . avy-goto-char-timer))

;; Display available keybindings in popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; A template system for Emacs
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))
