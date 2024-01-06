(use-package emacs
  :ensure nil
  :custom
  (frame-resize-pixelwise t "Resize frames by pixels rather than characters")
  (frame-inhibit-implied-resize t "Prevent automatic frame resizing"))

(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t "Bypass the Emacs splash screen")
  (ring-bell-function 'ignore "Suppress the bell")
  (use-short-answers t "Abbreviate yes/no input")
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (unless (eq system-type 'darwin)
    (menu-bar-mode -1)))

(use-package emacs
  :ensure nil
  :custom
  (user-full-name "Nicholas Scheurich" "My full name")
  (user-email-address "nick@scheurich.haus" "My personal email address")
  (custom-file (locate-user-emacs-file "custom-vars.el") "Set custom file path")
  (window-resize-pixelwise t "Resize windows by pixels rather than characters")
  (history-length 40 "Keep up to 40 files in history")
  (message-kill-buffer-query nil "Don't ask to confirm buffer kill"))

(use-package no-littering
  :config
  (defvar ngs-auto-save-directory
    (no-littering-expand-var-file-name "auto-save/")
    "Directory in which auto-save files should be stored")

  (defvar ngs-backup-directory
    (no-littering-expand-var-file-name "backup/")
    "Directory in which backup files should be stored")

  (mapcar (lambda (dir)
            (unless (file-directory-p dir)
              (make-directory dir)))
          `(,ngs-auto-save-directory ,ngs-backup-directory))

  (setq auto-save-file-name-transforms
        `((".*" ,ngs-auto-save-directory t)))

  (setq backup-directory-alist
        `((".*" . ,ngs-backup-directory))))

(use-package modus-themes
  :init
  (defun ngs-modus-themes-custom-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :box (:line-width 4 :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 4 :color ,bg-mode-line-inactive)))))))
  :config
  (add-hook 'modus-themes-after-load-theme-hook #'ngs-modus-themes-custom-faces)
  (load-theme 'modus-vivendi-tinted t))

(use-package rainbow-mode)

(use-package fontaine
  :init
  (setq fontaine-presets
        '((regular)
          (t
           :default-family "Berkeley Mono"
           :default-weight regular
           :default-height 120
           :variable-pitch-family "IBM Plex Sans")))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(setq-default line-spacing 0.4)

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package envrc
  :config
  (envrc-global-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(if (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq mac-right-option-modifier 'alt))

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer ngs-leader-def
    :keymaps '(normal emacs)
    :prefix "SPC")

  (general-create-definer ngs-local-leader-def
    :keymaps '(normal emacs)
    :prefix ","))

(use-package which-key
  :diminish
  :config
  (which-key-mode 1)
  (ngs-leader-def
    "a" '(:ignore a :which-key "apps")
    "f" '(:ignore f :which-key "find")
    "g" '(:ignore g :which-key "git")
    "h" '(:ignore h :which-key "help")
    "t" '(:ignore t :which-key "toggles")))

(ngs-leader-def
  "n"  '(:ignore n :which-key "narrow")
  "ne" '(sp-narrow-to-sexp n :which-key "sexp")
  "nn" '(narrow-to-defun :which-key "defun")
  "np" '(narrow-to-page n :which-key "page")
  "nr" '(narrow-to-region n :which-key "region")
  "nw" '(widen n :which-key "widen"))

(use-package evil
  :custom
  (evil-echo-state nil "Don't display the Evil state in the echo area")
  (evil-undo-system 'undo-redo "Use the Emacs-native undo/redo functionality")
  (evil-want-C-i-jump t "'C-i' to jump to next location")
  (evil-want-C-u-scroll t "'C-u' to scroll by half a page")
  (evil-want-Y-yank-to-eol t "'Y' to yank to the end of the line")
  (evil-want-keybinding nil "Don't load bindings for additional modes (see 'Evil Collection')")
  :general
  ("C-M-u" 'universal-argument)
  (general-nmap
    "<up>" 'evil-window-up
    "<down>" 'evil-window-down
    "<left>" 'evil-window-left
    "<right>" 'evil-window-right
    "-" 'dired-jump)
  :init
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :diminish
  :after evil
  :config
  (evil-commentary-mode))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package vertico
  :init (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :config
  (ngs-leader-def
  "/"   '(consult-line :which-key "lines")
  "SPC" '(consult-buffer :which-key "buffers")
  "fg"  '(consult-grep :which-key "grep")
  "fi"  '(consult-imenu :which-key "items")
  "fl"  '(consult-line :which-key "lines")
  "fr"  '(consult-recent-file :which-key "recents")
  "fd"  '(consult-flymake :which-key "diagnostics")))

(defvar ngs-corfu-extensions-directory
  (format "%sstraight/build/corfu/extensions" user-emacs-directory)
  "Path to Corfu’s included extensions")

(use-package corfu
  :load-path ngs-corfu-extensions-directory
  :custom (corfu-auto t)
  :bind (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

(defun ngs-corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active."
  (unless (or (bound-and-true-p vertico--active)
              (eq (current-local-map) read-passwd-map))
    (setq-local corfu-echo-delay nil
                corfu-popupinfo-delay '(0 . 0))
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'ngs-corfu-enable-in-minibuffer 1)

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history))

(use-package orderless
  :init
  (setq completion-styles '(orderless)))

(use-package cape)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package eldoc
  :ensure nil
  :diminish
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box)

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :init
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup))

(use-package eglot
  :ensure nil
  :config
  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   "K" 'eldoc-box-help-at-point)
  (ngs-local-leader-def
    "k" 'eldoc-doc-buffer
    "f" 'eglot-format-buffer))

(use-package elixir-ts-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "elixir-ls"))
  :hook (elixir-ts-mode . eglot-ensure)
  :mode
  (("\\.ex\\'" . elixir-ts-mode)
   ("\\.exs\\'" . elixir-ts-mode)))

(use-package typescript-ts-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode "typescript-language-server" "--stdio"))
  :hook (typescript-ts-mode . eglot-ensure)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)))

(use-package json-mode)

(use-package lua-mode)

(use-package markdown-mode)

(use-package nix-mode :mode "\\.nix\\'")

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode")
  :hook (gdscript-mode . eglot-ensure))

(use-package magit
  :config
  (ngs-leader-def
    "gb" '(magit-blame :which-key "blame")
    "gl" '(magit-log :which-key "log")
    "gs" '(magit-status :which-key "git")))

(use-package diff-hl
  :general
  (general-nmap
    :prefix "]"
    "h" 'diff-hl-next-hunk)
  (general-nmap
    :prefix "["
    "h" 'diff-hl-previous-hunk)
  :config
  (global-diff-hl-mode)
  (ngs-leader-def
    "gh" '(diff-hl-show-hunk :which-key "show hunk")
    "gr" '(diff-hl-revert-hunk :which-key "revert hunk")))

(defun ngs-org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (concat (expand-file-name user-emacs-directory) "config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'after-save-hook 'ngs-org-babel-tangle-config)))
