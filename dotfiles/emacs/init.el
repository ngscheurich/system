;;; init.el --- The Emacs Initialization File  -*- lexical-binding: t -*-

;; Author: N. G Scheurich <nick@scheurich.haus>
;; URL: https://github.com/ngscheurich/dotfiles
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is loaded when Emacs is started, and is the conventional
;; place for one to configure their Emacs system.
;;
;; For more information about the Emac Initialization File, see
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html>.

;;; Code:

;; Tell Emacs a little about myself
(setq user-full-name "Nicholas Scheurich"
      user-mail-address "nick@scheurich.haus")

;; Package manageement
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; Make <escape> quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Remap help prefix
(global-set-key (kbd "s-h") 'help-command)

;; Map Option key to Super
(setq mac-option-modifier 'super)

;; Themes
(use-package modus-themes
  :init
  (setq modus-themes-common-palette-overrides
	'((border-mode-line-active unspecified)
	  (border-mode-line-inactive unspecified)))
  (load-theme 'modus-operandi-tinted :no-confirm)
  :bind
  ("<f5>" . modus-themes-toggle))

Customize mode-line appearance
(defun my-modus-themes-custom-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     ;; Thicken the mode line
     `(mode-line ((,c :box (:line-width 3 :color ,bg-mode-line-active))))
     `(mode-line-inactive ((,c :box (:line-width 3 :color ,bg-mode-line-inactive)))))))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)
(my-modus-themes-custom-faces)

;; Typography
(set-face-attribute 'default nil :font "MonoLisa" :height 110)
(setq-default line-spacing 0.25)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq column-number-mode t)

;; Maintain separate workspaces
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init
  (persp-mode))

;; Vim emulation
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :bind
  (:map evil-normal-state-map
	("<up>" . 'evil-window-up)
	("<down>" . 'evil-window-down)
	("<left>" . 'evil-window-left)
	("<right>" . 'evil-window-right)
	("-" . dired-jump))
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; General
(use-package general
  :config
  (general-evil-setup)

  (general-create-definer ngs/leader-def
    :keymaps '(normal emacs)
    :prefix "SPC")

  (ngs/leader-def
    "f"   '(:ignore t :which-key "find")
    "ff"  '(project-find-file :which-key "file")
    "fl"  '(consult-line :which-key "line")
    "fr"  '(consult-recent-file :which-key "recent")
    "fb"  '(consult-buffer :which-key "buffer")
    "SPC" '(consult-buffer :which-key "buffer")

    "p"  '(:ignore t :which-key "project")
    "pf" '(project-find-file :which-key "find file")
    "pk" '(project-kill-buffers :which-key "kill buffers")
    "ps" '(project-switch-project :which-key "switch")

    "g"  '(:ignore g :which-key "git")
    "gs" '(magit-status :which-key "status")

    "h"  '(:ignore g :which-key "help")
    "hh" '(helpful-at-point :which-key "at point")
    "hf" '(helpful-callable :which-key "function")
    "hc" '(helpful-command :which-key "command")
    "hk" '(helpful-key :which-key "key")
    "hK" '(describe-keymap :which-key "keymap")
    "hm" '(describe-mode :which-key "mode")
    "hv" '(helpful-variable :which-key "variable")

    "t"  '(:ignore t :which-key "toggles")))

;; Hydra
(use-package hydra
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-decrease "down")
    ("k" text-scale-increase "up")))

;; VERTical Interactive COmpletion
(use-package vertico
  :init
  (vertico-mode))

;; Annotate completion candidates
(use-package marginalia
  :init
  (marginalia-mode))

;; Interesting completion lists
(use-package consult
  :bind (("C-x b" . consult-buffer)
	 ("M-s l" . consult-line)))

;; Completion Overlay Region FUnction
(use-package corfu
  :custom
  (corfu-auto t)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

;; Orderless completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless)))

;; Languages
(use-package elixir-ts-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package nix-mode
  :mode "\\.nix\\'")

;; Do I still need this with Emacs 29?
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode . ("/Users/nick/Projects/elixir-ls/release/language_server.sh")))
  :hook
  ((elixir-mode . eglot-ensure)
   (lua-mode . eglot-ensure)))

;; Wrap lines nicely when editing prose
(add-hook 'text-mode-hook 'visual-line-mode)

;; Automatically pair parentheses, et al.
(electric-pair-mode t)

;; Make <escape> quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Remap help prefix
(global-set-key (kbd "C-c h") 'help-command)

;; Confirm before quitting
(setq confirm-kill-emacs #'yes-or-no-p)

;; Resize windows pixel-wise rather than character-wise
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Save place in files
(save-place-mode t)

;; Remember input between sessions
(use-package savehist
  :init
  (savehist-mode))

;; Remember recent files
(recentf-mode t)

;; Abbreviate yes/no input
;; (defalias 'yes-or-no #'y-or-n-p)
(setq use-short-answers t)

;; Show contextual keybindings
(use-package which-key
  :config
  (which-key-mode))

;; Get Emacs path from shell $PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; direnv integration
(use-package direnv
  :config
  (direnv-mode))

;; Rainbow-colored delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :bind
  (("C-." . embark-act)))
(use-package embark-consult)

;; Buffer tabs
(use-package centaur-tabs
  :bind (("M-}" . centaur-tabs-forward-tab)
	 ("M-{" . centaur-tabs-backward-tab)))

;; Full-fledged terminal emulation
(use-package vterm)

;; Display icons
(use-package all-the-icons
  :if (display-graphic-p)
  :init (setq all-the-icons-scale-factor 1.2))

(use-package all-the-icons-dired
  :hook dired-mode)

;; Git interface
(use-package magit)

;; Get Emacs path from shell $PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; More useful *help*
(use-package helpful
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-command] . #'helpful-command)
  ([Remap describe-variable] . #'helpful-variable)
  ([remap describe-key] . #'helpful-key))

;; Wrangle copy/paste
(use-package simpleclip
  :bind
  (("M-c" . simpleclip-copy)
   ("M-v" . simpleclip-paste))
  :config
  (simpleclip-mode 1))

;; Use ELisp-powered snippets
;; (use-package yasnippet)

;; REST client
;; https://github.com/pashky/restclient.el
(use-package restclient)

;; Manage ephemeral windows
(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
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

;; Differentiate ephemeral windows
(use-package solaire-mode
  :config (solaire-global-mode +1))

;; Tree-based file explorer
(use-package treemacs
  :init (setq treemacs--icon-size 13))

;; Prevent eldoc from resizing the echo area
(use-package eldoc
  :ensure nil
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

;; Format LSP buffers on save
(add-hook 'before-save-hook 'eglot-format-buffer)

;; Efficient commenting
(use-package evil-nerd-commenter
  :after (general)
  :config
  (general-nmap  "gc" 'evilnc-comment-or-uncomment-lines))

;; Use command key as meta
(setq mac-command-modifier 'meta)

;; Efficient cursor use
(use-package avy
  :bind (:map evil-normal-state-map
	("s" . 'avy-goto-char-2)
	("f"  . 'evil-avy-goto-char-in-line)))

;; Generate tables of contents
(use-package toc-org
  :hook
  (org-mode . toc-org-mode)
  (markdown-mode . toc-org-mode))

;; EditorConfig integration
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; Customize mode-line
;; (setq mode-line-format
;;       '("%e" mode-line-front-space
;; 	(:propertize
;; 	 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
;; 	 display
;; 	 (min-width
;; 	  (5.0)))
;; 	mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
;; 	(vc-mode vc-mode)
;; 	"  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; Hide title bar
(add-to-list 'default-frame-alist '(undecorated-round . t))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((((class color) (min-colors 256)) :box (:line-width 3 :color "#cab9b2"))))
 '(mode-line-inactive ((((class color) (min-colors 256)) :box (:line-width 3 :color "#dfd9cf")))))
