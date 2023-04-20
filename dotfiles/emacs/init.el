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

(org-babel-load-file
  (expand-file-name "config.org" user-emacs-directory))

;; Use command key as meta
(setq mac-command-modifier 'meta)

;; Map option key to super
(setq mac-option-modifier 'super)

;; TODO
(use-package minions)
(use-package keycast)

;; Maintain separate workspaces
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init
  (persp-mode))

(use-package emacs
  :custom
  (message-kill-buffer-query nil "Don't ask to confirm buffer kill"))

;; General
(use-package general
  :config
  (general-evil-setup)

  (general-create-definer ngs/leader-def
    :keymaps '(normal emacs)
    :prefix "SPC")

  (ngs/leader-def
    "f"   '(:ignore t :which-key "find")
    "SPC" '(consult-buffer :which-key "buffer")
    "/"   '(consult-line :which-key "line")
    "ff"  '(project-find-file :which-key "file")
    "fl"  '(consult-line :which-key "line")
    "fr"  '(consult-recent-file :which-key "recent")
    "fb"  '(consult-buffer :which-key "buffer")

    "p"  '(:ignore t :which-key "project")
    "pf" '(project-find-file :which-key "find file")
    "pk" '(project-kill-buffers :which-key "kill buffers")
    "pp" '(project-switch-project :which-key "switch")

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

    "t"  '(:ignore t :which-key "toggles")
    "tt" '(treemacs :which-key "treemacs")))

;; Wrap lines nicely when editing prose
(add-hook 'text-mode-hook 'visual-line-mode)

;; Automatically pair parentheses, et al.
;; (electric-pair-mode t)
(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))

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
  ;; :init
  ;; (centaur-tabs-mode))

;; Full-fledged terminal emulation
(use-package vterm)

;; Display icons
(use-package all-the-icons
  :if (display-graphic-p)
  :init (setq all-the-icons-scale-factor 1.0))

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

;; TODO: Use ELisp-powered snippets
(use-package yasnippet)

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

;; Visually differentiate ephemeral windows
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
(use-package evil-nerd-commenter)
  ;; :after (general)
  ;; :config
  ;; (general-nmap  "gc" 'evilnc-comment-or-uncomment-lines))

;; Efficient cursor movement
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

;; "Buffers Encapsulated in Frames Realise Advanced Management of Emacs"
;; Isolate buffers per frame
(use-package beframe
  :config
  (beframe-mode 1))

;; Font ligatures
(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package poly-markdown)
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
