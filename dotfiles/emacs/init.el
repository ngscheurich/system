;;; init.el --- The Emacs Initialization File  -*- lexical-binding: t -*-

;; Author: N. G Scheurich <nick@scheurich.haus>
;; URL: https://github.com/ngscheurich/dotfiles
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.0"))

;;; Commentary
;; This file is loaded when Emacs is started, and is the conventional
;; place for one to configure their Emacs system.
;;
;; For more information about the Emac Initialization File, see
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html>.

;;; Code:

;; Profile Emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; Load literate config
(org-babel-load-file
  (expand-file-name "config.org" user-emacs-directory))

;; More useful *help*
(use-package helpful
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-command] . #'helpful-command)
  ([Remap describe-variable] . #'helpful-variable)
  ([remap describe-key] . #'helpful-key))

;; direnv integration
(use-package direnv
  :config
  (direnv-mode))

;; Keep folders tidy
;; See https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org#keep-folders-clean
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Show keys in mode line
(use-package keycast)

;; Maintain separate workspaces
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init
  (persp-mode))

;; Get Emacs path from shell $PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :bind
  (("C-." . embark-act)))
(use-package embark-consult)

;; Buffer tabs
(use-package centaur-tabs
  :bind (("M-}" . centaur-tabs-forward-tab)
         ("M-{" . centaur-tabs-backward-tab))
  :init
  (centaur-tabs-mode))

;; Git interface
(use-package magit)

;; EditorConfig integration
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; Format LSP buffers on save
(add-hook 'before-save-hook 'eglot-format-buffer)

;; REST client
;; https://github.com/pashky/restclient.el
(use-package restclient)

;; TODO: Check out tempel
(use-package yasnippet)

;; Icons
;; https://github.com/rainstormstudio/nerd-icons.el
;; https://github.com/rainstormstudio/nerd-icons-dired
;; https://github.com/rainstormstudio/treemacs-nerd-icons
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
;; (use-package nerd-icons-completion
;;   :config
;;   (nerd-icons-completion-mode))

;; Mode line


;;; init.el ends here
