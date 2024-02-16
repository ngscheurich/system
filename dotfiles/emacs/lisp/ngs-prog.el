;; Customizations for programming -*- lexical-binding: t -*-

;; Tab settings
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; Display numbers in programming buffers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

;; Language Server Protocol (LSP) client
;; https://joaotavora.github.io/eglot/
(use-package eglot
  :straight nil
  :bind
  (("C-c l f" . 'eglot-format-buffer)
   ("C-c l a" . 'eglot-code-actions)
   ("C-c l r" . 'eglot-rename)))

;; Show documentation in the minibuffer
;; https://elpa.gnu.org/packages/eldoc.html
(use-package eldoc
  :straight nil
  :diminish
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

;; Display eldoc info in a childframe
;; https://github.com/casouri/eldoc-box
(use-package eldoc-box
  :bind ("K" . 'eldoc-box-help-at-point))

;; Language support
(add-to-list 'load-path (locate-user-emacs-file "lisp/lang"))

(require 'ngs-elixir)
(require 'ngs-emacs-lisp)
(require 'ngs-typescript)
(require 'ngs-gdscript)

(use-package json-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package nix-mode)

(provide 'ngs-prog)
