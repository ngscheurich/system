;; Programming languages support -*- lexical-binding: t -*-

(add-to-list 'load-path (locate-user-emacs-file "lisp/lang"))

(require 'ngs-elixir)
(require 'ngs-emacs-lisp)
(require 'ngs-typescript)
(require 'ngs-gdscript)

(use-package json-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package nix-mode)

;; Language Server Protocol support
(use-package)

(provide 'ngs-lang)
