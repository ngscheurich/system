;; Programming languages support -*- lexical-binding: t -*-

(add-to-list 'load-path (locate-user-emacs-file "lisp/lang"))

(require 'ngs-elixir)
(require 'ngs-typescript)
(require 'ngs-gdscript)

(use-package json-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package nix-mode :mode "\\.nix\\'")

(provide 'ngs-lang)
