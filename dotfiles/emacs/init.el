;; Emacs initialization -*- lexical-binding: t -*-

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'ngs-straight)

(require 'ngs-core)
(require 'ngs-keybinds)

(require 'ngs-buffers)
(require 'ngs-completion)
(require 'ngs-dired)
(require 'ngs-env)
(require 'ngs-evil)
(require 'ngs-git)
(require 'ngs-help)
(require 'ngs-lang)
(require 'ngs-mode-line)
(require 'ngs-navigation)
(require 'ngs-prog)
(require 'ngs-prose)
(require 'ngs-shell)
(require 'ngs-snippets)
(require 'ngs-term)
(require 'ngs-theme)
