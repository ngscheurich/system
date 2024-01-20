;; Emacs initialization -*- lexical-binding: t; -*-

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp/lang"))

(require 'ngs-elpaca)

(require 'ngs-core)
(require 'ngs-theme)
(require 'ngs-mode-line)
(require 'ngs-keybinds)
(require 'ngs-evil)
(require 'ngs-buffers)
(require 'ngs-dired)
(require 'ngs-completion)
(require 'ngs-env)
(require 'ngs-shell)
(require 'ngs-discover)

; (require 'ngs-lang)
(require 'ngs-elixir)
; (require 'ngs-typescript)
; (require 'ngs-gdscript)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

; (require 'ngs-git)
; (require 'ngs-misc)
; (require 'ngs-org)
; (require 'ngs-prog)
