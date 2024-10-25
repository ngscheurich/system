;;; GNU Emacs initialization

;; Add Nix bin directories to `exec-path'
(add-to-list 'exec-path "/run/current-system/sw/bin/")
(add-to-list 'exec-path
	     (format "/etc/profiles/per-user/%s/bin/" (getenv "USER")))

;; Emacs settings
(use-package emacs
  :init
  (setopt user-full-name "Nicholas Scheurich"
          user-email-address "nick@scheurich.haus"
          history-length 40
          custom-file (locate-user-emacs-file "custom-vars.el")
	  shell-file-name "zsh"
	  sh-shell "zsh"
	  sh-shell-file "zsh"))

;; Theme
(load-theme 'modus-vivendi)


;; Enable package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load configuration modules
(add-to-list 'load-path (locate-user-emacs-file "ngs-lisp"))

(require 'ngs-emacs-core)
(require 'ngs-completion)
(require 'ngs-prog)
