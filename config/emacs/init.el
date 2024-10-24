;;; GNU Emacs initialization

;; Emacs settings
(use-package emacs
  :init
  (setopt user-full-name "Nicholas Scheurich"
          user-email-address "nick@scheurich.haus"
          history-length 40
          custom-file (locate-user-emacs-file "custom-vars.el")))

;; Enable package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load configuration modules
(add-to-list 'load-path (locate-user-emacs-file "ngs-lisp"))

(require 'ngs-emacs-core)
(require 'ngs-completion)
(require 'ngs-modality)

(use-package :meow)

; ;; Automatically insert closing delimeter
; (use-package elec-pair
;   :config (electric-pair-mode))
;
; ;; Allows undo/redo of window changes 
; (use-package winner
;   :config (winner-mode 1))
;
; ;; Built-in tab bar
; (use-package tab-bar
;   :config (tab-bar-mode))
