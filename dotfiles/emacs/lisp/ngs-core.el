;; Core Emacs customizations -*- lexical-binding: t -*-

;; Emacs settings
(use-package emacs
  :straight nil
  :custom
  (user-full-name "Nicholas Scheurich" "My full name")
  (user-email-address "nick@scheurich.haus" "My personal email address")
  (custom-file (locate-user-emacs-file "custom-vars.el") "Set custom file path")
  (window-resize-pixelwise t "Resize windows by pixels rather than characters")
  (frame-resize-pixelwise t "Resize frames by pixels rather than characters")
  (history-length 40 "Maximum number of files to keep in history")
  (inhibit-startup-message t "Bypass the Emacs splash screen")
  (message-kill-buffer-query nil "Don't ask to confirm buffer kill")
  (ring-bell-function 'ignore "Suppress the bell")
  (use-short-answers t "Abbreviate yes/no input")
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (unless (eq system-type 'darwin)
    (menu-bar-mode -1)))

;; Keeps track of opened files
(use-package recentf
  :straight nil
  :config
  (recentf-mode t))

;; Allows undo/redo of window changes 
(use-package winner
  :straight nil
  :config
  (winner-mode 1))

;; Saves minibuffer history
(use-package savehist
  :straight nil
  :config
  (setq history-length 40)
  (savehist-mode))

;; Temporary files
(use-package no-littering
  :config
  (defvar ngs-auto-save-directory
    (no-littering-expand-var-file-name "auto-save/")
    "Directory in which auto-save files should be stored")

  (defvar ngs-backup-directory
    (no-littering-expand-var-file-name "backup/")
    "Directory in which backup files should be stored")

  (mapcar (lambda (dir)
            (unless (file-directory-p dir)
              (make-directory dir)))
          `(,ngs-auto-save-directory ,ngs-backup-directory))

  (setq auto-save-file-name-transforms
        `((".*" ,ngs-auto-save-directory t)))

  (setq backup-directory-alist
        `((".*" . ,ngs-backup-directory))))

(provide 'ngs-core)
