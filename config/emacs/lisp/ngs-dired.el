;; Dired customizations -*- lexical-binding: t; -*-

;; Emacs directory editor
(use-package dired
  :straight nil
  :init
  (setq trash-directory "~/.Trash"
        delete-by-moving-to-trash t))

;; (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

(provide 'ngs-dired)
