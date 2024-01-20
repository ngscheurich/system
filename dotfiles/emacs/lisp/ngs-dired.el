;; Dired customizations -*- lexical-binding: t -*-

;; Move files to trash when deleting
(setq trash-directory "~/.Trash")
(setq delete-by-moving-to-trash t)

;; (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

(provide 'ngs-dired)
