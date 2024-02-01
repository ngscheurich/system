;;; Git VCS helpers -*- lexical-binding: t -*-

(use-package magit
  :config
  (ngs-leader-def
    "gg" '(magit-status :which-key "git")
    "gb" '(magit-blame :which-key "blame")
    "gl" '(magit-log :which-key "log")))

(provide 'ngs-git)
