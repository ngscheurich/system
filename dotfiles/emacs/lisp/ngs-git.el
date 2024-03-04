;;; Git VCS helpers -*- lexical-binding: t; -*-

(use-package magit
  :bind
  (("C-c g g" . 'magit-status)
    ("C-c g b" . 'magit-blame)
    ("C-c g l" . 'magit-log)))

(provide 'ngs-git)
