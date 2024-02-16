;; Customizations for prose modes -*- lexical-binding: t -*-

(defun ngs-set-visual-fill ()
  (interactive)
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package visual-fill-column
  :hook ((org-mode . ngs-set-visual-fill)
         (markdown-mode . ngs-set-visual-fill)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(use-package imenu-list)

(use-package org-imenu
  :straight
  (org-imenu :host github
             :repo "rougier/org-imenu"))

(provide 'ngs-prose)
