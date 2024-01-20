;; Customizations for the mode line -*- lexical-binding: t -*-

;; Show column number
(setq column-number-mode t)

;; Hide certain lighters
;; https://github.com/myrjola/diminish.el
(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'visual-line-mode))

(provide 'ngs-mode-line)
