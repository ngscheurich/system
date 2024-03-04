;; Customizations for the mode line -*- lexical-binding: t; -*-

;; Show column number
(setq column-number-mode t)

;; Hide certain lighters
;; https://github.com/myrjola/diminish.el
(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'evil-commentary-mode)
  (diminish 'evil-goggles-mode)
  (diminish 'visual-line-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode))

;; Mode line format
(setq-default mode-line-format
      '("%e" mode-line-front-space
        (:propertize
         ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
         display
         (min-width
          (5.0)))
        mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
        (vc-mode vc-mode)
        mode-line-format-right-align
        "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(provide 'ngs-mode-line)
