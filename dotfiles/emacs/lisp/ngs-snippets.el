;; Snippets support; -*- lexical-binding: t -*-

;; Template system
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :init
  (setq yas-snippet-dirs
        `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1)
  (diminish 'yas-minor-mode))

(provide 'ngs-snippets)
