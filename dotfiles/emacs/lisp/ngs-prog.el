;; Customizations for programming -*- lexical-binding: t -*-

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Language Server Protocol (LSP) client
;; https://joaotavora.github.io/eglot/
(use-package eglot
  :straight nil)

;; Show documentation in the minibuffer
;; https://elpa.gnu.org/packages/eldoc.html
(use-package eldoc
  :straight nil
  :diminish
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

;; Display eldoc info in a childframe
;; https://github.com/casouri/eldoc-box
(use-package eldoc-box
  :config
  (general-nmap "K" 'eldoc-box-help-at-point))

(provide 'ngs-prog)
