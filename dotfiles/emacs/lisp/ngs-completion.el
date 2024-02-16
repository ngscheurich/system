;; Enhancements to completion systems -*- lexical-binding: t -*-

;; VERTical Interactive COmpletion
;; https://github.com/minad/vertico
(use-package vertico
  :config (vertico-mode))

;; Rich annotations for minibuffer completion 
;; https://github.com/minad/marginalia
(use-package marginalia
  :config (marginalia-mode))

;; Additional completing-read sources
;; https://github.com/minad/consult
(use-package consult
  :bind
  (("C-c f l" . consult-line)
   ("C-c f b" . consult-buffer)
   ("C-c f g" . consult-ripgrep)
   ("C-c f i" . consult-imenu)
   ("C-c f r" . consult-recent-file)
   ("C-c f q" . consult-flymake)))

;;; COmpletion in Region FUnction
;; https://github.com/minad/corfu
(use-package corfu
  :load-path "~/.config/emacs/straight/builds/corfu/extensions"

  :custom
  (corfu-auto t)

  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))

  :config
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; Completion At Point Extensions
;; https://github.com/minad/cape
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Completion style that matches multiple regexps in any order
;; https://github.com/oantolin/orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(provide 'ngs-completion)
