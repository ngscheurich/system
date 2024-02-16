;; Enhancements to completion systems -*- lexical-binding: t -*-

;; VERTical Interactive COmpletion
;; https://github.com/minad/vertico
(use-package vertico
  :init (vertico-mode))

;; Rich annotations for minibuffer completion 
;; https://github.com/minad/marginalia
(use-package marginalia
  :init (marginalia-mode))

;; Additional completing-read sources
;; https://github.com/minad/consult
(use-package consult
  :config
  (ngs-leader-def
    "/"   '(consult-line :which-key "lines")
    "SPC" '(consult-buffer :which-key "buffers")
    "fg"  '(consult-ripgrep :which-key "grep")
    "fi"  '(consult-imenu :which-key "items")
    "fr"  '(consult-recent-file :which-key "recents")
    "fq"  '(consult-flymake :which-key "errors")))

;;; COmpletion in Region FUnction
;; https://github.com/minad/corfu
(use-package corfu
  :load-path "~/.config/emacs/straight/builds/corfu/extensions"
  :custom
  (corfu-auto t)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; Completion At Point Extensions
;; https://github.com/minad/cape
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Completion style that matches multiple regexps in any order
;; https://github.com/oantolin/orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless)))

(provide 'ngs-completion)
