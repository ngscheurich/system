;; Convenient navigation mechanisms -*- lexical-binding: t; -*-

;; Jump to things via a narrowing tree
;; https://github.com/abo-abo/avy 
(use-package avy
  :after evil
  :bind (:map evil-normal-state-map
              ("s" . 'avy-goto-char-timer)
              ("f" . 'avy-goto-char-in-line)))

;; Quickly switch windows
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :config
  (setq aw-dispatch-always t
        aw-minibuffer-flag t))

;; Tree-based file explorer
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs)
(use-package treemacs-evil)

(provide 'ngs-navigation)
