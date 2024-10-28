;; =====================================================================
;;  avy
;; ---------------------------------------------------------------------
;;  Jump to things in Emacs tree-style 
;;  https://github.com/abo-abo/avy
;; ---------------------------------------------------------------------
(use-package avy
  :ensure t
  :bind ("M-j" . avy-goto-char-timer))

(provide 'ngs-navigate)
