;; A collected miscellany -*- lexical-binding: t -*-

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("=" default-text-scale-increase "larger")
  ("-" default-text-scale-decrease "smaller")
  ("0" default-text-scale-reset "reset")
  ("q" nil "quit" :exit t))

(ngs-leader-def
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package paredit
  :diminish
  :general
  (:states '(normal insert)
           "C-M-]" 'paredit-forward-slurp-sexp
           "C-M-[" 'paredit-backward-slurp-sexp)
  :hook
  (emacs-lisp-mode . paredit-mode))


(provide 'ngs-misc)
;;; ngs-misc.el ends here
