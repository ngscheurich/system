;; The Emacs Lisp dialect; -*- lexical-binding: t -*-

(use-package paredit
  :diminish
  :bind
  (("C-M-]" . 'paredit-forward-slurp-sexp)
   ("C-M-[" . 'paredit-backward-slurp-sexp))
  :hook
  (emacs-lisp-mode . paredit-mode))

(provide 'ngs-emacs-lisp)
