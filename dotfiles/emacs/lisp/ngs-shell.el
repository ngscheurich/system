;; Improvements to the command-line experience -*- lexical-binding: t -*-

;; ;; Emulate A Terminal
;; ;; https://codeberg.org/akib/emacs-eat
;; (use-package eat)

(use-package vterm)
(use-package multi-vterm)

;; Use Zsh as the default shell for shell-mode
(setq explicit-shell-file-name "/etc/profiles/per-user/nick/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(provide 'ngs-shell)
