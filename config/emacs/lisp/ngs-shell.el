;; Customizations for the built-in shells -*- lexical-binding: t; -*-

(setq explicit-shell-file-name "/etc/profiles/per-user/nick/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))

(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))

(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(provide 'ngs-shell)
