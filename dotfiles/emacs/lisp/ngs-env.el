;; Environment customizations -*- lexical-binding: t -*-

;; Inherit environment variables from $SHELL
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Buffer-local direnc integration
;; https://github.com/purcell/envrc
(use-package envrc
  :config
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
  (envrc-global-mode))

;; Streamlines system clipboard interaction
;; https://github.com/rolandwalker/simpleclip
(use-package simpleclip
   :bind
   (("M-c" . simpleclip-copy)
    ("M-v" . simpleclip-paste))
   :config
   (simpleclip-mode 1))

(provide 'ngs-env)
