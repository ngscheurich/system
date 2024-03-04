;; Buffer management; -*- lexical-binding: t -*-

;; Treat certain buffers as "ephemeral"
;; https://github.com/karthink/popper
(use-package popper
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)

  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type)))

(provide 'ngs-buffers)
