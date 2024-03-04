;; Discoverability affordances -*- lexical-binding: t; -*-

;; Re-bind help key mapping
(keymap-global-set "C-c h" 'help-command)

;; A better *help* buffer
;; https://codeberg.org/akib/emacs-eat
(use-package helpful
  :bind
  (("C-c h h" . 'helpful-at-point)
    ("C-c h f" . 'helpful-callable)
    ("C-c h c" . 'helpful-command)
    ("C-c h k" . 'helpful-key)
    ("C-c h v" . 'helpful-variable)))

;; Display possible next keys for incomplete commands
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode 1))

(provide 'ngs-help)
