;; Discoverability affordances -*- lexical-binding: t -*-

;; Re-bind help key mapping
(global-set-key (kbd "C-c h") 'help-command)

;; A better *help* buffer
;; https://codeberg.org/akib/emacs-eat
(use-package helpful
  :config
  (general-nmap
    "C-c h h" 'helpful-at-point
    "C-c h f" 'helpful-callable
    "C-c h c" 'helpful-command
    "C-c h k" 'helpful-key
    "C-c h v" 'helpful-variable)

  (ngs-leader-def
    "hh" '(helpful-at-point :which-key "at point")
    "hf" '(helpful-callable :which-key "function")
    "hc" '(helpful-command :which-key "command")
    "hk" '(helpful-key :which-key "key")
    "hv" '(helpful-variable :which-key "variable")
    "hm" '(describe-mode :which-key "mode")))

;; Display possible next keys for incomplete commands
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

(provide 'ngs-discover)
