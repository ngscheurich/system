;;; Keyboard mappings -*- lexical-binding: t -*-

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer ngs-leader-def
    :keymaps '(normal emacs)
    :prefix "SPC")

  (general-create-definer ngs-local-leader-def
    :keymaps '(normal emacs)
    :prefix ",")

  (ngs-leader-def
    "a" '(:ignore a :which-key "apps")
    "f" '(:ignore f :which-key "find")
    "g" '(:ignore g :which-key "git")
    "h" '(:ignore h :which-key "help")
    "t" '(:ignore t :which-key "toggles"))

  (ngs-leader-def
    "n"  '(:ignore n :which-key "narrow")
    "ne" '(sp-narrow-to-sexp n :which-key "sexp")
    "nn" '(narrow-to-defun :which-key "defun")
    "np" '(narrow-to-page n :which-key "page")
    "nr" '(narrow-to-region n :which-key "region")
    "nw" '(widen n :which-key "widen")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(if (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq mac-right-option-modifier 'alt))

(provide 'ngs-keybinds)
