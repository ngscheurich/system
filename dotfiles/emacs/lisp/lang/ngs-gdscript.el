;; The GDScript programming language -*- lexical-binding: t -*-

(use-package gdscript-mode
  :after eglot
  :straight (gdscript-mode
             :host github
             :repo "godotengine/emacs-gdscript-mode")
  :hook (gdscript-mode . eglot-ensure))

(provide 'ngs-gdscript)
