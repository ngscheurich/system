;; -*- lexical-binding: t -*-

(use-package gdscript-mode
             :elpaca (gdscript-mode
                       :host github
                       :repo "godotengine/emacs-gdscript-mode")
  :hook (gdscript-mode . eglot-ensure))

(provide 'ngs-gdscript)
