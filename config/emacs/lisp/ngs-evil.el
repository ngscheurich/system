;; Evil mode customizations -*- lexical-binding: t; -*-

;; The extensible vi layer for Emacs
;; https://github.com/emacs-evil/evil
(use-package evil
  :custom
  (evil-echo-state nil "Don't display the Evil state in the echo area")
  (evil-undo-system 'undo-redo "Use the Emacs-native undo/redo functionality")
  (evil-want-C-i-jump t "`C-i' to jump to next location")
  (evil-want-C-u-scroll t "`C-u' to scroll by half a page")
  (evil-want-Y-yank-to-eol t "`Y' to yank to the end of the line")
  (evil-want-keybinding nil "Don't load bindings for additional modes")

  :bind
  (:map evil-normal-state-map
    ("<up>" . 'evil-window-up)
    ("<down>" . 'evil-window-down)
    ("<left>" . 'evil-window-left)
    ("<right>" . 'evil-window-right)
    ("-" . 'dired-jump))

  :config
  (evil-mode 1))

;; Evil bindings for additional packages and UIs
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Emulates surround.vim
;; https://github.com/tpope/vim-surround
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Emulates commetary.vim
;; https://github.com/tpope/vim-commentary
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Gives a quick visual indicator Evil operations
;; https://github.com/edkolev/evil-goggles
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(provide 'ngs-evil)
