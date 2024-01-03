(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; Elixir
(use-package elixir-ts-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "elixir-ls"))
  :hook (elixir-ts-mode . eglot-ensure)
  :mode
  (("\\.ex\\'" . elixir-ts-mode)
   ("\\.exs\\'" . elixir-ts-mode)))

;; TypeScript
(use-package typescript-ts-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode "typescript-language-server" "--stdio"))
  :hook (typescript-ts-mode . eglot-ensure)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)))

(use-package eglot
  :ensure nil
  :config
  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   "K" 'eldoc-box-help-at-point)
  (ngs-local-leader-def
    "k" 'eldoc-doc-buffer
    "f" 'eglot-format-buffer))

(use-package eldoc
  :ensure nil
  :diminish
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box)

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :init
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer))

(use-package eldoc-box
  :config
  (general-define-key
   :states 'normal
   :keymap global-map
   "K" 'eldoc-box-help-at-point))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package json-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package nix-mode :mode "\\.nix\\'")

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode")
  :hook (gdscript-mode . eglot-ensure))

(provide 'ngs-prog)
