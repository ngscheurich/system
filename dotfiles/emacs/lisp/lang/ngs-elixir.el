;; The Elixir programming language -*- lexical-binding: t -*-

(use-package elixir-ts-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "elixir-ls"))
  :hook (elixir-ts-mode . eglot-ensure)
  :mode
  (("\\.ex\\'" . elixir-ts-mode)
   ("\\.exs\\'" . elixir-ts-mode)))

(provide 'ngs-elixir)
