;; The Elixir programming language -*- lexical-binding: t; -*-

(use-package elixir-ts-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "/Users/nick/Projects/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
  :hook (elixir-ts-mode . eglot-ensure)
  :mode
  (("\\.ex\\'" . elixir-ts-mode)
   ("\\.exs\\'" . elixir-ts-mode)))

(provide 'ngs-elixir)
