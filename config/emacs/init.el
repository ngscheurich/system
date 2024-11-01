;;; GNU Emacs initialization

(add-to-list 'load-path (locate-user-emacs-file "ngs-lisp"))

(require 'ngs-env)
(require 'ngs-core)

(require 'ngs-buffers)
(require 'ngs-complete)
(require 'ngs-edit)
(require 'ngs-git)
(require 'ngs-help)
(require 'ngs-minibuf)
(require 'ngs-navigate)
(require 'ngs-prog)
(require 'ngs-shell)
(require 'ngs-snippets)
(require 'ngs-theme)

(require 'ngs-meow)
(require 'ngs-evil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'downcase-region 'disabled nil)

(use-package project
  :bind ("C-x p r" . project-find-regexp))

(use-package keycast
  :ensure t)

(use-package polymode
  :ensure t
  :mode ("\.ex$" . poly-elixir-markdown-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-ts-mode)
  (define-innermode poly-markdown-doc-elixir-innermode
    :mode 'markdown-ts-mode
    :head-matcher "@doc \"\"\""
    :tail-matcher "\"\"\""
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-elixir-doc-elixir-innermode
    :mode 'elixir-ts-mode
    :head-matcher "## Examples"
    :tail-matcher "\"\"\""
    :head-mode 'poly-markdown-doc-elixir-innermode
    :tail-mode 'poly-markdown-doc-elixir-innermode)
  (define-polymode poly-elixir-markdown-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-markdown-doc-elixir-innermode
                  poly-elixir-doc-elixir-innermode)))
