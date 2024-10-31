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
  (define-polymode poly-elixir-markdown-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-markdown-doc-elixir-innermode)))

(put 'downcase-region 'disabled nil)
