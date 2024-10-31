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

(use-package casual-suite
  :ensure t
  :bind
  (:map dired-mode-map
   ("C-o" . casual-dired-tmenu)
   :map ibuffer-mode-map
   ("C-o" . casual-ibuffer-tmenu)
   :map Info-mode-map
   ("C-o" . casual-info-tmenu)
   :map isearch-mode-map
   ("C-o" . casual-isearch-tmenu)
   :map reb-mode-map
   ("C-o" . casual-re-builder-tmenu)))

(use-package spacious-padding
  :ensure t
  :init
  (setopt spacious-padding-widths
          '(:internal-border-width 15
            :header-line-width 4
            :mode-line-width 3
            :tab-width 3
            :right-divider-width 15
            :scroll-bar-width 4
            :fringe-width 4)
          spacious-padding-subtle-mode-line nil)
  :config
  (spacious-padding-mode))

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
