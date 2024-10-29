;;; GNU Emacs initialization

(add-to-list 'load-path (locate-user-emacs-file "ngs-lisp"))

(require 'ngs-env)
(require 'ngs-core)
(require 'ngs-buffers)
(require 'ngs-complete)
(require 'ngs-git)
(require 'ngs-help)
(require 'ngs-minibuf)
(require 'ngs-navigate)
(require 'ngs-prog)
(require 'ngs-shell)
(require 'ngs-snippets)
(require 'ngs-theme)

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)))b
   
