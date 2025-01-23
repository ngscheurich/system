;;; GNU Emacs initialization

(add-to-list 'load-path (locate-user-emacs-file "ngs-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'ngs-env)
(require 'ngs-core)
(require 'ngs-buffers)
(require 'ngs-complete)
(require 'ngs-edit)
(require 'ngs-git)
(require 'ngs-help)
(require 'ngs-minibuf)
(require 'ngs-navigate)
(require 'ngs-notes)
(require 'ngs-prog)
(require 'ngs-shell)
(require 'ngs-snippets)
(require 'ngs-theme)

(require 'wgrep)

(use-package iedit
  :ensure t
  :bind ("C-c C-;" . iedit-mode))

(bind-key "C-x M-f" 'find-file-other-window)
(bind-key "C-x M-b" 'switch-to-buffer-other-window)
  
(use-package activities
  :ensure t
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(defun ngs-duplicate-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  (back-to-indentation))
