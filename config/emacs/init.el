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

;; Scratchpad
(bind-key "C-x M-f" 'find-file-other-window)
(bind-key "C-x M-b" 'switch-to-buffer-other-window)

(defun ngs-duplicate-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  (back-to-indentation))

(use-package beframe
  :ensure t
  :init
  ;; This is the default
  (setopt beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
  :config
  (beframe-mode 1)
  (define-key global-map (kbd "C-c b") #'beframe-prefix-map))
