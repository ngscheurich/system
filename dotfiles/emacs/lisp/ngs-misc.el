;; A collected miscellany -*- lexical-binding: t -*-

(use-package activities
  :config
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-c a n" . activities-new)
   ;; As resuming is expected to be one of the most commonly used
   ;; commands, this binding is one of the easiest to press.
   ("C-c a s" . activities-suspend)
   ("C-c a a" . activities-resume)
   ("C-c a k" . activities-kill)
   ;; This binding mirrors, e.g. "C-x t RET".
   ("C-c a RET" . activities-switch)
   ("C-c a g" . activities-revert)
   ("C-c a l" . activities-list)))

(use-package spacious-padding
  :init
  (setq spacious-padding-widths
        '(:internal-border-width 0
          :header-line-width 4
          :mode-line-width 4
          :tab-width 4
          :right-divider-width 16
          :scroll-bar-width 8))

  :config
  (spacious-padding-mode))

(use-package tab-bar
  :straight nil
  :config (tab-bar-mode))

(provide 'ngs-misc)
