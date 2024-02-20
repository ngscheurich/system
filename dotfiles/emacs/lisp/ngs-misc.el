;; A collected miscellany -*- lexical-binding: t -*-

;; https://github.com/alphapapa/activities.el
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

;; https://protesilaos.com/emacs/spacious-padding
(use-package spacious-padding
  :init
  (setq spacious-padding-widths
        '(:internal-border-width 16
          :header-line-width 4
          :mode-line-width 3
          :tab-width 4
          :right-divider-width 16
          :scroll-bar-width 8))

  :config
  (spacious-padding-mode))

(defvar-keymap ngs-leader-map
  :doc "My prefix key map."
  "SPC" #'consult-buffer
  "/" #'consult-line
  "." #'find-file
  "," #'recentf)

(keymap-set evil-normal-state-map "SPC" ngs-leader-map)

;; https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :config (global-page-break-lines-mode))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :init
  (setq dashboard-display-icons-p nil) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  (setq dashboard-set-heading-icons nil)
  (setq dashboard-set-file-icons nil)
  (setq dashboard-center-content nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-set-footer t)
  (setq dashboard-footer-messages '("Dashboard is pretty cool!"))
  ;; (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
  ;;                                                  :height 1.1
  ;;                                                  :v-adjust -0.05
  ;;                                                  :face 'font-lock-keyword-face))
  :config
  (dashboard-setup-startup-hook))

;; https://github.com/jdtsmith/eglot-booster
;; (use-package eglot-booster
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

;; https://github.com/jdtsmith/indent-bars
;; (use-package indent-bars
;;   :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
;;   :hook (prog-mode . indent-bars-mode)) ; or whichever modes you prefer

;; https://github.com/JasZhe/window-stool
;; https://github.com/jasonmj/.emacs.d/blob/main/config-org/editing.org#window-stool
;; (use-package window-stool
;;   :straight
;;   '(window-stool :host github :repo "jaszhe/window-stool") 
;;   :config
;;   (add-hook 'prog-mode-hook #'window-stool-mode))

;; https://github.com/alphapapa/bufler.el
;; (use-package bufler
;;   :straight '(bufler :host github :repo "alphapapa/bufler.el"))

;; https://github.com/jasonmj/.emacs.d/blob/main/config-org/dired.org
;; (use-package dired-single
;;   :after dired)

;; https://github.com/Fuco1/dired-hacks/tree/master?tab=readme-ov-file#dired-subtree
;; (use-package dired-subtree
;;   :after dired)

;; https://github.com/SpecialBomb/emacs-modern-fringes
;; (use-package modern-fringes)

(provide 'ngs-misc)
