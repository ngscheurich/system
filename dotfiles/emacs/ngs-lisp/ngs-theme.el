;;; ngs-theme.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 N. G. Scheurich

;; Author: N. G. Scheurich <nick@scheurich.haus>
;; URL: https://nick.scheurich.haus/system
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless (eq system-type 'darwin)
    (menu-bar-mode -1))

(set-face-attribute 'default nil :font "Berkeley Mono" :height 120)
(setq-default line-spacing 0.4)

(use-package modus-themes
  :init
  (defun ngs-modus-themes-custom-faces ()
    (interactive)
    (let ((width (if current-prefix-arg
                    current-prefix-arg
                  4)))
      (modus-themes-with-colors
        (custom-set-faces
        `(mode-line ((,c :box (:line-width ,width :color ,bg-mode-line-active))))
        `(mode-line-inactive ((,c :box (:line-width ,width :color ,bg-mode-line-inactive)))))))))

(use-package circadian
  :config
  (setq circadian-themes '(("07:30" . modus-vivendi-tinted)
                          ("18:30" . modus-vivendi-tinted)))
  (circadian-setup))

(add-hook 'circadian-after-load-theme-hook
          #'(lambda (theme)
              (print "Circadian!")
              ;; Line numbers appearance
              (setq linum-format 'linum-format-func)
              ;; Cursor
              (set-default 'cursor-type 'box)
              (set-cursor-color "#F52503")))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(setq column-number-mode t)

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'visual-line-mode))

(use-package default-text-scale)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("=" default-text-scale-increase "larger")
  ("-" default-text-scale-decrease "smaller")
  ("0" default-text-scale-reset "reset")
  ("q" nil "quit" :exit t))

(ngs-leader-def
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(provide 'ngs-theme)
;;; ngs-theme.el ends here
