;;; ngs-misc.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 N. G. Scheurich

;; Author: N. G. Scheurich <nick@scheurich.haus>
;; URL: https://nick.scheurich.haus/system
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.0"))

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

;;; Commentary:

;; A collected miscellany.

;;; Code:

(setq column-number-mode t)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("=" default-text-scale-increase "larger")
  ("-" default-text-scale-decrease "smaller")
  ("0" default-text-scale-reset "reset")
  ("q" nil "quit" :exit t))

(ngs-leader-def
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'visual-line-mode))

(use-package treemacs)

(use-package paredit
  :diminish
  :general
  (:states '(normal insert)
           "C-M-]" 'paredit-forward-slurp-sexp
           "C-M-[" 'paredit-backward-slurp-sexp)
  :hook
  (emacs-lisp-mode . paredit-mode))

(use-package ace-window
  :general
  (ngs-leader-def
    "w"  '(ace-window :which-key "windows"))
  :config
  (setq aw-dispatch-always t
        aw-minibuffer-flag t))

(ngs-leader-def
  "hK" '(describe-keymap :which-key "keymap")
  "hm" '(describe-mode :which-key "mode"))

(use-package helpful
  :config
  (ngs-leader-def
    "hh" '(helpful-at-point :which-key "at point")
    "hf" '(helpful-callable :which-key "function")
    "hc" '(helpful-command :which-key "command")
    "hk" '(helpful-key :which-key "key")
    "hv" '(helpful-variable :which-key "variable")))

(use-package avy
  :after evil
  :bind (:map evil-normal-state-map
              ("s" . 'avy-goto-char-2)
              ("f" . 'avy-goto-char-in-line)))

(provide 'ngs-misc)
;;; ngs-misc.el ends here
