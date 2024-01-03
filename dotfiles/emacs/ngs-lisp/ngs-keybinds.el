;;; ngs-keybinds.el -*- lexical-binding: t -*-

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

;;; Commentary:

;; Customizations for the human-keyboard interface.

;;; Code:

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer ngs-leader-def
    :keymaps '(normal emacs)
    :prefix "SPC")

  (general-create-definer ngs-local-leader-def
    :keymaps '(normal emacs)
    :prefix ","))

(ngs-leader-def
  "a" '(:ignore a :which-key "apps")
  "f" '(:ignore f :which-key "find")
  "g" '(:ignore g :which-key "git")
  "h" '(:ignore h :which-key "help")
  "t" '(:ignore t :which-key "toggles"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(if (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq mac-right-option-modifier 'alt))

(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

(ngs-leader-def
  "n"  '(:ignore n :which-key "narrow")
  "ne" '(sp-narrow-to-sexp n :which-key "sexp")
  "nn" '(narrow-to-defun :which-key "defun")
  "np" '(narrow-to-page n :which-key "page")
  "nr" '(narrow-to-region n :which-key "region")
  "nw" '(widen n :which-key "widen"))

(provide 'ngs-keybinds)
;;; ngs-keybinds.el ends here
