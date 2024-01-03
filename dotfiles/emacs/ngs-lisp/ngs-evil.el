;;; ngs-evil.el -*- lexical-binding: t -*-

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

;; Because Vim is built into my brain.

;;; Code:

(use-package evil
  :custom
  (evil-echo-state nil "Don't display the Evil state in the echo area")
  (evil-undo-system 'undo-redo "Use the Emacs-native undo/redo functionality")
  (evil-want-C-i-jump t "'C-i' to jump to next location")
  (evil-want-C-u-scroll t "'C-u' to scroll by half a page")
  (evil-want-Y-yank-to-eol t "'Y' to yank to the end of the line")
  (evil-want-keybinding nil "Don't load bindings for additional modes (see 'Evil Collection')")
  :general
  (general-nmap
    "<up>" 'evil-window-up
    "<down>" 'evil-window-down
    "<left>" 'evil-window-left
    "<right>" 'evil-window-right
    "-" 'dired-jump)
  :config
  (global-set-key (kbd "C-M-u") 'universal-argument)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :diminish
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-cleverparens
  :diminish
  :after evil
  :config
  (evil-cleverparens-mode))

(use-package evil-goggles
  :diminish
  :init
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(provide 'ngs-evil)
;;; ngs-evil.el ends here
