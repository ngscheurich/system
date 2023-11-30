;;; ngs-prog.el -*- lexical-binding: t -*-

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

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package elixir-ts-mode)
(use-package json-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package nix-mode :mode "\\.nix\\'")

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode")
  :hook (gdscript-mode . eglot-ensure))

(use-package eglot
  :config
  ; (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  (add-to-list 'eglot-server-programs '(sql-mode "sql-language-server" "up" "--method" "stdio"))
  (add-to-list 'eglot-server-programs '(javascript-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(javascript-mode "tailwindcss-language-server" "--stdio"))
  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   "K" 'eldoc-box-help-at-point)
  (ngs-local-leader-def
    "k" 'eldoc-doc-buffer
    "f" 'eglot-format-buffer)
  :hook
  ((elixir-ts-mode . eglot-ensure)
   (sql-mode . eglot-ensure)
   (javascript-mode . eglot-ensure)))

(use-package eldoc
  :ensure nil
  :diminish
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box)

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :init
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer))

(use-package eldoc-box
  :config
  (general-define-key
   :states 'normal
   :keymap global-map
   "K" 'eldoc-box-help-at-point))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(provide 'ngs-prog)
;;; ngs-prog.el ends here
