;;; ngs-prog.el --- Programming setup -*- lexical-binding: t; -*-

;; Copyright (C) 2024 N. G. Scheurich

;; Author: N. G. Scheurich <nick@scheurich.haus>
;; URL: https://scheurich.haus/emacs
;; Package-Requires: ((emacs "29.4"))

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

;; Programming setup for my Emacs configuration.

;;; Code:

;; Tree-sitter grammars
(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	      (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                  "split_parser"
                  "tree-sitter-markdown/src")
        (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                         "split_parser"
                         "tree-sitter-markdown-inline/src")))

;; Indentation settings
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Display numbers in programming buffers
(use-package display-line-numbers
  :hook prog-mode)

;; Language Server Protocol (LSP) client
(use-package eglot
  :bind
  (("C-c l f" . 'eglot-format-buffer)
   ("C-c l a" . 'eglot-code-actions)
   ("C-c l r" . 'eglot-rename)))

;; Show documentation in the minibuffer
(use-package eldoc
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

;; =====================================================================
;;  ElDoc box
;; ---------------------------------------------------------------------
;;  Childframe doc for eglot and anything that uses Eldoc
;;  https://github.com/casouri/eldoc-box
;; ---------------------------------------------------------------------
(use-package eldoc-box
  :ensure t
  :bind ("C-c d" . 'eldoc-box-help-at-point))

;; =====================================================================
;;  elixir-ts-mode
;; ---------------------------------------------------------------------
;;  Elixir major mode using Tree-sitter 
;;  https://github.com/wkirschbaum/elixir-ts-mode
;; ---------------------------------------------------------------------
(use-package elixir-ts-mode
  :ensure t
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
	             `((elixir-ts-mode heex-ts-mode) .
                 ("/Users/nscheurich/.local/share/nvim/mason/bin/lexical")))
  (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
  (add-hook 'heex-ts-mode-hook 'eglot-ensure)
  :mode
  (("\\.ex\\'" . elixir-ts-mode)
   ("\\.exs\\'" . elixir-ts-mode)))

;; =====================================================================
;;  exunit
;; ---------------------------------------------------------------------
;;  Emacs ExUnit test runner
;;  https://github.com/ananthakumaran/exunit.el
;; ---------------------------------------------------------------------
(use-package exunit
  :ensure t
  :hook elixir-ts-mode)

;; =====================================================================
;;  markdown-ts-mode
;; ---------------------------------------------------------------------
;;  Markdown major mode using Tree-sitter 
;;  https://github.com/LionyxML/markdown-ts-mode
;; ---------------------------------------------------------------------
(use-package markdown-ts-mode
  :ensure t)

;; =====================================================================
;;  nix-mode
;; ---------------------------------------------------------------------
;;  An Emacs major mode for editing Nix expressions 
;;  https://github.com/NixOS/nix-mode
;; ---------------------------------------------------------------------
(use-package nix-mode
  :mode "\\.nix\\'")

(provide 'ngs-prog)
;;; ngs-prog.el ends here
