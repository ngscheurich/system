;;; ngs-prog.el --- Modal keyboard setup -*- lexical-binding: t; -*-

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

;; Affordances for modal keyboard interaction.

;;; Code:
(use-package envrc
  :ensure t
  :config (envrc-global-mode))

;; Tree-sitter grammars
(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	(heex "https://github.com/phoenixframework/tree-sitter-heex")))

;; Elixir
(use-package elixir-ts-mode
  :ensure t
  :after eglot
  :init
  (add-to-list 'eglot-server-programs
	       `((elixir-ts-mode heex-ts-mode) . ("~/.local/share/elixir-ls/language_server.sh")))
  (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
  (add-hook 'heex-ts-mode-hook 'eglot-ensure)
  :mode
  (("\\.ex\\'" . elixir-ts-mode)
   ("\\.exs\\'" . elixir-ts-mode)))


(provide 'ngs-prog)
;;; ngs-prog.el ends here
