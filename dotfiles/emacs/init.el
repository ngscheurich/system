;;; init.el -*- lexical-binding: t -*-

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

;;; Code:

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(add-to-list 'load-path (locate-user-emacs-file "ngs-lisp"))

(require 'ngs-keybinds)
(require 'ngs-buffers)
(require 'ngs-completion)
(require 'ngs-emacs)
(require 'ngs-env)
(require 'ngs-evil)
(require 'ngs-git)
(require 'ngs-misc)
(require 'ngs-org)
(require 'ngs-prog)
(require 'ngs-term)
(require 'ngs-theme)
(require 'ngs-mode-line)
;;; init.el ends here
