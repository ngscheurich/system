;;; early-init.el -*- lexical-binding: t -*-

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

;; Customizations that are read before the GUI is initialized
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;; https://protesilaos.com/emacs/dotemacs#h:7b7b5898-09f7-4128-8af0-4041f67cb729

;;; Code:

;; Disable package manager
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
  (url-retrieve-synchronously
   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
   'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Frame resize settings
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Disable the bell
(setq ring-bell-function 'ignore)

;; y/n instead of yes/no
(setq use-short-answers t)

;; Disable startup (splash) screen
(setq inhibit-startup-screen t)

;; Hide menu bar, scroll bar, and tool bar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Hide menu bar, except on macOS (since it doesn't take up space in the frame)
(unless (eq system-type 'darwin)
    (menu-bar-mode -1))

;; TODO: Tune garbage collection threshold?
;; TODO: Initially set dark background color to avoid flash of light?
;; TODO: Name the default frame?
;;; early-init.el ends here
