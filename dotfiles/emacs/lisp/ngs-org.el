;;; ngs-org.el -*- lexical-binding: t -*-

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

;; Customizations for Org mode.

;;; Code:

(defun ngs-org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      "/etc/system/dotfiles/emacs/config.org")
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (message "Tangled config files."))))

(add-hook 'org-mode-hook (lambda ()
   (add-hook 'after-save-hook #'ngs-org-babel-tangle-config)))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(defun ngs-org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ngs-org-mode-visual-fill))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(use-package imenu-list)

(use-package org-imenu
  :straight
  (org-imenu :host github
             :repo "rougier/org-imenu"))

(provide 'ngs-org)
;;; ngs-org.el ends here
