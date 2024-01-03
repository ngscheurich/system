;;; ngs-emacs.el -*- lexical-binding: t -*-

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

;; Customization of Emacs and built-in packages.

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (custom-file (locate-user-emacs-file "custom-vars.el") "Set custom file path")
  (frame-resize-pixelwise t "Resize frames by pixels rather than characters")
  (gc-cons-threshold (* 2 1000 1000) "Make GC pauses faster by decreasing threshold")
  (history-length 40 "Maximum number of files to keep in history")
  (inhibit-startup-message t "Bypass the Emacs splash screen")
  (message-kill-buffer-query nil "Don't ask to confirm buffer kill")
  (ring-bell-function 'ignore "Suppress the bell")
  (use-short-answers t "Abbreviate yes/no input")
  (user-email-address "nick@scheurich.haus" "My personal email address")
  (user-full-name "Nicholas Scheurich" "My full name")
  (window-resize-pixelwise t "Resize windows by pixels rather than characters"))

(recentf-mode t)

(winner-mode 1)
(ngs-leader-def
  "u" '(winner-undo :which-key "winner-undo")
  "r" '(winner-redo :which-key "winner-redo"))

(use-package savehist
  :ensure nil
  :init
  (setq history-length 40)
  (savehist-mode))

(use-package project
  :ensure nil 
  :config
  (ngs-leader-def
    "pb" '(consult-project-buffer :which-key "buffers")
    "pf" '(project-find-file :which-key "find file")
    "pg" '(consult-ripgrep :which-key "grep")
    "pk" '(project-kill-buffers :which-key "kill buffers")
    "pp" '(project-switch-project :which-key "switch")
    "pt" '(multi-vterm-project :which-key "terminal")))

(provide 'ngs-emacs)
;;; ngs-emacs.el ends here
