;;; ngs-competion.el -*- lexical-binding: t -*-

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

(use-package vertico
  :init (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :config
  (ngs-leader-def
    "/"  '(consult-line :which-key "lines")
    ","  '(consult-buffer :which-key "buffers")
    "fg" '(consult-ripgrep :which-key "grep")
    "fi" '(consult-imenu :which-key "items")
    "fr" '(consult-recent-file :which-key "recents")
    "fq" '(consult-flymake :which-key "errors")))

(use-package orderless
  :init
  (setq completion-styles '(orderless)))

(defvar ngs-corfu-extensions-directory
  (format "%sstraight/build/corfu/extensions" user-emacs-directory)
  "Path to Corfu’s included extensions")

(use-package corfu
  :load-path ngs-corfu-extensions-directory
  :custom
  (corfu-auto t)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package cape)

;; Enable Corfu in the minibuffer
(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay '(0 . 0))
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(provide 'ngs-completion)
;;; ngs-completion.el ends here
