;;; ngs-completion.el --- Enhancements to completion systems -*- lexical-binding: t; -*-

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

;; Enhancements to completion systems.

;;; Code:

;; VERTical Interactive COmpletion
;; https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :config (vertico-mode))

;; Rich annotations for minibuffer completion 
;; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :config (marginalia-mode))

;; Additional completing-read sources
;; https://github.com/minad/consult
(use-package consult
  :ensure t
  :bind
  (("C-c f l" . consult-line)
   ("C-c f b" . consult-buffer)
   ("C-c f g" . consult-ripgrep)
   ("C-c f i" . consult-imenu)
   ("C-c f r" . consult-recent-file)
   ("C-c f q" . consult-flymake)))

;;; COmpletion in Region FUnction
;; https://github.com/minad/corfu
(use-package corfu
  :ensure t
  ; :load-path "~/.config/emacs/straight/builds/corfu/extensions"
  :init
  (setopt corfu-auto t)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :config
  (global-corfu-mode))
  ; (require 'corfu-popupinfo)
  ; (corfu-popupinfo-mode)
  ; (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; Completion At Point Extensions
;; https://github.com/minad/cape
(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Completion style that matches multiple regexps in any order
;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(provide 'ngs-completion)
;;; ngs-completion.el ends here
