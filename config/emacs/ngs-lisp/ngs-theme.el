;;; ngs-theme.el --- Typography, colors, iconography  -*- lexical-binding: t; -*-

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

;; Typography, colors, and iconography for my Emacs configuration.

;;; Code:

(add-to-list 'default-frame-alist '(font . "Iosevka Comfy-13"))
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
(setopt cursor-type 'hbar
        default-line-spacing 0.2)

;; =====================================================================
;;  Modus Themes
;; ---------------------------------------------------------------------
;;  Highly accessible themes for GNU Emacs
;;  https://protesilaos.com/emacs/modus-themes
;; ---------------------------------------------------------------------
(use-package modus-themes
  :ensure t
  :config
  (modus-themes-select 'modus-operandi-tinted))

;; =====================================================================
;;  Typographic Ligatures in Emacs
;; ---------------------------------------------------------------------
;;  Display typographical ligatures in Emacs 
;;  https://github.com/mickeynp/ligature.el
;; ---------------------------------------------------------------------
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   'prog-mode
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
     "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
     "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

;; =====================================================================
;;  nerd-icons.el
;; ---------------------------------------------------------------------
;;  A library for Nerd Font icons
;;  https://github.com/rainstormstudio/nerd-icons.el
;; ---------------------------------------------------------------------
(use-package nerd-icons
  :ensure )

;; =====================================================================
;; tsill pla nerd-icons-dired
;; ---------------------------------------------------------------------
;;  Use Nerd Font icons in dired 
;;  https://github.com/rainstormstudio/nerd-icons-dired
;; ---------------------------------------------------------------------
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; =====================================================================
;;  nerd-icons-completion
;; ---------------------------------------------------------------------
;;  User Nerd Font icons in completion interfaces 
;;  https://github.com/rainstormstudio/nerd-icons-completion
;; ---------------------------------------------------------------------
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; =====================================================================
;;  kind-icon
;; ---------------------------------------------------------------------
;;  Completion kind text/icon prefix labelling for in-region completion 
;;  https://github.com/jdtsmith/kind-icon
;; ---------------------------------------------------------------------
(use-package kind-icon
  :ensure t
  :after corfu
  :init
  (setopt kind-icon-default-face 'corfu-default
          kind-icon-blend-background t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; =====================================================================
;;  Pulsar
;; ---------------------------------------------------------------------
;;   Pulsar Unquestionably Luminates, Strictly Absent the Radiation
;;  https://protesilaos.com/emacs/pulsar
;; ---------------------------------------------------------------------
(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode 1))

(provide 'ngs-theme)
;;; ngs-theme.el ends here