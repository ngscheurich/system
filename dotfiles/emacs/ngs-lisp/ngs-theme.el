;;; ngs-theme.el -*- lexical-binding: t -*-

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

;; Visual enhacements such as colors, typography, and iconography.

;;; Code:

;; Modus is a family of highly-legible themes
;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :init
  (defun my-modus-themes-custom-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :box (:line-width 4 :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 4 :color ,bg-mode-line-inactive))))))))

(add-hook 'modus-themes-after-load-theme-hook #'ngs-modus-themes-custom-faces)
(load-theme 'modus-vivendi-tinted t)
(my-modus-themes-custom-faces)

;; Circadian switches between light/dark themes based on time of day
;; https://github.com/GuidoSchmidt/circadian.el
;; (use-package circadian
;;   :config
;;   (setq circadian-themes '(("07:30" . modus-vivendi-tinted)
;;                            ("18:30" . modus-vivendi-tinted)))
;;   (circadian-setup))

;; Rainbow mode highlights color values with the corresponding color
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode)

;; Fontaine allows for detailed, on-demand font configurations
;; https://protesilaos.com/emacs/fontaine
(use-package fontaine
  :init
  (setq fontaine-presets
        '((regular)
          (t
           :default-family "Berkeley Mono"
           :default-weight regular
           :default-height 120
           :variable-pitch-family "IBM Plex Sans")))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

;; Increase line-spacing
(setq-default line-spacing 0.4)

;; Icons library
;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons)

;; Icons in dired buffers
;; https://github.com/rainstormstudio/nerd-icons-dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Icons in completion interfaces
;; https://github.com/rainstormstudio/nerd-icons-completion
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Icons in file explorer
;; https://github.com/rainstormstudio/treemacs-nerd-icons
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'ngs-theme)
;;; ngs-theme.el ends here
