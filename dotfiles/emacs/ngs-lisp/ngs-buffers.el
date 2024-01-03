;;; ngs-buffers.el -*- lexical-binding: t -*-

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

;; Customizations to improve buffer management.

;;; Code:

;;; TODO: Can beframe replace this?
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init (persp-mode))

;;; Make buffers frame-local
;; See https://protesilaos.com/emacs/beframe
(use-package beframe
  :config
  ;; This is the default value.  Write here the names of buffers that
  ;; should not be beframed.
  (setq beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))

  (beframe-mode 1)

  ;; Bind Beframe commands to a prefix key, such as C-c b:
  (define-key global-map (kbd "C-c b") beframe-prefix-map)

  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe-consult-source
      `( :name     "Frame buffers"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source)))

;;; Make certain buffers "ephemeral"
;; https://github.com/karthink/popper
(use-package popper
  :config
  (popper-mode))

(provide 'ngs-buffers)
;;; ngs-buffers.el ends here
