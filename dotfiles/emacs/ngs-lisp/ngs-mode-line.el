;;; ngs-mode-line.el --- Code for my custom mode line -*- lexical-binding: t -*-

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

;; Customizations for the mode line.

;;; Code:

;;;; Remove mode line border

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-active nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;;;; Mode line palette

(defconst ngs-mode-line-palette
  '((bg-alt . . "#2B3045")
    ("evil-state-emacs" . "black")
    ("evil-state-hybrid" . "blue")
    ("evil-state-insert" . "brown")
    ("evil-state-motion" . "cyan")
    ("evil-state-normal" . "green")
    ("evil-state-operator" . "magenta")
    ("evil-state-replace" . "orange")
    ("evil-state-visual" . "purple")))

(defconst ngs-mode-line-evil-faces
  '(("emacs" . 'modus-themes-fg-magenta)
    ("hybrid" . 'modus-themes-fg-magenta)
    ("insert" . 'modus-themes-fg-green)
    ("motion" . 'modus-themes-fg-blue)
    ("normal" . 'default)
    ("operator" . 'modus-themes-fg-blue)
    ("replace" . 'modus-themes-fg-red)
    ("visual" . 'modus-themes-fg-cyan)))

(defun ngs-mode-line-color (color)
  (cdr (assoc color ngs-mode-line-palette)))

(defun ngs-mode-line-evil-color (state)
  (cdr (assoc (format "evil-state-%s" state) ngs-mode-line-palette)))

(defun ngs-mode-line-evil-face (state)
  (cdr (assoc state ngs-mode-line-evil-faces)))

;; Faces
(defface ngs-mode-line-alt
  (let ((bg (ngs-mode-line-color 'bg-alt)))
    `((t
       :background ,bg
       :foreground "gray90"
       :box (:line-width 5 :color ,bg)
       )))
  "Used to visually distinguish mode line constructs.")

(defface ngs-mode-line-padded
  `((t
     :box (:line-width 5 :color "#484d67")))
  "")

;;;; Evil state

(defun ngs-mode-line--evil-state-face ()
  "Return a face indicating `evil-state'."
  (cond
   ((eq evil-state 'insert)
    'modus-themes-fg-green)
   ((eq evil-state 'emacs)
    'modus-themes-fg-purple)
   ((eq evil-state 'hybrid)
    'modus-themes-fg-purple)
   ((eq evil-state 'normal)
    'modus-themes-fixed-pitch)
   ((eq evil-state 'visual)
    'modus-themes-fg-magenta)
   ((eq evil-state 'motion)
    'modus-themes-fg-yellow)
   ((eq evil-state 'operator)
    'modus-themes-fg-yellow)
   ((eq evil-state 'replace)
    'modus-themes-fg-red)
   (t
    "orchid1")))

(defvar-local ngs-mode-line-evil-state
    '(:eval (propertize " " 'face 'ngs-mode-line-alt)))

;;;; Buffer name

(defun ngs-mode-line--buffer-name ()
  "Return the buffer name with padding."
  (format " %s " (buffer-name)))

(defvar-local ngs-mode-line-buffer-name
    '(:eval
      (propertize (ngs-mode-line--buffer-name)
                  'face
                  'bold)))

;;;; Buffer info

(defun ngs-mode-line--buffer-info ()
  "Return the buffer info with padding."
  (format " %s " "%Z%1*%1+"))

(defvar-local ngs-mode-line-buffer-info
    '(:eval
      (propertize (ngs-mode-line--buffer-info)
                  'face (ngs-mode-line--evil-state-face))))

;;;; Major mode

(defun ngs-mode-line--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))

(defvar-local ngs-mode-line-major-mode
    '((:eval
       (list
        (nerd-icons-icon-for-buffer)
        " "
        (propertize (ngs-mode-line--major-mode-name) 'face 'ngs-mode-line-padded)))))

;;;; Git branch

;; Note: Uses a private function to get the branch name (not ideal).
;; See https://emacs.stackexchange.com/q/61837/41808
(defvar-local ngs-mode-line-git-branch
    '(:eval
      (list
       (propertize
        (format "%s" (nerd-icons-devicon "nf-dev-git_branch"))
        'face 'modus-themes-fg-red-faint)
       (propertize
        (format " %s" (vc-git--symbolic-ref (buffer-file-name)))))))

;; mode-line-misc-info
;; ((eglot--managed-mode
;;   (" [" eglot--mode-line-format "] "))
;;  (which-function-mode
;;   (which-func-mode
;;    ("" which-func-format " ")))
;;  (global-mode-string
;;   ("" global-mode-string)))

;;;; Risky local variables

(dolist (construct '(ngs-mode-line-evil-state
                     ngs-mode-line-buffer-name
                     ngs-mode-line-buffer-info
                     ngs-mode-line-major-mode
                     ngs-mode-line-git-branch))
  (put construct 'risky-local-variable t))

;; Mode line format
(defun ngs-activate-mode-line ()
  (interactive)
  (setq mode-line-right-align-edge 'right-margin)

  (let ((format `("%e"
                  ngs-mode-line-buffer-info
                  ngs-mode-line-buffer-name
                  flymake-mode-line-counters
                  mode-line-format-right-align
                  ngs-mode-line-major-mode
                  " "
                  eglot--mode-line-format
                  " "
                  ngs-mode-line-git-branch)))

    (setq-default mode-line-format format)
    (setq         mode-line-format format)))

(ngs-activate-mode-line)

(provide 'ngs-mode-line)
;;; ngs-mode-line.el ends here
