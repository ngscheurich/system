;;; ngs-mode-line.el --- Code for my custom mode line -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
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

;;;; Remove mode line border

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-active nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;;;; Mode line palette

(defconst ngs-mode-line-palette
  '((bg-alt . "#2B3045")
    ("evil-state-emacs" . "black")
    ("evil-state-hybrid" . "blue")
    ("evil-state-insert" . "brown")
    ("evil-state-motion" . "cyan")
    ("evil-state-normal" . "green")
    ("evil-state-operator" . "magenta")
    ("evil-state-replace" . "orange")
    ("evil-state-visual" . "purple")))

(defun ngs-mode-line-color (color)
  (cdr (assoc color ngs-mode-line-palette)))

(defun ngs-mode-line-evil-color (state)
  (cdr (assoc (format "evil-state-%s" state) ngs-mode-line-palette)))

;; Faces
(defface ngs-mode-line-alt
  (let ((bg (ngs-mode-line-color 'bg-alt)))
    `((t
       :background ,bg
       :foreground "gray90"
       :box (:line-width 5 :color ,bg))))
  "Used to visually distinguish mode line constructs.")

;;;; Evil state

(defun ngs-mode-line--evil-state-face ()
  "Return a face indicating `evil-state'."
  (cond
   ((eq evil-state 'insert)
    'modus-themes-intense-green)
   ((eq evil-state 'emacs)
    'modus-themes-intense-purple)
   ((eq evil-state 'hybrid)
    'modus-themes-intense-purple)
   ((eq evil-state 'normal)
    'modus-themes-intense-cyan)
   ((eq evil-state 'visual)
    'modus-themes-intense-magenta)
   ((eq evil-state 'motion)
    'modus-themes-intense-yellow)
   ((eq evil-state 'operator)
    'modus-themes-intense-yellow)
   ((eq evil-state 'replace)
    'modus-themes-intense-red)
   (t
    "orchid1")))

(defvar-local ngs-mode-line-evil-state
    '(:eval (propertize " " 'face (ngs-mode-line--evil-state-face))))

;;;; Buffer name

(defun ngs-mode-line--buffer-name ()
  "Return the buffer name with padding."
  (format " %s " (buffer-name)))

(defvar-local ngs-mode-line-buffer-name
    '(:eval
      (propertize (ngs-mode-line--buffer-name)
                  'face
                  'ngs-mode-line-alt)))

;;;; Buffer info

(defun ngs-mode-line--buffer-info ()
  "Return the buffer info with padding."
  (format " %s " "%Z%1*%1+"))

(defvar-local ngs-mode-line-buffer-info
    '(:eval
      (propertize (ngs-mode-line--buffer-info)
                  ;; 'face 'ngs-mode-line-state-normal-inherited
                  'face 'shadow)))

;;;; Major mode

(defun ngs-mode-line--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))

(defvar-local ngs-mode-line-major-mode
    '((:eval
       (list
        (nerd-icons-icon-for-buffer)
        " "
        (propertize (ngs-mode-line--major-mode-name))))))

;;;; Git branch

;; Note: Uses a private function to get the branch name (not ideal).
;; See https://emacs.stackexchange.com/q/61837/41808
(defvar-local ngs-mode-line-git-branch
    '(:eval
      (list
       (propertize
        (format " %s" (nerd-icons-devicon "nf-dev-git_branch"))
        'face 'ngs-mode-line-alt)
       (propertize
        (format "%s " (vc-git--symbolic-ref (buffer-file-name)))
        'face 'ngs-mode-line-alt))))

;;;; Risky local variables

(dolist (construct '(ngs-mode-line-evil-state
                     ngs-mode-line-buffer-name
                     ngs-mode-line-buffer-info
                     ngs-mode-line-major-mode
                     ngs-mode-line-git-branch))
  (put construct 'risky-local-variable t))

;; Mode line format
(setq mode-line-format
      `("%e"
        ngs-mode-line-evil-state
        ngs-mode-line-buffer-name
        ngs-mode-line-buffer-info
        ngs-mode-line-major-mode
        " "
        ngs-mode-line-git-branch))
;;; ngs-mode-line.el ends here
