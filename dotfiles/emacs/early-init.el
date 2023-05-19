;;; early.init.el --- The Early Init File  -*- lexical-binding: t -*-

;; Author: N. G Scheurich <nick@scheurich.haus>
;; URL: https://nick.scheurich.haus/system
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.0"))

;;; Commentary:

;; This file is loaded before the package system and GUI is
;; initialized and, as such, can affect that initialization process.
;;
;; I use this file in particular for setting up my package management
;; system and disabling some GUI elements that are not relevant to the
;; way in which I use Emacs.
;; 
;; Emacs has a built-in packager, package.el, but I prefer to use the
;; straight.el package manager for a few key reasons:
;;
;; * It affords the ability to use a specific Git revision of a package
;; * It only loads the packages present in my configuration
;; * It supports true portabilty/reproducibility via lockfiles
;; * It allows me to make ad-hoc changes to packages
;;
;; There are numerous other advantages, and some disadvantages, when
;; compared to package.el which are enumerated in straight.el’s README:
;; <https://github.com/raxod502/straight.el#comparison-to-packageel>
;;
;; For more information about the Early Init file, see
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>.

;;; Code:

;; Prevent package.el from loading any packages
(setq package-enable-at-startup nil)

;; Bootstrap straight.el, i.e. download and install and evaluate it if
;; it is not detected. See <https://github.com/raxod502/straight.el#getting-started>.
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

;; Disable unwanted UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; I prefer to bypass the Emacs splash screen and boot to the scratch
;; buffer by default.
(setq inhibit-startup-message t)

;; Disable bell-ringing
(setq ring-bell-function 'ignore)

;;; early-init.el ends here
