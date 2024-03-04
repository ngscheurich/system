;;; emacs-next.el -*- lexical-binding: t; -*-

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

;; The code in this file is part of GNU Emacs unless otherwise noted.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; At the time of writing, I am using GNU Emacs 29.1.  There are some
;; features from the upcoming official release of Emacs 30.0 that I am
;; interested in.  Rather than upgrading to a pre-release version of
;; Emacs, I've opted to manually add those features here by copying
;; the necessary Emacs Lisp code from the GNU Emacs `master' branch.

;;; Code:

(defcustom mode-line-right-align-edge 'window
  "Where function `mode-line-format-right-align' should align to.
Internally, that function uses `:align-to' in a display property,
so aligns to the left edge of the given area.  See info node
`(elisp)Pixel Specification'.

Must be set to a symbol.  Acceptable values are:
- `window': align to extreme right of window, regardless of margins
  or fringes
- `right-fringe': align to right-fringe
- `right-margin': align to right-margin"
  :type '(choice (const right-margin)
                 (const right-fringe)
                 (const window))
  :group 'mode-line
  :version "30.1")

(defun mode--line-format-right-align ()
  "Right-align all following mode-line constructs.

When the symbol `mode-line-format-right-align' appears in
`mode-line-format', return a string of one space, with a display
property to make it appear long enough to align anything after
that symbol to the right of the rendered mode line.  Exactly how
far to the right is controlled by `mode-line-right-align-edge'.

It is important that the symbol `mode-line-format-right-align' be
included in `mode-line-format' (and not another similar construct
such as `(:eval (mode-line-format-right-align)').  This is because
the symbol `mode-line-format-right-align' is processed by
`format-mode-line' as a variable."
  (let* ((rest (cdr (memq 'mode-line-format-right-align
			  mode-line-format)))
	 (rest-str (format-mode-line `("" ,@rest)))
	 (rest-width (progn
                       (add-face-text-property
                        0 (length rest-str) 'mode-line t rest-str)
                       (string-pixel-width rest-str))))
    (propertize " " 'display
		;; The `right' spec doesn't work on TTY frames
		;; when windows are split horizontally (bug#59620)
		(if (and (display-graphic-p)
                         (not (eq mode-line-right-align-edge 'window)))
		    `(space :align-to (- ,mode-line-right-align-edge
                                         (,rest-width)))
		  `(space :align-to (,(- (window-pixel-width)
                                         (window-scroll-bar-width)
                                         (window-right-divider-width)
                                         (* (or (cdr (window-margins)) 1)
                                            (frame-char-width))
                                         ;; Manually account for value of
                                         ;; `mode-line-right-align-edge' even
                                         ;; when display is non-graphical
                                         (pcase mode-line-right-align-edge
                                           ('right-margin
                                            (or (cdr (window-margins)) 0))
                                           ('right-fringe
                                            ;; what here?
                                            (or (cadr (window-fringes)) 0))
                                           (_ 0))
                                         rest-width)))))))

(defvar mode-line-format-right-align '(:eval (mode--line-format-right-align))
  "Mode line construct to right align all following constructs.")
;;;###autoload
(put 'mode-line-format-right-align 'risky-local-variable t)

;; The following is not part of GNU Emacs
(provide 'emacs-next)
;;; emacs-next.el ends here
