;;; stripes.el --- alternate the background color of lines

;; Copyright (C) 2003 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Created: 02 October 2003
;; Keywords: list alternation color
;; Version: 0.2

(defconst stripes-version "0.2"
  "Version of color alternation mode.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; highlights every even line with an alternative background
;; color. Useful for buffers that display lists of any kind - as a
;; guide for your eyes to follow these lines.

;; when invoked with a numeric prefix arg, color that many lines
;; instead of every other line.

;; Put this file into your load path,
;; (require 'stripes) and do a (turn-on-stripes-mode)
;;  whenever you need this (e.g. in hooks).

;; Changelog

; --- Version 0.2 (2003-11-01)

;; - added autoload cookies
;; - added turn-on method (for hooks)
;; - prefix arg specifies how many lines to treat as one line
;; - renamed the file from stripes-mode.el to stripes.el to
;;   make the name fit into the Emacs file naming conventions
;; - widen all restrictions when refreshing

; --- Version 0.1 (2003-10-02)

;;; Code:
(defvar stripes-mode nil)
(make-variable-buffer-local 'stripes-mode)

(defvar stripes-lcount 1)
(make-variable-buffer-local 'stripes-lcount)

(or (assq 'stripes-mode minor-mode-alist)
              (setq minor-mode-alist
                    (cons '(stripes-mode " ==")
     minor-mode-alist)))

(defface stripes-face
  `((t (:background "#f4f4f4")))
  "Face for alternate lines."
  :group 'stripes)

;;;###autoload
(defun stripes-mode (arg)
  "Toggle Color alternation mode.
With prefix ARG, enable Color alternation mode iff arg is nonzero.  In
that case the numeric arg (unless it is made by (multiple)
`\\[universal-argument]'s) specifies the number of subsequent lines
that should
be in one color (without alternation)."
  (interactive "P")
  (setq stripes-mode
        (if (null arg)
            (not stripes-mode)
          (> (prefix-numeric-value arg) 0)))
  (setq stripes-lcount
 (if (numberp arg)
     arg
   1))
  (force-mode-line-update)
  (if stripes-mode
      (stripes-create)
    (stripes-remove))
  (if (interactive-p)
      (if stripes-mode
   (if (= stripes-lcount 1)
       (message "Color alternation mode enabled")
     (message "Color alternation mode (%i lines) enabled"
       stripes-lcount))
        (message "Color alternation mode disabled"))))

;;;###autoload
(defun turn-on-stripes-mode ()
  "Turn on color alternation mode.
Useful for adding to a major mode hook variable.
Example:
    (add-hook 'gnus-summary-mode-hook 'turn-on-stripes-mode)
to automatically turn on color alternation when viewing the Gnus
article buffer."
  (interactive)
  (stripes-mode 1))

(defun stripes-remove ()
  "Remove all alternation colors."
   (let ((oli (overlays-in (point-min) (point-max))) ol)
    (while oli
      (setq ol (car oli)
     oli (cdr oli))
      (when (eq (overlay-get ol 'face) 'stripes-face)
 (delete-overlay ol)))))

(defun stripes-create ()
  "Colors lines in current buffer alternatively.
This will not monitor changes of the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (stripes-remove)
      (goto-char (point-min))
      (while (not (eobp))
 (forward-line stripes-lcount)
 (let ((ppp (point))
       ovl)
   (unless (eobp)
     (forward-line stripes-lcount)
     (setq ovl (make-overlay ppp (point)))
     (overlay-put ovl 'face 'stripes-face)))))))
   
(defun stripes-after-change-function (beg end length)
  "After change function for color alternation mode.
Refreshes all the highlighting.  This is slow, but as mostly lists are
not changed that often, it should be acceptable.  Arguments BEG END
and LENGTH are not used."
  (if stripes-mode
      (stripes-create)))

(add-hook 'after-change-functions
   'stripes-after-change-function)

;; legacy provide (if you have installed an old version as well)
(provide 'stripes-mode)

(provide 'stripes)

;;; stripes.el ends here
