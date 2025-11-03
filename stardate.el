;;; stardate.el --- insert stardate into a file -*- lexical-binding: t -*-

;; Copyright (C) 1988 Mark W. Eichin

;; Author: Mark W. Eichin <eichin@athena.mit.edu>

;; This file is not part of GNU Emacs.

;; This package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a relic of Emacs past....

;;; Code:

;;;Date: Thu, 8 Sep 88 18:30:15 EDT
;;;From: "Mark W. Eichin" <eichin@athena.mit.edu>
;;;Subject: [eichin@ATHENA.MIT.EDU: re: insert time/date and signature]

;;;{I sent this only to fischer, but then saw the complicated answer and
;;;figured I'd post...}

;;;   Date: 8 Sep 88 16:07:06 GMT
;;;   Reply-To: Scott Fischer <fischer%umn-cs.uucp@bbn.com>
;;;   Organization: University of Minnesota, Dept. of CSci.

;;;   Does anyone out there have a keybinding defintion that will insert
;;;   the current time or date along with an identification string at the
;;;   current point.  For example, I would like to type "M-x date" or define
;;;   that to a keystroke and it would insert.
;;;	   Thu Sep 8 10:59:40 CDT 1988  -- Scott W. Fischer

;;;What emacs? I have code for GNUemacs, M-x stardate, which gives me a
;;;very compact form: [eichin:19880908.1807EST] The code follows;
;;;however, for what you want (in GNU) you could just do:

;;;(defun date () 
;;;  "Insert a date label into the current buffer"
;;;  (interactive)
;;;  (insert (current-time-string) " -- " (user-full-name)))

;;;Thu Sep  8 18:24:17 1988 -- Mark W. Eichin

;;;(the second line is what happens when you run M-x date.) Note that
;;;this is *MUCH* faster than actually forking a date subprocess, even if
;;;you add elisp code to format the date. (I did use that solution once,
;;;but needed the speed and came up with this one...)
;;;  I keep stardate bound to ^X^J, and use it to mark comments in code...

;;;				Mark Eichin
;;;			<eichin@athena.mit.edu>
;;;		SIPB Member & Project Athena ``Watchmaker'' 


;;; stardate.el
;;; insert something like [eichin:19880309.0843EST] into a file, as a
;;; nerdly sort of timestamp.
;;;					[eichin:19880309.0843EST]
;;; There MUST be some way of speeding this up...
;;; sigh. there is. look at the rcslogs for the old icky version.
;;;					[eichin:19880309.0936EST]

(defvar stardate_el-RCS-id)
(setq stardate_el-RCS-id
      "$Header: stardate.el,v 1.2 88/03/09 09:44:01 eichin Exp $")

(defconst month-day-alist 
  '(("Jan"."01") ("Feb"."02") ("Mar"."03") ("Apr"."04") ("May"."05")
    ("Jun"."06") ("Jul"."07") ("Aug"."08") ("Sep"."09") ("Oct"."10")
    ("Nov"."11") ("Dec"."12"))
  "assoc list of months/numeric string numbers. See calendar.el.")

(defvar stardate-timezone "EST")

(setq date (current-time-string))
(defun insert-stardate ()
  "Put stardate at point."
  (interactive)
  (let ((date (current-time-string)))
    (insert "[" (getenv "USER") ":" 
    (insert "["
      (substring date -4 nil) ; yyyy
      (cdr (assoc (substring date 4 7)
            month-day-alist)) ; MM
      (let ((d (substring date 8 9)))
        (if (equal d " ") "0" d))
      (substring date 9 10)   ; d
      "."
      (substring date 11 13)    ; hh
      (substring date 14 16)    ; mm
      stardate-timezone
      "]")))

;;; stardate.el ends here
