;;; mon-rectangle-utils.el --- procedures for manipulating rectangles
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-rectangle-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-23T20:03:56-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-rectangle-utils provides procedures for manipulating rectangles
;;
;; FUNCTIONS:►►►
;; `mon-rectangle-capitalize'
;; `mon-rectangle-upcase'
;; `mon-rectangle-downcase'
;; `mon-rectangle-apply-on-region-points'
;; `mon-rectangle-operate-on'
;; `mon-rectangle-sum-column'
;; `mon-rectangle-columns'
;; `mon-kill-rectangle-w-beer-belly'
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;; :NOTE Aliases defined in :FILE mon-aliases.el
;;
;;  <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-rectangle-kill-w-longest-line' -> `mon-kill-rectangle-w-beer-belly'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-rectangle-capitalize'                     <- mon-utils.el
;; `mon-rectangle-upcase'                         <- mon-utils.el
;; `mon-rectangle-downcase'                       <- mon-utils.el
;; `mon-rectangle-apply-on-region-points'         <- mon-utils.el
;; `mon-rectangle-operate-on'                     <- mon-utils.el
;; `mon-rectangle-sum-column'                     <- mon-utils.el
;; `mon-rectangle-columns'                        <- mon-utils.el
;; `mon-kill-rectangle-w-beer-belly'              <- mon-utils.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-rectangle-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-rectangle-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-23T20:03:56-05:00Z}#{10472} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :PREFIX "mrc-"
;;; :CREATED <Timestamp: Friday June 05, 2009 @ 07:03.00 PM - by MON KEY>
(defun mon-rectangle-columns (start end)
  "Return column positions at START and END.\n
Mostly useful as a code template for rectangle related functions.\n
:SEE-ALSO `mon-rectangle-sum-column'.\n►►►"
  (interactive "r")
  (let ((mrc-col-beg (make-marker))
	(mrc-col-end (make-marker))
	mrc-cols)
    (save-excursion
      (goto-char start)
      (set-marker mrc-col-beg (point))
      (goto-char end)
      (set-marker mrc-col-end (point)))
    (setq mrc-cols    `(,(car (nth 6 (posn-at-point (marker-position mrc-col-beg))))
                        ,(car (nth 6 (posn-at-point (marker-position mrc-col-end))))))
    (prog1 
        mrc-cols
      (set-marker mrc-col-beg nil)
      (set-marker mrc-col-end nil))))

;;; ==============================
;;; :COURTESY Alex Schroeder
;;; :MODIFICATIONS Charlie Hethcoat <- Improved number regex.
;;; :MODIFICATIONS <Timestamp: #{2010-03-09T14:47:59-05:00Z}#{10102} - by MON KEY>
;;; Now dumps to temp-buffer. Added optional arg INTRP.
(defun mon-rectangle-sum-column (start end &optional intrp)
  "Add all integer, decimal, and floating-point numbers in selected rectangle.\n
Numbers which can be read include (nonexhaustive):\n
 2 +2 -2 2. +2. -2. 2.0 +2.0 -2.0 2e0 +2e0 -2e0 2E0 2e+0 2e-0, 2.e0, 2.0e0, etc.\n
:SEE-ALSO `mon-rectangle-columns', `mon-line-string-incr-padded',
`mon-line-number-region', `mon-string-incr', `mon-line-number-region-incr'.\n►►►"
  (interactive "r\np")
  (let ((rec-sumd 0))
    (save-excursion
      (kill-rectangle start end)
      (exchange-point-and-mark)
      (yank-rectangle)
      ;; :WAS (set-buffer (get-buffer-create "*calc-sum*")) 
      ;;        (erase-buffer) (yank-rectangle) (exchange-point-and-mark)
      (with-temp-buffer
        (save-excursion (yank-rectangle))
        (while (re-search-forward
                "[-+]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?"
                nil t)
          (setq rec-sumd (+ rec-sumd (string-to-number (match-string 0))))))
      (if intrp
          (message "Sum: %f" rec-sumd)
          rec-sumd))))

;;; ==============================
;;; :PREFIX "mroo-"
;;; :NOTE Functions for modifying buffer contents or display.
;;;       Brings in `operation-on-rectangle' for the old-school holmessss.
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el 
;;; :WAS `operate-on-rectangle' -> `apply-on-rectangle' -> `mon-rectangle-operate-on'
;;; ==============================
(defun mon-rectangle-operate-on (rec-fun start end &rest args)
  "Call REC-FUN for each line of rectangle with corners at START END.\n
REC-FUN is called with two arguments: the start and end columns of the
rectangle, plus ARGS extra arguments.\
Point is at the beginning of line when REC-FUN is called.\n
:SEE `apply-on-rectangle' in :FILE rect.el
:SEE-ALSO `mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
`mon-rectangle-downcase', `mon-rectangle-upcase', `mon-rectangle-capitalize'.\n►►►"
  (let (mroo-beg-col mroo-end-col mroo-beg-pnt mroo-end-pnt)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        (setq mroo-beg-col (current-column))
        (beginning-of-line)
        (setq mroo-beg-pnt (point))
        (goto-char end)
        (setq mroo-end-col (current-column))
        (forward-line 1)
        (setq mroo-end-pnt (point-marker))
        ;; Ensure the start column is the left one.
        (if (< mroo-end-col mroo-beg-col)
            (let ((col mroo-beg-col))
              (setq mroo-beg-col mroo-end-col mroo-end-col col)))
        ;; Start looping over lines.
        (goto-char mroo-beg-pnt)
        (while (< (point) mroo-end-pnt)
          (apply rec-fun mroo-beg-col mroo-end-col args)
          (forward-line 1))
        (widen)))))

;;; ==============================
;;; :PREFIX "mraorp-" 
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-apply-on-region-points (w-fun start end &rest w-args)
  "Like `apply-on-rectangle', but pass points in the buffer instead of columns.\n
W-FUN is a symbol naming function to apply to region from START to END.\n
W-ARGS are additional args to pass to W-FUN.\n
:SEE-ALSO`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
`mon-rectangle-downcase' `mon-rectangle-upcase' `mon-rectangle-capitalize'.\n►►►"
  (mon-rectangle-operate-on
   #'(lambda (mraorp-L-1-bcol mraorp-L-1-ecol)
       (apply w-fun
              (progn
                (move-to-column mraorp-L-1-bcol 'coerce-tab-to-space)
                (point))
              (progn
                (move-to-column mraorp-L-1-ecol 'coerce-tab-to-space)
                (prog1
                    (point)
                  (beginning-of-line)))
              w-args))
   start end))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-downcase (beg end)
  "Convert the marked rectangle to lower case.\n
:SEE-ALSO `mon-rectangle-upcase', `mon-rectangle-capitalize',
`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points'.\n►►►"
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'downcase-region beg end))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-upcase (beg end)
  "Convert the marked rectangle to upper case.
:SEE-ALSO `mon-rectangle-downcase' ,`mon-rectangle-operate-on',
`mon-rectangle-apply-on-region-points', `rect.el'."
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'upcase-region beg end))

;;; ==============================
(defun mon-rectangle-capitalize (beg end)
  "Convert the marked rectangle to Title case.\n
:SEE-ALSO `mon-rectangle-downcase', `mon-rectangle-upcase', `mon-rectangle-capitalize'
`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points'.\n►►►"
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'mon-region-capitalize beg end))

;;; ==============================
;;; :BUG #1184 of Thu, 16 Oct 2008 19:45:02 UTC
;;; "document how to deal with beer belly rectangles"
;;; :SEE (URL `http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=1184')
;;; :TODO Currently only handles situations where point is at column 0.\n
;;; :CREATED <Timestamp: #{2010-01-10T21:26:28-05:00Z}#{10017} - by MON>
(defun mon-kill-rectangle-w-beer-belly (belly-start belly-end)
  "Like `kill-rectangle' but adds trailing whitespace when column at mark is less
than the longest line in rectangle.\n
Does not handle situations where point is not at column 0.\n
The
following
is a rectangle-w-beer-belly.
The paragraph you are reading is a
potential rectangle. It is a PITA for the 
`kill-rectangle' command because it is hard to
put point and mark around being in that it is fat
in the middle. Thus, while it is easy to mark
its left side, how are you going to mark its
right upper or lower corner without
altering the buffer to add spaces
in order to get the cursor there?\n
:ALIASED-BY `mon-rectangle-kill-w-longest-line'\n
:SEE-ALSO `mon-rectangle-apply-on-region-points', `mon-rectangle-capitalize',
`mon-rectangle-columns', `mon-rectangle-downcase', `mon-rectangle-operate-on',
`mon-rectangle-sum-column', `mon-rectangle-upcase', `mon-line-length-max',
`mon-line-indent-from-to-col', `mon-line-strings-indent-to-col'.\n►►►"
  (interactive "r\n")
  (let ((max-len 0)
        fat-belly)
    (unwind-protect
         (narrow-to-region belly-start belly-end)
      (mon-g2be -1)
      (while (eq (forward-line) 0)
        (end-of-line)
        (when (> (current-column) max-len)
          (setq max-len (current-column))))
      (when (= (current-column) max-len)
        (setq fat-belly t)
        (kill-rectangle belly-start belly-end))
      (unless fat-belly
        (setq fat-belly (mon-buffer-sub-no-prop belly-start belly-end))
        (goto-char belly-start)
        (kill-line)
        (while (eq (forward-line) 0) (kill-line))
        (when (stringp fat-belly)
          (with-temp-buffer 
            (insert fat-belly)
            (mon-g2be -1)
            (while (eq (forward-line) 0)
              (let ((lebp `(,(line-beginning-position) . ,(line-end-position))))
                (unless (= (- (car lebp) (cdr lebp)) max-len)
                  (end-of-line) 
                  (insert (make-string (- max-len (- (cdr lebp) (car lebp))) 32)))))
            (kill-rectangle (mon-g2be -1 t) (mon-g2be 1 t)) )))
      (widen))))

;;; ==============================
(provide 'mon-rectangle-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-rectangle-utils.el ends here
;;; EOF
