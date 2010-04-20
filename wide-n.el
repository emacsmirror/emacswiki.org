;;; wide-n.el --- Cycle among buffer restrictions
;; 
;; Filename: wide-n.el
;; Description: Cycle among buffer restrictions
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2010, Drew Adams, all rights reserved.
;; Created: Sun Apr 18 12:58:07 2010 (-0700)
;; Version: 20.0
;; Last-Updated: Mon Apr 19 15:38:12 2010 (-0700)
;;           By: dradams
;;     Update #: 198
;; URL: http://www.emacswiki.org/cgi-bin/wiki/wide-n.el
;; Keywords: narrow restriction widen
;; Compatibility: Emacs 21.x, 22.x, 23.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;    Command `wide-n' cycles among previous buffer restrictions
;;    (narrowings).  Function `narrow-to-region' is advised, so that
;;    its restriction is added to a list of restrictions for the
;;    current buffer, `wide-n-restrictions'.
;;
;;    A repeatable version of command `wide-n' is bound to `C-x n x'.
;;    Repeating `x' after this repeats the action: `C-x n x x x x'
;;    etc.  Each time you hit `x' a different narrowing is made
;;    current.  You can thus use this as a way to browse your past
;;    narrowings.
;;
;;    Invoking `C-x n x' with a prefix argument changes the behavior
;;    as follows:
;;
;;    * A plain prefix arg (`C-u') widens the buffer completely.
;; 
;;    * A zero numeric prefix arg (e.g `C-0') widens completely
;;      and resets (empties) the list of restrictions.
;;
;;    * A numeric prefix arg N takes you directly to the abs(N)th
;;      previous restriction.  That is, it widens abs(N) times.
;;      Positive and negative args work the same, except that a
;;      negative arg also pops entries off the ring: it removes the
;;      ring entries from the most recent back through the (-)Nth one.
;;
;;  NOTE: If you use Emacs 21, then the repeatable nature of `C-x n x'
;;        is not available to you.  You can nevertheless bind command
;;        `wide-n' (instead of `wide-n-repeat') to `C-x n x' and use
;;        it without repeating `x'.
;;
;;  Commands defined here:
;;
;;    `wide-n', `wide-n-repeat', 
;;
;;  Non-interactive functions defined here:
;;
;;    `wide-n-repeat-command'.
;;
;;  Internal variables defined here:
;;
;;    `wide-n-restrictions'.
;;
;;  ***** NOTE: This EMACS PRIMITIVE has been ADVISED HERE:
;;
;;    `narrow-to-region'.
;;
;;  ***** NOTE: The following functions defined in `lisp.el' and
;;              `page.el' have been COPIED HERE, to take advantage of
;;              the advised definition of `narrow-to-region':
;;
;;    `narrow-to-defun', `narrow-to-page'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2010/04/19 dadams
;;     wide-n, narrow-to-region, wide-n-restrictions:
;;       Use nil default val & use make-local-variable, so can use destructive operations.
;;     narrow-to-region: Use delete, not remove.
;;     Zero prefix arg now widens completely and empties ring.
;;     Negative prefix arg now pops the ring.
;;     Added: standard definitions of narrow-to-(defun|page).
;;     Use narrow-map if defined.
;; 2010/04/18 dadams
;;     Created.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar wide-n-restrictions ()
  "List of buffer restrictions.
Each entry is either `all' or a cons (START . END), where START and
END are the limits of a buffer restriction.
`all' means no restriction (completely widened).")
(make-variable-buffer-local 'wide-n-restrictions)

(defun wide-n (arg)
  "Widen to a previous buffer restriction.
With no prefix arg, widen to the previous restriction.
With a plain prefix arg (`C-u'), widen completely.
With a zero  prefix arg (`C-0'), widen completely and reset (empty)
 the list of restrictions for this buffer.
With a numeric prefix arg N, widen abs(N) times (to the abs(N)th
 previous restriction).  Positive and negative args work the same,
 except that a negative arg also pops entries off the ring: it removes
 the ring entries from the most recent back through the (-)Nth one.
 (It never pops off the `all' pseudo-entry that represents complete
 widening, however.)"
  (interactive "P")
  (when (null wide-n-restrictions)
    (set (make-local-variable 'wide-n-restrictions) (list 'all)))
  (unless (cadr wide-n-restrictions) (error "Cannot widen; no previous narrowing"))
  (cond ((or (null (cdr wide-n-restrictions)) (consp arg))
         (widen) (message "No longer narrowed"))
        ((= (prefix-numeric-value arg) 0)
         (setq wide-n-restrictions  (list 'all))
         (widen) (message "No longer narrowed; no more restrictions"))
        (t
         (setq arg  (prefix-numeric-value arg))
         (let ((latest  ())
               (cntr    (abs arg)))
           (while (> cntr 0)
             (push (nth (1- cntr) wide-n-restrictions) latest)
             (setq cntr  (1- cntr)))
           (setq latest  (nreverse latest))
           (when (< arg 0)
             (setq arg     (abs arg)
                   latest  (if (member 'all latest) '(all) ())))
           (setq wide-n-restrictions  (nconc (nthcdr arg wide-n-restrictions) latest))
           (if (not (eq 'all (car wide-n-restrictions)))
               (condition-case err
                   (narrow-to-region (caar wide-n-restrictions) (cdar wide-n-restrictions))
                 (args-out-of-range
                  (setq wide-n-restrictions  (cdr wide-n-restrictions))
                  (error "Restriction removed because of invalid limits"))
                 (error (error (error-message-string err))))
             (widen) (message "No longer narrowed"))))))

(defadvice narrow-to-region (before push-wide-n-restrictions activate)
  "Push the region limits to `wide-n-restrictions'."
  (let ((mrk1  (make-marker))
        (mrk2  (make-marker)))
    (move-marker mrk1 start)
    (move-marker mrk2 end)
    (when (null wide-n-restrictions)
      (set (make-local-variable 'wide-n-restrictions) (list 'all)))
    (setq wide-n-restrictions (delete (cons mrk1 mrk2) wide-n-restrictions))
    (unless (and (= mrk1 1) (= mrk2 (1+ (buffer-size))))
      (setq wide-n-restrictions  (cons (cons mrk1 mrk2) wide-n-restrictions)))))

(defun wide-n-repeat-command (command)
  "Repeat COMMAND."
 (let ((repeat-previous-repeated-command  command)
       (repeat-message-function           'ignore)
       (last-repeatable-command           'repeat))
   (repeat nil)))

(defun wide-n-repeat (arg)
  "Cycle to the next buffer restriction.
This is a repeatable version of `wide-n'."
  (interactive "P")
  (require 'repeat)
  (wide-n-repeat-command 'wide-n))

(if (boundp 'narrow-map)
    (define-key narrow-map "x" 'wide-n-repeat)
  (define-key ctl-x-map "nx" 'wide-n-repeat))

;; Standard definition.  We copy it here so it will pick up the advised `narrow-to-region'.
(defun narrow-to-defun (&optional arg)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional ARG is ignored."
  (interactive)
  (save-excursion
    (widen)
    (let ((opoint (point))
	  beg end)
      ;; Try first in this order for the sake of languages with nested
      ;; functions where several can end at the same place as with
      ;; the offside rule, e.g. Python.
      (beginning-of-defun)
      (setq beg (point))
      (end-of-defun)
      (setq end (point))
      (while (looking-at "^\n")
	(forward-line 1))
      (unless (> (point) opoint)
	;; beginning-of-defun moved back one defun
	;; so we got the wrong one.
	(goto-char opoint)
	(end-of-defun)
	(setq end (point))
	(beginning-of-defun)
	(setq beg (point)))
      (goto-char end)
      (re-search-backward "^\n" (- (point) 1) t)
      (narrow-to-region beg end))))

;; Standard definition.  We copy it here so it will pick up the advised `narrow-to-region'.
(defun narrow-to-page (&optional arg)
  "Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (let ((adjust 0)
		(opoint (point)))
	    ;; If we are not now at the beginning of a page,
	    ;; move back one extra time, to get to the start of this page.
	    (save-excursion
	      (beginning-of-line)
	      (or (and (looking-at page-delimiter)
		       (eq (match-end 0) opoint))
		  (setq adjust 1)))
	    (forward-page (- arg adjust)))))
    ;; Find the end of the page.
    (set-match-data nil)
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction
    ;; at the beginning of that line.
    ;; Before checking the match that was found,
    ;; verify that forward-page actually set the match data.
    (if (and (match-beginning 0)
	     (save-excursion
	       (goto-char (match-beginning 0)) ; was (beginning-of-line)
	       (looking-at page-delimiter)))
	(goto-char (match-beginning 0))) ; was (beginning-of-line)
    (narrow-to-region (point)
		      (progn
			;; Find the top of the page.
			(forward-page -1)
			;; If we found beginning of buffer, stay there.
			;; If extra text follows page delimiter on same line,
			;; include it.
			;; Otherwise, show text starting with following line.
			(if (and (eolp) (not (bobp)))
			    (forward-line 1))
			(point)))))

;;;;;;;;;;;;;;;;;;;;

(provide 'wide-n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wide-n.el ends here
