;;; wide-n.el --- Cycle among buffer restrictions
;; 
;; Filename: wide-n.el
;; Description: Cycle among buffer restrictions
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2010, Drew Adams, all rights reserved.
;; Created: Sun Apr 18 12:58:07 2010 (-0700)
;; Version: 20.0
;; Last-Updated: Sun Apr 18 19:37:51 2010 (-0700)
;;           By: dradams
;;     Update #: 139
;; URL: http://www.emacswiki.org/cgi-bin/wiki/wide-n.el
;; Keywords: narrow restriction widen
;; Compatibility: Emacs 21.x, 22.x, 23.x
;; 
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp'.
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
;;    `wide-n' is bound to `C-x n x'.  Repeating `x' after this
;;    repeats the command: `C-x n x x x x' etc.
;;
;;    Invoking `wide-n' with a prefix argument changes the behavior as
;;    follows:
;;
;;    * A plain prefix arg (`C-u') widens the buffer completely.
;; 
;;    * A negative numeric prefix arg (e.g `C--') widens completely
;;      and resets (empties) the list of restrictions.
;;
;;    * A positive numeric prefix arg N takes you directly to the Nth
;;      previous restriction.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2020/04/18 dadams
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

(defvar wide-n-restrictions '(all)
  "List of buffer restrictions.
Each entry is either `all' or a cons (START . END), where START and
END are the limits of a buffer restriction.
`all' means no restriction (completely widened).")
(make-variable-buffer-local 'wide-n-restrictions)

(defun wide-n (arg)
  "Widen to a previous buffer restriction.
With a plain prefix arg (`C-u'), widen completely.
With a negative prefix arg (`C--'), widen completely and reset (empty)
 the list of restrictions for this buffer.
With a positive numeric prefix arg N, widen N times (to the Nth
 previous restriction)."
  (interactive "P")
  (unless (cadr wide-n-restrictions) (error "Cannot widen; no previous narrowing"))
  (cond ((or (null (cdr wide-n-restrictions)) (consp arg))
         (widen) (message "No longer narrowed"))
        ((< (prefix-numeric-value arg) 0)
         (setq wide-n-restrictions  (list 'all))
         (widen) (message "No longer narrowed; no more restrictions"))
        (t
         (setq arg  (prefix-numeric-value arg))
         (let ((latest  ())
               (cntr    arg))
           (while (> cntr 0)
             (push (nth (1- cntr) wide-n-restrictions) latest)
             (setq cntr  (1- cntr)))
           (setq latest  (nreverse latest))
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
    (setq wide-n-restrictions  (remove (cons mrk1 mrk2) wide-n-restrictions))
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

(define-key ctl-x-map "nx" 'wide-n-repeat)

;;;;;;;;;;;;;;;;;;;;

(provide 'wide-n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wide-n.el ends here
