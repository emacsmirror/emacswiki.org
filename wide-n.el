;;; wide-n.el --- Cycle among buffer restrictions
;;
;; Filename: wide-n.el
;; Description: Cycle among buffer restrictions
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2010-2014, Drew Adams, all rights reserved.
;; Created: Sun Apr 18 12:58:07 2010 (-0700)
;; Version: 2014.05.30
;; Package-Requires: ()
;; Last-Updated: Fri May 30 18:04:02 2014 (-0700)
;;           By: dradams
;;     Update #: 413
;; URL: http://www.emacswiki.org/wide-n.el
;; Doc URL: http://www.emacswiki.org/MultipleNarrowings
;; Keywords: narrow restriction widen
;; Compatibility: GNU Emacs: 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    This library modifies commands `narrow-to-region',
;;    `narrow-to-defun', and `narrow-to-page' (`C-x n n', `C-x n d',
;;    and `C-x n p') so that the current buffer restriction
;;    (narrowing) is added to a list of restrictions for the current
;;    buffer, `wide-n-restrictions'.
;;
;;    You can then use `C-x n x' to cycle among previous buffer
;;    restrictions.  Repeating `x' repeats the action: `C-x n x x x x'
;;    etc.  Each time you hit `x' a different narrowing is made
;;    current.  This gives you an easy way to browse your past
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
;;    By default, `C-x n x' is bound to command `wide-n-repeat'.  If
;;    you use Emacs 21 then you will want to change this key binding
;;    to command `wide-n', which is a non-repeatable version.
;;    Repeatability is not available before Emacs 22.
;;
;;    The mode-line lighter `Narrow' is still used for the ordinary
;;    Emacs narrowing commands.  But for `wide-n-repeat' (`C-x n x')
;;    the current restriction is indicated in the lighter by a
;;    identifying number: `Narrow-1', `Narrow-2', and so on.
;;    `mouse-2' on the `Narrow' part still widens completely, but
;;    `mouse-2' on the `-NUM' part uses `wide-n-repeat' to cycle to
;;    the next restriction.
;;
;;    Emacs markers are used to record restriction limits, so the same
;;    restriction is available even if you modify its context.  If for
;;    any reason `wide-n-restrictions' ever has any entries that use
;;    buffer positions (numbers) instead of markers, invoking `wide-n'
;;    corrects this by changing the positions to markers.
;;
;;    This means that you can serialize `wide-n-restrictions',
;;    converting all markers to positions, save the value
;;    persistently, and restore it later.  Library `bookmark+.el' does
;;    this in order to let you bookmark and restore a list of
;;    restrictions.
;;
;;    In normal use, only the interactive use of commands
;;    `narrow-to-region', `narrow-to-defun', and `narrow-to-page' is
;;    affected by this library.  When these functions are called
;;    non-interactively there is normally no change to the value of
;;    variable `wide-n-restrictions'.  However, if for some reason you
;;    want to add entries to the restrictions ring when narrowing with
;;    some Emacs-Lisp code (i.e. non-interactively), you can do so by
;;    binding variable `wide-n-push-anyway-p' around the narrowing
;;    call.
;;
;;
;;  Commands defined here:
;;
;;    `wide-n', `wide-n-repeat',
;;
;;  Non-interactive functions defined here:
;;
;;    `wide-n-highlight-lighter', `wide-n-markerize',
;;    `wide-n-mem-regexp', `wide-n-push', `wide-n-repeat-command',
;;    `wide-n-string-match-p'.
;;
;;  Internal variables defined here:
;;
;;    `wide-n-lighter-narrow-part', `wide-n-push-anyway-p',
;;    `wide-n-rassoc-delete-all', `wide-n-restrictions'.
;;
;;
;;  ***** NOTE: This EMACS PRIMITIVE has been ADVISED HERE:
;;
;;    `narrow-to-region'.
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' and
;;              `page.el' have been REDEFINED here:
;;
;;    `narrow-to-defun', `narrow-to-page'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/05/30 dadams
;;     Added: wide-n-lighter-narrow-part, wide-n-highlight-lighter, wide-n-string-match-p, wide-n-mem-regexp,
;;            wide-n-rassoc-delete-all.
;;     wide-n-restrictions: The format is now (NUM START . END), not (START . END).
;;     wide-n: Set wide-n-lighter-narrow-part.  Use wide-n-highlight-lighter.  Bind wide-n-push-anyway-p
;;             around narrow-to-region.
;;     wide-n-markerize, wide-n-push: Use new wide-n-restrictions format.
;;     wide-n-push: Added message about restriction.
;; 2011/04/09 dadams
;;     narrow-to-region defadvice:
;;       Use ad-get-arg - don't refer to args by name (work around Emacs bug #8457).
;; 2011/01/04 dadams
;;     Added autoload cookies (for commands).
;; 2010/04/26 dadams
;;     Added: wide-n-push, wide-n-push-anyway-p.
;;     narrow-to-*: Call wide-n-push when interactive or wide-n-push-anyway-p.
;; 2010/04/24 dadams
;;     Added: wide-n-markerize.
;;     Use non-destructive operations (again, as initially).
;;       wide-n-restrictions: Use (all) cons as init value.
;;       wide-n, narrow-to-region: Don't initialize to (all).
;;       wide-n: Use append, not nconc.
;;       narrow-to-region: Use remove, not delete.
;;     wide-n: Use wide-n-markerize.
;; 2010/04/21 dadams
;;     Bind non-repeatable version, wide-n, in Emacs 21.
;; 2010/04/19 dadams
;;     wide-n, narrow-to-region, wide-n-restrictions:
;;       Use nil default val & use make-local-variable, so can use destructive ops.
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


;; Quiet byte-compiler
(defvar narrow-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar wide-n-lighter-narrow-part ""
  "String to append to \" Narrow\" in mode-line lighter.")
(make-variable-buffer-local 'wide-n-lighter-narrow-part)

(defvar wide-n-restrictions '(all)
  "List of buffer restrictions.
Each entry is either `all' or a cons (NUM START . END), where NUM is a
counter identifying this buffer restriction, and START and END are its
limits.  `all' means no restriction (completely widened).")
(make-variable-buffer-local 'wide-n-restrictions)

(defvar wide-n-push-anyway-p nil
  "Non-nil means push to `wide-n-restrictions' even if non-interactive.
Normally, if a narrowing command is called non-interactively the
region limits are not pushed to `wide-n-restrictions'.  A non-nil
value here overrides the push inhibition.  You can bind this to
non-nil in Lisp code to populate `wide-n-restrictions' during
narrowing.")

;;;###autoload
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
  (unless (cadr wide-n-restrictions) (error "Cannot widen; no previous narrowing"))
  (cond ((or (null (cdr wide-n-restrictions))  (consp arg))
         (widen)
         (setq wide-n-lighter-narrow-part  "")
         (wide-n-highlight-lighter)
         (message "No longer narrowed"))
        ((= (prefix-numeric-value arg) 0)
         (setq wide-n-restrictions  (list 'all))
         (widen)
         (setq wide-n-lighter-narrow-part  "")
         (wide-n-highlight-lighter)
         (message "No longer narrowed; no more restrictions"))
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
                   latest  (if (member 'all latest) (list 'all) ())))
           (setq wide-n-restrictions         (append (nthcdr arg wide-n-restrictions) latest)
                 wide-n-restrictions         (mapcar #'wide-n-markerize wide-n-restrictions)
                 wide-n-lighter-narrow-part  (if (eq 'all (car wide-n-restrictions))
                                                 ""
                                               (format "-%d" (caar wide-n-restrictions))))
           (if (not (eq 'all (car wide-n-restrictions)))
               (condition-case err
                   (let ((wide-n-push-anyway-p  t))
                     (narrow-to-region (cadr (car wide-n-restrictions)) (cddr (car wide-n-restrictions)))
                     (wide-n-highlight-lighter))
                 (args-out-of-range
                  (setq wide-n-restrictions  (cdr wide-n-restrictions))
                  (error "Restriction removed because of invalid limits"))
                 (error (error "%s" (error-message-string err))))
             (widen)
             (wide-n-highlight-lighter)
             (message "No longer narrowed"))))))

(defun wide-n-highlight-lighter ()
  "Update minor-mode mode-line lighter to reflect narrowing/widening.
Put `wide-n' on `mouse-2' for the lighter suffix."
  (let* ((%n-cons  (wide-n-mem-regexp "%n\\(.*\\)\\'" mode-line-modes)))
    (when %n-cons
      (setcar %n-cons (replace-regexp-in-string
                       "%n\\(.*\\)"
                       (if (/= (- (point-max) (point-min)) (buffer-size)) ; `buffer-narrowed-p', for older Emacs
                           wide-n-lighter-narrow-part
                         "")
                       (car %n-cons) nil nil 1))
      (when (> (length (car %n-cons)) 2)
        (set-text-properties 2 (length (car %n-cons)) '(local-map (keymap (mode-line keymap (mouse-2 . wide-n)))
                                                        mouse-face mode-line-highlight
                                                        help-echo "mouse-2: Next Restriction")
                             (car %n-cons)))
      ;; Dunno why we need to do this.  Tried adjusting `rear-sticky' and `front-sticky',
      ;; but without this the whole field (not just the suffix) gets changed, in effect, to the above spec.
      (set-text-properties 0 2 '(local-map (keymap (mode-line keymap (mouse-2 . mode-line-widen)))
                                 mouse-face mode-line-highlight help-echo "mouse-2: Widen")
                           (car %n-cons)))))

(defun wide-n-mem-regexp (regexp xs)
  "Like `member', but tests by matching REGEXP against cars."
  (and (consp xs)  (if (and (stringp (car xs))  (wide-n-string-match-p regexp (car xs)))
                       xs
                     (wide-n-mem-regexp regexp (cdr xs)))))

;; Same as `tap-string-match-p' in `thingatpt+.el' and `icicle-string-match-p' in `icicles-fn.el'.
(if (fboundp 'string-match-p)
    (defalias 'wide-n-string-match-p 'string-match-p) ; Emacs 23+
  (defun wide-n-string-match-p (regexp string &optional start)
    "Like `string-match', but this saves and restores the match data."
    (save-match-data (string-match regexp string start))))

(defun wide-n-markerize (restriction)
  "Convert RESTRICTION to use markers if it uses only positions.
RESTRICTION is `all' or a dotted list of an identifier and two buffer
positions or markers.  This is a nondestructive operation: returns a
new cons."
  (unless (or (atom restriction)  (and (markerp (cadr restriction))  (markerp (cddr restriction))))
    (let ((mrk1     (make-marker))
          (mrk2     (make-marker)))
      (move-marker mrk1 (cadr restriction))
      (move-marker mrk2 (cddr restriction))
      (setcar (cdr restriction) mrk1)
      (setcdr (cdr restriction) mrk2)))
  restriction)

(defun wide-n-push (start end)
  "Push the region limits to `wide-n-restrictions'.
START and END are as for `narrrow-to-region'."
  (let ((mrk1  (make-marker))
        (mrk2  (make-marker))
        sans-id  id-cons  id)
    (move-marker mrk1 start)
    (move-marker mrk2 end)
    (setq sans-id              `(,mrk1 . ,mrk2)
          id-cons              (rassoc sans-id wide-n-restrictions)
          id                   (if id-cons (car id-cons) (length wide-n-restrictions))
          wide-n-restrictions  (wide-n-rassoc-delete-all sans-id wide-n-restrictions))
    (unless (and (= mrk1 1)  (= mrk2 (1+ (buffer-size))))
      (setq wide-n-restrictions  `((,id ,mrk1 . ,mrk2) ,@wide-n-restrictions)))
    (message "Narrowed: %d to %d" (marker-position mrk1) (marker-position mrk2))))

(defun wide-n-rassoc-delete-all (value alist)
  "Delete from ALIST all elements whose cdr is `equal' to VALUE.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))  (equal (cdar alist) value)) (setq alist  (cdr alist)))
  (let ((tail  alist)
        tail-cdr)
    (while (setq tail-cdr  (cdr tail))
      (if (and (consp (car tail-cdr))  (equal (cdar tail-cdr) value))
	  (setcdr tail (cdr tail-cdr))
	(setq tail  tail-cdr))))
  alist)

(defun wide-n-repeat-command (command)
  "Repeat COMMAND."
 (let ((repeat-previous-repeated-command  command)
       (repeat-message-function           'ignore)
       (last-repeatable-command           'repeat))
   (repeat nil)))

;;;###autoload
(defun wide-n-repeat (arg)
  "Cycle to the next buffer restriction.
This is a repeatable version of `wide-n'."
  (interactive "P")
  (require 'repeat)
  (wide-n-repeat-command 'wide-n))


(if (boundp 'narrow-map)
    (define-key narrow-map "x" 'wide-n-repeat)
  (define-key ctl-x-map "nx" (if (> emacs-major-version 21) 'wide-n-repeat 'wide-n)))


;; Call `wide-n-push' if interactive or `wide-n-push-anyway-p'.
;;
(defadvice narrow-to-region (before push-wide-n-restrictions activate)
  "Push the region limits to `wide-n-restrictions'.
You can use `C-x n x' to widen to a previous buffer restriction."
  (when (or (interactive-p) wide-n-push-anyway-p)
    (wide-n-push (ad-get-arg 0) (ad-get-arg 1)))) ; Args START and END.


;; REPLACE ORIGINAL in `lisp.el'.
;;
;; Call `wide-n-push' if interactive or `wide-n-push-anyway-p'.
;;
;;;###autoload
(defun narrow-to-defun (&optional arg)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional ARG is ignored."
  (interactive)
  (save-excursion
    (widen)
    (let ((opoint  (point))
	  beg end)
      ;; Try first in this order for the sake of languages with nested functions
      ;; where several can end at the same place as with the offside rule, e.g. Python.
      (beginning-of-defun)
      (setq beg  (point))
      (end-of-defun)
      (setq end  (point))
      (while (looking-at "^\n")
	(forward-line 1))
      (unless (> (point) opoint)
	;; `beginning-of-defun' moved back one defun, so we got the wrong one.
	(goto-char opoint)
	(end-of-defun)
	(setq end  (point))
	(beginning-of-defun)
	(setq beg  (point)))
      (goto-char end)
      (re-search-backward "^\n" (- (point) 1) t)
      (when (or (interactive-p)  wide-n-push-anyway-p) (wide-n-push beg end))
      (narrow-to-region beg end))))


;; REPLACE ORIGINAL in `page.el'.
;;
;; Call `wide-n-push' if interactive or `wide-n-push-anyway-p'.
;;
;;;###autoload
(defun narrow-to-page (&optional arg)
  "Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg  (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (let ((adjust  0)
		(opoint  (point)))
	    ;; If not now at the beginning of a page, move back one extra time, to get to start of this page.
	    (save-excursion
	      (beginning-of-line)
	      (or (and (looking-at page-delimiter)  (eq (match-end 0) opoint))
		  (setq adjust 1)))
	    (forward-page (- arg adjust)))))
    ;; Find the end of the page.
    (set-match-data nil)
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction at the beginning of that line.
    ;; Before checking the match that was found, verify that `forward-page' actually set the match data.
    (if (and (match-beginning 0)  (save-excursion (goto-char (match-beginning 0))
                                                  (looking-at page-delimiter)))
	(goto-char (match-beginning 0)))
    (let ((beg  (point))
          (end  (progn
                  ;; Find the top of the page.
                  (forward-page -1)
                  ;; If we found beginning of buffer, stay there.
                  ;; If extra text follows page delimiter on same line, include it.
                  ;; Otherwise, show text starting with following line.
                  (when (and (eolp)  (not (bobp))) (forward-line 1))
                  (point))))
      (when (or (interactive-p)  wide-n-push-anyway-p) (wide-n-push beg end))
      (narrow-to-region beg end))))

;;;;;;;;;;;;;;;;;;;;

(provide 'wide-n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wide-n.el ends here
