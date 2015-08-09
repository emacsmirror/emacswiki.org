;;; wide-n.el --- Cycle among buffer restrictions
;;
;; Filename: wide-n.el
;; Description: Cycle among buffer restrictions
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2010-2015, Drew Adams, all rights reserved.
;; Created: Sun Apr 18 12:58:07 2010 (-0700)
;; Version: 2014.05.30
;; Package-Requires: ()
;; Last-Updated: Sun Aug  9 12:12:28 2015 (-0700)
;;           By: dradams
;;     Update #: 831
;; URL: http://www.emacswiki.org/wide-n.el
;; Doc URL: http://www.emacswiki.org/MultipleNarrowings
;; Keywords: narrow restriction widen region zone
;; Compatibility: GNU Emacs: 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `zones'.
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
;;    In normal use, only the interactive use of standard commands
;;    `narrow-to-region', `narrow-to-defun', and `narrow-to-page' is
;;    affected by this library.  When these functions are called
;;    non-interactively there is normally no change to the value of
;;    variable `wide-n-restrictions'.  However, if for some reason you
;;    want to add entries to the restrictions ring when narrowing with
;;    some Emacs-Lisp code (i.e. non-interactively), you can do so by
;;    binding variable `wide-n-push-anyway-p' around the narrowing
;;    call.
;;
;;    You can use `C-x n C-d' (command `wide-n-delete') to delete a
;;    restriction, giving its number.
;;
;;    You can also add the current region to `wide-n-restrictions'
;;    without first narrowing to it, using `C-x n s' (command
;;    `wide-n-push').  You need not activate the region to do this.
;;
;;    Buffer-local variable `wide-n-restrictions' holds the
;;    restrictions for the current buffer, by default.  But the
;;    functions provided by `wide-n.el' can use any variable that
;;    holds restrictions.  And you can choose whether any such
;;    variable is buffer-local or not.  If it is not then it can hold
;;    restrictions from multiple buffers.
;;
;;    Several commands defined here let you specify the restrictions
;;    variable to use, by prompting you if you use a prefix argument.
;;    For example, `C-u C-x n s' (`wide-n-push') prompts you for a
;;    restrictions variable, and `C-- C-x n s' does the same but it
;;    does not make the variable buffer-local.  You can specify any
;;    variable name, even if the variable does not yet exist (in which
;;    case it is given an empty value, which contains only `all').
;;
;;    Moving among different buffer restrictions, i.e., narrowing the
;;    buffer to different zones, is one use of this library, but
;;    another important use case is performing actions on a set of
;;    buffer zones, including perhaps zones from different buffers.
;;
;;    `C-x n r' (command `wide-n-select-region-repeat') does this,
;;    performing the action of selecting a zone in
;;    `wide-n-restrictions' as the region.  Repeat to cycle among the
;;    zones: `C-x n r r r...'.
;;
;;    You can define your own commands that iterate over the buffers
;;    and then over the entries in `wide-n-restrictions' (or some
;;    subset of them) for each buffer.  Utility functions
;;    `wide-n-limits', `wide-n-limits-in-bufs', and `wide-n-read-bufs'
;;    can help with this.
;;
;;    As examples of such commands, if you use library `highlight.el'
;;    then you can use `C-x n h' (command `hlt-highlight-regions') to
;;    highlight the restrictions recorded for the current buffer.  You
;;    can use `C-x n H' (command `hlt-highlight-regions-in-buffers')
;;    to do the same across a set of buffers that you specify (or
;;    across all visible buffers).  If option `hlt-auto-faces-flag' is
;;    non-nil then each region gets a different face.  Otherwise, all
;;    of the regions are highlighted with the same face.
;;    Complementary (unbound) commands `hlt-unhighlight-regions' and
;;    `hlt-unhighlight-regions-in-buffers' unhighlight.
;;
;;    Another way to look at the possibility of acting on multiple
;;    buffer zones is to think of it as enlarging the notion of
;;    "region".  In effect, it can remove the requirement of target
;;    text being a contiguous sequence of characters.  A set of buffer
;;    zones is, in effect, a (typically) noncontiguous "region" of
;;    text.
;;
;;    Companion library `zones.el' provides utility functions for
;;    buffer zones.  It is not strictly required by `wide-n.el', but
;;    you need it if you want to use commands `wide-n-unite' and
;;    `wide-n-add-to-union'.  These commands coalesce overlapping and
;;    contiguous zones, uniting them as a single zone.
;;
;;    Pretty much anything you can do with the Emacs region you can do
;;    with a set of buffer restrictions (a non-contiguous "region",
;;    `wide-n-restrictions').  But existing Emacs commands that act on
;;    the region do not know about non-contiguous regions.  What you
;;    will need to do is define new commands that take these into
;;    account.
;;
;;    This can be simple or somewhat complex, depending on how the
;;    region is used in the code for the corresponding region-action
;;    Emacs command.  The definition of `hlt-highlight-regions' just
;;    calls existing function `hlt-highlight-region' once for each
;;    recorded region:
;;
;; (defun hlt-highlight-regions (&optional regions face msgp mousep buffers)
;;   "Apply `hlt-highlight-region' to regions in `wide-n-restrictions'."
;;   (interactive (list (wide-n-limits) nil t current-prefix-arg))
;;   (dolist (start.end  regions)
;;     (hlt-highlight-region (car start.end) (cdr start.end)
;;                           face msgp mousep buffers)))
;;    
;;    That's it - just iterate over `wide-n-restrictions' with a
;;    function that takes the region as an argument.  What `wide-n.el'
;;    offers in this regard is a way to easily define a set of buffer
;;    restrictions.
;;
;;
;;  Commands defined here:
;;
;;    `wide-n', `wide-n-add-to-union', `wide-n-delete', `wide-n-push',
;;    `wide-n-repeat', `wide-n-select-region',
;;    `wide-n-select-region-repeat', `wide-n-unite'.
;;
;;  Non-interactive functions defined here:
;;
;;    `wide-n-highlight-lighter', `wide-n-limits',
;;    `wide-n-limits-in-bufs', `wide-n-markerize',
;;    `wide-n-mem-regexp', `wide-n-push', `wide-n-rassoc-delete-all',
;;    `wide-n-read-any-variable', `wide-n-read-bufs',
;;    `wide-n-remove-if-not', `wide-n-renumber',
;;    `wide-n-repeat-command', `wide-n-restrictions',
;;    `wide-n-restrictions-from-zones', `wide-n-restrictions-p',
;;    `wide-n-start+end', `wide-n-string-match-p'.
;;
;;  Internal variables defined here:
;;
;;    `wide-n-lighter-narrow-part', `wide-n-push-anyway-p',
;;    `wide-n-restrictions', `wide-n-restrictions-var'.
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
;; 2015/08/08 dadams
;;     Added: wide-n-unite, wide-n-add-to-union, wide-n-restrictions-from-zones.
;;     Bind wide-n-unite to C-x n u and wide-n-add-to-union to C-x n S.
;;     wide-n-push, wide-n-delete: Return new value of VARIABLE.
;;     wide-n-push: Change optional arg NOMSG to MSGP (invert the sense).
;;     Soft-require zones.el.
;;     Bind hlt-highlight-regions to C-x n h and hlt-highlight-regions-in-buffers to C-x n H.
;; 2015/08/07 dadams
;;     Added: wide-n-select-region, wide-n-select-region-repeat.
;;     Bind wide-n-select-region-repeat to C-x n r.
;;     wide-n-push, wide-n-delete: Prefix arg >= 0: make var buffer-local; <= 0: set wide-n-restrictions-var.
;; 2015/08/05 dadams
;;     Added: wide-n-restrictions-p, wide-n-restrictions-var, wide-n-read-any-variable.
;;     wide-n-restrictions (function): Now returns the value of the current wide-n-restrictions-var variable.
;;     wide-n: Use wide-n-restrictions-var, not wide-n-restrictions.
;;     wide-n-push, wide-n-delete:
;;       Added optional arg VARIABLE.  Prefix arg reads it.  Use it and maybe set wide-n-restrictions-var to it.
;;       Raise error if var is not wide-n-restrictions-p.
;;     wide-n-renumber: Added optional arg VARIABLE.
;;     wide-n-limits(-in-bufs): Added optional arg RESTRICTIONS.
;;     wide-n, wide-n-renumber, wide-n-markerize: FIX: (car (cddr...)), not cddr.
;; 2015/08/01 dadams
;;     wide-n-start+end: Fix: use list, not cons.
;; 2015/07/31 dadams
;;     Renamed: wide-n-start.end to wide-n-start+end.  Added: function wide-n-restrictions.
;;     wide-n-restrictions: INCOMPATIBLE CHANGE: The format is now (NUM START END), not (NUM START . END).
;; 2015/07/11 dadams
;;     Added: wide-n-limits, wide-n-limits-in-bufs, wide-n-start.end, wide-n-read-bufs, wide-n-remove-if-not.
;;     Made wide-n-push interactive.
;;     Bind wide-n-delete to C-x n C-d and wide-n-push to C-x n s.
;; 2014/08/12 dadams
;;     Added: wide-n-delete, wide-n-renumber.
;;     wide-n: Added optional arg MSGP.
;;     wide-n-push: Added optional arg NOMSG.
;; 2014/05/30 dadams
;;     Added: wide-n-lighter-narrow-part, wide-n-highlight-lighter, wide-n-string-match-p, wide-n-mem-regexp,
;;            wide-n-rassoc-delete-all.
;;     wide-n-restrictions: INCOMPATIBLE CHANGE: The format is now (NUM START . END), not (START . END).
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

(require 'zones nil t) ;; (no error if not found) zzz-zone-union


;; Quiet the byte-compiler.
(defvar narrow-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar wide-n-lighter-narrow-part ""
  "String to append to \" Narrow\" in mode-line lighter.")
(make-variable-buffer-local 'wide-n-lighter-narrow-part)

(defvar wide-n-restrictions-var 'wide-n-restrictions
  "Buffer-restrictions variable currently used by `wide-n' commands.
The variable can be buffer-local or not.  If not, then its value can
include markers from multiple buffers.")

(defvar wide-n-restrictions '(all)
  "List of buffer restrictions.
Each entry is either `all' or a cons (NUM START END), where NUM is a
counter identifying this buffer restriction, and START and END are its
limits.  `all' means no restriction (completely widened).")
(make-variable-buffer-local 'wide-n-restrictions)

;; Not used.  Could use this if really needed.
(defun wide-n-restrictions ()
  "Value of current `wide-n-restrictions-var' variable, in latest format.
If the value has elements of old format, (NUM START . END), it is
converted to use the new format, with elements (NUM START END).  This
is a destructive operation.  The value of the variable is updated to
use the new format, and that value is returned."
  (let ((oldval  (symbol-value wide-n-restrictions-var))
        (newval  ()))
    (dolist (elt  oldval)
      (unless (or (eq elt 'all)  (consp (cddr elt)))
        (setcdr (cdr elt) (list (cddr elt)))))
    (symbol-value wide-n-restrictions-var)))

(defvar wide-n-push-anyway-p nil
  "Non-nil means narrowing always updates current `wide-n-restrictions-var'.
Normally, if a narrowing command is called non-interactively then the
region limits are not pushed to the variable that is the current value
of `wide-n-restrictions-var'.  A non-nil value here overrides the push
inhibition.  You can bind this to non-nil in Lisp code to populate the
current `wide-n-restrictions-var' during narrowing.")

;;;###autoload
(defun wide-n-select-region (arg &optional msgp)
  "Select a region.
The restrictions are those in the current `wide-n-restrictions-var'.
With no prefix arg, select the previous recorded region.
With a numeric prefix arg N, select the Nth previous region."
  (interactive "p\np")
  (let* ((var   wide-n-restrictions-var)
         (val   (symbol-value var))
         (cntr  (abs arg)))
    (unless (cadr val) (error "No region to select"))
    (let ((latest  ()))
      (while (> cntr 0)
        (push (nth (1- cntr) val) latest)
        (setq cntr  (1- cntr)))
      (setq latest  (nreverse latest))
      (setq val  (set var (append (nthcdr arg val) latest))
            val  (set var (mapcar #'wide-n-markerize val)))
      (when (eq 'all (car val))
        (setq val  (set var (append (nthcdr 1 val) '(all)))))
      (let* ((wide-n-push-anyway-p  t)
             (restriction           (car val))
             (beg                   (cadr restriction))
             (end                   (car (cddr restriction))))
        (goto-char beg)
        (push-mark end nil t))
      (when msgp (message "Region #%d restored" (caar val))))))

;;;###autoload
(defun wide-n (arg &optional msgp)
  "Widen to a previous buffer restriction (narrowing).
The restrictions are those in the current `wide-n-restrictions-var'.
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
  (interactive "P\np")
  (let* ((var  wide-n-restrictions-var)
         (val  (symbol-value var)))
    (unless (cadr val) (error "Cannot widen; no previous narrowing"))
    (cond ((or (null (cdr val))  (consp arg))
           (widen)
           (setq wide-n-lighter-narrow-part  "")
           (wide-n-highlight-lighter)
           (when msgp (message "No longer narrowed")))
          ((= (prefix-numeric-value arg) 0)
           (set var (list 'all))
           (widen)
           (setq wide-n-lighter-narrow-part  "")
           (wide-n-highlight-lighter)
           (when msgp (message "No longer narrowed; no more restrictions")))
          (t
           (setq arg  (prefix-numeric-value arg))
           (let ((latest  ())
                 (cntr    (abs arg)))
             (while (> cntr 0)
               (push (nth (1- cntr) val) latest)
               (setq cntr  (1- cntr)))
             (setq latest  (nreverse latest))
             (when (< arg 0)
               (setq arg     (abs arg)
                     latest  (if (member 'all latest) (list 'all) ())))
             (setq val                         (set var (append (nthcdr arg val) latest))
                   val                         (set var (mapcar #'wide-n-markerize val))
                   wide-n-lighter-narrow-part  (if (eq 'all (car val)) "" (format "-%d" (caar val))))
             (if (not (eq 'all (car val)))
                 (condition-case err
                     (let ((wide-n-push-anyway-p  t))
                       (narrow-to-region (cadr (car val)) (car (cddr (car val))))
                       (wide-n-highlight-lighter))
                   (args-out-of-range
                    (set var  (cdr val))
                    (error "Restriction removed because of invalid limits"))
                   (error (error "%s" (error-message-string err))))
               (widen)
               (wide-n-highlight-lighter)
               (when msgp (message "No longer narrowed"))))))))

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
  (unless (or (atom restriction)  (and (markerp (cadr restriction))  (markerp (car (cddr restriction)))))
    (let ((mrk1     (make-marker))
          (mrk2     (make-marker)))
      (move-marker mrk1 (cadr restriction))
      (move-marker mrk2 (car (cddr restriction)))
      (setcar (cdr restriction) mrk1)
      (setcdr (cdr restriction) mrk2)))
  restriction)

;;;###autoload
(defun wide-n-push (start end &optional variable msgp) ; Bound to `C-x n s'.
  "Add a restriction from START to END to those of VARIABLE.
Return the new value of VARIABLE.

VARIABLE defaults to the value of `wide-n-restrictions-var'.
START and END are as for `narrow-to-region'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  If the
prefix arg is non-negative (>= 0) then make the variable buffer-local.
If the prefix arg is non-positive (<= 0) then set
`wide-n-restrictions-var' to that variable symbol.  (Zero: do both.)

Non-interactively:
* VARIABLE is the optional restrictions variable to use.
* Non-nil MSGP means echo the region size."
  (interactive (let ((beg    (region-beginning))
                     (end    (region-end))
                     (var    (and current-prefix-arg  (wide-n-read-any-variable "Variable: ")))
                     (npref  (prefix-numeric-value current-prefix-arg)))
                 (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var))
                 (when (and current-prefix-arg  (<= npref 0)) (setq wide-n-restrictions-var var))
                 (list beg end var t)))
  (let* ((mrk1    (make-marker))
         (mrk2    (make-marker))
         (var     (or variable  wide-n-restrictions-var)) 
         (IGNORE  (unless (boundp var) (set var  (list 'all))))
         (val     (symbol-value var))
         sans-id  id-cons  id)
    (unless (wide-n-restrictions-p val) (error "Not a buffer-restrictions variable: `%s', value: `%S'" var val))
    (move-marker mrk1 start)
    (move-marker mrk2 end)
    (setq sans-id  (list mrk1 mrk2)
          id-cons  (rassoc sans-id val)
          id       (if id-cons (car id-cons) (length val))
          val      (set var (wide-n-rassoc-delete-all sans-id val)))
    (unless (and (= mrk1 1)  (= mrk2 (1+ (buffer-size))))
      (set var `((,id ,mrk1 ,mrk2) ,@val)))
    (when msgp (message "%s region: %d to %d" (if (interactive-p) "Recorded" "Narrowed")
                        (marker-position mrk1) (marker-position mrk2)))
    (symbol-value var)))

(defun wide-n-restrictions-p (value)
  "Return non-nil if VALUE is a list of buffer restrictions.
That is, non-nil means that VALUE has the form of `wide-n-restrictions'."
  (and (listp value)  (listp (cdr (last value))) ; Proper list.
       (let ((res   t)
             (allp  nil))
         (catch 'wide-n-restrictions-p
           (dolist (nn  value)
             (unless (setq res  (if (eq 'all nn)
                                    (setq allp  t)
                                  (and (consp nn)  (condition-case nil
                                                       (and (number-or-marker-p (car nn))
                                                            (number-or-marker-p (cadr nn))
                                                            (number-or-marker-p (car (cddr nn))))
                                                     (error nil)))))
               (throw 'wide-n-restrictions-p nil))))
         (and allp  res))))

(defun wide-n-rassoc-delete-all (value alist)
  "Delete from ALIST all elements whose cdr is `equal' to VALUE.
This is a destructive operation.
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

;;;###autoload
(defun wide-n-delete (n &optional variable msgp) ; Bound to `C-x n C-d'.
  "Delete the restriction(s) numbered N from VARIABLE.
This renumbers the remaining restrictions.
Return the new value of VARIABLE.

You are prompted for the number N.
VARIABLE defaults to the value of `wide-n-restrictions-var'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  If the
prefix arg is non-negative (>= 0) then make the variable buffer-local.
If the prefix arg is non-positive (<= 0) then set
`wide-n-restrictions-var' to that variable symbol.  (Zero: do both.)

Non-nil optional arg NOMSG means do not display a status message."
  (interactive
   (let* ((var     (or (and current-prefix-arg  (wide-n-read-any-variable "Variable: "))
                       wide-n-restrictions-var))
          (npref   (prefix-numeric-value current-prefix-arg))
          (IGNORE  (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var)))
          (IGNORE  (when (and current-prefix-arg  (<= npref 0)) (setq wide-n-restrictions-var var)))
          (IGNORE  (unless (boundp var) (set var  (list 'all))))
          (val     (symbol-value var))
          (IGNORE  (unless (cadr val)
                     (error "No restrictions - you have not narrowed this buffer (`%s')" (buffer-name))))
          (len     (1- (length val)))
          (num     (if (= len 1) 1 (read-number (format "Delete restriction number (1 to %d): " len)))))
     (while (or (< num 1)  (> num len))
       (setq num  (read-number (format "Number must be between 1 and %d: " len))))
     (list num var t)))
  (unless variable (setq variable  wide-n-restrictions-var))
  (let ((val  (symbol-value variable)))
    (unless (wide-n-restrictions-p val) (error "Not a buffer-restrictions variable: `%s', value: `%S'" var val))
    (setq val  (set variable (delete 'all val)))
    (set variable (assq-delete-all n val)))
  (wide-n-renumber variable)
  (when msgp (message "Deleted restriction number %d" n))
  (symbol-value variable))

(defun wide-n-renumber (&optional variable)
  "Renumber restrictions of this buffer in current `wide-n-restrictions-var'."
  (let* ((var   (or variable  wide-n-restrictions-var))
         (orig  (symbol-value var)))
    (set var (list 'all))
    (dolist (nn  orig) (wide-n-push (cadr nn) (car (cddr nn)) var nil))))

(defun wide-n-limits-in-bufs (buffers &optional restrictions)
  "Return a list of all `wide-n-limits' for each buffer in BUFFERS.
That is, return a list of all recorded buffer narrowings for BUFFERS.
Optional arg RESTRICTIONS is the list of narrowings to use (default:
value of current `wide-n-restrictions-var')."
  (let ((limits  ()))
    (if buffers
        (dolist (buf  buffers)
          (with-current-buffer buf (setq limits  (nconc limits (wide-n-limits restrictions)))))
      (wide-n-limits restrictions))
    limits))

(defun wide-n-limits (&optional restrictions)
  "Return a list like RESTRICTIONS, but with no identifiers or `all'.
Optional input list RESTRICTIONS has the same structure as
`wide-n-restrictions'.  That is, each entry in the return value has
the form (START END).  The conses are new - they do not share with any
conses in RESTRICTIONS.

If RESTRICTIONS is nil then the value of the current
`wide-n-restrictions-var' (normally `wide-n-restrictions') is used."
  (delq nil (mapcar #'wide-n-start+end (or restrictions  (symbol-value wide-n-restrictions-var)))))

(defun wide-n-start+end (restriction)
  "Return a new cons (START END) corresponding to RESTRICTION, or nil.
If RESTRICTION is a cons then it has the form (NUM START END), so
 return a new cons (START END).
Otherwise RESTRICTION is `all', so return nil."
  (and (consp restriction)  (list (cadr restriction) (car (cddr restriction)))))

;; Useful for commands that want to act on regions in multiple buffers.
(defun wide-n-read-bufs ()
  "Read names of buffers, one at a time.  `C-g' ends reading."
  (let ((bufs  ())
        buf)
    (while (condition-case nil
               (setq buf  (read-buffer "Buffer (C-g to end): "
                                       (and (not (member (buffer-name (current-buffer)) bufs))
                                            (current-buffer))
                                       t))
             (quit nil))
      (push buf bufs))
    (delq nil (mapcar #'get-buffer (nreverse bufs)))))

;; Useful for commands that want to act on  regions in multiple buffers (e.g., visible buffers only).
;;
;; Same as `icicle-remove-if-not' etc.
(defun wide-n-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

;; Like `read-any-variable' in `strings.el', but uses lax completion.
(defun wide-n-read-any-variable (prompt &optional default-value)
  "Read name of a variable and return it as a symbol.
Unlike `read-variable', which reads only user options, this reads the
name of any variable.  In fact, it reads any symbol, but with
completion against variable names.

Prompts with arg string PROMPT.  By default, return DEFAULT-VALUE if
non-nil.  If DEFAULT-VALUE is nil and the nearest symbol to the cursor
is a variable, then return that by default."
  (let ((symb  (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                     ((fboundp 'symbol-at-point) (symbol-at-point))
                     (t nil)))
        (enable-recursive-minibuffers t))
    (when (and default-value  (symbolp default-value))
      (setq default-value  (symbol-name default-value)))
    (intern (completing-read prompt obarray 'boundp nil nil 'minibuffer-history
                             (or default-value  (and symb  (boundp symb)  (symbol-name symb)))
                             t))))

(defun wide-n-repeat-command (command)
  "Repeat COMMAND."
 (let ((repeat-previous-repeated-command  command)
       (repeat-message-function           'ignore)
       (last-repeatable-command           'repeat))
   (repeat nil)))

;;;###autoload
(defun wide-n-repeat (arg)              ; Bound to `C-x n x'.
  "Cycle to the next buffer restriction (narrowing).
This is a repeatable version of `wide-n'."
  (interactive "P")
  (require 'repeat)
  (wide-n-repeat-command 'wide-n))

;;;###autoload
(defun wide-n-select-region-repeat (arg) ; Bound to `C-x n r'.
  "Cycle to the next region.
This is a repeatable version of `wide-n-select-region'."
  (interactive "P")
  (require 'repeat)
  (wide-n-repeat-command 'wide-n-select-region))

(defun wide-n-restrictions-from-zones (zones)
  "Return a list of regions like `wide-n-restrictions', based on ZONES.
ZONES is a list of zones as in `zones.el': Each zone has the form
\(LIMIT1 LIMIT2 . EXTRA), where each of the limits is a buffer
position (a number or marker) and EXTRA is a list.

\(wide-n-restrictions-from-zones (wide-n-limits)) = wide-n-restrictions
and
\(wide-n-limits (wide-n-restrictions-from-zones ZONES)) = ZONES
except possibly for the position of element `all'."
  (let* ((ii    0)
         (regs  (mapcar (lambda (zz) (cons (setq ii  (1+ ii)) zz)) zones)))
    (nreverse (cons 'all regs))))

;;;###autoload
(defun wide-n-unite (&optional variable msgp)
  "Merge the restrictions of VARIABLE to form their union.
Return the new value of VARIABLE.

VARIABLE defaults to the value of `wide-n-restrictions-var'.
With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  If the
prefix arg is non-negative (>= 0) then make the variable buffer-local.
If the prefix arg is non-positive (<= 0) then set
`wide-n-restrictions-var' to that variable symbol.  (Zero: do both.)

Non-interactively:
* VARIABLE is the optional restrictions variable to use.
* Non-nil MSGP show status message.

You need library `zones.el' for this command."
  (interactive (let* ((var    (and current-prefix-arg  (wide-n-read-any-variable "Variable: ")))
                      (npref  (prefix-numeric-value current-prefix-arg)))
                 (unless (require 'zones nil t) (error "You need library `zones.el' for this command"))
                 (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var))
                 (when (and current-prefix-arg  (<= npref 0)) (setq wide-n-restrictions-var var))
                 (list var t)))
  (let* ((IGNORE      (unless (require 'zones nil t)
                        (error "You need library `zones.el' for this command")))
         (var         (or variable  wide-n-restrictions-var))
         (IGNORE      (unless (boundp var) (set var  (list 'all))))
         (val         (symbol-value var))
         (IGNORE      (unless (wide-n-restrictions-p val)
                        (error "Not a buffer-restrictions variable: `%s', value: `%S'" var val)))
         (zone-union  (zzz-zone-union (wide-n-limits val))))
    (set var  (wide-n-restrictions-from-zones zone-union))
    (when msgp (message "Restrictions united for `%s'" var))
    (symbol-value var)))

;;;###autoload
(defun wide-n-add-to-union (start end &optional variable msgp)
  "Add a restriction from START to END to those of VARIABLE, and unite.
Uses `wide-n-push' to add the region, then applies `wide-n-unite'.
Return the new value of VARIABLE.

VARIABLE defaults to the value of `wide-n-restrictions-var'.
START and END are as for `narrow-to-region'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  If the
prefix arg is non-negative (>= 0) then make the variable buffer-local.
If the prefix arg is non-positive (<= 0) then set
`wide-n-restrictions-var' to that variable symbol.  (Zero: do both.)

Non-interactively:
* VARIABLE is the optional restrictions variable to use.
* Non-nil MSGP means echo the region size."
  (interactive (let ((beg    (region-beginning))
                     (end    (region-end))
                     (var    (and current-prefix-arg  (wide-n-read-any-variable "Variable: ")))
                     (npref  (prefix-numeric-value current-prefix-arg)))
                 (when (and current-prefix-arg  (>= npref 0)) (make-local-variable var))
                 (when (and current-prefix-arg  (<= npref 0)) (setq wide-n-restrictions-var var))
                 (list beg end var t)))
  (wide-n-push start end variable msgp)
  (wide-n-unite variable msgp)
  (symbol-value variable))


;;---------------------

(cond ((boundp 'narrow-map)
       (define-key narrow-map "\C-d" 'wide-n-delete)
       (when (fboundp 'hlt-highlight-regions)
         (define-key narrow-map "h"  'hlt-highlight-regions))
       (when (fboundp 'hlt-highlight-regions)
         (define-key narrow-map "H"  'hlt-highlight-regions-in-buffers))
       (define-key narrow-map "r"    'wide-n-select-region-repeat)
       (define-key narrow-map "s"    'wide-n-push)
       (define-key narrow-map "S"    'wide-n-add-to-union)
       (define-key narrow-map "u"    'wide-n-unite)
       (define-key narrow-map "x"    'wide-n-repeat))
      (t
       (define-key ctl-x-map "n\C-d" 'wide-n-delete)
       (when (fboundp 'hlt-highlight-regions)
         (define-key ctl-x-map "nh"  'hlt-highlight-regions))
       (when (fboundp 'hlt-highlight-regions)
         (define-key ctl-x-map "nH"  'hlt-highlight-regions-in-buffers))
       (define-key ctl-x-map "nr"    'wide-n-select-region-repeat)
       (define-key ctl-x-map "ns"    'wide-n-push)
       (define-key ctl-x-map "nS"    'wide-n-add-to-union)
       (define-key ctl-x-map "nu"    'wide-n-unite)
       (define-key ctl-x-map "nx"    (if (> emacs-major-version 21) 'wide-n-repeat 'wide-n))))


;; Call `wide-n-push' if interactive or if `wide-n-push-anyway-p'.
;;
(defadvice narrow-to-region (before push-wide-n-restrictions activate)
  "Push the region limits to the current `wide-n-restrictions-var'.
You can use `C-x n x' to widen to a previous buffer restriction."
  (when (or (interactive-p) wide-n-push-anyway-p)
    (wide-n-push (ad-get-arg 0) (ad-get-arg 1) nil 'MSG))) ; Args START and END.


;; REPLACE ORIGINAL in `lisp.el'.
;;
;; Call `wide-n-push' if interactive or `wide-n-push-anyway-p'.
;;
;;;###autoload
(defun narrow-to-defun (&optional arg)
  "Make text outside current defun invisible.
The visible defun is the one that contains point or follows point.
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
      (when (or (interactive-p)  wide-n-push-anyway-p) (wide-n-push beg end nil 'MSG))
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
      (when (or (interactive-p)  wide-n-push-anyway-p) (wide-n-push beg end nil 'MSG))
      (narrow-to-region beg end))))

;;;;;;;;;;;;;;;;;;;;

(provide 'wide-n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wide-n.el ends here
