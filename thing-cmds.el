;;; thing-cmds.el --- Commands that use things, as defined by `thingatpt.el'.
;;
;; Filename: thing-cmds.el
;; Description: Commands that use things, as defined by `thingatpt.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2006-2016, Drew Adams, all rights reserved.
;; Created: Sun Jul 30 16:40:29 2006
;; Version: 0
;; Package-Requires: ((hide-comnt "0"))
;; Last-Updated: Thu Dec 31 16:15:03 2015 (-0800)
;;           By: dradams
;;     Update #: 758
;; URL: http://www.emacswiki.org/thing-cmds.el
;; Doc URL: http://www.emacswiki.org/ThingAtPointCommands
;; Keywords: thingatpt, thing, region, selection
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  You can use the commands defined here to select or move to
;;  different kinds of text entities ("things") that are at or near
;;  point.  They are especially useful in combination with Transient
;;  Mark mode.
;;
;;
;;  Commands defined here:
;;
;;    `cycle-thing-region', `mark-enclosing-list',
;;    `mark-enclosing-list-backward', `mark-enclosing-list-forward',
;;    `mark-thing', `next-visible-thing', `next-visible-thing-repeat',
;;    `previous-visible-thing', `previous-visible-thing-repeat',
;;    `select-thing-near-point', `thgcmd-bind-keys', `thing-region'.
;;
;;  User options defined here:
;;
;;    `thing-types'.
;;
;;  Non-interactive functions defined here:
;;
;;    `thgcmd-bounds-of-thing-at-point', `thgcmd-invisible-p',
;;    `thgcmd-next-visible-thing-1', `thgcmd-next-visible-thing-2',
;;    `thgcmd-repeat-command', `thgcmd-things-alist'.
;;
;;  Internal variables defined here:
;;
;;    `thgcmd-defined-thing-p', `thgcmd-last-thing-type',
;;    `thgcmd-thing-region-index', `thgcmd-thing-region-point'.
;;
;;  Put this in your init file (`~/.emacs'):
;;
;;   (require 'thing-cmds)
;;   (thgcmd-bind-keys) ; Only if you want the key bindings it defines
;;
;;  See also the doc strings of `next-visible-thing' and
;;  `thgcmd-bind-keys', for more information about thing navigation
;;  keys.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/12/01 dadams
;;     Typo: emacs-version -> emacs-major-version.
;; 2014/11/17 dadams
;;     Top level: Added eval-when-compile for cl.el, for Emacs 20.
;;     thing-types: Need require cl.el at compile time, for push and dolist, for Emacs 20.
;; 2013/11/07 dadams
;;     Renamed: mark-enclosing-sexp* to mark-enclosing-list*.
;;     mark-enclosing-list-(forward|backward): 2nd arg to mark-enclosing sexp just needs to be t.
;; 2013/10/02 dadams
;;     thgcmd-next-visible-thing-1: Put back <=, not <, for comparison.  See comment.
;; 2013/09/08 dadams
;;     next-visible-thing: Use point-max if region is active but empty.
;; 2013/07/22 dadams
;;     Code that uses with-comments-hidden conditional needs Emacs 21+, for hide-comnt.el.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Added: thgcmd-bounds-of-thing-at-point.
;;     bounds-of-thing-at-point -> thgcmd-bounds-of-thing-at-point everywhere.
;; 2012/08/17 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;;     Require thingatpt.el before thingatpt+.el.
;; 2011/12/15 dadams
;;     mark-thing:
;;       If not on a thing, use thing-region to capture one.  If in a thing and not extending
;;       existing region, then extend region back to beginning. to select the whole thing.
;; 2011/07/02 dadams
;;     Removed autoload cookie from thing-types.  Thx to Michael Heerdegen.
;; 2011/05/24 dadams
;;     thgcmd-invisible-p: Use invisible-p if available (Emacs 22+).
;; 2011/05/14 dadams
;;     Added: thgcmd-things-alist.
;;     thing-region, mark-thing, (next|previous)-visible-thing:
;;       Use thgcmd-things-alist, not thing-types.
;; 2011/05/13 dadams
;;     Added: thgcmd-defined-thing-p (from icicle-defined-thing-p).
;;     Combined thgcmd-last-visible-thing-type and thgcmd-mark-thing-type into
;;       thgcmd-last-thing-type.
;;     Renamed: cycle-thing-region-point to thgcmd-thing-region-point.
;;     thing-types: Default value defined using thgcmd-defined-thing-p, not a constant.
;;     thing-region, mark-thing: Use name of thgcmd-last-thing-type as default value.
;;     thing-region: Save THING as thgcmd-last-thing-type.
;;     (next|previous)-visible-thing: Use completing-read, not read-string.
;;     thgcmd-next-visible-thing-2: Be sure not to return an empty thing ("").
;; 2011/05/11 dadams
;;     Added: thgcmd-invisible-p.
;;     Moved comment hide/show stuff to new library hide-comnt.el, and require that.
;;       ignore-comments-flag, hide/show-comments, with-comments-hidden.
;;     Renamed to use prefix thgcmd-: next-visible-thing-(1|2), last-visible-thing-type,
;;                                    mark-thing-type, thing-region-index
;;     thgcmd-next-visible-thing-2: Separate handling overlay & text prop. Use thgcmd-invisible-p.
;; 2011/05/10 dadams
;;     Added (copied here from icicles-cmd2.el):
;;      ignore-comments-flag, hide/show-comments, last-visible-thing-type, with-comments-hidden,
;;      next-visible-thing(-1|-2), previous-visible-thing.
;;     Added: thgcmd-repeat-command, thgcmd-bind-keys, (next|previous)-visible-thing-repeat.
;;     Extended (next|previous)-visible-thing to work with thgcmd-repeat-command.
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom and commands.
;;     Added groups for defcustom.
;; 2010/12/17 dadams
;;     Added: mark-enclosing-sexp(-forward|-backward).
;; 2008/11/29 dadams
;;     mark-thing: Set point to beginning/end of thing, so whole thing gets marked.
;;                 Make completion default be the first element of thing-types.
;; 2007/07/15 dadams
;;     Added cycle-thing-region-point.
;;     cycle-thing-region: Save point in cycle-thing-region-point and reuse it.
;; 2006/07/30 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

(when (< emacs-major-version 22) (eval-when-compile (require 'cl))) ;; for Emacs 20: dolist

(require 'thingatpt) ;; bounds-of-thing-at-point
(when (and (require 'thingatpt+ nil t)  ; (no error if not found): tap-bounds-of-thing-at-point
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
  ;; tap-bounds-of-thing-at-point, bounds-of-thing-nearest-point

(when (> emacs-major-version 20)        ; `hide-comnt.el' is for Emacs 21+.
  (require 'hide-comnt));; with-comments-hidden, so also hide/show-comments, ignore-comments-flag.

;; Quiet the byte-compiler
(defvar last-repeatable-command)        ; Defined in `repeat.el'.

;;;;;;;;;;;;;;;;;;;;;;;;

;; Same as `icicle-bounds-of-thing-at-point'.
(defun thgcmd-bounds-of-thing-at-point (thing &optional syntax-table)
  "`thingatpt+.el' version of `bounds-of-thing-at-point', if possible.
`tap-bounds-of-thing-at-point' if defined, else
`bounds-of-thing-at-point'.
if non-nil, set SYNTAX-TABLE for the duration."
  (if (fboundp 'tap-bounds-of-thing-at-point)
      (tap-bounds-of-thing-at-point thing syntax-table)
    (if (fboundp 'with-syntax-table)    ; Emacs 21+.
        (with-syntax-table syntax-table (bounds-of-thing-at-point thing syntax-table))
      (bounds-of-thing-at-point thing syntax-table))))

(defun thgcmd-defined-thing-p (thing)
  "Return non-nil if THING (type) is defined as a thing-at-point type."
  (let ((forward-op    (or (get thing 'forward-op)  (intern-soft (format "forward-%s" thing))))
        (beginning-op  (get thing 'beginning-op))
        (end-op        (get thing 'end-op))
        (bounds-fn     (get thing 'bounds-of-thing-at-point))
        (thing-fn      (get thing 'thing-at-point)))
    (or (functionp forward-op)
        (and (functionp beginning-op)  (functionp end-op))
        (functionp bounds-fn)
        (functionp thing-fn))))

(defcustom thing-types (let ((types  ()))
                         (eval-when-compile ;; `push', `dolist', for Emacs 20
                          (when (< emacs-major-version 21) (require 'cl)))
                         (mapatoms
                          (lambda (tt)
                            (when (thgcmd-defined-thing-p tt) (push (symbol-name tt) types))))
                         (setq types  (sort types #'string-lessp))
                         ;; Remove types that do not make sense.
                         (dolist (typ  '("sexp" "thing" "buffer" "point"))
                           (setq types (delete typ types)))
                         (setq types  (cons "sexp" types))) ; Put `sexp' first.

  ;; ("sexp" "button" "char" "char-same-line" "comment" "decimal-number" "defun" "email"
  ;;  "filename" "hex-number" "line" "list" "list-contents" "non-nil-symbol-name" "number"
  ;;  "overlay" "page" "region-or-word" "sentence" "string" "string-contents" "symbol"
  ;;  "unquoted-list" "url" "whitespace" "whitespace-&-newlines" "word")

  "*List of thing types.
Each is a string that names a type of text entity for which there is a
either a corresponding `forward-'thing operation, or corresponding
`beginning-of-'thing and `end-of-'thing operations.

The default value includes the names of most symbols that satisfy
`thgcmd-defined-thing-p' at the time the `defcustom' is evaluated.
These types are excluded: `thing', `buffer', `point'.

Command `cycle-thing-region' cycles through this list, in order."
  :type '(repeat string) :group 'lisp :group 'editing)

(defvar thgcmd-thing-region-index 0 "Index of current thing in `thing-types'.")

(defvar thgcmd-thing-region-point nil "Position of point before invoking `cycle-thing-region'.")

(defvar thgcmd-last-thing-type 'sexp "Last thing type (a symbol) used by various commands.")

(defun thgcmd-things-alist ()
  "List of most thing types currently defined.
Each is a string that names a type of text entity for which there is a
either a corresponding `forward-'thing operation, or corresponding
`beginning-of-'thing and `end-of-'thing operations.  The list includes
the names of the symbols that satisfy `thgcmd-defined-thing-p', but
with these excluded: `thing', `buffer', `point'."
  (let ((types  ()))
    (mapatoms
     (lambda (tt)
       (when (thgcmd-defined-thing-p tt) (push (symbol-name tt) types))))
    (dolist (typ  '("thing" "buffer" "point")) ; Remove types that do not make sense.
      (setq types (delete typ types)))
    (setq types  (sort types #'string-lessp))))

;;;###autoload
(defun thing-region (thing)
  "Set the region around a THING near the cursor.
You are prompted for the type of thing.  Completion is available (lax).
The cursor is placed at the end of the region.  You can return it to
the original location by using `C-u C-SPC' twice.
Non-interactively, THING is a string naming a thing type."
  (interactive (list (let ((icicle-sort-function  nil))
                       (completing-read "Thing (type): " (thgcmd-things-alist) nil nil nil nil
                                        (symbol-name thgcmd-last-thing-type)))))
  (setq thgcmd-last-thing-type  (intern thing))
  (let* ((bds    (if (fboundp 'bounds-of-thing-nearest-point) ; In `thingatpt+.el'.
                     (bounds-of-thing-nearest-point (intern thing))
                   (thgcmd-bounds-of-thing-at-point (intern thing))))
         (start  (car bds))
         (end    (cdr bds)))
    (cond ((and start  end)
           (push-mark (point) t)        ; Mark position, so can use `C-u C-SPC'.
           (goto-char end)
           (push-mark start t 'activate)
           (setq deactivate-mark  nil)
           thing)                       ; Return thing.
          (t
           (message "No `%s' near point" thing)
           (setq deactivate-mark  nil)
           nil))))                      ; Return nil: no thing found.

;;;###autoload
(defalias 'select-thing-near-point 'cycle-thing-region)
;;;###autoload
(defun cycle-thing-region ()
  "Select a thing near point.  Successive uses select different things.
The default thing type is the first element of option `thing-types'.
In Transient Mark mode, you can follow this with `\\[mark-thing]' to select
successive things of the same type, but to do that you must first use
`C-x C-x': `\\[cycle-thing-region] C-x C-x \\[mark-thing]'"
  (interactive)
  (if (eq last-command this-command)
      (goto-char thgcmd-thing-region-point)
    (setq thgcmd-thing-region-index  0
          thgcmd-thing-region-point   (point)))
  (let* ((thing    (elt thing-types thgcmd-thing-region-index))
         (success  (thing-region thing)))
    (setq thgcmd-thing-region-index  (1+ thgcmd-thing-region-index))
    (when success
      (setq thgcmd-last-thing-type  (intern thing))
      (message "%s" (capitalize (elt thing-types (1- thgcmd-thing-region-index)))))
    (when (>= thgcmd-thing-region-index (length thing-types))
      (setq thgcmd-thing-region-index  0))))

;;;###autoload
(defun mark-thing (thing &optional arg allow-extend)
  "Set point at one end of THING and set mark ARG THINGs from point.
Put mark at the same place command `forward-'THING would move point
with the same prefix argument.

Put point at the beginning of THING, unless the prefix argument (ARG)
is negative, in which case put it at the end of THING.

THING is a symbol that names a type of thing.  Interactively, you are
prompted for it.  Completion is available (lax).

If `mark-thing' is repeated or if the mark is active (in Transient
Mark mode), then it marks the next ARG THINGs, after the ones already
marked.  The type of THING used is whatever was used the last time
`mark-thing' was called.

This region extension reusing the last type of THING happens even if
the active region is empty.  This means that you can, for instance,
just use `C-SPC' to activate an empty region and then use `mark-thing'
to select more THINGS of the last kind selected."
  (interactive "i\nP\np")               ; THING arg is nil (ignored) interactively.
  (let ((this-cmd  this-command)
        (last-cmd  last-command)
        (regionp   mark-active))
    (cond ((and allow-extend  (or (and (eq last-cmd this-cmd)  (mark t))
                                  (and transient-mark-mode  mark-active)))
           (setq arg  (if arg  (prefix-numeric-value arg)  (if (< (mark) (point)) -1 1)))
           (set-mark (save-excursion (goto-char (mark))
                                     (forward-thing thgcmd-last-thing-type arg)
                                     (point))))
          (t
           (setq thgcmd-last-thing-type
                 (or thing
                     (prog1 (let ((icicle-sort-function  nil))
                              (intern (completing-read
                                       "Thing (type): " (thgcmd-things-alist) nil nil nil nil
                                       (symbol-name thgcmd-last-thing-type))))
                       (setq this-command  this-cmd))))
           (push-mark (save-excursion
                        (forward-thing thgcmd-last-thing-type (prefix-numeric-value arg))
                        (point))
                      nil t)))
    (let ((bnds  (thgcmd-bounds-of-thing-at-point thgcmd-last-thing-type)))
      (unless (or regionp  bnds)
        ;; If we are not on a thing, use `thing-region' to capture one.
        ;; Because it always puts point after mark, flip them if necessary.
        (thing-region (symbol-name thgcmd-last-thing-type))
        (when (natnump (prefix-numeric-value arg)) (exchange-point-and-mark)))
      ;; If we are not extending existing region, and we are in a thing (BNDS non-nil), then:
      ;; We have moved forward (or backward if ARG < 0) to the end of the thing.
      ;; Now we extend the region backward (or forward if ARG < 0) up to its beginning
      ;; (or end if ARG < 0), to select the whole thing.
      (unless (or regionp  (not bnds)  (eql (point) (car bnds)))
        (forward-thing thgcmd-last-thing-type (if (< (mark) (point)) 1 -1)))))
  (setq deactivate-mark  nil))

;;;###autoload
(defun mark-enclosing-list (&optional arg allow-extend) ; `C-M-U' (aka `C-M-S-u')
  "Select a list surrounding the current cursor position.
If the mark is active (e.g. when the command is repeated), widen the
region to a list that encloses it.

The starting position is added to the mark ring before doing anything
else, so you can return to it (e.g. using `C-u C-SPC').

A prefix argument determines which enclosing list is selected: 1 means
the immediately enclosing list, 2 means the list immediately enclosing
that one, etc.

A negative prefix argument puts point at the beginning of the region
instead of the end.

\"List\" here really means a balanced-parenthesis expression.  The
syntax table determines which characters are such balanced delimiters.
See (emacs) `Moving by Parens' and (elisp) `List Motion'.

This command might does not work as expected if point is in a string
or a comment."
  (interactive "P\np")
  (cond ((and allow-extend  (or (and (eq last-command this-command)  (mark t))
                                (and transient-mark-mode  mark-active)))
	 (setq arg  (if arg  (prefix-numeric-value arg)  (if (< (mark) (point)) 1 -1)))
	 (set-mark (save-excursion (up-list (- arg)) (point)))
         (up-list arg))
	(t
         (push-mark nil t)              ; So user can get back.
	 (setq arg  (prefix-numeric-value arg))
         (push-mark (save-excursion (up-list (- arg)) (point)) nil t)
         (up-list arg))))

;;;###autoload
(defun mark-enclosing-list-forward (&optional arg) ; `C-M-F' or maybe `C-M-)'
  "`mark-enclosing-list' leaving point at region end."
  (interactive "P")
  (if (or (and (eq last-command this-command)  (mark t))  (and transient-mark-mode  mark-active))
      (mark-enclosing-list nil t)
    (mark-enclosing-list (prefix-numeric-value arg) t)))

;;;###autoload
(defun mark-enclosing-list-backward (&optional arg) ; `C-M-B' or maybe `C-M-('
  "`mark-enclosing-list' leaving point at region start."
  (interactive "P")
  (if (or (and (eq last-command this-command)  (mark t))  (and transient-mark-mode  mark-active))
      (mark-enclosing-list nil t)
    (mark-enclosing-list (- (prefix-numeric-value arg)) t)))

(when (> emacs-major-version 20)        ; `hide-comnt.el' is for Emacs 21+.
  (defun previous-visible-thing (thing start &optional end)
    "Same as `next-visible-thing', except it moves backward, not forward."
    (interactive
     (list (or (and (memq last-command '(next-visible-thing previous-visible-thing))
                    thgcmd-last-thing-type)
               (if (or (not (boundp 'DO-NOT-USE-!@$%^&*+))
                       (prog1 DO-NOT-USE-!@$%^&*+  (setq DO-NOT-USE-!@$%^&*+  nil)))
                   ;; Save state for `repeat'.
                   (let ((last-command-event       last-command-event)
                         (last-repeatable-command  last-repeatable-command))
                     (intern (completing-read
                              "Thing (type): " (thgcmd-things-alist) nil nil nil nil
                              (symbol-name thgcmd-last-thing-type))))
                 thgcmd-last-thing-type))
           (point)
           (if mark-active  (min (region-beginning) (region-end))  (point-min))))
    (if (interactive-p)
        (with-comments-hidden start end (next-visible-thing thing start end 'BACKWARD))
      (next-visible-thing thing start end 'BACKWARD))))

(when (> emacs-major-version 20)        ; `hide-comnt.el' is for Emacs 21+.
  (defun next-visible-thing (thing &optional start end backward)
    "Go to the next visible THING.
Start at START.  If END is non-nil then look no farther than END.
Interactively:
 - START is point.
 - If the region is not active, END is the buffer end.  If the region
   is active, END is the region end: the greater of point and mark.

Ignores (skips) comments if `ignore-comments-flag' is non-nil.  If you
also use Icicles then you can toggle this ignoring of comments using
`C-M-;' in the minibuffer, but depending on when you do so you might
need to invoke the current command again.

If you use this command or `previous-visible-thing' successively, even
mixing the two, you are prompted for the type of THING only the first
time.  You can thus bind these two commands to simple repeatable keys
\(e.g. `f8', `f9'), to navigate among things quickly.

If you do not want to sacrifice two simple repeatable keys for this,
then you can instead use commands `next-visible-thing-repeat' and
`previous-visible-thing-repeat', binding them each to a less rare key
sequence that uses a prefix key.  Command `thgcmd-bind-keys' does
this: it binds them to `C-x down' and `C-x up', so you can repeat them
separately using `C-x down down...' etc.  However, unlike bindings for
`next-visible-thing' and `previous-visible-thing', switching from one
direction to the other requires you to again enter the THING type.

Non-interactively, THING is a symbol, and optional arg BACKWARD means
go to the previous thing.

Return (THING THING-START . THING-END), with THING-START and THING-END
the bounds of THING.  Return nil if no such THING is found."
    (interactive
     (list (or (and (memq last-command '(next-visible-thing previous-visible-thing))
                    thgcmd-last-thing-type)
               (if (or (not (boundp 'DO-NOT-USE-!@$%^&*+))
                       (prog1 DO-NOT-USE-!@$%^&*+ (setq DO-NOT-USE-!@$%^&*+  nil)))
                   ;; Save state for `repeat'.
                   (let ((last-command-event       last-command-event)
                         (last-repeatable-command  last-repeatable-command))
                     (intern (completing-read
                              "Thing (type): " (thgcmd-things-alist) nil nil nil nil
                              (symbol-name thgcmd-last-thing-type))))
                 thgcmd-last-thing-type))
           (point)
           (if (and mark-active  (not (eq (region-beginning) (region-end))))
               (max (region-beginning) (region-end))
             (point-max))))
    (setq thgcmd-last-thing-type  thing)
    (unless start (setq start  (point)))
    (unless end   (setq end  (if backward (point-min) (point-max))))
    (cond ((< start end) (when   backward (setq start  (prog1 end (setq end  start)))))
          ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
    (if (interactive-p)
        (with-comments-hidden start end (thgcmd-next-visible-thing-1 thing start end backward))
      (thgcmd-next-visible-thing-1 thing start end backward)))

  (defun thgcmd-next-visible-thing-1 (thing start end backward)
    "Helper for `next-visible-thing'.  Get thing past point."
    (let ((thg+bds  (thgcmd-next-visible-thing-2 thing start end backward)))
      (if (not thg+bds)
          nil
        ;; $$$$$$ Which is better, > or >=, < or <=, for the comparisons?
        ;;        Seems that < is better than <=, at least for `icicle-search-thing':
        ;;        for XML elements and lists, <= misses the first one.
        ;; $$$$$$ No, I don't think that is the case (anymore).
        ;;        <= is OK and is needed for interactive use of `next-visible-thing'.  
        (while (and thg+bds  (if backward (> (cddr thg+bds) (point)) (<= (cadr thg+bds) (point))))
          (if backward
              (setq start  (max end (1- (cadr thg+bds))))
            (setq start  (min end (1+ (cddr thg+bds)))))
          (setq thg+bds  (thgcmd-next-visible-thing-2 thing start end backward)))
        (when thg+bds (goto-char (cadr thg+bds)))
        thg+bds)))

  (defun thgcmd-next-visible-thing-2 (thing start end &optional backward)
    "Helper for `thgcmd-next-visible-thing-1'.  Thing might not be past START."
    (and (not (= start end))
         (save-excursion
           (let ((bounds  nil))
             ;; If BACKWARD, swap START and END.
             (cond ((< start end) (when   backward (setq start  (prog1 end (setq end  start)))))
                   ((> start end) (unless backward (setq start  (prog1 end (setq end  start))))))
             (catch 'thgcmd-next-visible-thing-2
               (while (if backward (> start end) (< start end))
                 (goto-char start)
                 ;; Skip invisible text.
                 (when (and (if backward (> start end) (< start end))  (thgcmd-invisible-p start))
                   (setq start  (if (get-text-property start 'invisible) ; Text prop.
                                    (if backward
                                        (previous-single-property-change start 'invisible nil end)
                                      (next-single-property-change start 'invisible nil end))
                                  (if backward ; Overlay prop.
                                      (previous-overlay-change start)
                                    (next-overlay-change start))))
                   (goto-char start))
                 (when (and (setq bounds  (thgcmd-bounds-of-thing-at-point thing))
                            (not (equal (car bounds) (cdr bounds)))) ; Not an empty thing, "".
                   (throw 'thgcmd-next-visible-thing-2
                     (cons (buffer-substring (car bounds) (cdr bounds)) bounds)))
                 (setq start  (if backward (1- start) (1+ start))))
               nil))))))

(defun thgcmd-invisible-p (position)
  "Return non-nil if the character at POSITION is invisible."
  (if (fboundp 'invisible-p)            ; Emacs 22+
      (invisible-p position)
    (let ((prop  (get-char-property position 'invisible))) ; Overlay or text property.
      (if (eq buffer-invisibility-spec t)
          prop
        (or (memq prop buffer-invisibility-spec)  (assq prop buffer-invisibility-spec))))))

(defun thgcmd-repeat-command (command)
  "Repeat COMMAND."
  (let ((repeat-message-function  'ignore))
    (setq last-repeatable-command  command)
    (repeat nil)))

;;;###autoload
(defun next-visible-thing-repeat ()
  "Go to and get the next visible THING.
This is a repeatable version of `next-visible-thing'."
  (interactive)
  (require 'repeat)
  (let ((DO-NOT-USE-!@$%^&*+  t))  (thgcmd-repeat-command 'next-visible-thing)))

;;;###autoload
(defun previous-visible-thing-repeat ()
  "Go to and get the previous visible THING.
This is a repeatable version of `previous-visible-thing'."
  (interactive)
  (require 'repeat)
  (let ((DO-NOT-USE-!@$%^&*+  t))  (thgcmd-repeat-command 'previous-visible-thing)))

;;;###autoload
(defun thgcmd-bind-keys (&optional msgp)
  "Bind some keys to commands defined in `thing-cmds.el'.
NOTE concerning the visible-thing navigation keys:

`C-x down' and `C-x up' are bound here (for Emacs 21 and later) to
`next-visible-thing-repeat' and `previous-visible-thing-repeat',
respectively.  This means you can use `C-x down down down...' etc. to
move forward to successive things, and similarly for `C-x up...' and
backward.  You are asked for the thing type only the first time you
hit `down' or `up' after `C-x'.

However, you cannot mix the directions forward/backward without
inputting the thing type again.  For example, If you do `C-x down up',
the `up' does not perform thing navigation (it probably does
`previous-line', the default `up' binding) .

To change direction without getting prompted for the thing type, you
need to bind, not commands `next-visible-thing-repeat' and
`previous-visible-thing-repeat', but commands `next-visible-thing' and
`previous-visible-thing' (no `-repeat' suffix).  Bind these to simple,
repeatable keys, such as `f8' and `f9'.  Because such keys are rare
\(mostly taken already), the only bindings made here for thing
navigation are `C-x down' and `C-x up'."
  (interactive "p")
  (when (or (not msgp)  (y-or-n-p "Bind thing-command default keys?"))
    ;;   The first two replace the standard bindings for `mark-sexp' & `mark-word':
    (global-set-key [(control meta ? )] 'mark-thing) ; vs `mark-sexp'
    (global-set-key [(meta ?@)] 'cycle-thing-region) ; vs `mark-word'
    (global-set-key [(control meta shift ?u)] 'mark-enclosing-list)
    (global-set-key [(control meta shift ?b)] ; Alternative to consider: [(control meta ?()]
                    'mark-enclosing-list-backward)
    (global-set-key [(control meta shift ?f)] ; Alternative to consider: [(control meta ?))]
                    'mark-enclosing-list-forward)
    (when (> emacs-major-version 21)
      (define-key ctl-x-map [down]  'next-visible-thing-repeat)
      (define-key ctl-x-map [up]    'previous-visible-thing-repeat))
    (when msgp (message "Thing-command keys bound"))))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'thing-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thing-cmds.el ends here
