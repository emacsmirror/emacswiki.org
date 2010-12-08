;;; mon-buffer-utils.el --- buffer related procedures for mon-*utils features
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-buffer-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-22T15:55:11-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-buffer-utils provides buffer related procedures for mon-*utils features
;;
;; FUNCTIONS:►►►
;; `mon-g2be', `mon-buffer-sub-no-prop', `mon-buffer-sub-no-prop-check',
;; `mon-buffer-narrowed-p', `mon-buffer-empty-p', `mon-buffer-exists-so-kill',
;; `mon-get-buffer-hidden', `mon-get-buffer-window-if',
;; `mon-print-buffer-object-readably', `mon-get-buffer-w-mode',
;; `mon-buffer-name->kill-ring', `mon-append-to-buffer', `mon-make-shell-buffer',
;; `mon-shell', `mon-buffer-longlines-mode-p', `mon-buffer-check-major-mode',
;; `mon-buffer-check-local-value',
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
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; <UNQUALIFIED-ALIAS>                  <PREFIX>-<NON-CORE-SYMBOL>
;; `get-buffer-window-if'            -> `mon-get-buffer-window-if'
;; `buffer-narrowed-p'               -> `mon-buffer-narrowed-p'
;;
;; <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-longlines-mode-p'            -> `mon-buffer-longlines-mode-p' 
;; `mon-buffer-make-shell'           -> `mon-make-shell-buffer'
;; `mon-buffer-name-print-readably'  -> `mon-print-buffer-object-readably'
;; `mon-buffer-get-w-mode'           -> `mon-get-buffer-w-mode'
;; `mon-buffer-append-to'            -> `mon-append-to-buffer'
;; `mon-buffer-end'                  -> `mon-g2be'
;; `mon-buffer-get-hidden'           -> `mon-get-buffer-hidden'
;; `mon-get-hidden-buffers'          -> `mon-get-buffer-hidden'
;; `mon-help-hidden-buffers'         -> `mon-get-buffer-hidden'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-buffer-longlines-mode-p'                <- mon-replacement-utils.el
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
;; URL: http://www.emacswiki.org/emacs/mon-buffer-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-buffer-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-22T15:55:11-05:00Z}#{10471} - by MON KEY>
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
;;; :MODIFICATIONS <Timestamp: #{2010-03-20T12:59:35-04:00Z}#{10116} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-03-10T20:04:58-05:00Z}#{10104} - by MON KEY>
;;; :WAS (defun mon-g2be (&optional min/max)
(defun mon-g2be (&optional min/max-go no-go)
  "Move point as with `goto-char' to `point-max' or `point-min'.\n
If optional arg MIN/MAX-GO is non-nil it may be any of the following:
 `:min`, `:max`, `t', `nil', <INTEGER>, <MARKER>,
Default is to `goto-char' `point-min'.\n
When optional arg NO-GO is non-nil return a buffer position
`point-min'/`min-max' but do not move point.\n
:EXAMPLE\n\n\(mon-g2be :min t\)\n
\(mon-g2be -1 t\)\n
\(mon-g2be 0 t\)\n
\(mon-g2be nil t\)\n
\(mon-g2be \(set-marker \(make-marker\) \(buffer-end 0\)\) t\)
\(mon-g2be 1 t\)\n
\(mon-g2be :max t\)\n
\(mon-g2be \(set-marker \(make-marker\) \(buffer-end 1\)\) t\)
\(mon-g2be nil t\)\n
\(mon-g2be t t\)\n
;; Following move point:
\(mon-g2be\)\n
\(mon-g2be :min)\n
\(mon-g2be -1\)\n
\(mon-g2be 0\)\n
\(mon-g2be t\)\n
\(mon-g2be \(set-marker \(make-marker\) \(buffer-end 0\)\)\)\n
\(mon-g2be 1\)\n
\(mon-g2be \(aref [10 13 15] \(random 3\)\) t\)\n
\(mon-g2be :max\)\n
\(mon-g2be \(set-marker \(make-marker\) \(buffer-end 1\)\)\)\n
:NOTE As a special case when MIN/MAX-GO is `markerp' and  points to a buffer
postion in `current-buffer', location of return value needn't be either only
`point-min'/`point-max'.
This allows reading in match-data variables bound to buffer local markers etc. 
When the constraints on passing a marker value to this function are
satisfied the return value is a marker not an integer including when the NO-GO
arg is ommitted (i.e. point moves). 
This return value is unlike `goto-char' in that: 
 \(goto-char \(1+ \(point\)\)\) 
returns the value of the point it moved into as an integer value.\n
:ALIASED-BY `mon-buffer-end'\n
:SEE-ALSO `mon-buffer-narrowed-p', `point-min', `point-max', `buffer-end',
`mon-buffer-narrowed-p', `mon-buffer-sub-no-prop', `mon-buffer-sub-no-prop-check',
`mon-help-buffer-functions'.\n►►►"
  ;;(let ((
  ;; (with-current-buffer (current-buffer)
  ;; 
  (setq min/max-go
        (or (and min/max-go
                 (or (and 
                      ;; its an integer or marker
                      (integer-or-marker-p min/max-go)
                      (or  (and ;; :NOTE Can't do `natnump' because MIN/MAX-GO can be -1
                            (integerp min/max-go) 
                            (or (and (> min/max-go 0)  (point-max))
                                (and (<= min/max-go 0) (point-min))))
                           (and (markerp min/max-go)
                                (and 
                                 ;; its a marker in current buffer.
                                 (or (eq (marker-buffer min/max-go) (current-buffer))
                                     (error (concat ":FUNCTION `mon-g2be' "
                                                    "-- ARG min/max-go satisfies `markerp' "
                                                    "but `marker-buffer' not `current-buffer', "
                                                    "got: %S in-buffer: %S")
                                            min/max-go (current-buffer)))
                                 ;; its a marker with a position return it, else signal
                                 (or (and (marker-position min/max-go) min/max-go)
                                     (error (concat ":FUNCTION `mon-g2be' "
                                                    "-- ARG min/max-go satisfies `markerp' "
                                                    "but `marker-position' points nowhere"
                                                    "got: %S in-buffer: %S") 
                                            min/max-go (current-buffer)))
                                 ))))
                     (and (or (eq min/max-go 'max) (eq min/max-go :max)) (point-max))
                     (and (or (eq min/max-go 'min) (eq min/max-go :min)) (point-min))
                     (and (booleanp min/max-go)
                          (or (and min/max-go (point-max))
                              (point-min)))))
            (point-min)))
  (or (and no-go min/max-go)
      (progn 
        (goto-char min/max-go)
        min/max-go)))

;;; ==============================
;;; :CHANGESET 2171
;;; :CREATED <Timestamp: #{2010-10-02T17:18:20-04:00Z}#{10396} - by MON KEY>y
(defun mon-buffer-sub-no-prop (&optional buf-beg buf-end)
  "Convenience function like `buffer-substring-no-properties'.\n
Return buffer contents from `point-min' to `point-max' without text-properties.\n
When optional args BUF-BEG and BUF-END non-nil they should satisfy the predicate
`integer-or-marker-p' and when narrowing is in effect as per satisfaction of
`mon-buffer-narrowed-p' integer or marker loccations should not extend outside
the region narrowed to.\n
:EXAMPLE\n\n\(mon-buffer-sub-no-prop\)\n
\(mon-buffer-sub-no-prop  8 12\)\n
\(mon-buffer-sub-no-prop  8 \(set-marker \(make-marker\) 12\)\)\n
\(mon-buffer-sub-no-prop \(set-marker \(make-marker\) 8\) 12\)\n
\(mon-buffer-sub-no-prop \(set-marker \(make-marker\) 8\) \(set-marker \(make-marker\) 12\)\)\n
Following will fail:\n
\(mon-buffer-sub-no-prop  nil nil\)\n
\(mon-buffer-sub-no-prop  8 nil\)\n
\(mon-buffer-sub-no-prop  8\)\n
\(mon-buffer-sub-no-prop  t \(set-marker \(make-marker\) 12\)\)\n
\(mon-buffer-sub-no-prop  8 t\)\n
\(mon-buffer-sub-no-prop  8 \"string\"\)\n
\(unwind-protect
    \(prog2 
        \(narrow-to-region \(line-beginning-position\) \(line-end-position\)\)
        \(mon-buffer-sub-no-prop \(set-marker \(make-marker\) \(point-min\)\) \(+ \(point-max\) 3\)\)\)
  \(widen\)\)\n
\(let \(\(pmin \(set-marker \(make-marker\) \(point-min\)\)\)\)
   \(unwind-protect
       \(prog2 
           \(narrow-to-region \(line-beginning-position\) \(line-end-position\)\)
           \(mon-buffer-sub-no-prop pmin \(point-max\)\)\)
     \(widen\)\)\)\n
:SEE-ALSO `mon-buffer-sub-no-prop-check', `mon-buffer-narrowed-p',
`mon-get-buffer-window-if', `mon-get-buffer-hidden',
`mon-buffer-exists-so-kill', `mon-buffer-exists-p', `mon-get-buffer-w-mode',
`mon-with-file-buffer', `mon-print-buffer-object-readably',
`mon-print-in-buffer-if-p', `mon-help-buffer-functions'.\n►►►"
  (if ;; (or buf-beg buf-end)
      (and buf-beg buf-end)
      (apply #'buffer-substring-no-properties 
             (mon-buffer-sub-no-prop-check buf-beg buf-end (current-buffer)))
    (buffer-substring-no-properties (mon-g2be -1 t) (mon-g2be 1 t))))

;;; ==============================
;;; :PREFIX "mbsnpc-"
;;; :CHANGESET 2171
;;; :CREATED <Timestamp: #{2010-10-02T17:17:28-04:00Z}#{10396} - by MON KEY>
(defun mon-buffer-sub-no-prop-check (bfr-beg bfr-end bfr-current)
  "Helper function for `mon-buffer-sub-no-prop'.\n
Checks that args BFR-BEG BFR-END are either integers or markers.\n
When markers converts to integer.\n
Returns a two element proper-list or signals an error.\n
Signals errors when args do not satisfy `integer-or-marker-p', are negative
integers, point to locations outside a narrowed range when narrowing is in
effect, etc.\n
:EXAMPLE\n\n\(mon-buffer-sub-no-prop-check 8 12\)\n
\(mon-buffer-sub-no-prop-check \(set-marker \(make-marker\) 8\) 12\)\n
\(mon-buffer-sub-no-prop-check  8 \(set-marker \(make-marker\) 12\)\)\n
\(mon-buffer-sub-no-prop-check \(set-marker \(make-marker\) 8\) \(set-marker \(make-marker\) 12\)\)\n
Following will fail:\n
\(mon-buffer-sub-no-prop-check 1 -12\)
\(mon-buffer-sub-no-prop-check -1 12\)
\(mon-buffer-sub-no-prop-check 0 12\)
\(mon-buffer-sub-no-prop-check nil nil\)\n
\(mon-buffer-sub-no-prop-check  8 nil\)\n
\(mon-buffer-sub-no-prop-check  8\)\n
\(mon-buffer-sub-no-prop-check  t \(set-marker \(make-marker\) 12\)\)\n
\(mon-buffer-sub-no-prop-check  8 t\)\n
\(mon-buffer-sub-no-prop-check  8 \"string\"\)\n
\(unwind-protect
    \(prog2 
        \(narrow-to-region \(line-beginning-position\) \(line-end-position\)\)
        \(mon-buffer-sub-no-prop \(set-marker \(make-marker\) \(point-min\)\) \(+ \(point-max\) 3\)\)\)
  \(widen\)\)\n
\(let \(\(pmin \(set-marker \(make-marker\) \(point-min\)\)\)\)
   \(unwind-protect
       \(prog2 
           \(narrow-to-region \(line-beginning-position\) \(line-end-position\)\)
           \(mon-buffer-sub-no-prop pmin \(point-max\)\)\)
     \(widen\)\)\)\n
:SEE-ALSO `mon-buffer-sub-no-prop', `mon-buffer-narrowed-p',
`mon-get-buffer-window-if', `mon-get-buffer-hidden',
`mon-buffer-exists-so-kill', `mon-buffer-exists-p', `mon-get-buffer-w-mode',
`mon-with-file-buffer', `mon-print-buffer-object-readably',
`mon-print-in-buffer-if-p', `mon-help-buffer-functions'.\n►►►"
  (let ((mbsnpc-err #'(lambda (err-str &rest args) 
                        (apply 'error
                               `(,(concat ":FUNCTION `mon-buffer-sub-no-prop-check' -- " err-str)
                                 ,@args))))
        (mbsnpc-bfr (get-buffer bfr-current))
        mbsnpc-nwdp
        ;; Use the narrowing data if needed 
        ;; mbsnpc-pmin mbsnpc-pmax 
        )
    (with-current-buffer mbsnpc-bfr
      (setq mbsnpc-nwdp (mon-buffer-narrowed-p mbsnpc-bfr))
      (if (and (or (and bfr-beg (integer-or-marker-p bfr-beg))
                   (funcall mbsnpc-err 
                            "optional arg BFR-BEG `null' or not `integer-or-marker-p', got: %S in-buffer: %S" 
                            bfr-beg mbsnpc-bfr))
               (or (and bfr-end (integer-or-marker-p bfr-end))
                   (funcall mbsnpc-err 
                            "optional arg BFR-END `null' or not `integer-or-marker-p' got: %S in-buffer: %S" 
                            bfr-end mbsnpc-bfr)))
          (let ((mbeg (or 
                       ;; BFR-BEG is marker
                       (and (markerp bfr-beg)
                            (or (eq (marker-buffer bfr-beg) mbsnpc-bfr)
                                (funcall mbsnpc-err 
                                         "arg BFR-BEG is `markerp' but not in `current-buffer', got-marker: %S in-buffer: %S"
                                         bfr-beg mbsnpc-bfr))
                            (or (marker-position bfr-beg) ;; (marker-position (set-marker (make-marker) nil))
                                (funcall mbsnpc-err 
                                         "arg BFR-BEG is `markerp' but pointing nowhere, got: %S in-buffer: %S" bfr-end mbsnpc-bfr))
                            (and (marker-position bfr-beg)
                                 (or (and mbsnpc-nwdp 
                                          (< (marker-position bfr-beg) (mon-g2be -1 t))
                                          (funcall mbsnpc-err 
                                                   (concat "arg BFR-BEG is a marker outside a `narrow-to-region', "
                                                           "got: %S `point-min' narrowing-details: %s in-buffer: %S"
                                                           bfr-beg mbsnpc-nwdp mbsnpc-bfr)))
                                     (marker-position bfr-beg))))
                       ;; BFR-BEG is integer
                       (and (or (integerp bfr-beg) 
                                (funcall mbsnpc-err 
                                         "arg BFR-BEG does not satisfy `integerp', got: %S in-buffer: %S" bfr-beg mbsnpc-bfr))
                            (or (and (<= bfr-beg 0)
                                     (funcall mbsnpc-err 
                                              (concat "arg BFR-BEG is `integerp' but `<' `point-min' or `zerop', "
                                                      "got: %d point-min-was: %d in-buffer: %S")
                                              bfr-beg (mon-g2be -1 t) mbsnpc-bfr))
                                (and mbsnpc-nwdp 
                                     (<  bfr-beg (mon-g2be -1 t))
                                     (funcall mbsnpc-err 
                                              (concat "arg BFR-BEG is `integerp' but `<' `point-min' outside a `narrow-to-region', "
                                                      "got: %d narrowing-details: %s in-buffer: %S")
                                              bfr-beg mbsnpc-nwdp mbsnpc-bfr))
                                t)
                            (or (and mbsnpc-nwdp 
                                     (>  bfr-beg (mon-g2be 1 t))
                                     (funcall mbsnpc-err 
                                              (concat "arg BFR-BEG is `>' `point-max' outside a `narrow-to-region', "
                                                      "got: %d narrowing-details: %s in-buffer: %S")
                                              bfr-beg mbsnpc-nwdp mbsnpc-bfr))
                                t)
                            bfr-beg)))
                (mend (or 
                       ;; BFR-END is marker
                       (and (markerp bfr-end) 
                            (or (eq (marker-buffer bfr-end) mbsnpc-bfr)
                                (funcall mbsnpc-err 
                                         (concat "arg BFR-END is `markerp' `%S' but not in `current-buffer', "
                                                 "got: %S in-buffer: %S")  bfr-end mbsnpc-bfr))
                            (or (marker-position bfr-end) ;; (marker-position (set-marker (make-marker) nil))
                                (funcall mbsnpc-err  (concat "arg BFR-END is `markerp' but pointing nowhere, "
                                                             "got: %S in-buffer: %S") bfr-end mbsnpc-bfr))
                            (or (and mbsnpc-nwdp 
                                     (< (marker-position bfr-end)  (mon-g2be -1 t))
                                     (funcall mbsnpc-err  
                                              (concat "arg BFR-END is `markerp' but outside a `narrow-to-region', "
                                                      "got: %S narrowing-details: %s in-buffer: %S")
                                              bfr-end mbsnpc-nwdp  mbsnpc-bfr))
                                (marker-position bfr-end)))
                       ;; BFR-END is integer
                       (and ;; Can replace some of below w/ `natnump'??
                        (or (integerp bfr-end) 
                            (funcall mbsnpc-err 
                                     "arg BFR-END does not satisfy `integerp', got: %S in-buffer: %S " 
                                     bfr-end mbsnpc-bfr))
                        (or (and (< bfr-end 0)
                                 (funcall mbsnpc-err 
                                          "arg BFR-END is `integerp' and `<' 0, got: %d in-buffer: %S" bfr-beg mbsnpc-bfr))
                            (and mbsnpc-nwdp 
                                 (<  bfr-end (mon-g2be -1 t))
                                 (funcall mbsnpc-err 
                                          (concat "arg BFR-END is `integerp' and `<' `point-min' outside a `narrow-to-region', "
                                                  "got: %d narrowing-details: %s in-buffer: %S ")
                                          bfr-end mbsnpc-nwdp mbsnpc-bfr ))
                            t)
                        (or (and mbsnpc-nwdp 
                                 (>  bfr-end (mon-g2be 1 t))
                                 (funcall mbsnpc-err 
                                          (concat "arg BFR-END is `integerp' and `>' `point-max' outside a `narrow-to-region', "
                                                  "arg BFR-END was: %d narrowing-details: %s in-buffer: %S") 
                                          bfr-end mbsnpc-nwdp mbsnpc-bfr))
                            t)
                        bfr-end))))
            (if (and mbeg mend)
                (list mbeg mend)
              (funcall mbsnpc-err "Should not see this error")))
        (funcall mbsnpc-err "an argument is wrong, for BFR-BEG got: %S for BFR-END got: %S" bfr-beg bfr-end)))))

;;; ==============================
;;; :CHANGESET 2171
;;; :CREATED <Timestamp: #{2010-10-02T13:45:41-04:00Z}#{10396} - by MON KEY>
(defun mon-buffer-narrowed-p (&optional buffer-or-name)
  "Test if narrowing is in effect in BUFFER-OR-NAME.\n
Return nil when no detectable‡ narrowing is effect.\n
When buffer is narrowed return a consed pair, car is `t', cdr is a 5 elt vector.\n
In which case return value has the format:\n
 (t . [ <POINT-MIN-WIDEN>    <POINT-MAX-WIDEN> 
        <POINT-MIN-NARROW>   <POINT-MAX-NARROW>
        <BUFFER-SIZE> ] )\n
When optional arg BUFFER-OR-NAME is non-nil check for narrowing in that buffer
instead. Default is `current-buffer'.\n
:EXAMPLE\n\n\(prog2 
    \(narrow-to-region \(line-beginning-position\) \(line-end-position\)\)
     \(unwind-protect \(mon-buffer-narrowed-p \"*Help*\"\) \(widen\)\)\)\n
:NOTE ‡There isn't a clean way to check if BUFFER-OR-NAME is narrowed when the
size of the narrowed-region is = `buffer-size', esp. when not
`buffer-modified-p', e.g:\n
 \(with-temp-buffer 
   \(narrow-to-region \(point-min\) \(point-max\)\)
   \(list :bfr-min \(point-min\) :bfr-pnt \(point\) :bfr-max \(point-max\)
         :bfr-gap \(gap-position\) :bfr-sz \(buffer-size \(current-buffer\)\)
         :bfr-endp \(eobp\) :bfr-modp \(buffer-modified-p\)
         :brf-nrrw-p \(mon-buffer-narrowed-p\)\)\)\n
:ALIASED-BY `buffer-narrowed-p'\n
:SEE-ALSO `mon-g2be', `mon-buffer-sub-no-prop', `mon-buffer-sub-no-prop-check',
`mon-buffer-exists-p', `mon-buffer-exists-so-kill', `mon-buffer-get-hidden',
`mon-buffer-get-w-mode', `mon-buffer-name->kill-ring',
`mon-buffer-name-print-readably', `mon-buffer-written-p',
`mon-buffer-append-to', `mon-buffer-do-with-undo-disabled',
`mon-buffer-end'.\n►►►"
  (let (chk-bffr or-chk-wdn)
    (with-current-buffer (if buffer-or-name
                             (or (setq chk-bffr (get-buffer buffer-or-name))
                                 (error  (concat ":FUNCTION `mon-buffer-narrowed-p' "
                                                 "-- optional arg BUFFER-OR-NAME "
                                                 " does not find buffer: `%S'")
                                         buffer-or-name))
                           (setq chk-bffr (current-buffer)))
      (setq or-chk-wdn `[,(mon-g2be -1 t) ,(mon-g2be 1 t) ,(buffer-size chk-bffr)])
      (and
       (and
        (or (>  (aref or-chk-wdn 0) 1)
            (/= (aref or-chk-wdn 1) (1+ (aref or-chk-wdn 2)))
            (/= (- (aref or-chk-wdn 1) (aref or-chk-wdn 0)) (aref or-chk-wdn 2))
            (setq or-chk-wdn))
        (save-restriction 
          (widen)
          (setq or-chk-wdn `(t . [,(mon-g2be -1 t) ,(mon-g2be 1 t) ,@(append or-chk-wdn nil)]))))
       or-chk-wdn))))


;;; ==============================
;;; :COURTESY anything.el :WAS `anything-empty-buffer-p'
;;; Added default to `current-buffer' and signal if BUFFER-OR-NAME is provided
;;; but doesn't exist.
;;; :CHANGESET 2299
;;; :CREATED <Timestamp: #{2010-11-11T21:35:35-05:00Z}#{10454} - by MON KEY>
(defun mon-buffer-empty-p (&optional buffer-or-name)
  "Return non-nil if buffer-size of current-buffer is zerop.\n
When optional arg BUFFER-OR-NAME is non-nil check that buffer instead if 
it satisfies `mon-buffer-exists-p' signal an error if not.\n
\(with-temp-buffer \(mon-buffer-empty-p\)\)\n
\(mon-buffer-empty-p\)\n
\(mon-buffer-empty-p \"bubbas-non-existent-buffer\"\)\n
:SEE-ALSO `mon-buffer-narrowed-p', `mon-buffer-exists-so-kill',
`mon-with-file-buffer', `mon-buffer-written-p', `mon-get-buffer-window-if'.\n►►►"
  (with-current-buffer
      (or (and buffer-or-name 
               (or (mon-buffer-exists-p buffer-or-name)
                   (error 
                    (concat ":FUNCTION `mon-buffer-empty-p' "
                            "-- arg BUFFER-OR-NAME not `mon-buffer-exists-p', got: %S")
                    buffer-or-name)))
          (current-buffer))
    (zerop (buffer-size))))

;;; ==============================
;;; :PREFIX "mbep-"
;;; :CREATED <Timestamp: #{2010-02-05T14:21:16-05:00Z}#{10055} - by MON KEY>
(defun mon-buffer-exists-so-kill (buffer-to-kill)
  "If BUFFER-TO-KILL exists kill it.\n
Return `#<killed buffer>' if buffered killed, else nil.\n
:EXAMPLE\n\n\(let \(\(not-much-longer \(get-buffer-create \"not-much-longer\"\)\)\)
  \(mon-buffer-exists-so-kill \(buffer-name not-much-longer\)\)\)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-empty-p' `mon-with-file-buffer',
`mon-buffer-written-p', `mon-buffer-narrowed-p', `mon-buffer-sub-no-prop',
`mon-buffer-sub-no-prop-check', `mon-buffer-name->kill-ring',
`mon-print-in-buffer-if-p', `mon-with-buffer-undo-disabled',
`mon-get-buffer-w-mode', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir',
`with-current-buffer', `with-temp-file', `with-temp-buffer',
`mon-help-buffer-functions'.\n►►►"
  (let ((mbep-sok (mon-buffer-exists-p buffer-to-kill)))
    (if (when mbep-sok (kill-buffer mbep-sok))        
        (get-buffer mbep-sok))))

;;; ==============================
;;; :PREFIX "mgbh-"
;;; :CHANGESET 2088
;;; :CREATED <Timestamp: #{2010-08-27T20:26:53-04:00Z}#{10345} - by MON KEY>
(defun mon-get-buffer-hidden (&optional intrp)
  "Return a list conses of the currently hidden buffers.\n
Elements of list have the form:\n
 \( <BUFFER-NAME> . <BUFFER> \)\n
Hidden buffers are those that completion can't find because the buffers name
begins with withespace. Generally these are considered \"internal\" buffers that
the user doesn't need to see and therefor not useful for completion. However, on
occasion it can be useful to know what you don't know. Some commonly hidden
buffers include:
 \" *autoload*\" \" *autoload-file*\" \" apropos-temp\" \" *Bookmarks*\"
 \" *completion-save-buffer*\" \" *code-conversion-work*\" \" *code-converting-work*\"
 \" *Compiler Input*\"  \" *Compiler Output*\" \" *Custom-Work*\"  
 \" *Deletions*\"  \" *dired-check-process output*\" \" *dot-dired*\" \" *DOC*\"  
 \" *Echo Area <N>*\" \" *Format Temp <N>*\" 
 \" *gnus work*\"
 \" *ido session*\" \" *IDO Trace*\" 
 \" *info-browse-tmp*\"  \" *info tag table*\" \" *Input History*\"
 \" *jka-compr-error*\" \" *jka-compr-error*\"
 \" *jka-compr-flc-temp*\" \" *jka-compr-wr-temp*\"
 \" *Marked Files*\"  \" *message-viewer <BUFFER/FILE>*\" \" *Minibuf-<N>*\"
 \" pp-to-string\"
 \" *recover*\" \" *spool temp*\" \" *string-output*\" \" *temp*\" 
 \" temp-info-look\" \" *Temp Input History*\" \" *text-props*\"
 \" *tmp-reporter-buffer*\" \" *tramp temp*\"
 \" widget-choose\" \" *Unicode Data*\" \" *url-work\" 
:EXAMPLE\n\n\(mon-get-buffer-hidden\)\n
\(mon-get-buffer-hidden\)\n
:ALIASED-BY `mon-get-hidden-buffers'\n
:ALIASED-BY `mon-help-hiddent-buffers'\n
:SEE-ALSO `ido-ignore-buffers', `mon-help-buffer-spc-*DOC*',
`mon-abort-autosave-when-fucked', `mon-abort-recursive-edit'.\n►►►"
  (interactive "P")
  (let ((mgbh-bl (buffer-list))
        mgbh-rslt)
    (dolist (mgbh-dobl mgbh-bl (unless (null mgbh-rslt)
                       (setq mgbh-rslt (nreverse mgbh-rslt))))
      (when (eq ?\s (aref (buffer-name mgbh-dobl) 0))
        (push `(,(buffer-name mgbh-dobl) . ,mgbh-dobl) mgbh-rslt)
        mgbh-rslt))))
;;
;;; :TEST-ME (mon-get-buffer-hidden)
;;; :TEST-ME (mon-get-buffer-hidden t)

;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-29T19:16:42-04:00Z}#{10393} - by MON KEY>
(defun mon-get-buffer-window-if (buffer-or-name &optional frame)
  "Return window if BUFFER-OR-NAME exists and there is a window displaying it.\n
Optional arg FRAME is as per `get-buffer-window', it is one of:\n
 { visible | 0 | t | nil | <FRAME> }\n
When ommitted or nil the default is to search only the `selected-frame'.\n
:EXAMPLE\n\n\(mon-get-buffer-window-if \"*Help*\"\)\n
:ALIASED-BY `mon-window-get-if-buffer'
:ALIASED-BY `get-buffer-window-if'\n
:SEE-ALSO `mon-buffer-exists-p', `mon-print-buffer-object-readably',
`mon-get-buffer-w-mode', `mon-buffer-written-p', `mon-with-file-buffer',
`mon-window-map-active-to-plist'.\n►►►"
 (let ((myb-wdw (mon-buffer-exists-p buffer-or-name)))
   (when myb-wdw (get-buffer-window myb-wdw))))

;;; ==============================
;;; :PREFIX "mpbor-"
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-24T11:49:29-04:00Z}#{10385} - by MON KEY>
(defun mon-print-buffer-object-readably (buffer-or-name &optional as-form)
  "Return a form containing a printed representation of BUFFER-OR-NAME.\n
Output in a format output that `read' can handle as if by `prin1-to-string'.\n
Default is to return a lisp form for `eval'.\n
:EXAMPLE\n\n\(mon-print-buffer-object-readably \(current-buffer\)\)
\(mon-print-buffer-object-readably \(buffer-name\)\)
\(mon-print-buffer-object-readably \(current-buffer\) t\)
\(mon-print-buffer-object-readably \(buffer-name\) t\)
\(mon-print-buffer-object-readably 
 \(car \(mon-string-wonkify \"prob-not-a-buffer\" 1\)\) t\)
\(mon-print-buffer-object-readably 
 \(car \(mon-string-wonkify \"prob-not-a-buffer\" 1\)\)\)
:ALIASED-BY `mon-buffer-name-print-readably'
:SEE-ALSO `mon-buffer-exists-p', `mon-get-buffer-window-if'
`mon-buffer-narrowed-p', `mon-buffer-sub-no-prop',
`mon-buffer-sub-no-prop-check'.\n►►►"
  (let ((mpbor-get-bfr (mon-buffer-exists-p buffer-or-name)))
    (cond ((stringp mpbor-get-bfr)
           (or (and as-form
                    (prin1-to-string
                     `(get-buffer-create ,mpbor-get-bfr)))
               `(get-buffer-create ,mpbor-get-bfr)))
          ((bufferp mpbor-get-bfr)
           (setq mpbor-get-bfr
                 `(get-buffer-create ,(prin1-to-string mpbor-get-bfr t)))
           (or (and as-form (prin1-to-string mpbor-get-bfr))
               mpbor-get-bfr))
          (t (or (and as-form "(get-buffer \"\")")
                 `(get-buffer ""))))))

;;; ==============================
;;; :COURTESY :FILE slime.el :WAS `slime-recently-visited-buffer'
;;; :CHANGESET 1967
;;; :CREATED <Timestamp: #{2010-07-12T12:42:51-04:00Z}#{10281} - by MON KEY>
(defun mon-get-buffer-w-mode (w-mode &optional not-visible-only)
  "Return the most recently visited buffer whose major-mode is W-MODE.\n
When optional arg NOT-VISIBLE-ONLY is non-nil only considers buffers that are
not already visible. Default is to consider all buffers on all frames.\n
:EXAMPLE\n\n\(mon-get-buffer-w-mode 'help-mode 'not-visible-only\)\n
\(mon-get-buffer-w-mode 'fundamental-mode\)\n
:ALIASED-BY `mon-buffer-get-w-mode'\n
:SEE-ALSO `mon-buffer-exists-p', `mon-with-file-buffer', `mon-buffer-written-p',
`mon-buffer-narrowed-p', `mon-buffer-sub-no-prop',
`mon-buffer-sub-no-prop-check', `mon-buffer-name->kill-ring',
`mon-print-in-buffer-if-p', `mon-with-buffer-undo-disabled',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer', `mon-help-buffer-functions'.\n►►►"
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode w-mode))
                  (not (string-match "^ " (buffer-name buffer)))
                  (null (if not-visible-only
                            (get-buffer-window buffer 'visible)
                          (get-buffer-window buffer t))))
        return buffer
        finally (error (concat ":FUNCTION `mon-get-buffer-w-mode' "
                               "-- can not locate buffer W-MODE: %S") w-mode)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-11-22T19:48:23-05:00Z}#{10471} - by MON KEY>
(defun mon-buffer-check-local-value (w-predicate check-value check-for 
                                                 &optional w-buffer)
  "Whether a buffer-local-value is non-nil in a buffer.\n
Return non-nil when the buffer-local-value of CHECK-VALUE satisfies the
W-PREDICATE testing CHECK-FOR in current-buffer or W-BUFFER.\n
W-PREDICATE is a an applicable symbol argument to `mon-equality-or-predicate'.\n
CHECK-VALUE is a symbol to check.\n
CHECK-FOR is a value to test for.\n
When optional arg W-BUFFER is non-nil it is a buffer object or string naming one.
Signal an error if W-BUFFER does not satisfy `mon-buffer-exists-p'.
Default is value of current-buffer.\n
:EXAMPLE\n\n\(mon-buffer-check-local-value 'eq 'major-mode 'help-mode\)\n
\(mon-buffer-check-local-value 'eq 'major-mode 'help-mode \(current-buffer\)\)\n
\(mon-buffer-check-local-value 'eq 'buffer-read-only t \(current-buffer\)\)\n
\(mon-buffer-check-local-value 'eq 'buffer-read-only t \"*Help*\"\)\n
;; Following successfully signals an error:
\(mon-buffer-check-local-value 'eq 'major-mode 'help-mode \"Probably-not-a-real-buffer\"\)
:SEE-ALSO `mon-buffer-exists-p', `*mon-equality-or-predicate-function-types*'.\n►►►"
  (mon-equality-or-predicate 
   w-predicate check-for
   (buffer-local-value 
    check-value
    (or (let ((mbllmp-bfrp (mon-buffer-exists-p (or w-buffer (current-buffer)))))
          (and mbllmp-bfrp (get-buffer mbllmp-bfrp)))
        (error (concat ":FUNCTION `mon-buffer-check-local-value' "
                       "-- arg W-BUFFER does not satisfy `mon-buffer-exists-p', got %S")
               w-buffer)))))


;;; ==============================
;;; :CREATED <Timestamp: #{2010-11-22T19:21:08-05:00Z}#{10471} - by MON KEY>
(defun mon-buffer-check-major-mode (check-mode &optional in-buffer)
  "Check if CHECK-MODE is the current major-mode IN-BUFFER.\n
Return non-nil when `buffer-local-value' IN-BUFFER is eq CHECK-MODE.\n
CHECK-MODE is a symbol naming a major-mode.\n
When optional arg IN-BUFFER is non-nil it is a buffer object or string naming one.
Signal an error if IN-BUFFER does not satisfy `mon-buffer-exists-p'.
Default is value of current-buffer.\n
:EXAMPLE\n\n\(mon-buffer-check-major-mode 'help-mode\)\n
\(mon-buffer-check-major-mode 'help-mode \(current-buffer\)\)\n
\(mon-buffer-check-major-mode 'help-mode \"*Help*\"\)\n
;; Following successfully signals an error:
\(mon-buffer-check-major-mode 'help-mode \"Probably-not-a-real-buffer\"\)\n
:SEE-ALSO `mon-get-buffer-w-mode'.\n►►►"
  (mon-buffer-check-local-value 'eq 'major-mode check-mode in-buffer))

;;; ==============================
;;; :NOTE Weird stuff happens with longlines-mode checks:
;;;  (memq 'longlines-mode (assoc 'longlines-mode (buffer-local-variables (get-buffer <THE-BUFFER>))))
;;; :CREATED <Timestamp: #{2010-03-12T13:26:58-05:00Z}#{10105} - by MON KEY>
(defun mon-buffer-longlines-mode-p (&optional llm-in-buffer)
  "Return non-nil if buffer is in `longlines-mode'.
When optional arg LLM-IN-BUFFER is non-nil check value in that buffer.\n
Signal an error if that buffer does not exist. Default is current-buffer.\n
:EXAMPLE\n\n\(mon-buffer-longlines-mode-p\)\n
\(let \(chk\) 
  \(with-temp-buffer 
    \(longlines-mode\) \(push \(mon-buffer-longlines-mode-p\) chk\) 
    \(longlines-mode\) \(push \(mon-buffer-longlines-mode-p\) chk\)
    \(nreverse chk\)\)\)\n
\(let* \(ltb-chk
       \(ltb \(get-buffer-create \"*LLM-TEST*\"\)\)
       \(do-ltb #'\(lambda \(\) \(with-current-buffer ltb \(longlines-mode\)\)
                         \(push \(mon-buffer-longlines-mode-p \(get-buffer ltb\)\) ltb-chk\)\)\)\)
  \(dotimes \(l 2 \(progn \(with-current-buffer ltb \(kill-buffer\)\)
                       \(nreverse ltb-chk\)\)\)
    \(funcall do-ltb\)\)\)\n
:SEE-ALSO `mon-toggle-restore-llm', `mon-naf-mode-toggle-restore-llm',
`mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'.\n►►►"
  (mon-buffer-check-local-value 'eq 'longlines-mode t llm-in-buffer))

;;
;; ,---- :UNCOMMENT-TO-TEST
;; | (let (chk) 
;; |   (with-temp-buffer 
;; |     (longlines-mode) (push (mon-buffer-longlines-mode-p) chk) 
;; |     (longlines-mode) (push (mon-buffer-longlines-mode-p) chk)
;; |     (nreverse chk)))
;; `----
;;
;; ,---- :UNCOMMENT-TO-TEST
;; | (let* ((ltb (get-buffer-create "*llm-test-buffer*"))
;; |        ltb-chk
;; |        (do-ltb #'(lambda () (with-current-buffer ltb (longlines-mode))
;; |                          (push (mon-buffer-longlines-mode-p (get-buffer ltb)) ltb-chk))))
;; |   (dotimes (l 2 (progn (with-current-buffer ltb (kill-buffer))
;; |                        (nreverse ltb-chk)))
;; |     (funcall do-ltb)))
;; `----
;; 
;;; :TEST-ME (mon-buffer-longlines-mode-p)


;;; ==============================
;;; :PREFIX "mbnkr-"
;;; :CREATED <Timestamp: #{2009-10-22T16:45:38-04:00Z}#{09434} - by MON>
(defun mon-buffer-name->kill-ring (&optional or-buffer insrtp)
  "Put buffer-name of current-buffer on kill-ring.\n
When OR-BUFFER is non-nil put that buffer's name on kill ring instead.
When INSRTP is non-nil or called-interactively with prefix arg insert 
buffer-name at point. Does not move point.\n
:EXAMPLE\\n(mon-buffer-name->kill-ring)\n
\(call-interactively 'mon-buffer-name->kill-ring)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-exists-so-kill',
`mon-buffer-written-p', `mon-print-in-buffer-if-p', `mon-with-file-buffer',
`mon-with-buffer-undo-disabled', `mon-get-buffer-w-mode',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `mon-help-buffer-functions'.\n►►►"
  (interactive "i\nP")
  (let ((mbnkr-kn (kill-new (format "%S" (buffer-name or-buffer)))))
    (if insrtp 
        (save-excursion (newline) (princ mbnkr-kn (current-buffer)))
      (princ mbnkr-kn))))

;;; ==============================
;;; <Timestamp: #{2010-07-28T19:49:04-04:00Z}#{10303} - by MON KEY>
;;; :TODO Abstract following to a macro for use with all for all loadtime
;;; functions that bind big lists.
;;
;; (let ((old-msgs (get-buffer-create "*OLD-MESSAGES*"))
;;       (msgs (get-buffer-create "*Messages*")))
;;   (unwind-protect 
;;       (progn
;;         (with-current-buffer (get-buffer msgs)
;;           (buffer-swap-text old-msgs))
;;         (setq <SOME-BIG-VAR-VALUE> <SOME-BIG-VALUE-IN-MESSAGES>)
;;         (with-current-buffer (get-buffer old-msgs)
;;           (buffer-swap-text msgs)))
;;     (with-current-buffer (get-buffer old-msgs)
;;       (kill-buffer (current-buffer)))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 18, 2009 @ 11:26.02 AM - by MON KEY>
(defun mon-append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.\n
Insert it into BUFFER before point.\n
BUFFER names an existing buffer.\n
START and END specify the portion of the current buffer to be copied.\n
This is an alternative definition of `append-to-buffer' with a \"\n\".\n
:ALIASED-BY `mon-buffer-append-to'\n
:SEE-ALSO `mon-append-to-register', `mon-kill-appending', `mon-buffer-exists-p',
`mon-g2be', `mon-buffer-narrowed-p', `mon-buffer-sub-no-prop',
`mon-buffer-sub-no-prop-check', `mon-help-buffer-functions'.\n►►►"
  (interactive `(,(read-buffer 
                   (concat ":FUNCTION `mon-append-to-buffer'"
                           "-- append to buffer: ")
                   (other-buffer (current-buffer) t))
                 ,@(if (use-region-p)
                       `(,(region-beginning) ,(region-end))
                     `(,(point) ,(point)))))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
	     (windows (get-buffer-window-list append-to t t))
	     point)
	(set-buffer append-to)
	(setq point (point))
	(barf-if-buffer-read-only)
	(newline)  ;; :ADDED `newline', else identical to `append-to-buffer'.
	(insert-buffer-substring oldbuf start end)
	(dolist (window windows)
	  (when (= (window-point window) point)
	    (set-window-point window (point))))))))


;;; ==============================
;;; "mmsb-"
;;; :CREATED <Timestamp: #{2009-12-01T13:54:34-05:00Z}#{09492} - by MON KEY>
(defun mon-make-shell-buffer ()
  "Return a new *shell* buffer.\n
If *shell* exists increment by 1 and return *shell-N*.\n
:EXAMPLE\n\(let \(\(kl-bf \(mon-make-shell-buffer\)\)\)
  \(progn \(momentary-string-display 
          \(format \" The buffer %s is about to die\" kl-bf\) \(point\)\)
         \(kill-buffer kl-bf\)\)\)\n
:NOTE could also be accomplished similarly with:\n
\(get-buffer-create \(generate-new-buffer-name \"*shell*\"\)\)\n
But, this way MON has fine-grain control over the assigned name suffix.\n
:CALLED-BY `mon-shell'\n
:ALIASED-BY  `mon-buffer-make-shell'
:SEE-ALSO `generate-new-buffer', `generate-new-buffer-name',
`mon-make-shell-buffer', `mon-terminal', `mon-help-process-functions',
`shell'.\n►►►"
  (let (mmsb-bfrs mmsb-bfrs-str)
    (setq mmsb-bfrs (with-temp-buffer
                  (princ (buffer-list) (current-buffer))
                  (mon-buffer-sub-no-prop)))
    (setq mmsb-bfrs (read mmsb-bfrs))
    (setq mmsb-bfrs (mapcar #'(lambda (mmsb-L-1) (format "%s" mmsb-L-1)) mmsb-bfrs))
    (mapc #'(lambda (mmsb-L-2)
              (when (string-match-p "\\*shell" mmsb-L-2)
                (push mmsb-L-2 mmsb-bfrs-str)))
          mmsb-bfrs)
    (setq mmsb-bfrs (car mmsb-bfrs-str))
    (cond ((null mmsb-bfrs) (get-buffer-create "*shell*"))
          ((= (length mmsb-bfrs) 7) (get-buffer-create "*shell-1*"))
          ((> (length mmsb-bfrs) 7) 
           (get-buffer-create
            (format "*shell-%d*" 
                    (1+ (string-to-number (substring mmsb-bfrs 7 8)))))))))
;;
;;; :TEST-ME (mon-make-shell-buffer)
;;; :TEST-ME (let ((kl-bf (mon-make-shell-buffer)))
;;;              (prog1 (princ kl-bf) (kill-buffer kl-bf)))

;;; ==============================
;;; :NOTE I tried to figure out how to do this with `defadvice'... that was bad!
;;; :CREATED <Timestamp: #{2009-12-01T15:18:38-05:00Z}#{09492} - by MON KEY>
(defun mon-shell ()
  "Return *shell* buffer.\n
If *shell* exists increment by 1 and return *shell-N*.\n
:ALIASED-BY `mon-buffer-get-shell'\n
:SEE-ALSO `mon-make-shell-buffer', `mon-terminal', `mon-help-process-functions',
`shell'.\n►►►"
  (interactive)
  (shell (mon-make-shell-buffer)))
;;
;;; :TEST-ME (progn (mon-shell) (mon-shell))

;;; ==============================
(provide 'mon-buffer-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-buffer-utils.el ends here
;;; EOF
