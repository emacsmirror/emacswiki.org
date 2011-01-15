;;; mon-string-utils.el --- string frobbing procedures for mon-*utils features
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-string-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-23T17:55:13-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-string-utils provides string frobbing procedures for mon-*utils features
;;
;; FUNCTIONS:►►►
;; `mon-string-justify-left', `mon-string-fill-to-col', 
;; `mon-string-chop-spaces', `mon-string-split-on-regexp',
;; `mon-string-split',
;; `mon-string-sub-old->new', `mon-string-replace-char', 
;; `mon-string-spread', 
;; `mon-string-sort-descending', `mon-string-has-suffix',
;; `mon-string-upto-index', `mon-string-after-index', `mon-string-index',
;; `mon-string-position',
;; `mon-string-ify-list'
;; `mon-string-to-hex-list', `mon-string-to-hex-string',
;; `mon-string-to-hex-list-cln-chars',
;; `mon-string-from-hex-list',
;; `mon-string-infix',
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
;; `*mon-string-utils-xrefs*'
;;
;; GROUPS:
;; `mon-string-utils'
;;
;; ALIASED/ADVISED/SUBST'D:
;; :NOTE Aliases defined in :FILE mon-aliases.el
;;
;; <PREFIX>-<QUALIFIED>                 <CORE-SYMBOL>
;; `mon-string-combine-and-quote'    -> `combine-and-quote-strings'
;; `mon-string-split-and-unquote'    -> `split-string-and-unquote'
;; `mon-string-set-char-at-idx'      -> `store-substring'     
;; `mon-string-insert-string-at-idx' -> `store-substring'
;;
;; <PREFIX>-<QUALIFIED>                 <PREFIX>-<CORE-SYMBOL>
;; `mon-string-prefix-p'             -> `vc-string-prefix-p'
;; `mon-string-at-point'             -> `comint-extract-string' 
;;
;;  <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-remove-char-in-string'       -> `mon-string-replace-char'
;; `mon-replace-char-in-string'      -> `mon-string-replace-char'
;; `mon-split-string'                -> `mon-string-split'
;;
;; DEPRECATED:
;;
;; RENAMED:
; `mon-convert-list-regexp'          -> `mon-string-to-regexp'
;;
;; MOVED:
;; `mon-string-repeat'                               <- mon-utils.el
;; `mon-string-sub-old->new'                         <- mon-utils.el
;; `mon-string-replace-char'                         <- mon-utils.el
;; `mon-string-split-on-regexp'                      <- mon-utils.el
;; `mon-string-ify-list'                             <- mon-utils.el
;; `mon-string-chop-spaces'                          <- mon-utils.el
;; `mon-string-has-suffix'                           <- mon-utils.el
;; `mon-string-position'                             <- mon-utils.el
;; `mon-string-sort-descending'                      <- mon-utils.el
;; `mon-string-after-index'                          <- mon-utils.el
;; `mon-string-upto-index'                           <- mon-utils.el
;; `mon-string-index'                                <- mon-utils.el
;; `mon-string-fill-to-col'                          <- mon-utils.el
;; `mon-string-justify-left'                         <- mon-utils.el
;; `mon-string-spread'                               <- mon-utils.el
;; `mon-string-split'                                <- mon-utils.el
;; `mon-string-to-hex-list'                          <- mon-utils.el
;; `mon-string-from-hex-list'                        <- mon-utils.el
;; `mon-string-to-hex-string'                        <- mon-utils.el
;; `mon-string-to-hex-list-cln-chars'                <- mon-utils.el
;; `mon-string-infix'                                <- mon-name-utils.el
;; `mon-string-permute'                              <- mon-name-utils.el
;; `mon-string-splice-sep'                           <- mon-name-utils.el
;; `mon-string->strings-splice-sep'                  <- mon-name-utils.el
;; `mon-string-to-regexp'                            <- mon-name-utils.el
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
;; URL: http://www.emacswiki.org/emacs/mon-string-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-25T02:27:00-05:00Z}#{10476} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-string-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-23T17:55:13-05:00Z}#{10472} - by MON KEY>
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
;; Copyright © 2010-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))


;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T19:05:30-05:00Z}#{11022} - by MON KEY>
(defgroup mon-string-utils  nil
  "Customization group for variables and functions of :FILE mon-string-utils.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE" "http://www.emacswiki.org/emacs/mon-string-utils.el")
  :link '(emacs-library-link "mon-string-utils.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T19:05:32-05:00Z}#{11022} - by MON KEY>
(defcustom *mon-string-utils-xrefs* 
  '(mon-string-split mon-string-spread mon-string-justify-left
    mon-string-fill-to-col mon-string-index mon-string-upto-index
    mon-string-after-index mon-string-sort-descending mon-string-position
    mon-string-has-suffix mon-string-chop-spaces mon-string-ify-list
    mon-string-split-on-regexp mon-string-replace-char mon-string-sub-old->new
    mon-string-repeat mon-string-to-hex-list-cln-chars mon-string-to-hex-string
    mon-string-from-hex-list mon-string-to-hex-list mon-string-infix
    mon-string-explode mon-string-permute mon-string-permute-line
    mon-string-splice-sep mon-string->strings-splice-sep mon-string-to-regexp
    *mon-string-utils-xrefs*)
  "Xrefing list of mon string related symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-string-utils.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-buffer-utils-xrefs*',
`*mon-line-utils-xrefs*', `*mon-plist-utils-xrefs*'
`*mon-seq-utils-xrefs*', `*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-string-utils 
  :group 'mon-xrefs)

 
;;; ==============================
;;; :NOTE Heavily modified version of `dired-split' :SEE :FILE dired-aux.el
;;; Which had the comment, "here should be a builtin split function - inverse to mapconcat."
;;; :CHANGESET 2178
;;; :CREATED <Timestamp: #{2010-10-06T20:15:46-04:00Z}#{10403} - by MON KEY>
(defun mon-string-split (split-pattern w-string-to-split &optional limit-to w-empty-string)
  "With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into a list of substrings.\n
Optional third arg LIMIT (>= 1) is a limit to the length of the resulting list.
:EXAMPLE\n\n\(mon-string-split \"sp\"
 \"With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into a list of substrings.\"\)\n
\(mon-string-split \" \"
 \"With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into a list of substrings.\"3\)\n
\(mon-string-split \"Wi\"
 \"With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into a list of substrings.\" 1\)\n
\(mon-string-split \"s.\"
 \"With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into a list of substrings.\" 66\)\n
\(mon-string-split \".\" 
 \"With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into list of substrings.\"\)\n
\(mon-string-split \".\" 
 \"With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into list of substrings.\" nil t\)\n
\(mon-string-split-TEST\)\n
:NOTE When SPLIT-PATTERN matches _only_ itself and optional arg limit is omitted.
return value of following form will always be equal W-STRING-TO-SPLIT:\n
  \(mapconcat 'identity \(mon-string-split SEP W-STRING-TO-SPLIT\) SEP\)\n
:ALIASED-BY `mon-split-string'\n
:SEE-ALSO `mon-string-split-TEST', `mon-region-split-commas',
`mon-string-to-symbol', `mon-string-ify-list', `mon-string-chop-spaces',
`mon-string-replace-char', `mon-string-from-sequence', `mon-string-to-sequence',
`mon-string-justify-left', `mon-string-index', `mon-string-position'.\n►►►"
  (let ((mch-max (length w-string-to-split)))
    (if (or (and limit-to (= limit-to 0))
            (not (string-match-p split-pattern w-string-to-split))
            (save-match-data (string-match split-pattern w-string-to-split) 
                             (= (match-end 0) mch-max)))
        (list w-string-to-split)
      (save-match-data
        (let* ((mch-beg  (string-match split-pattern w-string-to-split))
               (mch-rslt (if (= mch-beg 0) ;; Its an empty string just get the match
                             (list (match-string 0 w-string-to-split))
                           (list (match-string 0 w-string-to-split)
                                 (substring w-string-to-split 0 mch-beg))))
               (mch-end  (and mch-beg (match-end 0)))
               (limit-to (or (and limit-to (1- limit-to)))))
          (when mch-end
            (while (and (or (null limit-to) (and (>= (decf limit-to) 0)))
                        mch-end 
                        (string-match split-pattern w-string-to-split mch-end))
              (setq mch-beg (match-beginning 0))
              (if w-empty-string
                  (push (substring w-string-to-split mch-end mch-beg) mch-rslt)
                (unless (= mch-end mch-beg) 
                  (push (substring w-string-to-split mch-end mch-beg) mch-rslt)))
              (push (match-string 0 w-string-to-split) mch-rslt)
              (setq mch-end (match-end 0))))
          ;; At the end push any remaning unless its an empty string.
          (when (and mch-end (not (= mch-end mch-max))) 
            (push (substring w-string-to-split mch-end) mch-rslt))
          (nreverse mch-rslt))))))

;;; ==============================
;;; :COURTESY calendar/calendar.el :WAS `calendar-string-spread'
;;; The algorithm is based on equation (3.25) on page 85 of Concrete
;;; Mathematics by Ronald L. Graham, Donald E. Knuth, and Oren Patashnik,
;;; Addison-Wesley, Reading, MA, 1989.
;;; :CHANGESET 1995
;;; :CREATED <Timestamp: #{2010-07-26T18:06:51-04:00Z}#{10301} - by MON KEY>
(defun mon-string-spread (string-bag spread-char spread-length) ;; 
  "Concatenate list of STRING-BAG separated by SPREAD-CHAR to SPREAD-LENGTH.\n
The effect is as if `mapconcat', but separating pieces are balanced if
possible.\n 
Each item of STRING-BAG is evaluated with `eval' before concatenation.
So, in effect elts of STRING-BAG may also be expressions that evaluates to a string.\n
If SPREAD-LENGTH is too short STRING-BAG is concatenated and result truncated.\n
:EXAMPLE\n\n\(let \(\(base-str \"string\"\)
      \(base-str-inc 0\)
      \(biggest-rand 0\)
      \(base-str-rndmz 
       #'\(lambda \(eg-str\)  
           \(let \(\(rand-pad \(random 16\)\)
                 \(rand-pad-char \(elt \(string-to-list \"#-._+\"\) \(random 4\)\)\)
                 rand-len\)
             \(setq eg-str
                   \(concat 
                    \(make-string  rand-pad rand-pad-char\)
                    \" \" eg-str \(number-to-string base-str-inc\) \" \"
                    \(make-string  rand-pad rand-pad-char\)\)\)
             \(setq rand-len \(length eg-str\)\)
             \(when \(> rand-len biggest-rand\)
               \(setq biggest-rand rand-len\)\)
             eg-str\)\)\)\)
  \(mon-string-spread
   `\(,\(funcall base-str-rndmz base-str\)
     \(identity ,\(funcall base-str-rndmz base-str\)\)
     \(apply 'string \(append ,\(funcall base-str-rndmz base-str\) nil\)\)\)
   \(elt \(number-sequence 33 47 1\) \(random 16\)\)
   \(* \(+ biggest-rand 2\) 3\)\)\)\n
:SEE-ALSO `mon-string-fill-to-col', `truncate-string-to-width',
`mon-line-strings-indent-to-col', `mon-line-indent-from-to-col',
`mon-string-split', `mon-string-set-char-at-idx' `mon-string-insert-string-at-idx',
`mon-string-index', `mon-string-upto-index', `mon-string-after-index',
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces',
`mon-string-ify-list', `mon-string-replace-char', `mon-string-sub-old->new',
`mon-string-repeat'.\n►►►"
  (let* ((mss-str-bag 
          ;; :NOTE This `eval' seems ugly, is there a better way?
          (mapcar #'eval
                  (if (< (length string-bag) 2)
                      (append (list "") string-bag (list ""))
                    string-bag)))
         (mss-n (- spread-length (length (apply #'concat mss-str-bag))))
         (mss-m (1- (length mss-str-bag)))
         (mss-s (car mss-str-bag))
         (mss-strings (cdr mss-str-bag))
         (mss-i 0))
    (dolist (mss-D-1 mss-strings)
      (setq mss-s (concat mss-s
                          (make-string (max 0 (/ (+ mss-n mss-i) mss-m)) spread-char)
                          mss-D-1)
            mss-i (1+ mss-i)))
    (substring mss-s 0 spread-length)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((base-str "string")
;; |       (base-str-inc 0)
;; |       (biggest-rand 0)
;; |       (base-str-rndmz #'(lambda (eg-str)  
;; |                           (let ((rand-pad (random 16))
;; |                                 (rand-pad-char (elt (string-to-list "#-._+") (random 4)))
;; |                                 rand-len)
;; |                             (setq eg-str
;; |                                   (concat 
;; |                                    (make-string  rand-pad rand-pad-char)
;; |                                    " " eg-str (number-to-string base-str-inc) " "
;; |                                    (make-string  rand-pad rand-pad-char)))
;; |                             (setq rand-len (length eg-str))
;; |                             (when (> rand-len biggest-rand)
;; |                               (setq biggest-rand rand-len))
;; |                             eg-str
;; |                             ))))
;; |   (mon-string-spread
;; |    `(,(funcall base-str-rndmz base-str)
;; |      (identity ,(funcall base-str-rndmz base-str))
;; |      (apply 'string (append ,(funcall base-str-rndmz base-str) nil)))
;; |    (elt (number-sequence 33 47 1) (random 16))
;; |    (* (+ biggest-rand 2) 3)))
;; `----

;;; ==============================


;;; ==============================
;;; :NOTE An alternative approach to justification might be something like:
;;;       (progn
;;;         (insert (propertize "\t" 'display '(space :align-to 32)) " ")  
;;;        (untabify))
;;;       Or mabye, 
;;;       (insert (propertize "\40" 'display '(space :align-to 32)) " ")
;;;
;;; :PREFIX "msjl-"
;;; :COURTESY Pascal Bourguignon :HIS pjb-strings.el :WAS `string-justify-left'
;;; :CHANGESET 1738 <Timestamp: #{2010-05-17T08:57:22-04:00Z}#{10201} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T18:08:59-05:00Z}#{10053} - by MON KEY>
;;; :ADDED `save-match-data' for `split-string'
;;; :RENAMED LEFT-MARGIN arg -> lft-margin. `left-margin' is a global var.
;;; :MODIFICATIONS <Timestamp: #{2010-02-20T14:55:40-05:00Z}#{10076} - by MON KEY>
;;; Added optional arg NO-RMV-TRAIL-WSPC. Relocated save-match-data and
;;; conditional type error checks. Rewrote docstring
(defun mon-string-justify-left (justify-string &optional justify-width 
                                               lft-margin no-rmv-trail-wspc)
  "Return a left-justified string built from JUSTIFY-STRING.\n
When optional arg JUSTIFY-WIDTH is non-nil it is a width JUSTIFY-STRING to
counting from column 0.  Default JUSTIFY-WIDTH is `current-column' or 72.\n
When optional arg LFT-MARGIN it is a column to JUSTIFY-STRING beginning from.
Default is `left-margin' or 0.\n
The word separators are those of `split-string':
      [ \\f\\t\\n\\r\\v]+
This means that JUSTIFY-STRING is justified as one paragraph.\n
When NO-RMV-TRAIL-WSPC is non-nil do not remove trailing whitespace.
Default is to remove any trailing whiespace at end of lines.\n
:EXAMPLE\n
\(let \(\(jnk-arg '\(\(68 4\) \(18 8 t\)\)\) ;;<- With and without arg NO-RMV-TRAIL-WSPC
      jnk jnk1\)
  \(dotimes \(j 2 
              \(with-current-buffer 
                  \(get-buffer-create \"*MON-STRING-JUSTIFY-LEFT-EG*\"\)
                \(erase-buffer\)
                \(insert \";; :FUNCTION `mon-string-justify-left'\\n;;\\n\"
                        \(mapconcat 'identity \(nreverse jnk1\) \"\\n\"\)\)
                \(display-buffer \(current-buffer\) t\)\)\)
    \(dotimes \(i 8 
                \(progn
                  \(push \(format \(if \(= j 0\) 
                                    \";; :FIRST-TIME-W-ARGS %S\\n\" 
                                  \"\\n;; :SECOND-TIME-W-ARGS %S\\n\"\)
                                \(car jnk-arg\)\) 
                        jnk1\)
                  \(push \(apply 'mon-string-justify-left jnk \(pop jnk-arg\)\) jnk1\)
                  \(setq jnk nil\)\)\)
                \(dolist \(i '\(64 94\)\)
                  \(setq jnk 
                        \(concat \" \" 
                                \(make-string \(elt \(mon-nshuffle-vector [7 5 3 9]\) 3\) i\) 
                      jnk\)\)\)\)\)\)\n
:SEE-ALSO `mon-string-fill-to-col', `truncate-string-to-width',
`mon-string-spread', `comment-padleft', `comment-padright', `mon-string-split',
`mon-string-set-char-at-idx',
`mon-string-insert-string-at-idx', `mon-string-index', `mon-string-upto-index',
`mon-string-after-index', `mon-string-has-suffix', `mon-string-sub-old->new',
`mon-string-repeat'.\n►►►"
  (let* ((lft-margin (if (null lft-margin) (or left-margin 0) lft-margin)) 
         (msjl-width (if (null justify-width) (or fill-column 72) justify-width))
         (msjl-string (if (not (stringp justify-string)) 
                          (error  (concat ":FUNCTION `string-justify-left' "
                                          "-- arg JUSTIFY-STRING must be a string"))
                        justify-string))
         (msjl-col (if (not (and (integerp justify-width) (integerp lft-margin)))
                       (error (concat  ":FUNCTION `string-justify-left' "
                                       "-- arg LFT-MARGIN or JUSTIFY-WIDTH not an integer"))
                     lft-margin))
         (msjl-split (save-match-data (split-string msjl-string))) ;; :WAS splited
         (msjl-margin (make-string lft-margin 32)) ;; :WAS margin
         (msjl-jstfy (substring msjl-margin 0 msjl-col)) ;; :WAS justified
         (msjl-len-wrd 0)
         (msjl-sep "")
         msjl-wrd)
    (while msjl-split
      (setq msjl-wrd (car msjl-split))
      (setq msjl-split (cdr msjl-split))
      (setq msjl-len-wrd (length msjl-wrd))
      (if (> msjl-len-wrd 0)
          (if (>= (+ msjl-col (length msjl-wrd)) msjl-width)
              (progn
                (setq msjl-jstfy (concat msjl-jstfy "\n" msjl-margin msjl-wrd))
                (setq msjl-col (+ left-margin msjl-len-wrd)))
              (progn
                (setq msjl-jstfy (concat msjl-jstfy msjl-sep msjl-wrd))
                (setq msjl-col (+ msjl-col 1 msjl-len-wrd)))))
      (setq msjl-sep " "))
    (when (< msjl-col msjl-width) 
      (setq msjl-jstfy (concat msjl-jstfy (make-string (- msjl-width msjl-col) 32)))) ;;))
    (if no-rmv-trail-wspc
        msjl-jstfy
      (setq msjl-jstfy (replace-regexp-in-string "[[:space:]]+$" "" msjl-jstfy)))))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((jnk-arg '((68 4) (18 8 t))) ;;<- With and without no-rmv-trail-wspc arg.
;;|       jnk jnk1)
;;|   (dotimes (j 2 
;;|               (with-current-buffer 
;;|                   (get-buffer-create "*MON-STRING-JUSTIFY-LEFT-EG*")
;;|                 (erase-buffer)
;;|                 (insert ";; :FUNCTION `mon-string-justify-left'\n;;\n"
;;|                         (mapconcat 'identity (nreverse jnk1) "\n"))
;;|                 (display-buffer (current-buffer) t)))
;;|     (dotimes (i 8 
;;|                 (progn
;;|                   (push (format (if (= j 0) 
;;|                                     ";; :FIRST-TIME-W-ARGS %S\n" 
;;|                                   "\n;; :SECOND-TIME-W-ARGS %S\n")
;;|                                 (car jnk-arg)) 
;;|                         jnk1)
;;|                   (push (apply 'mon-string-justify-left jnk (pop jnk-arg)) jnk1)
;;|                   (setq jnk nil)))
;;|                 (dolist (i '(64 94))
;;|                   (setq jnk 
;;|                         (concat " " 
;;|                                 (make-string (elt (mon-nshuffle-vector [7 5 3 9]) 3) i) 
;;|                       jnk))))))
;;`----

;;; ==============================
;;; :PREFIX "msftc-"
;;; :CREATED <Timestamp: #{2009-12-09T12:02:47-05:00Z}#{09503} - by MON>
(defun mon-string-fill-to-col (str to-col)
  "Return a string STR filled to column number TO-COL.\n
:EXAMPLE\n\n\(mon-string-fill-to-col \(mon-get-system-specs\) 72\)\n
\(mon-string-fill-to-col \(mon-get-system-specs\) 18\)\n
:SEE-ALSO `mon-line-strings-indent-to-col', `mon-line-indent-from-to-col',
`mon-string-justify-left', `truncate-string-to-width'.\n►►►"
  (let (msftc-fstr)  
    (setq msftc-fstr 
          (with-temp-buffer
            (let ((fill-column to-col))
              (insert str)
              (fill-region (mon-g2be -1 t) (mon-g2be 1 t))
              (mon-buffer-sub-no-prop) )))
    msftc-fstr))
;;
;;; :TEST-ME (mon-string-fill-to-col (mon-get-system-specs) 72)

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-index'
(defun mon-string-index (string-to-idx needle &optional frompos)
  "Return position in STRING-TO-IDX beginning with first occurence of NEEDLE.\n
Return nil if needle is not found.\n
NEEDLE is a char, number, or string.\n
When FROMPOS is non-nil begin search for needle from position. 
Default is to search from start of string.\n
:EXAMPLE\n\(mon-string-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-upto-index', `mon-string-after-index',
`mon-alphabet-as-type', `mon-string-position', `mon-string-has-suffix',
`mon-string-chop-spaces', `mon-string-replace-char'.\n►►►"
  (string-match 
   (regexp-quote 
    (cond ((or (characterp needle) (integerp needle)) (format "%c" needle))
          ((stringp needle) needle)
          (t ;; e.g. (integerp (set-marker (make-marker) (point)))
           (error (concat ":FUNCTION `mon-string-index' "
                          "-- arg NEEDLE expecting number or string, got: %S")
                  needle))))
   string-to-idx frompos))
;; 
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (= (mon-string-index "string before ### string after" "###") 14)
;; |     
;; | (string-equal 
;; |  (substring "string before ### string after" 0 
;; |             (mon-string-index "string before ### string after" "###"))
;; |  "string before ")
;; `----

;; (mon-string-index "string before ### string after" 18)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-01T15:16:26-04:00Z}#{09404} - by MON KEY>
(defun mon-string-upto-index (in-string upto-string)
  "Return substring of IN-STRING UPTO-STRING.\n
The arg UPTO-STRING is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-upto-index \"string before ### string after\" \"###\"\)\n  
:SEE-ALSO `mon-string-index', `mon-string-after-index'
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces',
`mon-string-replace-char'.\n►►►"
  (substring in-string 0 (mon-string-index in-string upto-string)))
;;
;;; :TEST-ME (mon-string-upto-index "string before ### string after" "###")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-01T15:16:29-04:00Z}#{09404} - by MON KEY>
(defun mon-string-after-index (in-str after-str)
  "Return substring of IN-STR AFTER-STR.\n
AFTER-STR is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-after-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-upto-index', `mon-string-position',
`mon-string-has-suffix', `mon-string-chop-spaces',
`mon-string-replace-char'.\n►►►"
  (substring in-str (+ (mon-string-index in-str after-str) (length after-str))))
;;
;;; :TEST-ME (mon-string-after-index "string before ### string after" "###")

;;; ==============================
;;; :PREFIX "mssd-"
;;; :NOTE This is a slow implementation.
;;; :CREATED <Timestamp: #{2010-03-23T17:38:29-04:00Z}#{10122} - by MON>
(defun mon-string-sort-descending (list-to-sort)
  "Destructively sort the list of strings by length in descending order.\n
:EXAMPLE\n\n\(let \(\(mk-str-l\)\)
  \(dotimes \(i 16 \(mon-string-sort-descending mk-str-l\)\)
    \(push \(make-string \(random 24\) 42\) mk-str-l\)\)\)\n
:SEE-ALSO `sort', `stable-sort', `string-lessp'.\n►►►"
  (let ((mssd-srt-l list-to-sort)
        (mssd-srt-pred #'(lambda (mssd-L-1 mssd-L-2) 
                           (let ((mssd-L-1-prd1 (length mssd-L-1))
                                 (mssd-L-1-prd2 (length mssd-L-2)))
                             (> mssd-L-1-prd1 mssd-L-1-prd2)))))
    (setq mssd-srt-l (sort mssd-srt-l mssd-srt-pred))))
;;
;;; :TEST-ME (let ((mk-str-l))
;;;               (dotimes (i 16 (mon-string-sort-descending mk-str-l))
;;;                 (push (make-string (random 24) 42) mk-str-l)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-position'
(defun mon-string-position (in-string find-substr &optional from-psn)
  "Return the position IN-STRING of the first occurence of FIND-SUBSTR
searching FROM-PSN, or from the start if FROM-PSN is absent or nil. 
If the SUBSTR is not found, then return nil.\n
:EXAMPLE\n\(mon-string-position \"dogmeat\" \"meat\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-upto-index', `mon-string-after-index',
`mon-string-to-sequence', `mon-string-from-sequence',
`mon-string-replace-char'.\n►►►"
  (string-match (regexp-quote find-substr) in-string from-psn))
;;
;;; :TEST-ME (= (mon-string-position "dogmeat" "meat") 3)

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-has-suffix'
(defun mon-string-has-suffix (check-string has-suffix &optional w-no-empty-strings)
  "Return t when CHECK-STRING HAS-SUFFIX as a component.\n
When optional arg W-NO-EMPTY-STRINGS is non-nil siganl an error if either
CHECK-STRING or HAS-SUFFIX is `mon-string-not-null-and-zerop'.\n
:EXAMPLE\n\n\(mon-string-has-suffix \"dogmeat\" \"meat\"\)\n
\(mon-string-has-suffix \"butt\" \"chicken-butt\"\)\n
\(mon-string-has-suffix \"suffix always true\" \"\"\)\n
\(mon-string-has-suffix \"\" \"\"\)\n
\(mon-string-has-suffix \(buffer-name\) (substring (buffer-name) 0 0)\)\n
;; The following signal:\n
\(mon-string-has-suffix \"\" \(current-buffer\)\)\n
\(mon-string-has-suffix 'bubba \(buffer-name\)\)\n
\(mon-string-has-suffix \"\" \"bubba\" t\)\n
\(mon-string-has-suffix \"bubba\" \"\" t\)\n
\(mon-string-has-suffix (substring (buffer-name) 0 0) \"bubba\" t\)\n
\(mon-string-has-suffix \"bubba\" \"\" t\)\n
:ALIASED-BY `mon-string-suffix-p'\n
:SEE-ALSO `mon-string-prefix-p', `mon-string-position', `mon-string-index',
`mon-string-upto-index', `mon-string-after-index',
`mon-string-replace-char'.\n►►►"
  (and (or (and w-no-empty-strings
                (or (mon-string-not-null-nor-zerop check-string)
                    (error (concat ":FUNCTION `mon-string-has-suffix' " 
                                   "-- with optional arg W-NO-EMPTY-STRINGS non-nil, "
                                   "arg CHECK-STRING does not satisfy `mon-string-not-null-nor-zerop', "
                                   "got: %S type-of: %S" )
                           check-string (type-of check-string)))
                (or (mon-string-not-null-nor-zerop has-suffix)
                    (error (concat ":FUNCTION `mon-string-has-suffix' " 
                                   "-- with optional arg W-NO-EMPTY-STRINGS non-nil, "
                                   "arg HAS-SUFFIX does not satisfy `mon-string-not-null-nor-zerop', "
                                   "got: %S type-of: %S" )
                           has-suffix (type-of has-suffix))))
           t)
       (and (or (stringp check-string)
                (mon-error-string-err-format "mon-string-has-suffix" "check-string" check-string t))
            (or (stringp has-suffix)
                (mon-error-string-err-format "mon-string-has-suffix" "has-suffix" has-suffix t))))
  (cond ((< (length check-string) (length has-suffix)) nil)
        ;; :WAS (t (string-equal {...} 
        ;; ,---- :NOTE Doesn't seem taht string-equal gets us much:
        ;; |  (let ((bubba "bubba")
        ;; |        (bubba2 "bubba"))
        ;; |     (set-text-properties 0 (length bubba) '(font-lock-face 'bold) bubba)
        ;; |     (equal bubba bubba2))
        ;; `----
        (t (equal (substring check-string (- (length check-string) (length has-suffix)))
                  has-suffix))))
;;
;;; :TEST-ME (and (mon-string-has-suffix "dogmeat" "meat") t)
;;; :TEST-ME (and (mon-string-has-suffix "suffix always true" "") t)
;;; :TEST-ME (and (mon-string-has-suffix "" "") t)
;;; :TEST-ME (and (not (mon-string-has-suffix "butt" "chicken-butt")) t)
;;; :TEST-ME (not (ignore-errors (mon-string-has-suffix 88 "chicken-butt")))
;;; :TEST-ME (not (ignore-errors (mon-string-has-suffix "butt" 88)))
;;; :TEST-ME (and (ignore-errors (mon-string-has-suffix "suffix always true" "" t) t)
;;; :TEST-ME (and (mon-string-has-suffix "" "" t) t)

;;; ==============================
;;; :PREFIX "mscs-"
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-advices.el :WAS `pjb-chop-spaces'
;;; :CREATED <Timestamp: #{2009-09-28T16:39:34-04:00Z}#{09401} - by MON>
(defun mon-string-chop-spaces (chop-string)
  "Return substring of `chop-string' with \(char 32\) removed left and right.\n
:EXAMPLE\n\n\(mon-string-chop-spaces \" some string no spaces \"\)\n
:SEE-ALSO `mon-region-split-commas', `mon-string-split-on-regexp',
`mon-string-sub-old->new', `mon-string-chop-spaces', `mon-string-position',
`mon-string-index', `mon-string-upto-index', `mon-string-after-index',
`mon-alphabet-as-type', `mon-string-replace-char'.\n►►►"
  (let ((mscs-itr 0)
        (mscs-len (1- (length chop-string)))
        (mscs-spc 32))
    (while (and (< 0 mscs-len) (eq (aref chop-string mscs-len) mscs-spc))
      (setq mscs-len (1- mscs-len)))
    (setq mscs-len (1+ mscs-len))
    (while (and (< mscs-itr mscs-len) (eq (aref chop-string mscs-itr) mscs-spc))
      (setq mscs-itr (1+ mscs-itr)))
    (substring chop-string mscs-itr mscs-len)))
;;
;;; :TEST-ME (mon-string-chop-spaces " some string no spaces ")

;;; ==============================
;;; :PREFIX "msil-"
;;; :RENAMED `mon-stringify-list' -> `mon-string-ify-list'
(defun mon-string-ify-list (string-given)
  "Return a list of strings by breaking STRING-GIVEN at space boundaries.\n
:EXAMPLE\n\(mon-string-ify-list \"Make this sentence a list of strings\")\n
:SEE-ALSO `mon-stringify-list' ,`mon-insert-string-ify', 
`mon-line-string-split', `mon-line-get-next', 
`mon-word-get-list-in-buffer', `mon-alphabet-as-type',
`mon-string-replace-char'.\n►►►"
  (let ((msil-str string-given)
        msil-lst)
    (set-match-data nil)
    (while (string-match " *\\([^ ]+\\) *" msil-str (match-end 0))
      (setq msil-lst
	    (cons (substring msil-str (match-beginning 1) (match-end 1)) msil-lst)))
    (nreverse msil-lst)))
;;
;;; :TEST-ME (equal  
;;;           (mon-string-ify-list "Make this sentence a list of strings")
;;;            '("Make" "this" "sentence" "a" "list" "of" "strings"))

;;; ==============================
;;; :PREFIX "mssor-"
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-split'
(defun mon-string-split-on-regexp (w-split-str at-regexp)
  "Return list of strings splitting W-SPLIT-STR AT-REGEXP.\n
This function is patterned after the awk split\(\) function.\n
:EXAMPLE\n\n\(mon-string-split-on-regexp \"split-on-split\" \"-split\"\)\n
:SEE-ALSO `mon-string-split', `mon-string-chop-spaces', `mon-string-sub-old->new',
`mon-string-position', `mon-string-index', `mon-string-replace-char'.\n►►►"
  
  (if ;; Either W-SPLIT-STR or AT-REGEXP is null or zero length return nil
      (or (mon-string-or-null-and-zerop w-split-str)
          (mon-string-or-null-and-zerop at-regexp))
      nil
    ;; Else split the string and return a list of strings.
    (let ((mssor-gthr nil) 
          (mssor-mtch 0))  
      (while (and (< mssor-mtch (length w-split-str)) 
                  (string-match at-regexp w-split-str mssor-mtch))
        (setq mssor-gthr 
              (nconc mssor-gthr 
                     (list (substring w-split-str mssor-mtch (match-beginning 0)))))
        (setq mssor-mtch (match-end 0)))
      (setq mssor-gthr (nconc mssor-gthr (list (substring w-split-str mssor-mtch))))
      mssor-gthr)))
;;
;;; :TEST-ME (mon-string-split-on-regexp "split-on-split" "-split")

;;; ==============================
;;; ;PREFIX "msrc-"
;;; :CREATED <Timestamp: #{2010-01-30T15:07:18-05:00Z}#{10046} - by MON KEY>
(defun mon-string-replace-char (from-char target-string)
  "Return TARGET-STRING with all instances of FROM-CHAR removed.\n
Signal and error if either TARGET-STRING or FROM-CHAR evaluate non-nil for
`stringp' and `chararacterp' respectively.\n
:EXAMPLE\n
\(mon-string-replace-char 0 \"\x0I'm\x0 an\x0 ugly\x0 string.\")\n
:SEE-ALSO `subst-char-in-string', `mon-string-from-hex-list', 
`mon-string-to-hex-string', `mon-help-char-representation'.\n►►►"
  (if (and (characterp from-char) (stringp target-string))
      (let ((msrc-ts (append target-string nil)))
        (setq msrc-ts (apply 'string (remq from-char msrc-ts))))
    (error 
     (concat ":FUNCTION `mon-string-replace-char' "
             "-- arg FROM-CHAR not a valid char or TARGET-STRING not a string"))))
;;
;;; :TEST-ME (mon-remove-char-in-string 0 "\x0I'm\x0 an\x0 ugly\x0 string.")

;;; ==============================
;;; :PREFIX "msson-"
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `melvyl-sub'
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:26:52-0400Z - by MON KEY>
(defun mon-string-sub-old->new (old-str new-str cpy-str)
  "Return copy of CPY-STR with first occurrence of OLD-STR substituted by NEW-STR.\n
Signal an error when an arg does not satisfy `stringp'.\n
:EXAMPLE\n\n\(mon-string-sub-old->new \"old\" \"new\" \"old old new\"\)\n
:SEE-ALSO `mon-string-split-on-regexp', `mon-string-chop-spaces',
`mon-string-position', `mon-string-index', `mon-string-replace-char'.\n►►►"
  (and (or (stringp old-str)
           (mon-error-string-err-format  "mon-string-sub-old->new" "old-str" old-str t))
       (or (stringp new-str)
           (mon-error-string-err-format  "mon-string-sub-old->new" "new-str" new-str t))
       (or (stringp cpy-str)
           (mon-error-string-err-format  "mon-string-sub-old->new" "cpy-str" cpy-str t)))
  (let ((msson-itr 0))
    (while (and 
            (< msson-itr (1+ (- (length cpy-str) (length old-str))))
            (not 
             (string-equal 
              old-str 
              (substring cpy-str msson-itr (+ msson-itr (length old-str))))))
      (setq msson-itr (1+ msson-itr)))
    (if (and 
         (< msson-itr (1+ (- (length cpy-str) (length old-str))))
         (string-equal old-str (substring cpy-str msson-itr (+ msson-itr (length old-str)))))
        (concat (substring cpy-str 0 msson-itr) new-str
                (substring cpy-str (+ msson-itr (length old-str)) (length cpy-str)))
      cpy-str)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (string-equal (mon-string-sub-old->new"old" "new" "old old new") "new old new")
;; | (not (ignore-errors (mon-string-sub-old->new "old" 'make-new-signal "old old new")))
;; | (not (ignore-errors (mon-string-sub-old->new 'make-old-signal "new" "old old new")))
;; | (not (ignore-errors (mon-string-sub-old->new "old" "new" 'make-copy-signal)))
;; `----

;;; ==============================
;;; :PREFIX "msr-"
;;; :COURTESY Jared D. :WAS `string-repeat'
;;; :SEE (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :MODIFICATIONS <Timestamp: #{2009-08-19T20:13:32-04:00Z}#{09344} - by MON KEY>
(defun mon-string-repeat (rpt-str rpt-cnt &optional insrtp w-spc intrp)
  "Return the string RPT-STR RPT-CNT times.\n
When optional INSRTP is non-nil or called-interactively insert STR at point.
Does not move point. 
When W-SPC is non-nil return string with whitespace interspersed.\n
:EXAMPLE\n\(mon-string-repeat \"bubba\" 3 nil t\)\n
:SEE-ALSO `mon-insert-string-ify', `mon-string-incr', 
`mon-insert-string-n-fancy-times', `mon-insert-string-n-times',
`mon-string-replace-char'.\n►►►"
  (interactive 
   (list 
    (replace-regexp-in-string "[\\[:space:]]+$" ""  ;; :WAS "[[:space:]]+$" "" 
                              (read-string  (concat ":FUNCTION `mon-string-repeat' "
                                                    "-- string to repeat: ")))
    (read-number (concat ":FUNCTION `mon-string-repeat' "
                         "-- times to repeat: "))
    nil
    (yes-or-no-p (concat ":FUNCTION `mon-string-repeat' "
                         "-- with whitespace: "))))
  (let ((msr-rtn ""))
    (dotimes (msr-D-1 rpt-cnt)
      (if w-spc 
          (setq msr-rtn (concat msr-rtn rpt-str " "))
        (setq msr-rtn (concat msr-rtn rpt-str))))
    (if (or insrtp intrp)
        (save-excursion (insert msr-rtn)))
    msr-rtn))
;;
;;; :TEST-ME (mon-string-repeat "bubba" 3)
;;; :TEST-ME (mon-string-repeat "bubba" 3 t)
;;; :TEST-ME (mon-string-repeat "bubba" 3 t t)
;;; :TEST-ME (mon-string-repeat "bubba" 3 nil t)
;;; :TEST-ME (call-interactively 'mon-string-repeat) 


 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-10T13:17:24-04:00Z}#{10146} - by MON>
(defun mon-string-to-hex-list-cln-chars (rep-str)
  "Return list of hex chars in REP-STR with chars: `,' `00' `\\' removed.\n
:EXAMPLE\n\n\(mon-string-to-hex-list-cln-chars
 \"72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\\ \"\)\n
\(mon-hex-list-as-string
 \(mon-string-to-hex-list-cln-chars 
  \"72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\\ \"\)\)\n
:NOTE Intended usage is for quick conversion of registry keys e.g.:\n
 [HKEY_CLASSES_ROOT\\InternetShortcut\\shell\\printto\\command]
 @=hex(2):72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\\
 { ... }
 22,00,00,00\n
:SEE-ALSO `mon-string-to-hex-string', `mon-string-from-hex-list',
`mon-string-to-hex-list', `url-hexify-string', `url-unhex-string',
`url-unhex', `slime-net-encode-length', `slime-net-decode-length'.\n►►►"
  (car (read-from-string 
        (format "(%s)" (replace-regexp-in-string "00\\|[,\]" " " rep-str)))))
;;
;;: :TEST-ME (mon-string-to-hex-list-cln-chars 
;;;              "72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\ ")
;;; :TEST-ME(mon-hex-list-as-string
;;;             (mon-string-to-hex-list-cln-chars 
;;;              "72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\ "))
 
;;; ==============================
;;; :PREFIX "msths-"
;;; :CREATED <Timestamp: #{2009-11-06T17:41:33-05:00Z}#{09455} - by MON>
(defun* mon-string-to-hex-string (&key hxify-str w-dlim prand-hex-len)
  "Return HXIFY-STR as a string of hex numbers.
When keyword W-DLIM is non-nil delimit hex numbers W-DLIM.\n
When keyword PRAND-HEX-LEN \(an integer >= 80\) is non-nil, return a
pseudo-random string of length N generated with `mon-generate-prand-id'.\n
Useful for generating throw-away WPA keys.\n
:EXAMPLE\n\n\(mon-string-to-hex-string :hxify-str \"bubba\"\)\n
\(mon-string-to-hex-string :hxify-str \"bubba\" :w-dlim \":\"\)\n
\(mon-string-to-hex-string :hxify-str \"bubba\" :w-dlim \" \"\)\n
\(mon-string-to-hex-string :prand-hex-len 64\)\n
\(mon-string-to-hex-string :prand-hex-len 81\) ;<-Should Fail.\n
:NOTE :HEXADECIMAL 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
      :DECIMAL     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15\n
:SEE-ALSO `mon-string-from-hex-list', `mon-string-to-hex-list',
`mon-string-to-hex-list-cln-chars', `mon-generate-WPA-key',
`mon-generate-prand-seed', `mon-string-replace-char',
`hexl-hex-string-to-integer', `url-hexify-string', `url-unhex-string',
`url-unhex', `slime-net-encode-length', `slime-net-decode-length'.\n►►►"
  (let (msths-rtn)
    (unless prand-hex-len
      ;; :NOTE Consider using (append hxify-str nil) instead of the mapping.
      (mapc #'(lambda (msths-L-1) 
                (setq msths-rtn (cons msths-L-1 msths-rtn)))
            hxify-str)
      (setq msths-rtn (reverse msths-rtn))
      (setq msths-rtn
            (mapconcat #'(lambda (msths-L-2) 
                           (format "%x" msths-L-2))
                       msths-rtn
                       (if (and w-dlim (stringp w-dlim)) w-dlim ""))))
    (when prand-hex-len 
      (if (<= prand-hex-len 80)
          (setq msths-rtn
                (substring 
                 (concat (car (mon-generate-prand-id))
                         (car (mon-generate-prand-id))) 0 prand-hex-len))
        (error (concat ":FUCTION `mon-string-to-hex-string' "
                       "-- %s is too large or not a number") prand-hex-len)))
    msths-rtn))
;;
;;; :TEST-ME (mon-string-to-hex-string :hxify-str "bubba")
;;; :TEST-ME (mon-string-to-hex-string :hxify-str "bubba" :w-dlim ":")
;;; :TEST-ME (mon-string-to-hex-string :hxify-str "bubba" :w-dlim " ")
;;; :TEST-ME (mon-string-to-hex-string :prand-hex-len 64)
;;; :TEST-ME (mon-string-to-hex-string :prand-hex-len 81) ;Should Fail.

;;; ==============================
;;; :PREFIX "msfhl-"
;;; :CREATED <Timestamp: #{2009-11-07T14:50:16-05:00Z}#{09456} - by MON>
(defun mon-string-from-hex-list (hex-lst)
  "Return HEX-LST \(a list of hex chars) as a string.\n
Useful for working with w32 registry keys of type REG_BINARY.\n
:EXAMPLE\n\(mon-hex-list-as-string 
 '(43 00 3a 00 5c 00 50 00 72 00 6f 00 67 00 72 00 61 00 6d 00 20 00 46 00 69 00
 6c 00 65 00 73 00 5c 00 74 00 65 00 78 00 6c 00 69 00 76 00 65 00 5c 00 32 00
 30 00 30 00 38 00 5c 00 62 00 69 00 6e 00 5c 00 77 00 69 00 6e 00 33 00 32 00
 5c 00 70 00 61 00 74 00 67 00 65 00 6e 00 2e 00 65 00 78 00 65 \)\)\n
\(mon-string-from-hex-list
 \(split-string \(mon-string-to-hex-string :hxify-str \"bubba\" :w-dlim \":\"\) \":\" t\)\)\n
:NOTE :HEXADECIMAL 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
      :DECIMAL     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15\n
:ALIASED-BY `mon-hex-list-as-string'\n
:SEE-ALSO `mon-string-to-hex-list', `mon-string-to-hex-string',
`mon-string-to-hex-list-cln-chars', `hexl-hex-string-to-integer',
`url-hexify-string', `url-unhex-string', `url-unhex'
`slime-net-encode-length', `slime-net-decode-length'.\n►►►"
  (eval-when-compile (require 'hexl)) ;; `hexl-hex-string-to-integer'
  (let (msfhl-str msfhl-int)
    (mapc #'(lambda (msfhl-L-1) (push (format "%s" msfhl-L-1) msfhl-str))
          hex-lst)
    (mapc #'(lambda (msfhl-L-2) (push (hexl-hex-string-to-integer msfhl-L-2) msfhl-int))
          msfhl-str)
    (mon-string-from-sequence msfhl-int)))
;;
;;; :TEST-ME 
;;; (mon-string-from-hex-list
;;;  (split-string 
;;;   (mon-string-to-hex-string :hxify-str "bubba" :w-dlim ":")
;;;   ":" t))

;;; ==============================
;;; :PREFIX "msthl-"
;;; :CREATED <Timestamp: #{2010-01-30T19:41:27-05:00Z}#{10047} - by MON KEY>
(defun mon-string-to-hex-list (string-hexify)
  "Return string as a list of hex values.\n
:NOTE Can roundtrip the output of `mon-string-to-hex-list'.\n
:EXAMPLE\n\n\(mon-string-to-hex-list \"bùbbä_◄\\\"\t\\\"►mô'búbbá\"\)\n
\(mon-string-to-hex-list \"\"\)\n
;; Following example of roundtriping string -> hex-list -> string:\n
\(mon-string-from-hex-list 
  \'(62 f9 62 62 e4 5f 25c4 22 9 22 25ba 6d f4 27 62 fa 62 62 e1\)\)\n
\(mon-string-from-hex-list 
  \(mon-string-to-hex-list \"bùbbä_◄\\\"\t\\\"►mô'búbbá\"\)\)\n
;; Following fails successfully:\n
\(mon-string-to-hex-list 8\)\n
:NOTE :HEXADECIMAL 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
      :DECIMAL     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15\n
:SEE-ALSO `mon-string-from-hex-list', `mon-generate-prand-id',
`mon-string-to-hex-list-cln-chars', `mon-string-to-hex-string',
`hexl-hex-string-to-integer', `url-hexify-string', `url-unhex-string',
`url-unhex', `slime-net-encode-length', `slime-net-decode-length'.\n►►►"
  (if (mon-string-or-null-and-zerop string-hexify)
      nil
    (let (msthl-hexify)
      (mapc #'(lambda (msthl-L-1)
                ;; :NOTE Is there a better way to do this than using `read-from-string'?
                (let ((car-char (car (read-from-string (format "%x" msthl-L-1)))))
                  (push car-char msthl-hexify)))
            (if (stringp string-hexify)
                (append string-hexify nil)
              (mon-error-string-err-format "mon-string-to-hex-string" "string-hexify" string-hexify t)))
      (setq msthl-hexify (nreverse msthl-hexify)))))
;; 
;;; :TEST-ME (mon-string-to-hex-list "bubba\x09mo'bubba")
;;; :TEST-ME (mon-string-to-hex-list "")
;;; :TEST-ME (mon-string-from-hex-list (mon-string-to-hex-list "bubba"))
;;; :TEST-ME (mon-string-to-hex-list 8) ;Fails successfully

;;; ==============================
;;; :COURTESY Marc Tfardy  
;;; :SOURCE Newsgroups: comp.emacs 
;;; :SUBJECT Re: re-search-forward and assoc list
;;;
;;; :MODIFICATONS <Timestamp: Sunday May 31, 2009 @ 08:22.06 AM - by MON KEY>
;;; :CREATED <Timestamp: Friday March 27, 2009 @ 04:50.09 PM - by MON KEY>
;;; ==============================
(defun mon-string-infix (string-lst infix-str)
  "Create a string with STRING-LST, which are separated by INFIX-STR.\n
STRING-LST is a list of strings to intersperse with INFIX-STR.\n
INFIX-STR is a string to intersperse with.
:EXAMPLE\n\n\(mon-string-infix '\(\"a\" \"b\" \"c\" \"d\"\) \"+\"\)\n
\(mon-with-inhibit-buffer-read-only
  (save-excursion
    \(call-interactively 
     #'\(lambda \(&optional rplc-alst\)
         \"Convert certain strings according to rplc-alst key-value pairs.\"
         \(interactive\)
         \(or rplc-alst \(setq rplc-alst '\(\(\"x\" . \"bar\"\) \(\"y\" . \"foo\"\)\)\)\)
         \(while \(re-search-forward 
                 \(concat \"\\\\\(\" \(mon-string-infix \(mapcar #'car rplc-alst\) \"\\\\|\"\) \"\\\\\)\"\) nil t)
           \(replace-match \(cdr \(assoc-string \(match-string 1\) rplc-alst\)\)\)\)\)\)\)\)\n\nx y\n
:NOTE This is similiar to `mapconcat' but without the FUNCTION argument.\n
:SEE-ALSO `mon-string-splice-sep', `mon-string->strings-splice-sep',
`mon-string-ify-list' `mon-list-intersperse'.\n►►►"
  (unless (stringp infix-str) 
    (error (mon-error-string-err-format "mon-string-infix" "infix-str" infix-str)))
  (cond ((null string-lst) "")
        ((null (cdr string-lst))
         (car string-lst))
        ((cdr string-lst)
         (concat (car string-lst) infix-str
                 (mon-string-infix (cdr string-lst) infix-str)))))
;;
;;; :TEST-ME (equal (mon-string-infix '("a" "b" "c" "d") "+") "a+b+c+d")
;;; :TEST-ME (not (ignore-errors (mon-string-infix '("a" "b" "c" "d") 'I-fail-silently)))


;;; ==============================
;;; :CHANGESET 2331
;;; :CREATED <Timestamp: #{2010-11-27T17:44:28-05:00Z}#{10476} - by MON KEY>
(defun mon-string-explode (w-str)
  "Split characters in string W-STR into list of strings.\n
:EXAMPLE\n\n\(mon-string-explode \"bubba\"\)\n
\(mon-string-infix 
 \(mon-mapcan #'\(lambda \(x &rest y\) \(mon-string-explode x\)\)
             \(mon-mapcan #'\(lambda \(x &rest y\) \(cons x y\)\)
                         '\(\"Bubba\" \"Bobby\" \"Sally\" \"Suzy\"\)
                         '\(\"Botho\" \"Bastian\" \"Svenja\" \"Selma\"\)\)\)
 \"-\"\)\n
:SEE-ALSO `mon-string-split', `mon-string-infix', `mon-string-splice-sep',
`mon-string->strings-splice-sep'.\n►►►"
  (loop 
   for mse-itr across w-str 
   collect (format "%c" mse-itr)))

;;; ==============================
;;; :PREFIX "msprmt-"
;;; :CREATED <Timestamp: #{2010-02-07T19:03:12-05:00Z}#{10057} - by MON KEY>
(defun mon-string-permute (permute-string &optional intrp)
  "Return list of permutations of PERMUTE-STRING.\n
When called-interactively or optional arg INTRP is non-nil prompt for a string
to permute and insert PERMUTE-STRING in current-buffer with each permuatation on
a newline. Does not move point.\n
:EXAMPLE\n\n\(mon-string-permute \"bubba\")\n
:NOTE Duplicate chars in a string are not ommitted and case is signifcant:
 \(length \(mon-string-permute \"buba\"\)\)
 \(length \(mon-string-permute \"bubba\"\)\)
 \(length \(mon-string-permute \"buBa\"\)\)
 \(length \(mon-string-permute \"bůḇBá\"\)\)
:ALIASED-BY `mon-permute-string'\n
:SEE-ALSO `mon-permute-combine', , `mon-list-variant-forms',
`mon-permute-combine-functions-TEST', `mon-list-permute-variants', `mon-list-permute-1'.\n►►►"
  (interactive "i\np")
  ;; :NOTE Maybe let-bind `max-lisp-eval-depth', `max-specpdl-size'?
  ;; NO, better to just remove duplicate chars inside `msprmt-str` initial let binding
  (let ((msprmt-str 
         (append (if (and intrp (not permute-string))
                     (read-string 
                      (concat ":FUNCTION `mon-string-permute' "
                              "-- string to permute: "))
                   permute-string)
                 nil))
        msprmt-rtn-prm)
    (setq msprmt-rtn-prm 
          (delete-dups (mon-list-permute-variants msprmt-str)))
    (setq msprmt-rtn-prm (mapcar #'(lambda (msprmt-L-1) 
                                     (concat msprmt-L-1))
                                 msprmt-rtn-prm))
    (when intrp 
      (save-excursion
        (newline)
        (insert (mapconcat #'identity msprmt-rtn-prm "\n"))))
    msprmt-rtn-prm))
;;
;;; :TEST-ME (length (mon-string-permute "bubzzfmmfifi"))
;;; :TEST-ME (mon-string-permute "bu" t)

;;; ==============================
;;; :PREFIX "mspl-"
;;; :COURTESY: Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `perm-words'
;;; :WAS `mon-perm-words' ->`mon-string-permute-line'
(defun mon-string-permute-line (&optional delimit-with)
  "Insert after current line all permutations of words on the current line.\n
:EXAMPLE\n\n\(let \(return-permuted-eg\)
  \(setq return-permuted-eg 
        \(with-temp-buffer 
          \(save-excursion \(insert \"word word2 word3 word4\"\)\)
          \(mon-string-permute-line\)
          \(buffer-substring-no-properties \(buffer-end 0\)\(buffer-end 1\)\)
          \(mon-buffer-sub-no-prop\)\)\)\)\n
:SEE-ALSO `mon-list-permute-variants', `mon-string-ify-current-line'.\n►►►"
  (interactive)
  (let ((mspl-wrds (car (read-from-string
                     (format "(%s)" 
			     (mon-buffer-sub-no-prop
			      (progn (beginning-of-line) (point))
			      (progn (end-of-line) (point)))))))
	(mspl-dlm (if delimit-with delimit-with "")))
    (end-of-line)
    (insert "\n")
    (dolist (mspl-D-ln (mon-list-permute-variants mspl-wrds))
      (dolist (mspl-D-wrd mspl-D-ln)
        (insert (format "%s %s"
			(if (and (listp mspl-D-wrd) (eq 'quote (car mspl-D-wrd))) 
			    (cadr mspl-D-wrd) mspl-D-wrd)
			mspl-dlm))) ;)))
      (insert "\n"))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let (return-permuted-eg)
;; |   (setq return-permuted-eg 
;; |         (with-temp-buffer 
;; |           (save-excursion (insert "word word2 word3 word4"))
;; |           (mon-string-permute-line)
;; |           (mon-buffer-sub-no-prop) )))
;; `----

;;; ==============================
;;; :RENAMED `mon-strings-splice-sep' -> `mon-string-splice-sep'
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 07:28.25 AM - by MON KEY>
(defun mon-string-splice-sep (strings &optional seperator insert-str insrtp intrp)
  "Return concatenation of STRINGS spliced together with SEPARATOR.\n
When SEPERATOR (a string) is non-nil it's value inserted between STRINGS.
Default is \" | \" which can be useful to build up name lists in `naf-mode'.\n
Called interactively insert converted string at point. Moves point.\n
When INSERT-STR is non-nil insert as string as if by princ else as if by prin1.\n
When INSRTP is non-nil and INSERT-STR is null returan as if by prin1.\n
:EXAMPLE\n
\(mon-string-splice-sep '\(\"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\"\) \" \" \)\n
\(mon-string-splice-sep '\(\"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\"\) \"|\"\)\n
\(mon-string-splice-sep '\(\"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\"\) \"| \"\)\n
\(mon-string-splice-sep '\(\"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\"\) \" |\"\)\n
\(mon-string-splice-sep '\(\"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\"\) \"_\"\)\n
\(mon-string-splice-sep '\(\"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\" \"AAOA\" \"aaoa\"\) \" - \"\)\n
:SEE-ALSO `mon-string->strings-splice-sep', `mon-string-infix'.\n►►►"
  (interactive (list
                (read-string  (concat ":FUNCTION `mon-string-splice-sep' " 
                                      "-- string to splice: "))
                (read-string (concat ":FUNCTION `mon-string-splice-sep' " 
                                     "-- separate with: "))
                (yes-or-no-p (concat ":FUNCTION `mon-string-splice-sep' " 
                                     "-- insert as string?: "))
                nil t))
  (let ((msss-str)
	(msss-sep (if seperator seperator " | ")))
    (while strings
      (setq msss-str (concat msss-str (car strings)))
      (if (cdr strings)
	  (setq msss-str (concat msss-str msss-sep)))
      (setq strings (cdr strings)))
    (cond (intrp 
           (if insert-str
               (princ msss-str (current-buffer))
             (prin1 msss-str (current-buffer))))
          ((and (not intrp) (or insert-str insrtp))
           (if insert-str
               (princ msss-str (current-buffer))
             (prin1 msss-str (current-buffer))))
          ((and (not intrp) (not insert-str) (not insrtp))
           msss-str))))

;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (equal 
;; |  (mon-string-splice-sep '("AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA") " " )
;; |  "AAOA aaoa AAOA aaoa AAOA aaoa AAOA")
;; | (equal 
;; |  (mon-string-splice-sep '("AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA") "|")
;; |  "AAOA|aaoa|AAOA|aaoa|AAOA|aaoa|AAOA")
;; | (equal 
;; |  (mon-string-splice-sep '("AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA") "| ")
;; |  "AAOA| aaoa| AAOA| aaoa| AAOA| aaoa| AAOA")
;; | (equal 
;; |  (mon-string-splice-sep '("AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA") " |")
;; |  "AAOA |aaoa |AAOA |aaoa |AAOA |aaoa |AAOA")
;; | (equal 
;; |  (mon-string-splice-sep '("AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA") "_")
;; |  "AAOA_aaoa_AAOA_aaoa_AAOA_aaoa_AAOA")
;; | (equal 
;; |  (mon-string-splice-sep '("aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa") " - ")
;; |  "aaoa - AAOA - aaoa - AAOA - aaoa - AAOA - aaoa")
;; | (equal
;; |  (with-temp-buffer
;; |    (mon-string-splice-sep '("AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA") "|" t t)
;; |    (buffer-string))
;; |  "AAOA|aaoa|AAOA|aaoa|AAOA|aaoa|AAOA")
;; | (equal
;; |  (with-temp-buffer
;; |    (mon-string-splice-sep '("AAOA" "aaoa" "AAOA" "aaoa" "AAOA" "aaoa" "AAOA") "|" nil t)
;; |    (buffer-string))
;; |  "\"AAOA|aaoa|AAOA|aaoa|AAOA|aaoa|AAOA\"")
;; `----

;;; ==============================
;;; :RENAMED `mon-string2strings-splice-sep' -> `mon-string->strings-splice-sep'
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 02:51.45 PM - by MON KEY>
(defun mon-string->strings-splice-sep (string2strings &optional seperator insrtp w-princ)
  "Like `mon-strings-splice-sep' but converts string -> strings before seperation.\n
STRING2STRINGS \(a string\) is the string to split.\n
SEPERATOR \(a string\) delimits the return value - default is \" | \".n
When called interactively or INSRTP is non-nil insert string conversion at point.
Moves point.\n
When called with prefix arg or W-PRINC is non-nil insert as with princ.
Moves point.\n
:SEE-ALSO `mon-string-infix', `mon-string-splice-sep',
`mon-string-ify-list'.\n►►►"
  (interactive (list (read-string 
                      (concat ":FUNCTION `mon-string->strings-splice-sep' "
                              "-- string to splice: " ))

                     (read-string 
                      (concat ":FUNCTION `mon-string->strings-splice-sep' "
                              "-- separate with: " ))
                     t
                     (yes-or-no-p 
                      (concat ":FUNCTION `mon-string->strings-splice-sep' "
                              "-- (Y) inserts unquoted (N) to insert string?: "))))
  (let* ((mssss-stngs (mon-string-ify-list string2strings))
         (mssss-str-splc (mon-string-splice-sep mssss-stngs seperator)))
    (when insrtp
      (if w-princ
          (princ mssss-str-splc (current-buffer))
        (prin1 mssss-str-splc (current-buffer))))
    mssss-str-splc))
;;
;;; :TEST-ME (mon-string->strings-splice-sep  "Make this sentence a list of strings" "++")
;;; :TEST-ME (mon-string->strings-splice-sep  "Make this sentence a list of strings")
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" nil t)
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" nil t t)
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" "*_*" t)
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" "*_*" t t)
;;; :TEST-ME (call-interactively 'mon-string->strings-splice-sep)

;;; =======================
(defun mon-string-to-regexp (string-to-cnv &optional insrtp intrp)
  "Return a `regex-opt'd list of strings.\n
String obtained by splitting read-string from mini-buffer.\n
:EXAMPLE\n\n(mon-string-to-regexp \"Return a regexp-opt list of strings splitting\")\n
\(call-interactively 'mon-string-to-regexp\)\n
:SEE-ALSO `mon-string-rotate-to-regexp', `mon-string-ify-list'.\n►►►"
  (interactive "s:FUNCTION `mon-string-to-regexp' -- string to convert: \nP\np")
  (let*  ((mstr-strngfy (mon-string-ify-list string-to-cnv))
	  (mstr-cnvrtd (mon-string-rotate-to-regexp mstr-strngfy)))
    (if (or insrtp intrp)
	(prin1 mstr-cnvrtd (current-buffer))
      mstr-cnvrtd)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (equal (mon-string-to-regexp "Returns a regexp-opt list of strings splitting")
;; |        "\\(?:Returns\\|a\\|list\\|of\\|regexp-opt\\|s\\(?:plitting\\|trings\\)\\)")
;; | (equal
;; |  (with-temp-buffer 
;; |    (mon-string-to-regexp "Returns a regexp-opt list of strings splitting" t)
;; |    (buffer-string))
;; |  "\"\\\\\(?:Returns\\\\|a\\\\|list\\\\|of\\\\|regexp-opt\\\\|s\\\\\(?:plitting\\\\|trings\\\\\)\\\\\)\"")
;; | (equal
;; |  (with-temp-buffer 
;; |    (mon-string-to-regexp "Returns a regexp-opt list of strings splitting" nil t)
;; |    (buffer-string))
;; |  "\"\\\\\(?:Returns\\\\|a\\\\|list\\\\|of\\\\|regexp-opt\\\\|s\\\\\(?:plitting\\\\|trings\\\\\)\\\\\)\"")
;; `----
;;;
;;; :TEST-ME (call-interactively 'mon-string-to-regexp)


;;; ==============================
;;; :PREFIX "msmil-"
;;; :NOTE Modelled after `erc-list-match' but using catch/throw instead of memq/mapcar
;;; :CHANGESET 2365
;;; :CREATED <Timestamp: #{2010-12-16T14:36:12-05:00Z}#{10504} - by MON KEY>y
(defun mon-string-match-in-list-p (regexp-lst match-str &optional match-from)
  "Return non-nil if any regexp in REGEXP-LST matches MATCH-STR.\n
REGEXP-LST is a list of strings.\n
MATCH-STR is a string to try matching.\n
When optional arg MATCH-FROM is non-nil match from position in MATCH-STR as if
by `string-match-p'.\n
:EXAMPLE\n\n
:SEE-ALSO .\n►►►"
  (assert (and (mon-string-not-null-nor-zerop match-str)
               (or (not match-from)
                   (and match-from (<= match-from (length match-from))))))
  (catch 'msmil-mtched
    (mapc #'(lambda (regexp)
              (and (string-match-p regexp match-str)
                   (throw 'msmil-mtched match-str)))
          regexp-lst)
    nil))

;;; ==============================
(provide 'mon-string-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-string-utils.el ends here
;;; EOF
