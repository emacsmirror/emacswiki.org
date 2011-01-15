;;; mon-line-utils.el --- line centric procedures for mon-*utils features
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-line-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-22T17:05:52-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-line-utils provides line centric procedures for mon-*utils features
;;
;; FUNCTIONS:►►►
;; `mon-line-length-max'
;; `mon-line-count-matchp'
;; `mon-line-count-buffer'
;; `mon-line-count-region'
;; `mon-line-find-duplicates'
;; `mon-line-find-duplicates-cln'
;; `mon-line-get-next'
;;
;; `mon-line-drop-in-words'
;; `mon-line-previous-duplicate'
;;
;; `mon-line-string-insert-chars-under'
;; `mon-line-string-rotate-namestrings-combine'
;; `mon-line-string-unrotate-namestrings'
;; `mon-line-string-rotate-namestrings'
;; `mon-line-string-rotate-name'
;; `mon-line-string-incr-padded'
;; `mon-line-string-split'
;; `mon-line-number-region-incr'
;; `mon-line-string-get' 
;;
;; `mon-line-strings-one-list'
;; `mon-line-strings-to-list'
;; `mon-line-strings-pipe-to-col'
;; `mon-line-indent-from-to-col'
;; `mon-line-strings-indent-to-col'
;; `mon-line-strings-pipe-bol'
;; `mon-line-strings-bq-qt-sym-bol'
;; `mon-line-strings-qt-region'
;; `mon-line-strings-region'
;; `mon-line-strings'
;; 
;; `mon-line-end-or-code-end'
;; `mon-line-eol-is-eob'
;; `mon-line-next-bol-is-eol'
;; `mon-line-previous-bol-is-eol'
;; `mon-line-bol-is-eol'
;;
;; `mon-spacep'
;; `mon-spacep-first'
;; `mon-spacep-at-eol'
;; `mon-spacep-is-after-eol-then-graphic'
;; `mon-spacep-is-after-eol'
;; `mon-spacep-is-after-bol'
;; `mon-spacep-not-bol'
;;
;; `mon-backspace'
;;
;; `mon-goto-line-25%'
;; `mon-goto-line-50%'
;; `mon-goto-line-75%'
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
;; `*mon-line-utils-xrefs*'
;;
;; GROUPS:
;; `mon-line-utils'
;;
;; ALIASED/ADVISED/SUBST'D:
;; :NOTE Aliases defined in :FILE mon-aliases.el
;;
;; <UNQUALIFIED-ALIAS>                  <PREFIX>-<NON-CORE-SYMBOL>
;; `goto-line-25%'                   -> `mon-goto-line-25%'
;; `goto-line-50%'                   -> `mon-goto-line-50%'
;; `goto-line-50%'                   -> `mon-goto-line-75%'
;;
;; <PREFIX>-<QUALIFIED>                               <CORE-SYMBOL>
;; `mon-line-keep-match'                           -> `keep-lines'
;; `mon-line-delete-match'                         -> `flush-lines'
;; `mon-line-count-match'                          -> `how-many'
;; `mon-line-join-previous'                        -> `delete-indentation'
;;
;; <PREFIX>-<QUALIFIED>                               <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-string-ify-current-line'                   -> `mon-line-string-split' 
;; `mon-cln-duplicate-lines'                       -> `mon-line-find-duplicates-cln'
;; `mon-remove-duplicate-lines'                    -> `mon-line-find-duplicates-cln'
;; `mon-region-increment-line-numbers'             -> `mon-line-number-region-incr'
;; `mon-region-increment-numbered-lines'           -> `mon-line-number-region-incr'
;; `mon-indent-lines-from-to-col'                  -> `mon-line-indent-from-to-col'
;; `mon-line-same-p'                               -> `slime-same-line-p'
;; 
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-string-split-line'                         ->  `mon-line-string-get'
;; `mon-string-incr-padded'                        ->  `mon-line-string-incr-padded'
;; `mon-pipe-list'                                 ->  `mon-line-pipe-lines'
;;
;; MOVED:
;; `mon-line-number-region-incr'                    <- mon-replacement-utils.el
;; `mon-pipe-list'                                  <- mon-replacement-utils.el
;; `mon-line-find-duplicates-cln'                   <- mon-replacement-utils.el
;; `mon-line-string-incr-padded'                    <- mon-insertion-utils.el
;; `mon-line-strings-bq-qt-sym-bol'                 <- mon-utils.el
;; `mon-line-strings-qt-region'                     <- mon-utils.el
;; `mon-line-strings-indent-to-col'                 <- mon-utils.el
;; `mon-line-indent-from-to-col'                    <- mon-utils.el
;; `mon-line-strings-pipe-bol'                      <- mon-utils.el
;; `mon-line-strings-pipe-to-col'                   <- mon-utils.el
;; `mon-line-strings'                               <- mon-utils.el
;; `mon-line-strings-region'                        <- mon-utils.el
;; `mon-line-string-insert-chars-under'             <- mon-utils.el
;; `mon-line-strings-one-list'                      <- mon-utils.el
;; `mon-line-strings-to-list'                       <- mon-utils.el
;; `mon-line-string-rotate-name'                    <- mon-utils.el
;; `mon-line-string-unrotate-namestrings'           <- mon-utils.el
;; `mon-line-string-rotate-namestrings'             <- mon-utils.el
;; `mon-line-string-rotate-namestrings-combine'     <- mon-utils.el
;; `mon-line-count-region'                          <- mon-utils.el
;; `mon-line-count-matchp'                          <- mon-utils.el
;; `mon-line-length-max'                            <- mon-utils.el
;; `mon-line-count-buffer'                          <- mon-utils.el
;; `mon-line-find-duplicates'                       <- mon-utils.el
;; `mon-spacep'                                     <- mon-utils.el
;; `mon-spacep-not-bol'                             <- mon-utils.el
;; `mon-spacep-is-bol'                              <- mon-utils.el
;; `mon-spacep-is-after-eol'                        <- mon-utils.el
;; `mon-spacep-is-after-eol-then-graphic'           <- mon-utils.el
;; `mon-spacep-at-eol'                              <- mon-utils.el
;; `mon-spacep-first'                               <- mon-utils.el
;; `mon-line-bol-is-eol'                            <- mon-utils.el
;; `mon-line-previous-bol-is-eol'                   <- mon-utils.el
;; `mon-line-next-bol-is-eol'                       <- mon-utils.el
;; `mon-line-eol-is-eob'                            <- mon-utils.el
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
;; URL: http://www.emacswiki.org/emacs/mon-line-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-25T02:27:00-05:00Z}#{10476} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-line-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-22T17:05:52-05:00Z}#{10471} - by MON KEY>
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
;;; :CREATED <Timestamp: #{2011-01-11T18:53:00-05:00Z}#{11022} - by MON KEY>
(defgroup mon-line-utils nil
  "Customization group for variables and functions of :FILE mon-line-utils.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE" "http://www.emacswiki.org/emacs/mon-line-utils.el")
  :link '(emacs-library-link "mon-line-utils.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T18:52:06-05:00Z}#{11022} - by MON KEY>
(defcustom *mon-line-utils-xrefs*
  '(mon-line-get-next mon-line-find-duplicates mon-line-find-duplicates-cln
    mon-line-previous-duplicate mon-line-count-region mon-line-count-buffer
    mon-line-count-matchp mon-line-length-max mon-line-strings
    mon-line-strings-region mon-line-strings-qt-region
    mon-line-strings-bq-qt-sym-bol mon-line-strings-pipe-bol
    mon-line-strings-indent-to-col mon-line-indent-from-to-col
    mon-line-strings-pipe-to-col mon-line-pipe-lines mon-line-strings-to-list
    mon-line-strings-one-list mon-line-string-rotate-name
    mon-line-string-rotate-namestrings mon-line-string-unrotate-namestrings
    mon-line-string-rotate-namestrings-combine
    mon-line-string-insert-chars-under mon-line-drop-in-words
    mon-line-string-incr-padded mon-line-number-region-incr mon-line-string-get
    mon-line-string-split mon-spacep mon-spacep-not-bol mon-spacep-is-bol
    mon-spacep-is-after-eol mon-spacep-is-after-eol-then-graphic
    mon-spacep-at-eol mon-spacep-first mon-line-bol-is-eol
    mon-line-previous-bol-is-eol mon-line-next-bol-is-eol mon-line-eol-is-eob
    mon-line-end-or-code-end *mon-line-utils-xrefs*)
  "Xrefing list of `mon-*-<TYP>'symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-line-utils.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-buffer-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-line-utils
  :group 'mon-xrefs)

;;; ==============================
;;; :PREFIX "mlgn-"
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of May 27, 2001
;;; :WAS `get-next-line' -> `mon-line-get-next'
;;; :CHANGESET 1771 <Timestamp: #{2010-05-25T19:42:55-04:00Z}#{10212} - by MON KEY>
(defun mon-line-get-next ()
  "Return the next line in the buffer, leaving point following it.\n
Return nil at `end-of-buffer'.\n
:EXAMPLE\n\n(mon-line-get-next)\nBubba on a line.\n
\(save-excursion \(equal \(mon-line-get-next\) \"\"\)\)\n
:SEE-ALSO `mon-string-ify-current-line'.\n►►►"
  (let (mlgn-start)
    (setq mlgn-start (progn (forward-line 0) (point)))
    (forward-line 1)
    (if (eql mlgn-start (point))
	nil
      (progn (setq mlgn-start (point)) 
             ;; (buffer-substring-no-properties mlgn-start (progn (end-of-line) (point)))
             (mon-buffer-sub-no-prop mlgn-start (progn (end-of-line) (point)))))))
;;
;;; :TEST-ME (save-excursion (equal (mon-line-get-next) ""))

;;; ==============================
;;; :PREFIX "mlfd-"
;;; :CHANGESET 1708 <Timestamp: #{2010-04-12T16:41:21-04:00Z}#{10151} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-08T15:18:51-04:00Z}#{09372} - by MON>
(defun mon-line-find-duplicates (&optional insrtp intrp)
  "Locate adjacent duplicate lines in buffer.\n
Functions which find duplicate lines don't always sort lines.\n
Where lines of a file are presorted can be use to locate duplicates before
removing, i.e. situations of type: `uniquify-maybe'.\n
Can extend `find-duplicate-lines' by comparing its result list with one or more
list comparison procedures `set-difference', `union', `mon-intersection', etc.\n
:SEE-ALSO `mon-line-find-duplicates-cln', `mon-cln-uniq-lines',
`mon-cln-blank-lines', `mon-line-get-next', `uniq', `uniq-region'.\n►►►"
  (interactive "i\np")
  (let ((mlfd-pnt-mx (line-number-at-pos (mon-g2be 1 t)))
	mlfd-gthr)
    (save-excursion
      (while (< (line-number-at-pos) mlfd-pnt-mx) (= (forward-line) 0)
	   (let ((mlfd-this-line 
                  (mon-buffer-sub-no-prop  (line-beginning-position 1) (line-end-position 1)))
		 (mlfd-next-line 
                  (mon-buffer-sub-no-prop (line-beginning-position 2) (line-end-position 2))))
	     (when (equal mlfd-this-line mlfd-next-line)  
               (setq mlfd-gthr (cons mlfd-this-line mlfd-gthr))))))
    (setq mlfd-gthr (remove "" mlfd-gthr))
    (if (or insrtp intrp)
          (progn (newline) (princ mlfd-gthr (current-buffer)) (newline))
        mlfd-gthr)))

;;; ==============================
;;; :PREFIX "mlfdc-"
;;; :CHANGESET 1708
;;; :CREATED <Timestamp: #{2010-04-12T16:12:49-04:00Z}#{10151} - by MON KEY>
(defun mon-line-find-duplicates-cln (cln-from cln-to &optional insrtp intrp)
  "Remove duplicate lines CLN-FROM to CLN-TO return list of lines cleaned.\n
Upon return point is at CLN-FROM.\n
When called-interactively or INSRTP is non-nil insert the list of lines cleaned
before CLN-FROM.\n
:NOTE Procedure occurs inside of the `mon-toggle-restore-llm' macro so should be
safe to evaluate in `naf-mode' buffers.\n
:ALIASED-BY `mon-cln-duplicate-lines'
:ALIASED-BY `mon-remove-duplicate-lines'\n
:SEE-ALSO `mon-line-find-duplicates', `mon-cln-uniq-lines'.\n►►►"
  (interactive "r\ni\np")
  (mon-toggle-restore-llm nil
    (let ((mlfdc-rgn (mon-buffer-sub-no-prop cln-from cln-to))
          mlfdc-clnd)
      (with-temp-buffer 
        (save-excursion 
          (insert mlfdc-rgn)
          (mon-g2be -1)
          (setq mlfdc-clnd (mon-line-find-duplicates)))
        (dolist (mlfdc-D-1 mlfdc-clnd)
          (save-excursion 
            (mon-g2be -1)
            (while (search-forward-regexp (concat "^" mlfdc-D-1 "$") nil t)
              (let ((mlfdc-D-1-lcl-fnd `(,(match-beginning 0) . ,(match-end 0))))
                (when  (equal (mon-buffer-sub-no-prop (car mlfdc-D-1-lcl-fnd) (cdr mlfdc-D-1-lcl-fnd))
                              (mon-buffer-sub-no-prop (line-beginning-position 2) (line-end-position 2)))
                  (delete-region (car mlfdc-D-1-lcl-fnd) (cdr mlfdc-D-1-lcl-fnd))
                  (unless (eobp)
                    (when (eq (line-end-position) (line-beginning-position))
                      (delete-char 1))))))))
        (setq mlfdc-rgn (mon-buffer-sub-no-prop)))
      (save-excursion 
        (goto-char cln-from)
        (delete-region cln-from cln-to)
        (insert mlfdc-rgn))
      (if (or insrtp intrp)
          (save-excursion
            (princ (format ";;; :W-REGION :FROM %d :TO %d :REPLACED-DUPLICATES\n%s\n;;;\n"
                           cln-from cln-to mlfdc-clnd)(current-buffer)))
        mlfdc-clnd))))


;;; ==============================
;;; :SEE (URL `http://lists.gnu.org/archive/html/bug-gnu-emacs/2010-11/msg00622.html')
;;; :CHANGESET 2331
;;; :CREATED <Timestamp: #{2010-11-29T12:55:26-05:00Z}#{10481} - by MON KEY>
(defun mon-line-previous-duplicate (&optional keep-props insrtp intrp)
  "Return content of previous line.\n
When optional arg KEEP-PROPS is non-nil return value is as if by `buffer-substring'. 
Default is as if by `buffer-substring-no-properties'.\n
When optional arg INSRTP is non-nil or called-interactively, insert return value
at point. Does not move point.\n
:EXAMPLE\n
;; I'm a bubba on a line.
\(mon-with-inhibit-buffer-read-only \(mon-line-previous-duplicate\)\)\n
;; I'm another bubba on a line.
\(mon-with-inhibit-buffer-read-only \(mon-line-previous-duplicate nil t\)\)\n
;; Double Bubbas on a line.
\(mon-with-inhibit-buffer-read-only \(mon-line-previous-duplicate t t\)\)\n
;; I'm a Bubba on a line found interactively.
\(mon-with-inhibit-buffer-read-only 
    \(progn \(beginning-of-line -1\)
    \(apply #'mon-line-previous-duplicate '\(nil nil t\)\)\)\)\n
:SEE-ALSO `mon-line-get-next', `mon-line-string-insert-chars-under'.\n►►►"
  (interactive "*P\ni\np")
  (save-excursion
    (let ((mlpd-psns `(,(progn (forward-line -1) (point)) .
                       ,(progn (forward-line 1) (point)))))
      (set (or (and intrp  (quote intrp))
               (and insrtp (quote insrtp))
               (and (set (quote intrp) (quote insrtp)) (quote insrtp)))
           (or (and keep-props (buffer-substring  (car mlpd-psns) (cdr mlpd-psns)))
               (mon-buffer-sub-no-prop (car mlpd-psns) (cdr mlpd-psns)))))
    (or (and (not (eq intrp 'insrtp)) (stringp insrtp) (insert insrtp))
        (and intrp (stringp intrp) (insert intrp))
        insrtp)))
;;
;;; :TEST-ME (mon-line-previous-duplicate)                    
;;; :TEST-ME (mon-line-previous-duplicate nil t)              
;;; :TEST-ME (mon-line-previous-duplicate t t)                
;;; :TEST-ME (mon-line-previous-duplicate t t)                


;;; ==============================
(defun mon-line-count-region (start end)
  "Return a mini-buffer message with regions' number of lines and characters.\n
:SEE-ALSO `mon-line-count-buffer', `mon-word-count-chars-region',
`mon-word-count-region', `mon-word-count-analysis',
`mon-word-count-occurrences'.\n►►►"
  (interactive "r")
  (count-lines-region start end))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-01T11:05:33-05:00Z}#{10091} - by MON KEY>
(defun mon-line-count-buffer (&optional some-other-buffer intrp)
  "Return cons'd list of number of lines and chars in buffer.\n
Car of return value is number of lines cdr is the number of chars.\n
When optional arg SOME-OTHER-BUFFER is non-nil return line and char count for
that buffer. Default is current-buffer.\n
:EXAMPLE\n\n\(mon-line-count-buffer\)\n\n(apply 'mon-line-count-buffer nil '(t)\)\n
\(let \(\(mlcb-tmp-buf \(get-buffer-create \"*MLCB-TMP-BUF*\"\)\)
      \(mlcb-rnd-str \(random 1024\)\) 
      \(mlcb-rnd-len \(random 68\)\)
      mlcb-cnt\)
  \(unwind-protect
       \(progn
         \(with-current-buffer mlcb-tmp-buf
           \(dotimes \(r \(1- \(/ mlcb-rnd-str mlcb-rnd-len\)\)\)
             \(princ 
               \(concat \(make-string mlcb-rnd-len 32\) \"\\n\"\)
              \(current-buffer\)\)\)\)
         \(setq mlcb-cnt \(mon-line-count-buffer mlcb-tmp-buf\)\)\)
    \(kill-buffer mlcb-tmp-buf\)\)
  mlcb-cnt\)\n
:SEE-ALSO `mon-line-count-region', `mon-line-count-match',
`mon-line-count-matchp', `mon-word-count-analysis',
`mon-word-count-chars-region', `mon-word-count-occurrences',
`mon-word-count-region', `count-lines', `buffer-size',
`line-number-at-pos'.\n►►►"
  (interactive "i\np")
  (let (mlcb-cnt)
    (setq mlcb-cnt (if some-other-buffer
                   (with-current-buffer some-other-buffer
                     ;; :WAS  `(,(line-number-at-pos (buffer-end 1)) . ,(buffer-size)))
                     `(,(line-number-at-pos (mon-g2be 1 t)) . ,(buffer-size)))
                 `(,(line-number-at-pos (buffer-end 1)) . ,(buffer-size))))
    (if intrp (prin1 mlcb-cnt) mlcb-cnt)))
;;
;;; :TEST-ME (mon-line-count-buffer)
;;; :TEST-ME (apply 'mon-line-count-buffer nil '(t))

;;; ==============================
;;; :TODO The default value for BOL-CHAR-TEST needs to be refactored/extended.
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 04:42.13 PM - by MON KEY>
(defun mon-line-count-matchp (test-from line-count &optional bol-char-test intrp)
  "Return non-nil when number of lines in region is eq LINE-COUNT.\n
Arg TEST-FROM is a buffer pos to start counting from.\n
:SEE-ALSO `mon-word-count-chars-region', `mon-word-count-region',
`mon-line-count-buffer', `mon-word-count-analysis',
`mon-word-count-occurrences'.\n►►►"
  ;;(interactive "r")
  (save-excursion
    (let ((rg-start (line-number-at-pos test-from))
	  ;; Apparently when this was first written I decided that # was a good
	  ;; default BOL char to test for.
          (bct (if (and bol-char-test) 
                   bol-char-test 
                   35))  ;; (char-to-string 35) -> #
	  rg-end rg-diff)
      (progn
	(goto-char test-from)
	(line-move line-count)
	(cond ((eq (char-after (point)) bct)
	       (move-to-column 7))
	      ((eolp) (setq rg-end (line-number-at-pos (point))))))
      (setq rg-diff (- rg-end rg-start))
      (message (concat ":FUNCTION `mon-line-count-matchp' "
                       "-- line-count: %d matches: %S")
               rg-diff (eq rg-diff line-count))
      (eq rg-diff line-count))))

;;; ==============================
(defun mon-line-length-max (&optional intrp)
  "Return the maximum line length of the current buffer.\n
When called-interactively return message in mini-buffer:
\"The longest line in buffer `mon-utils.el' ends at column 115.\"\n
:SEE-ALSO `mon-line-count-buffer', `mon-region-length'.\n►►►"
  (interactive "p")
  (let ((max-len 0))
    (save-excursion
      (mon-g2be -1)
      (while (eq (forward-line) 0)
        (end-of-line)
        (when (> (current-column) max-len)
          (setq max-len (current-column)))))
    (if intrp  (message 
                (concat ":FUNCTION `mon-line-length-max' " 
                        "-- buffer: %s longest line ends at column: %d")
                (buffer-name (current-buffer)) max-len)
      max-len)))
;;
;;; :TEST-ME (mon-line-length-max)
;;; :TEST-ME (mon-line-length-max t)

;;; ==============================
;;; :PREFIX "mls-"
;;; :CREATED <Timestamp: #{2009-12-09T16:14:41-05:00Z}#{09503} - by MON>
(defun mon-line-strings (start end &optional insrtp intrp)
  "Return lines of region from START to END as strings.\n
Each line is replaced with a quoted string.
When called-interactively or INSRTP is non-nil replace region with strings and 
move point to START.\n
:EXAMPLE\n(mon-help-overlay-for-example 'mon-line-strings 5 'line)\n
►\nHassan-i Sabbah\nTristan and Iseult\nBroder Rusche
Pier Gerlofs Donia\nBöŏvarr Bjarki\n◄\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-strings-region',
`mon-line-strings-region-delimited',
`mon-line-strings-qt-region',`mon-line-drop-in-words',
`mon-string-ify-list',`mon-string-ify-current-line'.\n►►►"
  (interactive "r\ni\np")
  (let ((mls-str-beg (make-marker))
        (mls-str-end (make-marker))
        (mls-ln-str))
    (set-marker mls-str-beg start)
    (set-marker mls-str-end end)
    (setq mls-ln-str (mon-buffer-sub-no-prop mls-str-beg mls-str-end))
    (setq mls-ln-str
          (with-temp-buffer 
            (insert mls-ln-str)
            (mon-g2be -1)
            (while (search-forward-regexp "^\\(.*\\)$" nil t)
              (replace-match "\"\\1\""))
            (mon-buffer-sub-no-prop)))
    ;; (unwind-protect 
    (if (or insrtp intrp)
        (progn
          (delete-region mls-str-beg mls-str-end)
          (insert mls-ln-str)
          (goto-char mls-str-beg))
      mls-ln-str)
    ;; (set-marker mls-str-beg nil) (set-marker mls-str-end nil)
    ))
;;
;;; :TEST-ME
;;; (let ((legs)
;;;       (legb (1+ (search-forward-regexp "►")))
;;;       (lege (- (search-forward-regexp "◄") 2)))
;;;   (setq legs (mon-line-strings legb lege)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |►
;; |Hassan-i Sabbah
;; |Tristan and Iseult
;; |Broder Rusche
;; |Pier Gerlofs Donia
;; |Böŏvarr Bjarki
;; |◄
;; `----

;;; ==============================
;;; :PREFIX "mlsr-"
;;; :CREATED <Timestamp: #{2009-12-08T12:36:48-05:00Z}#{09502} - by MON>
(defun mon-line-strings-region (start end &optional insrtp intrp)
  "Return each line of region as a string followed by a `\n'.
When called-interactively or INSRTP is non-nil insert strings at point.
Does not move point.\n
Use with concat for formated indentation in source.\n
:EXAMPLE\n\(mon-help-overlay-for-example 'mon-line-strings-region 4 'line)\n
►\nI-will-be-a-string\nI too will be a string.\nMe as well.
More stringification here\n◄\n
:SEE-ALSO `mon-line-strings', `mon-line-strings-to-list',
`mon-line-strings-region-delimited', `mon-line-strings-bq-qt-sym-bol',
`mon-string-ify-list', `mon-string-ify-current-line', `mon-string-split-line',
`mon-line-drop-in-words', `mon-cln-up-colon'.\n►►►"
  (interactive "r\ni\np")
  (let ((mlsr-ln-rgn-beg (make-marker))
        (mlsr-ln-rgn-end (make-marker))
        mlsr-lns-qtd)
    (set-marker mlsr-ln-rgn-beg start)
    (set-marker mlsr-ln-rgn-end end)
    (setq mlsr-lns-qtd (mon-buffer-sub-no-prop mlsr-ln-rgn-beg mlsr-ln-rgn-end))
    (setq mlsr-lns-qtd 
          (with-temp-buffer 
            (insert mlsr-lns-qtd)
            (mon-g2be -1)
            (while (search-forward-regexp "^\\(.*\\)$" nil t)
              (replace-match "\"\\1\\\\n\"" t)) ;; Do not alter case.
            (mon-buffer-sub-no-prop)))
    ;; (unwind-protect
    (if (or insrtp intrp)
        (save-excursion 
          (delete-region mlsr-ln-rgn-beg mlsr-ln-rgn-end)
          (goto-char mlsr-ln-rgn-beg)
          (insert mlsr-lns-qtd))
        mlsr-lns-qtd)
    ;; (set-marker mlsr-ln-rgn-beg nil) (set-marker mlsr-ln-rgn-end nil))
    ))
;;
;;; :TEST-ME (mon-line-strings-region
;;;           (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |►
;; |I-will-be-a-string
;; |I too will be a string.
;; |Me as well.
;; |More stringification here
;; |◄
;; `----

;;; ==============================
;;; :PREFIX "mlsqr-"
;;; :CREATED <Timestamp: #{2009-10-23T16:16:47-04:00Z}#{09435} - by MON KEY>
(defun mon-line-strings-qt-region (start end &optional insrtp intrp)
  "Return symbols at each BOL in region wrapped in double-quotes `\"'.\n
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.\n
Line's symbol should be without trailing whitespace.\n
If whitespace is present at EOL it is destructively removed.\n
When following characters are at BOL no replacement is peformed on symbol:
  ;( ) ` ' \" Likewise, do not replace if \" or ' follows symbol.\n
:NOTE will not quote symbols containing whitespace.\n
:EXAMPLE\n\n\(mon-help-overlay-for-example 'mon-line-strings-qt-region 4 'line\)
\(princ (mon-line-strings-qt-region
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"◄\"\) 2\)\)\)
\n►\nI-will-be-a-string\n\"I-am-almost-a-string\nI-am-a-half-string\"
I-am-not-a-string'\n◄\n 
:SEE-ALSO `mon-line-strings-bq-qt-sym-bol', `mon-line-strings-pipe-bol',
`mon-line-strings-region-delimited', `mon-cln-up-colon',
`mon-line-strings',`mon-line-strings-indent-to-col', `mon-line-strings-to-list',
`mon-line-strings-region', `mon-string-ify-list', `mon-string-ify-current-line',
`mon-string-split-line', `mon-line-drop-in-words'.\n►►►"
  (interactive "r\ni\np")
  (let (mlsqr-rtn)
    (setq mlsqr-rtn (mon-buffer-sub-no-prop start end))
    (setq mlsqr-rtn
          (with-temp-buffer 
            (insert mlsqr-rtn)
            (delete-trailing-whitespace)
            (mon-g2be -1) ;; (goto-char (buffer-end 0))
            (while (not (= (line-end-position) (mon-g2be 1 t))) 
              (beginning-of-line)            
              (when ;; Use `looking-at-p' here instead?
                  (looking-at "^\\([^;`'()\"\\[:blank:]]\\)\\([\\[:graph:]]+[^\"']\\)$")
                (replace-match (concat "\"" (match-string-no-properties 0) "\"")))
              (forward-line 1)
              (when (and (= (line-end-position) (mon-g2be 1 t) )
                         (looking-at ;; use `looking-at-p' here instead?
                          "^\\([^;`'()\\[:blank:]]\\)\\([\\[:graph:]]+\\([^\"']\\)\\)$"))
                (replace-match (concat "\"" (match-string-no-properties 0) "\""))))
            ;; :WAs (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) ))
            (mon-buffer-sub-no-prop)))
    (if (or insrtp intrp)
        (save-excursion (delete-region start end) (insert mlsqr-rtn))
        mlsqr-rtn)))
;;
;;; :TEST-ME
;;; (princ (mon-line-strings-qt-region
;;;  (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2)))
;; ,---- :UNCOMMENT-TO-TEST first case should pass, the rest fail
;; |►
;; |I-will-be-a-string
;; |"I-am-almost-a-string
;; |I-am-a-half-string"
;; |I-am-not-a-string'
;; |►
;; `----

;;; ==============================
;;; :PREFIX "mlsbqsb-"
;;; :MODIFICATIONS <Timestamp: #{2009-10-23T18:04:19-04:00Z}#{09435} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-16T14:30:28-04:00Z}#{09425} - by MON KEY>
;;; Updated to find with trailing symbols or (and EOL (not WSP)).
;;; :CREATED <Timestamp: #{2009-10-06T14:45:00-04:00Z}#{09412} - by MON KEY>
(defun mon-line-strings-bq-qt-sym-bol (start end &optional insrtp intrp)
  "Return symbols at BOL in region START to END wrapped in backquote and quote.\n
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.\n
When following characters are at BOL no replacement is peformed on symbol:\n
^ ; , . ( ) < > ` ' # ► \| Likewise, do not replace if ' follows symbol.\n
:EXAMPLE\n\n(mon-line-strings-bq-qt-sym-bol
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"◄\"\) 2\)\)\n
►\ncall-next-method &rest replacement-args
call-next-method &rest replacement-args
`call-next-method &rest replacement-args
call-next-method' &rest replacement-args\n◄\n
\(mon-line-strings-bq-qt-sym-bol-TEST\)\n
:SEE-ALSO `mon-line-strings-bq-qt-sym-bol-TEST', `mon-line-strings',
`mon-line-strings-qt-region', `mon-cln-up-colon',
`mon-line-strings-region-delimited', `mon-line-strings-pipe-bol',
`mon-line-strings-indent-to-col', `mon-line-strings-to-list',
`mon-line-strings-region', `mon-string-ify-list', `mon-string-ify-current-line',
`mon-string-split-line', `mon-line-drop-in-words'.\n►►►"
  (interactive "r\ni\np")
  (let (mlsbqsb-rtn)
    (setq mlsbqsb-rtn ;; :WAS (buffer-substring-no-properties start end))
          (mon-buffer-sub-no-prop start end))
    (setq mlsbqsb-rtn
          (with-temp-buffer 
            (insert mlsbqsb-rtn)
            (mon-g2be -1) ;(goto-char (buffer-end 0))
            (while (not (= (line-end-position) (buffer-end 1)))
              (beginning-of-line)            
              (save-match-data
                (when (looking-at "^\\([^;,.()<>`'#► ]\\)[\\[:graph:]]+[^']$")
                  (replace-match (concat "`" (match-string-no-properties 0) "'"))))
              (forward-line 1)
              (save-match-data
                (when (and (= (line-end-position) (buffer-end 1))       
                           (looking-at "^\\([^;,.()<>`'#► ]\\)[\\[:graph:]]+[^' ]$"))
                  (replace-match  (concat "`" (match-string-no-properties 0) "'"))))
              (save-match-data
                (when (and (= (line-end-position) (buffer-end 1))
                           (looking-at "^\\([^;,.()<>`'#► ]\\)[\\[:graph:]]+[^' ]$"))
                  (replace-match  (concat "`" (match-string-no-properties 0) "'")))))
            (mon-g2be -1) ;; (goto-char (buffer-end 0))
            ;; :WAS            
            ;; (search-forward-regexp 
            ;;  "^\\([^;,.()<>`'#►\|\\[:blank:]][\\[:graph:]]+[^'\\[:blank:]]+\\)\\( \\)\\(.*\\)$" nil t) 
            ;;  (replace-match "`\\1'\\2\\3"))
            ;; :WAS "^\\([^;,.()<>`'#►\|\\[:blank:]]\\)\\([\\[:graph:]]+[^']\\)\\([^^']\\)\\([ ]\\{1,2\\}\\)\\(.*\\)$" nil t)
            ;;       (replace-match "`\\1\\2' \\5"))
            (while 
                (search-forward-regexp 
                 "^\\([^;,.()<>`'#►\|\\[:blank:]]\\)\\([\\[:graph:]]+[^']\\)\\([^^ ']\\)\\([ ]\\{1,2\\}\\)\\(.*\\)$" nil t)
              ;; ^^^^1^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^2^^^^^^^^^^^^^^^^^^^^^^^3^^^^^^^^^^4^^^^^^^^^^^^^^^^^5^^^^^
              (replace-match "`\\1\\2\\3' \\5"))
            ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) ))
            (mon-buffer-sub-no-prop)))
    (if (or insrtp intrp)
        (save-excursion (delete-region start end) (insert mlsbqsb-rtn))
        mlsbqsb-rtn)))


;;; ==============================
;;; :PREFIX "mlspb-"
;;; :CREATED <Timestamp: #{2009-12-02T11:58:40-05:00Z}#{09493} - by MON>
(defun mon-line-strings-pipe-bol (start end &optional insrtp intrp)
  "Return BOL in region START to END replaced with `| '.\n
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.\n
:EXAMPLE\n\(save-excursion\n \(mon-line-strings-pipe-bol
   \(1+ \(search-forward-regexp \"►\"\)\)
   \(- \(search-forward-regexp \"◄\"\) 2\)\)\)\n
►\n Craig Balding\n Emmanuel Bouillon\n Bernardo Damele Assumpcao Guimarase
 Jean-Paul Fizaine\n Rob Havelt\n Chris Wysopal\n◄\n 
:SEE-ALSO `mon-line-strings-pipe-to-col', `mon-line-strings-bq-qt-sym-bol', 
`mon-line-strings', `mon-line-strings-qt-region',  `mon-line-strings-region', 
`mon-line-strings-indent-to-col', `mon-line-strings-to-list'.\n►►►"
  (interactive "r\ni\np")
  (let ((mlspb-rgn-beg (make-marker))
        (mlspb-rgn-end (make-marker))
        mlspb-rplc)
    (set-marker mlspb-rgn-beg start)
    (set-marker mlspb-rgn-end end)
    (setq mlspb-rplc ;; :WAS (buffer-substring-no-properties mlspb-rgn-beg mlspb-rgn-end))
          (mon-buffer-sub-no-prop mlspb-rgn-end))
    (setq mlspb-rplc
          (with-temp-buffer 
            (insert mlspb-rplc)
            (mon-g2be -1) ;; (goto-char (buffer-end 0))
            (while (search-forward-regexp "^\\(.*\\)$" nil t)
              (replace-match "| \\1"))
            ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) ))
            (mon-buffer-sub-no-prop)))
      (if (or insrtp intrp)
          (save-excursion
            (delete-region mlspb-rgn-beg mlspb-rgn-end)
            (insert mlspb-rplc))
          mlspb-rplc)))
;;
;;; :TEST-ME (save-excursion (mon-line-strings-pipe-bol
;;;          (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2)))
;;
;; ,---- :UNCOMMENT-TO-TEST
;; |►
;; | Craig Balding
;; | Emmanuel Bouillon
;; | Bernardo Damele Assumpcao Guimarase
;; | Jean-Paul Fizaine
;; | Rob Havelt
;; | Chris Wysopal
;; |◄
;; `----

;;; ==============================
;;; :PREFIX "mlsitc-"
;;; :CREATED <Timestamp: #{2009-12-02T11:58:40-05:00Z}#{09493} - by MON>
(defun mon-line-strings-indent-to-col (start end col-num &optional insrtp intrp)
  "Return region lines indented to column number COL-NUM.\n
When called-interactively with non-nil prefix arg COL return region indented to
col-numumn number. When prefix arg is nil prompt for COL-NUM.\n
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.\n
:NOTE following example used in conjunction with `mon-line-strings-pipe-bol'.\n
:EXAMPLE\n\(let \(\(rs \(1+ \(search-forward-regexp \"►\"\)\)\)
      \(re \(- \(search-forward-regexp \"◄\"\) 2\)\)\n      \(tmp\)\)
  \(setq tmp \(buffer-substring-no-properties rs re\)\)
  \(setq tmp \(with-temp-buffer \n              \(insert tmp\)
              \(mon-line-strings-pipe-bol \(buffer-end 0\) \(buffer-end 1\) t\)
              \(mon-line-strings-indent-to-col \(buffer-end 0\) \(buffer-end 1\) 7 t\)
              \(buffer-substring-no-properties \(buffer-end 0\) \(buffer-end 1\)\)\)\)
  tmp\)\n\n►\nCraig Balding\nEmmanuel Bouillon\nBernardo Damele Assumpcao Guimaraes
Jean-Paul Fizaine\nRob Havelt\nChris Wysopal\n◄\n
:SEE-ALSO `mon-line-indent-from-to-col', `mon-line-strings-pipe-to-col',
`mon-comment-divider->col', `mon-comment-lisp-to-col',
`mon-line-strings', `mon-string-fill-to-col',
`mon-line-strings-qt-region', `mon-line-strings-region', 
`mon-line-strings-bq-qt-sym-bol',`mon-line-strings-to-list'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((mlsitc-col-n (if (and intrp (not col-num))
                  (read-number "Indent to column number: ")
                  col-num))
        (mlsitc-rgn-beg (make-marker))
        (mlsitc-rgn-end (make-marker))
        mlsitc-rplc)
    (set-marker mlsitc-rgn-beg start)
    (set-marker mlsitc-rgn-end end)
    (setq mlsitc-rplc ;; :WAS (buffer-substring-no-properties mlsitc-rgn-beg mlsitc-rgn-end))
          (mon-buffer-sub-no-prop  mlsitc-rgn-end))
    (setq mlsitc-rplc
          (with-temp-buffer 
            (insert mlsitc-rplc)
            (mon-g2be -1) ;; (goto-char (buffer-end 0))
            (while (not (mon-line-eol-is-eob))
              (indent-line-to mlsitc-col-n)
              (line-move 1 t)
              (when (mon-line-eol-is-eob)
                (indent-line-to mlsitc-col-n)))
            ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) ))
            (mon-buffer-sub-no-prop)))
    (if (or insrtp intrp)
        (save-excursion
          (delete-region mlsitc-rgn-beg mlsitc-rgn-end)
          (insert mlsitc-rplc))
        mlsitc-rplc)))
;;
;;; :TEST-ME 
;;; (save-excursion 
;;;   (let ((rs (1+ (search-forward-regexp "►")))
;;;         (re (- (search-forward-regexp "◄") 2)))
;;;     (goto-char rs)
;;;     (mon-line-strings-pipe-bol rs re t)
;;;     (goto-char rs)
;;;     (mon-line-strings-indent-to-col rs re 7 t)))
;;
;; ,---- :UNCOMMENT-TO-TEST
;; ,►
;; |Craig Balding
;; |Emmanuel Bouillon
;; |Bernardo Damele Assumpcao Guimaraes
;; |Jean-Paul Fizaine
;; |Rob Havelt
;; |Chris Wysopal
;; |◄
;; `----

;;; ==============================
;;; :PREFIX "mliftc-"
;;; :NOTE Does not work for one line regions.
;;; :CREATED <Timestamp: #{2009-12-08T18:12:32-05:00Z}#{09502} - by MON>
(defun mon-line-indent-from-to-col (from-col to-col start end &optional intrp)
  "Indent to column starting FROM-COL identing TO-COL in region START to END.\n
When called-interactively prompt for column numer of FROM-COL and TO-COL.\n
:EXAMPLE\n
\(mon-with-inhibit-buffer-read-only
    \(let \(\(st-pnt \(make-marker\)\)
          \(nd-pnt \(make-marker\)\)
          \(fndr  #'\(lambda \(y\) \(search-forward-regexp y nil t\)\)\)\)
      \(save-excursion
        \(set-marker st-pnt \(funcall fndr \"^►\"\)\)
        \(set-marker nd-pnt \(funcall fndr \"◄$\"\)\)
        \(goto-char st-pnt\)
        \(mon-line-indent-from-to-col 24 32 st-pnt nd-pnt\)
        \(goto-char st-pnt\)
        \(mon-line-indent-from-to-col 46 58 st-pnt nd-pnt\)
        \(set-marker st-pnt nil\)
        \(set-marker nd-pnt nil\)\)\)\)\n
►emacsen.auto_apart     001           001
emacsen.rug_compat_42   00            00
emacsen.rug_compt_adorn 00            00       
emacsen.cache_empire    080           080      
emacsen.hashdelimiter   no-hash       no-hash
emacsen.rookie_romain   no value      no value◄\n
\(mon-line-indent-from-to-col-TEST\)\n
:NOTE Does not work for one line regions.\n
:SEE :FILE align.el for alternative approaches.\n
:ALIASED-BY `mon-indent-lines-from-to-col'\n
:SEE-ALSO `mon-line-strings-indent-to-col', `mon-line-strings-pipe-to-col',
`mon-string-fill-to-col', `mon-comment-divider->col', `mon-comment-lisp-to-col'.\n►►►"
  (interactive "i\ni\ni\ni\np")
  (let ((mliftc-frm-c (cond (from-col from-col)
                            ((or intrp t)
                             (read-number 
                              (concat ":FUNCTION `mon-line-indent-from-to-col' "
                                      "-- col to start from: ")
                              (car (posn-actual-col-row (posn-at-point)))))))
        (mliftc-to-c (cond (to-col to-col)
                           ((or intrp t)
                            (read-number (concat ":FUNCTION `mon-line-indent-from-to-col' "
                                                 "-- col to indent to: ")
                                         (car (posn-actual-col-row (posn-at-point)))))))
        (mliftc-beg-c (cond (start start)
                            ((region-active-p) (region-beginning))))
        (mliftc-end-c (cond (end end)
                            ((region-active-p) (region-end))))
        (mliftc-mrk-beg  (make-marker))
        (mliftc-mrk-end  (make-marker))
        (indent-wrk t))
    (set-marker mliftc-mrk-beg mliftc-beg-c)
    (set-marker mliftc-mrk-end mliftc-end-c)
    (progn 
      (goto-char mliftc-beg-c)
      ;; (line-move -1) ;; :TODO Add logic for the single lined region.
      (beginning-of-line))
    (while indent-wrk
      (move-to-column mliftc-frm-c)
      (indent-to-column mliftc-to-c)
      (cond ((< (line-number-at-pos (line-end-position 2)) (line-number-at-pos mliftc-mrk-end))
             (prog1  
                 (line-move 1 t) 
               (beginning-of-line)))
            ((>= (line-number-at-pos (line-end-position 2)) (line-number-at-pos mliftc-mrk-end))
             (progn  
               (line-move 1 t)
               (beginning-of-line) 
               (move-to-column mliftc-frm-c)
               (indent-to-column mliftc-to-c)
               (setq indent-wrk nil)))))))
;;
;;; :TEST-ME (mon-line-indent-from-to-col-TEST)

;;; ==============================
;;; :PREFIX "mlsptc-"
;;; :NOTE (length "=> TO-COLM-NUM-19-!")
;;; :CREATED <Timestamp: #{2009-12-09T15:07:13-05:00Z}#{09503} - by MON>
(defun mon-line-strings-pipe-to-col (start end &optional to-col insrtp intrp)
  "Return region's BOL piped and indented to column number.\n
When TO-COL is non-nil return region indented TO-COL, default column number 7.
When called-interactively or INSRTP is non-nil replace region.\n
:EXAMPLE\n\n\(let \(\(reb \(1+ \(search-forward-regexp \"►\"\)\)\)
      \(ree \(- \(search-forward-regexp \"◄\"\) 2\)\)\)
  \(momentary-string-display
   \(concat \"\\n\\n=> TO-THE-19th-COL-!\\n\\n\"
           \(mon-line-strings-pipe-to-col reb ree 19\)
           \"\\n\\n... and beyond ... :\)\\n\"\) \(point\)\)\)\n
\(mon-help-overlay-for-example 'mon-line-strings-pipe-to-col nil 'region 28\)\n
►\nWilliam Gibson\nBruce Sterling\nDan Brown\nNeal Stephenson\nLoyd Blankenship
Erik Gordon Corley\n◄\n
\(mon-line-strings-pipe-to-col-TEST\)\n
:SEE :FILE align.el for alternative approaches.\n
:SEE-ALSO `mon-line-strings-pipe-to-col-TEST', `mon-line-strings-pipe-bol',
`mon-line-strings-indent-to-col', `mon-line-strings',
`mon-line-indent-from-to-col', `mon-comment-divider->col',
`mon-comment-lisp-to-col'.\n►►►"
  (interactive "i\n\i\nP\ni\np")
  (let  ((mlsptc-rgn-beg (make-marker))
         (mlsptc-rgn-end (make-marker))
         mlsptc-tmp-pipe)
    (set-marker mlsptc-rgn-beg (cond (intrp (region-beginning)) ;; use-region-p
                                     (start start)))
    (set-marker mlsptc-rgn-end (cond (intrp (region-end)) ;; use-region-p
                                     (end end)))
    (setq mlsptc-tmp-pipe (mon-buffer-sub-no-prop mlsptc-rgn-beg mlsptc-rgn-end))
    (setq mlsptc-tmp-pipe 
          (with-temp-buffer 
            (insert mlsptc-tmp-pipe)
            (mon-line-strings-pipe-bol (mon-g2be -1 t) (mon-g2be  1 t) t)
            (mon-line-strings-indent-to-col (mon-g2be -1 t) (mon-g2be  1 t)
                                            (if to-col to-col 7) t)
            (untabify (mon-g2be -1 t) (mon-g2be  1 t))
            (mon-buffer-sub-no-prop)))
    (prog1 
        (if (or insrtp intrp)
            (save-excursion 
              (delete-region mlsptc-rgn-beg mlsptc-rgn-end)
              (mon-g2be mlsptc-rgn-beg)
              (insert mlsptc-tmp-pipe))
          mlsptc-tmp-pipe)
      (set-marker mlsptc-rgn-beg nil)
      (set-marker mlsptc-rgn-end nil))))
;;
;;; :TEST-ME (mon-line-strings-pipe-to-col-TEST)

;;; ==============================
;;; :PREFIX "mpl-"
;;; :CREATED <Timestamp: Wednesday February 11, 2009 @ 04:34.31 PM - by MON KEY>
(defun mon-line-pipe-lines (start end  &optional intrp)
  "Insert \" | \" between each item on an item per line region.\n
Useful for building piped lists in sections of `naf-mode' .naf files including:
  Used-for: Appeared-in: Ads-for: Artist-associated: Authors-associated:
  Products-associated: Slogans: Content-and-subjects: etc.\n
:NOTE Item on last line in region should be an empty line.\n
:SEE-ALSO `mon-cln-piped-list',`mon-line-strings-pipe-bol',
`mon-line-strings-pipe-to-col', `naf-backup-the-list',
`mon-delete-back-up-list'.\n►►►"
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mon-g2be 1)
      (progn
	(unless (and (mon-g2be 1 t) (mon-spacep))
	  (newline))
	(while (mon-spacep)
	  (let* ((mpl-frwd-cnt (skip-chars-forward "[:space:]")) ;; is this used?
		 (mpl-bkwd-cnt (skip-chars-backward "[:space:]"))
		 (mpl-empt (and (eolp) (bolp))))
	    (when mpl-empt
	      (delete-backward-char 1))
	    (when (< mpl-bkwd-cnt 0)
	      (let* ((mpl-cnt-bak (abs mpl-bkwd-cnt)))
		(delete-char mpl-cnt-bak)))
            ;; Test for abutting characters.
	    (if (and (not (mon-spacep)) (not (mon-spacep nil t)))
		(progn
		  (insert " | ")
		  (beginning-of-line)))))
	(when (and (mon-spacep) (bolp) (not (mon-spacep nil t)))
	  (progn
	    (backward-char 1)
	    (if (eolp)
		(delete-char 1)
	      (while (mon-spacep nil t)
		(delete-char 1)))
            ;; Test for abutting characters.
	    (when (and (not (mon-spacep)) (not (mon-spacep nil t)))
	      (progn
		(insert " | ")
		(beginning-of-line))))))
      (mon-g2be 1)
      ;; Matches trailing " | " on tail of returned piped-list.
      (progn		   
	(re-search-backward "[[:space:]]|[[:space:]]$" nil t)
	(replace-match ""))))
  (when intrp (message 
               (concat ":FUNCTION `mon-line-pipe-lines' "
                       "-- finished piping that list"))))
;;
;;(defalias ' ' )

(declare-function mon-cln-trail-whitespace "mon-replacement-utils" ())
;;; ==============================
;;; :PREFIX "mlstl-"
;;; :CREATED <Timestamp: #{2009-09-13T09:30:42-04:00Z}#{09377} - by MON>
(defun mon-line-strings-to-list (start end &optional w-cdr w-wrap insrtp intrp)
  "Return region's lines as list, each list elt contains string content of line.\n
Region between START END should be passed as a line per string/symbol.\n
Strips trailing whitespace. Does not preseve tabs converts them to spaces.\n
When W-CDR is non-nil or called-interactively with prefix-arg return each
element of list with an empty string as cdr.\n
Optional arg W-WRAP is a string to put in teh car position of returned list.
Default is:\n\n  \";; defvar defconst let let* setq\"\n
:EXAMPLE\n
Mon Key\nMON\nMon\nMON KEY\n\n;; When W-CDR nil:
=>\((\"Mon Key\"\)\n   \(\"MON\"\)\n   \(\"Mon\"\)\n   \(\"MON KEY\"\)\)\n
;; When W-CDR non-nil:\n=>\(\(\"Mon Key\" \"\"\)\n   \(\"MON\" \"\"\)
   (\"Mon\" \"\"\)\n   \(\"MON KEY\" \"\"\)\)\n
\(mon-line-strings-to-list-TEST t nil\)\n
\(mon-line-strings-to-list-TEST\)\n
:SEE-ALSO `mon-line-strings-to-list-TEST', `mon-line-strings-one-list',
`mon-line-strings-region-delimited', `mon-line-string-rotate-name',
`mon-line-string-rotate-namestrings', `mon-line-string-unrotate-namestrings',
`mon-line-string-rotate-namestrings-combine', `mon-make-lastname-firstname',
`mon-make-name-lispy', `mon-make-names-list', `mon-string-ify-current-line',
`mon-line-strings-qt-region', `mon-string-ify-list', `mon-string-split-line',
`mon-line-strings', `mon-line-strings-region', `mon-line-drop-in-words'.\n►►►"
  ;; (<REGION> &optional <W-CDR> <W-WRAP> <INSRTP> <INTRP>)
  (interactive "r\ni\nP\ni\np") ;; (interactive "r\nP\ni\ni\np") make w-cdr the pref arg
  (let ((mlstl-rgn-beg start)
        (mlstl-rgn-end end)
        (w-wrap-hist '(";; defvar defconst let let* setq" 
                       "let " "let* " "setq " "setf "
                       "defvar " "defconst "))
        mlstl-rgn-lst)
    (when (and intrp w-wrap)       
      (setq w-wrap
            (read-from-minibuffer 
             (concat ":FUNCTION `mon-line-strings-to-list' " 
                     "-- string to wrap return value with: ") ;prompt
             ;; init  ;keymap  ;read   ;history         ;default
             nil    nil      nil      '(w-wrap-hist . 0))) ;; w-wrap-hist  
      ;; exited the mini-buffer empty handed 
      (unless (or (mon-string-or-null-and-zerop w-wrap)
                  (not history-add-new-input))
        (add-to-history minibuffer-history w-wrap)))
    ;; non interactive and w-wrap was non-nil but not string
    (when (and w-wrap (not intrp)) 
      (setq w-wrap
            (or (and (stringp w-wrap) w-wrap)
                ;; Maybe should signal here instead?
                (and (not (stringp w-wrap)) (car w-wrap-hist)))))
    (setq mlstl-rgn-lst (mon-buffer-sub-no-prop mlstl-rgn-beg mlstl-rgn-end))
    (save-excursion
      (setq mlstl-rgn-lst (with-temp-buffer
                            (insert mlstl-rgn-lst) 
                            (untabify (point-min) (point-max))
                            (mon-cln-trail-whitespace) ;; (point-min) (point-max))
                            (mon-g2be -1) ;; (goto-char (point-min))
                            (while (search-forward-regexp "^\\(.*\\)$" nil t)
                              (if w-cdr 
                                  (replace-match "(\"\\1\" \"\")" t)
                                (replace-match "(\"\\1\")" t)))
                            (mon-g2be 1)
                            (if w-wrap (insert "))") (insert ")"))
                            (mon-g2be -1)
                            (if w-wrap
                                (save-excursion 
                                  ;; ";; defvar defconst let let* setq"
                                  (insert "(" w-wrap "\n'(")
                                  ;; (when (or intrp insrtp)
                                  ;;   (with-syntax-table emacs-lisp-mode-syntax-table (indent-pp-sexp 1)))
                                  )
                              (insert "("))
                            (mon-buffer-sub-no-prop))))
    (if (or insrtp intrp)
        (progn
          (save-excursion 
            (delete-region mlstl-rgn-beg mlstl-rgn-end)
            (insert mlstl-rgn-lst))
          (with-syntax-table emacs-lisp-mode-syntax-table (indent-pp-sexp 1)))
      mlstl-rgn-lst)))

;;; ==============================
;;; :PREFIX "mlsol-"
;;; :CREATED <Timestamp: #{2010-01-17T12:48:16-05:00Z}#{10027} - by MON>
(defun mon-line-strings-one-list (start end &optional insrtp intrp)
  "Return lines in region from START to END as a list of strings.\n
When optional arg insrtp is non-nil or called-interactively replace region with
the list. Does not move point.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-strings',
`mon-line-strings-region-delimited', `mon-string-ify-list'.\n►►►"
  (interactive "r\ni\np")
  (let ((mlsol-lst (mon-buffer-sub-no-prop start end)))
    ;; :WAS (let (mlsol-lst) (setq mlsol-lst (mon-buffer-sub-no-prop start end))
    (with-temp-buffer      
      (insert mlsol-lst)
      (setq mlsol-lst (mon-line-strings (buffer-end 0) (buffer-end 1))))
    (if (or insrtp intrp)
        (save-excursion 
          (goto-char start)
          (delete-region start end)
          ;; :NOTE `prin1' is probably what is wanted here but the existing caller
          ;; `mon-word-get-list-in-buffer' appears to rely on (and work OK) with `princ'
          ;; consider adding an optional arg?
          (princ (list mlsol-lst) (current-buffer)))
      mlsol-lst)))

;;; ==============================
;;; :PREFIX "mlsrn-"
;;; :CREATED <Timestamp: #{2009-09-19T13:53:29-04:00Z}#{09386} - by MON>
(defun mon-line-string-rotate-name (name-str-or-elt &optional as-list)
  "Rotate the namestring NAME-STR-OR-ELT.\n
Return the last whitespace delimited name in string at head top of string.\n
Remaining names in string returned inside a parenthetical group.\n
NAME-STR-OR-ELT is a string containing one nameform or one elt listsame 
holding a string containing one nameform.\n
:EXAMPLE\n\(mon-line-string-rotate-name \"István Tisza\")\n
\(mon-line-string-rotate-name '\(\"Stanisław Marcin Ulam\")\)\n
\(mon-line-string-rotate-name '\(\"Dmitri Pavlovich Romanov\")\)\n
\(mapconcat #'\(lambda \(x\) \(mon-line-string-rotate-name x\)\)
           '\(\(\"George Charles Aid\"\)\(\"Thomas Pollock Anshutz\"\)
             \(\"Cecilia Beaux\"\)\(\"Frank Weston Benson\"\)
             \(\"Thomas Hart Benton\"\)\(\"Saul Bernstein\"\)
             \(\"George Biddle\"\)\(\"Gutzon Borglum\"\)\)
           \"\\n\"\)\n
\(mon-line-string-rotate-name-TEST\)\n
:SEE-ALSO `mon-line-strings', `mon-line-strings-to-list',
`mon-line-string-rotate-namestrings', `mon-line-string-unrotate-namestrings',
`mon-line-string-rotate-namestrings-combine', `mon-make-lastname-firstname',
`mon-make-name-lispy', `mon-make-names-list', `mon-line-strings-region',
`mon-line-string-rotate-name-TEST'.\n►►►"
  (let* ((mlsrn-nm-elt (if (atom name-str-or-elt)
                        name-str-or-elt
                      (let ((mlsrn-hd name-str-or-elt))
                        (while (consp mlsrn-hd)
                          (setq mlsrn-hd (car mlsrn-hd)))
                        mlsrn-hd)))
         (mlsrn-splt (split-string mlsrn-nm-elt))
         (mlsrn-splt-len (length mlsrn-splt))
         (mlsrn-last-in (cond ((= mlsrn-splt-len 1) (format "%s" (car mlsrn-splt)))
                        ((> mlsrn-splt-len 1) 
                         (let ((rot-split 
                                (append (edmacro-subseq mlsrn-splt -1)
                                        (edmacro-subseq mlsrn-splt 0 (1- mlsrn-splt-len)))))
                           (format "%s %s" (car rot-split) (cdr rot-split))))
                        ((= mlsrn-splt-len 0) nil))))
    (if as-list (list mlsrn-last-in) mlsrn-last-in)))
;;
;;; :TEST-ME (mon-line-string-rotate-name-TEST)

;;; ==============================
;;; :PREFIX "mlsrn-"
;;; :CREATED <Timestamp: #{2009-09-22T16:39:59-04:00Z}#{09392} - by MON KEY>
(defun mon-line-string-rotate-namestrings (start end &optional as-strings insrtp intrp)
  "Rotate namestrings in region.\n
Namestring are formatted one name per line Firstname Middlenames Lastname.\n
Return Lastname (Firstname Middlename).\n
When AS-STRINGS is non-nil retrun namestrings as strings as with prin1.\n
When INSRTP is non-nil or called-interactively insert rotated names at point.\n
Does not move point.\n
:SEE-ALSO `mon-line-string-unrotate-namestrings', `mon-line-string-rotate-name', 
`mon-line-string-rotate-namestrings-combine', `mon-line-strings-to-list',
`mon-make-lastname-firstname', `mon-make-names-list', `mon-make-name-lispy',
`mon-line-strings-region'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((mlsrn-rot-nm-beg start)
	(mlsrn-rot-nm-end  end)
	mlsrn-get-nm-strs)
    (setq mlsrn-get-nm-strs 
	  (mapconcat #'(lambda (mlsrn-L-1) 
                         (mon-line-string-rotate-name (car mlsrn-L-1))) 
     		     (read (mon-line-strings-to-list mlsrn-rot-nm-beg mlsrn-rot-nm-end)) "\n"))
    (if (or insrtp intrp)
        (progn
          (save-excursion
            (delete-region mlsrn-rot-nm-beg mlsrn-rot-nm-end)
            (if as-strings
                (mapc #'(lambda (mlsrn-L-2) 
                          (newline) 
                          (prin1 mlsrn-L-2 (current-buffer)))
                      (split-string mlsrn-get-nm-strs "\n"))
              (insert mlsrn-get-nm-strs)))
          (when as-strings (delete-char 1)))
      (if as-strings 
          (split-string mlsrn-get-nm-strs "\n") 
        mlsrn-get-nm-strs))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |
;; | (mon-line-string-rotate-namestrings 
;; |  (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2))
;; | (mon-line-string-rotate-namestrings 
;; |  (1+ (search-forward-regexp "◄")) (- (search-forward-regexp "◄") 2) t)
;; | 
;; |►
;; |George Charles Aid
;; |Thomas Pollock Anshutz
;; |Cecilia Beaux
;; |Frank Weston Benson
;; |Thomas Hart Benton
;; |Saul Bernstein
;; |George Biddle
;; |Gutzon Borglum
;; |◄
;; | 
;; `----

;; ==============================
;;; :PREFIX "mlsun-"
;;; :CREATED <Timestamp: #{2009-09-23T20:12:26-04:00Z}#{09394} - by MON KEY>
(defun mon-line-string-unrotate-namestrings (start end &optional as-strings insrtp intrp)
  "Unrotate namestrings in region.\n
Namestrings are formatted name per line e.g. `Lastname (Firstname Middlenames)'
Return `Firstname Middlename Lastname'
When INSRTP is non-nil or Called-interactively insert rotated names at point.
Does not move point. When AS-STRINGS is non-nil return rotated names as strings.\n
:EXAMPLE\n\(mon-line-string-unrotate-namestrings 
   (1+ \(search-forward-regexp \"►\"\)) \(- \(search-forward-regexp \"◄\"\) 2\)\)\n
►\nKennan (George Frost)\nAlbert (Lukács János)\nAchesonn (Dean Gooderham)
Harriman (William Averell)\nMcCloy (John Jay)\nBohlen (Charles Eustis)
Lovett (Robert Abercrombie)\n◄\n
:SEE-ALSO `mon-line-string-rotate-name', `mon-line-string-rotate-namestrings',
`mon-line-string-rotate-namestrings-combine', `mon-line-strings-to-list',
`mon-make-lastname-firstname', `mon-make-name-lispy', `mon-make-names-list',
`mon-line-strings-region'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((mlsun-rgn-beg start)
        (mlsun-rgn-end end)
        mlsun-go-tmp)
    (setq mlsun-go-tmp ;; :WAS (buffer-substring-no-properties mlsun-rgn-beg mlsun-rgn-end))
          (mon-buffer-sub-no-prop mlsun-rgn-beg mlsun-rgn-end))
    (save-excursion
      (setq mlsun-go-tmp
            (with-temp-buffer
              (insert mlsun-go-tmp)
              (whitespace-cleanup)
              (mon-g2be -1) ;; (goto-char (buffer-end 0))
              (while (search-forward-regexp  
                      "^\\([A-z-]+\\) \\((\\)\\(.*\\)\\()\\)$" (buffer-end 1) t)
                      ;;^^1^^^^^^^^^^^^^2^^^^^^^3^^^^^^4^^^^
                (replace-match  "\\3 \\1"))
              (if as-strings
                  (mon-line-strings-to-list (buffer-end 0) (buffer-end 1))
                ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) ))))
                (mon-buffer-sub-no-prop)))))
    (if (or insrtp intrp)
        (progn
          (save-excursion 
            (delete-region mlsun-rgn-beg mlsun-rgn-end)
            (if as-strings
                (let ((mlsun-as-str1 (read mlsun-go-tmp)))
                  (mapc #'(lambda (mlsun-L-1) 
                            (newline) 
                            (prin1 (car mlsun-L-1) (current-buffer)))
                        mlsun-as-str1))
              (insert mlsun-go-tmp)))
          (when as-strings (delete-char 1)))
      ;; elseif
      (if as-strings
          (let ((mlsun-as-str2 (read mlsun-go-tmp))
                mlsun-rtn-str)
            (setq mlsun-rtn-str (mapcar #'(lambda (mlsun-L-2) 
                                            (car mlsun-L-2))
                                        mlsun-as-str2))
            mlsun-rtn-str)
        mlsun-go-tmp))))
;;
;;,---- :UNCOMMENT-TO-TEST:
;;| (mon-line-string-unrotate-namestrings 
;;|   (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2))
;;| 
;;| ►
;;| George Frost Kennan
;;| Dean Gooderham Acheson
;;| William Averell Harriman
;;| Lukács János Albert 
;;| John Jay McCloy
;;| Charles Eustis Bohlen 
;;| Robert Abercrombie Lovett
;;| ◄
;;`----

;;; ==============================
;;; :PREFIX "mlsrnc-"
;;; :CREATED <Timestamp: #{2009-09-24T14:18:44-04:00Z}#{09394} - by MON>
(defun mon-line-string-rotate-namestrings-combine (start end &optional insrtp intrp)
  "Return lists of namestrings from START to END both rotated and normalalized.\n
Elements of list returned have the form:\n
 \(\"Fname Lname\" \"Lname \(Fname\)\"\)\n
:EXAMPLE\n\n\(mon-line-string-rotate-namestrings-combine
   (1+ \(search-forward-regexp \"►\"\)) \(- \(search-forward-regexp \"◄\"\) 2\)\)\n
►\nEmil Max Hödel\nJohn Wilkes Booth\nLeon Frank Czolgosz\nLee Harvey Oswald
Dmitry Grigoriyevich Bogrov\nPaul Gorguloff\nJohn Bellingham
Charles Julius Guiteau\n◄\n
:SEE-ALSO `mon-line-string-rotate-namestrings',
`mon-line-string-unrotate-namestrings', `mon-make-lastname-firstname',
`mon-make-name-lispy', `mon-make-names-list', `mon-line-string-rotate-name',
`mon-line-strings-to-list', `mon-line-string-insert-chars-under'.\n►►►"
  (interactive "r\ni\np")
  (let ((mlsrnc-rotd-nms (mon-line-string-rotate-namestrings start end t))
        mlsrnc-unrotd
        mlsrnc-cmbnd)
    (with-temp-buffer
      (progn
        (save-excursion
          (mapc #'(lambda (mlsrnc-L-1) 
                    (newline) 
                    (princ mlsrnc-L-1 (current-buffer)))
                mlsrnc-rotd-nms))
        (delete-char 1))
      (setq mlsrnc-unrotd
            (mon-line-string-unrotate-namestrings (point-min) (point-max) t)))
    (mapc #'(lambda (mlsrnc-L-2)
            (let ((mlsrnc-orig (pop mlsrnc-rotd-nms)))
              (setq mlsrnc-cmbnd (cons `(,mlsrnc-L-2 ,mlsrnc-orig) mlsrnc-cmbnd))))
          mlsrnc-unrotd)
    (if (or insrtp intrp)
        (prin1 mlsrnc-cmbnd (current-buffer))
        mlsrnc-cmbnd)))
;;
;; ,---- :UNCOMMENT-TO-TEST
;; | 
;; | (mon-line-string-rotate-namestrings-combine
;; |    (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2))
;; | 
;; | ►
;; | Emil Max Hödel
;; | John Wilkes Booth
;; | Leon Frank Czolgosz
;; | Lee Harvey Oswald
;; | Dmitry Grigoriyevich Bogrov
;; | Paul Gorguloff
;; | John Bellingham
;; | Charles Julius Guiteau
;; | ◄
;; `----

;;; ==============================
;;; :PREFIX "mlsicu-"
;;; CREATED: <Timestamp: #{2009-10-20T16:16:44-04:00Z}#{09432} - by MON>
(defun mon-line-string-insert-chars-under (&optional w-char intrp)
  "Insert a string of `='s \(char 61\) beneath the current line.\n
Inserted string has the length of current line. Does not move point.\n
When WITH-CHAR (char or string) is non-nil insert that char instead.\n
When called-interactively with prefix-arg prompt for a char to use.\n
:EXAMPLE\n
\(mon-with-inhibit-buffer-read-only \(mon-line-string-insert-chars-under\)\)\n
\(mon-with-inhibit-buffer-read-only \(mon-line-string-insert-chars-under 9658\)\)\n
\(mon-with-inhibit-buffer-read-only \(mon-line-string-insert-chars-under 9668\)\)\n
\(mon-with-inhibit-buffer-read-only \(mon-line-string-insert-chars-under \"►\"\)\)\n
\(mon-with-inhibit-buffer-read-only \(mon-line-string-insert-chars-under t t\)\)\n
\(mon-line-string-insert-chars-under-TEST\)\n
:SEE-ALSO `mon-line-previous-duplicate', `mon-line-get-next',
`mon-line-strings-to-list', `mon-line-strings-region-delimited'.\n►►►"
(interactive "P\np")
  (let ((mlsicu-ln-spec
         (save-match-data
           (if (looking-at-p "^$")
               (error (concat ":FUNCTION `mon-line-string-insert-chars-under' "
                              "-- no line at point: %d") (point))
             (bounds-of-thing-at-point 'line))))
        (w-char (if (and w-char intrp)
                    (read-char (concat ":FUNCTION `mon-line-string-insert-chars-under' "
                                       "-- char to use: "))
                  w-char)))
    (save-excursion
      (end-of-line)
      (when (= (mon-g2be 1 t)
               (cdr mlsicu-ln-spec))
        (setcdr mlsicu-ln-spec (1+ (cdr mlsicu-ln-spec))))
      (open-line 1)
      (forward-char 1)
      (insert (make-string ;;(- (1- (cdr mlsicu-ln-spec)) (car mlsicu-ln-spec)) 
               (- (1- (cdr mlsicu-ln-spec)) (car mlsicu-ln-spec))
               (if w-char                              
                   (if (stringp w-char)
                       (string-to-char w-char)
                     w-char)
                 61))))))
;;
;;; :TEST-ME (mon-line-string-insert-chars-under-TEST)

;;; ==============================
;;; :PREFIX "mldiw-"
;;; :MODIFICATIONS-OF Drew Adams' :HIS strings.el
;;; :CREATED <Timestamp: Thursday February 19, 2009 @ 06:31.47 PM - by MON KEY>
(defun mon-line-drop-in-words (&optional buffer-w-line split-on keep-nulls insrtp intrp)
  "Split current line of text in the buffer BUFFER-W-LINE into single words.\n
BUFFER-W-LINE names a buffer to get line from.\n
When optional arg SPLIT-ON is non-nil `split-string' with SPLIT-ON else defaults
to value of `split-string-default-separators'.\n
When optional arg KEEP-NULLS is non-nil keep zero length substrings.\n
The split line inserted with each word on a new line in `current-buffer'.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-string-ify-list',
`mon-insert-string-ify', `mon-line-string-get',
`mon-word-get-list-in-buffer'.\n►►►"
  (interactive "i\nP\ni\ni\np")
  (let ((mldiw-ln-strs 
         #'(lambda (mldiw-L-1)
             (let (mldiw-L-1-rtn)
               (cond ((and intrp split-on)
                      (save-match-data 
                        (setq mldiw-L-1-rtn
                              (split-string mldiw-L-1 (read-regexp 
                                                 (concat ":FUNCTION `mon-line-drop-in-words' "
                                                         "-- regexp to split with: "))
                                            (if keep-nulls nil t)))))
                     (t (setq mldiw-L-1-rtn
                              (save-match-data (split-string mldiw-L-1 split-on 
                                                             (if keep-nulls nil t))))))
               (setq mldiw-L-1-rtn (mapconcat #'identity mldiw-L-1-rtn "\n")))))
        (mldiw-in-bfr (cond ((and buffer-w-line (get-buffer buffer-w-line))
                       (get-buffer buffer-w-line))
                      ((and buffer-w-line (not (get-buffer buffer-w-line)))
                       (error (concat ":FUNCTION `mon-line-drop-in-words' "
                                      "-- arg BUFFER-W-LINE does not exist")))
                      (t (current-buffer))))
        (mldiw-w-insrt #'(lambda (mldiw-L-2-typ mldiw-L-2-insrt-str &optional mldiw-L-2-insrt-at)
                           (case mldiw-L-2-typ
                             ;; Interactive calls don't pass BUFFER.
                             ;; Safe to move point and insert in `current-buffer'.
                             (intrp (save-excursion 
                                      (goto-char (marker-position mldiw-L-2-insrt-at))
                                      (newline)
                                      (princ mldiw-L-2-insrt-str (current-buffer))))
                             ;; We don't know which buffer we're in so move point.
                             (insrtp (goto-char (or (marker-position mldiw-L-2-insrt-at) (line-end-position)))
                                     (princ mldiw-L-2-insrt-str (current-buffer))))))
        mldiw-rtn-ln-str)
    (with-current-buffer (get-buffer mldiw-in-bfr)
      ;; :NOTE `inhibit-field-text-motion' here if reqd. Default binding is nil.
      (setq mldiw-rtn-ln-str (mon-buffer-sub-no-prop (line-end-position) (line-beginning-position)))
      (setq mldiw-rtn-ln-str (funcall mldiw-ln-strs mldiw-rtn-ln-str)))
    (if (or insrtp intrp)
        ;; Check if `mon-naf-mode-toggle-restore-llm' is avaialbe in environement.
        ;; If we're in a `naf-mode' buffer we need to toggle `longlines-mode' first
        ;; We need the marker to make sure that insertion happens at ``soft'' newlines.
            (if (fboundp 'mon-naf-mode-toggle-restore-llm)
                (let (mrk-eol-myb)
                  (when intrp 
                    (setq mrk-eol-myb (make-marker))
                    (set-marker mrk-eol-myb (line-end-position)))
                  (mon-naf-mode-toggle-restore-llm 
                   nil
                   (funcall mldiw-w-insrt (if insrtp 'insrtp 'intrp) mldiw-rtn-ln-str mrk-eol-myb)))
              (funcall mldiw-w-insrt (if insrtp 'insrtp 'intrp) mldiw-rtn-ln-str))
            mldiw-rtn-ln-str)))

;;; ==============================
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
(defun mon-line-string-incr-padded (start-num end-num &optional padp insrtp intrp)
  "Return numbers from START-NUM to END-NUM \(inclusive\).\n
Each number is returned on a separate line.\n
START-NUM may be less than END-NUM, in which case counting is backward.\n
When PADP is non-nil or called-interactively with prefix arg, pad all numbers
with sufficient leading zeros so they are the same width.\n
When INSRTP is non-nil or called-interactively insert current-buffer.
Does not move point.\n
:EXAMPLE\n\n\(mon-line-string-incr-padded 88 120 1\)\n
:SEE-ALSO `mon-string-incr',`mon-line-number-region', `mon-line-number-region-incr', 
`mon-rectangle-sum-column', `mon-string-justify-left', `mon-line-indent-from-to-col'.\n►►►"
  (interactive "n:FUNCTION `mon-line-string-incr-padded' -- start-num: \nnend-num: \nP\ni\np")
  (let* ((msip-add-fncn (if (<= start-num end-num) '1+ '1-))
         (msip-cmp-fncn (if (<= start-num end-num) '<= '>=))
         (msip-incr start-num)
         (msip-fmt (and padp (format "%%.%dd"
                                (length (int-to-string (max (abs start-num)
                                                            (abs end-num)))))))
         msip-rtn)
    (setq msip-rtn    
          (with-temp-buffer
            (while (funcall msip-cmp-fncn msip-incr end-num)
              (insert (if msip-fmt 
                          (format msip-fmt msip-incr) 
                        (int-to-string msip-incr)) "\n")
              (setq msip-incr (funcall msip-add-fncn msip-incr)))
            (mon-buffer-sub-no-prop)))
    (if (or insrtp intrp)
        (save-excursion (newline) (insert msip-rtn))
        msip-rtn)))
;;
;;; :TEST-ME (mon-line-string-incr-padded 88 120 1)

;;; ==============================
;;; :PREFIX "mlnri-"
;;; :TODO Needs to take a step argument to adjust count-rep's increment on each pass.
;;; :RENAMED `mon-re-number-region' -> `mon-line-number-region-incr'
;;; :MODIFICATIONS <Timestamp: #{2010-01-26T20:16:13-05:00Z}#{10043} - by MON KEY>
;;; :CREATED <Timestamp: Saturday February 28, 2009 @ 02:25.53 PM - by MON KEY>
(defun mon-line-number-region-incr (start end &optional start-num intrp)
  "Sequentially renumber numbers (0-999 inclusive) in a region.\n
When called-interactively, prompt for starting number. Default is 0.\n
Useful for re-numbering out of sequence numbers in filenames.\n
:ALIASED-BY `mon-region-increment-line-numbers'
:ALIASED-BY `mon-region-increment-numbered-lines'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-string-incr',`mon-line-string-incr-padded', `mon-line-number-region',
`mon-rectangle-sum-column', `mon-line-count-region', `mon-line-count-matchp',
`mon-line-length-max', `mon-line-count-buffer',
`mon-line-find-duplicates'.\n►►►"
  (interactive "r\nP\np")
  (mon-toggle-restore-llm nil  
       (let ((mlnri-cnt-rep 
              (cond (start-num start-num)
                    ((and (not intrp) (not start-num)) 0)
                    (intrp (read-number 
                            (concat ":FUNCTION `mon-line-number-region-incr'" 
                                    " -- start from number: ") 0))))
             (mlnri-rgn-nums)
             (mlnri-rgn-strt start)
             (mlnri-rgn-end end))
         (setq mlnri-rgn-nums (mon-buffer-sub-no-prop mlnri-rgn-strt mlnri-rgn-end))
         (setq mlnri-rgn-nums
               (with-temp-buffer
                 (insert mlnri-rgn-nums)
                 (mon-g2be -1)
                 (while (search-forward-regexp "[0-9]\\{1,3\\}" nil t )
                   (replace-match
                    (number-to-string
                     (prog1
                         (identity mlnri-cnt-rep)
                       (setq mlnri-cnt-rep (1+ mlnri-cnt-rep))))))
                 (mon-buffer-sub-no-prop) ))
         (if intrp
             (save-excursion
               (delete-region mlnri-rgn-strt mlnri-rgn-end)
               (insert mlnri-rgn-nums))
           mlnri-rgn-nums))))

;;; ==============================
;;; :PREFIX "mssl-"
;;; :COURTESY Drew Adams :HIS strings.el
;;; :RENAMED `mon-split-string-line' -> `mon-line-string-get'
;;; :MODIFICATIONS <Timestamp: #{2009-09-23T18:49:22-04:00Z}#{09393} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2010-01-11T23:34:36-05:00Z}#{10021} - by MON>
(defun mon-line-string-get (&optional w-buffer insrtp intrp)
  "Return current line of text in W-BUFFER as a string.\n
When INSRTP is non-nil or called interactively insert return string at point.\n
Does not move-point.\n
:SEE-ALSO `mon-line-strings-qt-region', `mon-line-strings-to-list',
`mon-stringify-list', `mon-insert-string-ify', `mon-line-drop-in-words',
`mon-line-string-split',`mon-word-get-list-in-buffer',
`mon-string-replace-char'.\n►►►"
  (interactive "i\nb\np")
  (let (mssl-splt-str-s mssl-splt-str-e mssl-splt-str)  
    (save-excursion    
      (with-current-buffer (if w-buffer (get-buffer w-buffer) (current-buffer))
        (setq mssl-splt-str
              (mon-buffer-sub-no-prop 
               (progn (end-of-line 1) (setq mssl-splt-str-e (point)))
               (progn (beginning-of-line 1) (setq mssl-splt-str-s (point)))))))
    (if (or insrtp intrp)
        (if (not buffer-read-only)      
            (save-excursion (prin1 mssl-splt-str (current-buffer)))
          (prin1 mssl-splt-str))
      mssl-splt-str)))

;;; ==============================
;;; :PREFIX "mscl-"
;;; :MODIFICATIONS <Timestamp: #{2009-10-15T18:19:18-04:00Z}#{09424} - by MON KEY>
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 03:08.46 PM - by MON KEY>
(defun mon-line-string-split (&optional intrp split-on delim)
  "Return line at point as a list of strings.\n
When non-nil split-on is a string which should be split on.
When non-nil delim is a delimter to be concatenated to _front_ of each string. 
Called interacively kills current line replacing with string per-word
unless in an unreadable buffer where just retruns.
Neither SPLIT-ON nor DELIM have an effect when invoked interactively.\n
:EXAMPLE\n
\(mon-line-string-split\) split me to a list of strings\n
\(mon-line-string-split nil \"s\" \"S\"\) split me to a list of strings\n
\(mon-line-string-split nil nil \"|\"\) split me to a list of strings\n\n
:ALIASED-BY `mon-string-ify-current-line'\n
:SEE-ALSO `mon-line-strings-qt-region', `mon-line-strings-to-list',
`mon-string-ify-list', `mon-insert-string-ify', `mon-line-string-get',
`mon-line-drop-in-words', `mon-word-get-list-in-buffer',
`mon-string-replace-char'.\n►►►"
  (interactive "p")
  (let* ((mls-str (if split-on " "))
	 (mls-dlm (cond (delim delim)
		    ((not delim)
		     (if intrp  "\""  ""))))
	 (mls-ss (split-string (mon-line-string-get) split-on t)))
    (cond ((and intrp (not buffer-read-only))
	   (save-excursion
	     (progn 
	       (kill-line)
               ;; :WAS (mapcar '(lambda (mls-L-1) 
               ;;                (princ (format "%s%s%s " mls-dlm mls-L-1 mls-dlm) (current-buffer))) mls-ss)
               (mapc #'(lambda (mls-L-1) 
                         (princ (format "%s%s%s " mls-dlm mls-L-1 mls-dlm) (current-buffer))) mls-ss)
	       (delete-char -1))) mls-ss)
	  ((and intrp buffer-read-only)
	   (progn
	     (kill-new (format "%S" mls-ss))
	     (message (concat ":FUNCTION `mon-line-string-split' " 
                              "-- buffer is read only, line split is on kill ring\n %S")
                      mls-ss)))
	  ((and (not intrp) mls-dlm)
	   (let (mls-ss2)
             (setq mls-ss2 nil)
	     (mapc #'(lambda (mls-L-2) (setq mls-ss2 (cons (format "%s%s" mls-dlm mls-L-2) mls-ss2))) mls-ss)
       	     ;; :WAS (mapcar '(lambda (mls-L-2) (setq mls-ss2 (cons (format "%s%s" mls-dlm mls-L-2) mls-ss2)))mls-ss)
	     mls-ss2))
	  (t mls-ss))))
;;
;;; :TEST-ME (mon-line-string-split) ;split me to a list of strings
;;; :TEST-ME (mon-line-string-split nil \"s\" \"S\"\) split me to a list of strings
;;; :TEST-ME (mon-line-string-split nil nil \"|\"\) split me to a list of strings

;;; ==============================
;;; :PREFIX "mspcp-"
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `space-p'
;;; :MODIFICATIONS <Timestamp: Tuesday February 10, 2009 @ 04:11.49 PM - by MON KEY>
(defun mon-spacep (&optional w-psn w-char-psn-after)
  "Return non-nil when char before point is a 'space' character.\n ;
When optional arg W-PSN is non-nil it is a char position satisfying
`number-or-marker-p', return non-nil for a 'space' before/after W-PSN.\n
When optional arg W-CHAR-PSN-AFTER is non-nil, return non-nil when char-after
point is a 'space'.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-line-bol-is-eol',
`mon-line-next-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (let* ((mspcp-look-pnt 
          (and w-psn 
               (or 
                (and (number-or-marker-p w-psn) w-psn)
                (error (concat ":FUNCTION `mon-spacep' "
                               "-- arg W-PSN not `number-or-marker-p', "
                               "got: %S type-of: %S")
                       w-psn (type-of w-psn)))))
	 (mspcp-pnt-chr 
          (cond ((and w-psn w-char-psn-after) (char-after mspcp-look-pnt))
                (w-char-psn-after (char-after))
                (w-psn (char-before mspcp-look-pnt))
                ((and (not w-psn) (not w-char-psn-after)) (char-before))))
	 (mspcp-tst-chr (memq mspcp-pnt-chr *mon-whitespace-chars*)))
    (and mspcp-tst-chr t)))

;;; ==============================
;;; :PREFIX "mspnb-"
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:11.19 PM - by MON KEY>
(defun mon-spacep-not-bol (&optional intrp)
  "Return non-nil if character after point at BOL is not a space.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep', `mon-line-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let* ((mspnb-char-bol (char-after (point-at-bol)))
	 (mspnb-not-spc (not (memq mspnb-char-bol *mon-whitespace-chars*))))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-not-bol' "
                                  "-- char after point at BOL"
                                  (if mspnb-not-spc " _NOT_ " " IS ") "whitespace")))
          (t mspnb-not-spc))))
;; 
;;; :TEST-ME (mon-spacep-not-bol)
;;; :TEST-ME (mon-spacep-not-bol t)

;;; ==============================
;;; :PREFIX "msib-"
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:11.19 PM - by MON KEY>
(defun mon-spacep-is-bol (&optional intrp)
  "Return non-nil if character after point at BOL _is_ a space.\n
:SEE-ALSO `mon-spacep-not-bol', `mon-spacep', `mon-line-bol-is-eol',
`mon-line-next-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let* ((msib-chr-bol (char-after (point-at-bol)))
	 (msib-is-spc (and (car (memq msib-chr-bol *mon-whitespace-chars*)))))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-is-bol' "
                                  "-- char after point at BOL"
                                  (if msib-is-spc " IS " " _NOT_ ") "whitespace")))
          (t msib-is-spc))))
;; 
;;; :TEST-ME (mon-spacep-is-bol)
;;; :TEST-ME (mon-spacep-is-bol t)

;;; ==============================
;;; :PREFIX "msiae-"
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 05:39.17 PM - by MON KEY>
(defun mon-spacep-is-after-eol (&optional intrp)
  "Return non-nil if character after eol _is_ a space.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let* ((msiae-aftr-eol (char-after (1+ (line-end-position))))
	 (msiae-is-spc (and (car (memq msiae-aftr-eol *mon-whitespace-chars*)))))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-is-after-bol' "
                                  "-- whitespace" (if msiae-is-spc  " IS " " _NOT_ ") 
                                  "after EOL")))
          (t msiae-is-spc))))

;;; ==============================
;;; :PREFIX "msiaetg-"
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 05:54.27 PM - by MON KEY>
(defun mon-spacep-is-after-eol-then-graphic (&optional intrp)
  "Return non-nil if character after eol _is_ a space and next char is not.\n
:EXAMPLE\n\n(mon-spacep-is-after-eol-then-graphic t\)
\(mon-spacep-is-after-eol-then-graphic t)\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-at-eol', `mon-cln-spc-tab-eol',
`*mon-whitespace-chars*', `*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let* ((msiaetg-aftr-eol (char-after (1+ (line-end-position))))
	 (msiaetg-eol-then (char-after (+ (line-end-position) 2)))
	 (msiaetg-is-spc (and (car (memq msiaetg-aftr-eol *mon-whitespace-chars*))))
	 (msiaetg-not-spc (not (memq msiaetg-eol-then *mon-whitespace-chars*)))
         (msiaetg-rtn (and msiaetg-is-spc msiaetg-not-spc)))
    (cond (intrp (message
                  (concat ":FUNCTION `mon-spacep-is-after-eol-then-graphic' "
                          "-- space or tab" (if msiaetg-rtn " IS " " _NOT_")
                          "at beggining of next line " 
                          (if msiaetg-rtn "and next char IS " 
                            "or next char _NOT_ ") 
                          "graphic")))
          (t msiaetg-rtn))))
;;
;;; :TEST-ME (mon-spacep-is-after-eol-then-graphic)
;;; :TEST-ME (mon-spacep-is-after-eol-then-graphic t)
;;; :TEST-ME (mon-spacep-is-after-eol-then-graphic t)

;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:58.38 PM - by MON KEY>
(defun mon-spacep-at-eol (&optional intrp)
  "Return non-nil if character at eol is either TAB (char 9) or SPC (char 32).\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol',`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let ((rtrn  (or (= (char-before (point-at-eol)) 9)
                   (= (char-before (point-at-eol)) 32))))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-at-eol' "
                                  "-- space or tab" (if rtrn " _IS_ " " _NOT_ ")
                                  "at EOL")))
          (t rtrn))))

;;; ==============================
;;; :PREFIX "mspcf-"
;;; :COURTESY Andy Stewart <lazycat.manatee@gmail.com> :WAS `colp'
;;; :SEE (URL `http://www.emacswiki.org/emacs/lazycat-toolkit.el')
(defun mon-spacep-first ()
  "Return non-nil if point is first non-whitespace character of line.\n
Test is as if by `back-to-indentation'.\n 
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (let (mspcf-pnt)
    (setq mspcf-pnt (point))
    (save-excursion
      (back-to-indentation)
      (equal mspcf-pnt (point)))))

;;; ==============================
;;; :PREFIX "mlbie="
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:17.51 PM - by MON KEY>
(defun mon-line-bol-is-eol (&optional intrp)
  "Return non-nil if postion at beginning of line is eq end of line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let ((mlbie-bol-eol (= (line-end-position) (line-beginning-position))))
    (cond (intrp (concat ":FUNCTION `mon-line-bol-is-eol' "
                         " -- BOL " (if mlbie-bol-eol " _IS_ " " _NOT_ ") " EOL"))
          (t mlbie-bol-eol))))
;;
;;; :TEST-ME (save-excursion (previous-line) (beginning-of-line) (mon-line-bol-is-eol))

;;; ==============================
;;; "mlpbie-"
;;; :MODIFICATIONS <Timestamp: #{2009-10-15T18:16:04-04:00Z}#{09424} - by MON KEY>
;;; Tired of compiler warnings for `previous-line' changed to (forward-line - n)
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:38.46 PM - by MON KEY>
(defun mon-line-previous-bol-is-eol (&optional intrp move-times)
  "Return t if position at beginning of previous line is eq end of line.
When not called-interactively MOVE-TIMES arg examines Nth previous line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol',`mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let ((mlpbie-p-bol-eol (save-excursion 
                     ;;(previous-line) move-times) 
                     (forward-line (if move-times (- move-times) (- 1)))
		     (beginning-of-line) 
		     (mon-line-bol-is-eol))))
    (cond (intrp
           (concat ":FUNCTION `mon-line-previous-bol-is-eol' "
                   "-- previous line " (if mlpbie-p-bol-eol " _IS_ " " _NOT_ ")
                   "BOL and"  (if mlpbie-p-bol-eol " _IS_ " " _NOT_ ") "EOL"))
          (t mlpbie-p-bol-eol))))
;;
;;; :TEST-ME  (mon-line-previous-bol-is-eol)
;;; :TEST-ME  (mon-line-previous-bol-is-eol 4)

;;; ==============================
;;; :PREFIX "mlnbie-"
;;; :MODIFICATIONS <Timestamp: #{2009-10-15T18:17:13-04:00Z}#{09424} - by MON KEY>
;;; Tired of compiler warnings for `next-line' changed to (forward-line n)
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:38.46 PM - by MON KEY>
(defun mon-line-next-bol-is-eol (&optional intrp move-times)
  "Return t if position at beginning of previous line is eq end of line.\n
When not called-interactively MOVE-TIMES arg examines Nth previos line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol', `*mon-whitespace-chars*',
`*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let ((mlnbie-n-bol-eol (save-excursion 
                     (forward-line (if move-times move-times))
                     ;; (next-line move-times) 
		     (beginning-of-line) 
		     (mon-line-bol-is-eol))))
    (cond (intrp
           (concat ":FUNCTION `mon-line-next-bol-is-eol' "
                   "-- next line BOL "(if mlnbie-n-bol-eol " _IS_ " " _NOT_ ") "EOL"))
          (t mlnbie-n-bol-eol))))
;;
;;; :TEST-ME (mon-line-next-bol-is-eol)

;;; ==============================
;;; :PREFIX "mleie-"
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:58.27 PM - by MON KEY>
(defun mon-line-eol-is-eob (&optional intrp)
  "Return t if point EOL is also EOB \(point-max\).\n
:NOTE Does not test for narrowing!\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-eol-is-eob' `mon-spacep-at-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`*mon-whitespace-chars*', `*regexp-whitespace-chars*'.\n►►►"
  (interactive "p")
  (let ((mleie-rtrn (= (point-at-eol) (mon-g2be 1 t)))) ;(buffer-end 1) )))
    (cond (intrp
           (concat 
            ":FUNCTION `mon-line-eol-is-eob' "
            "-- EOL" (if mleie-rtrn " _IS_ " " _NOT_ ") "EOB"))
          (t mleie-rtrn))))
;;
;;; :TEST-ME (mon-line-eol-is-eob t)
;;; :TEST-ME (with-temp-buffer (mon-line-eol-is-eob t))

;;; ==============================
;;; "To get the same same type of functionality at the end of the line, try this
;;; function. I bind it to my <end> key just like the <home> key above. It jumps
;;; between the actual end-of-line and the end of the code line which is different
;;; if the line has comments on the end."
;;; :SEE (URL `http://www.emacswiki.org/emacs/BackToIndentationOrBeginning')
;;; :PREFIX "mleoce-"
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:36.44 PM - by MON KEY>
;;; ==============================
(defun mon-line-end-or-code-end () 
  "Move point to EOL. If point is already there, to EOL sans comments.\n
That is, the end of the code, ignoring any trailing comment or whitespace.\n
:NOTE this does not handle 2 character  comment starters like // or /*.
Instances of such chars are be skipped.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol'.\n►►►"
  (skip-chars-backward " \t")
  (let ((mleoce-pt (point))
        (mleoce-lbp (line-beginning-position))
        mleoce-lim)
      (when (re-search-backward "\\s<" mleoce-lbp t)
	(setq mleoce-lim (point))
	(if (re-search-forward "\\s>" (1- mleoce-pt) t)
	    (goto-char mleoce-pt)
	  (goto-char mleoce-lim)               ; test here ->
          (while (looking-back "\\s<" (1- (point)))
            (backward-char))
          (skip-chars-backward " \t")))))


;;; ==============================
;;; :CHANGESET 2382
;;; :CREATED <Timestamp: #{2011-01-07T15:33:33-05:00Z}#{11015} - by MON KEY>
(defun mon-backspace  ()
   (interactive)
   (or (while (memq (char-before (point)) *mon-whitespace-chars*)
         (delete-backward-char 1))
       (delete-backward-char 1)))

 
;;; ==============================
;;; :CHANGESET 2379
;;; :CREATED <Timestamp: #{2011-01-06T13:12:16-05:00Z}#{11014} - by MON KEY>
(defun mon-goto-line-25% ()
  "Move point to character at 25% of `point-max' as if by `goto-char'.\n
Return `line-number-at-pos' moved to.\n
Like `goto-line' but point is not moved to `line-beginning-position'.\n
:EXAMPLE\n\n\(mon-goto-line-25%\)\n
:NOTE Useful for moving point in really large buffers with wrapped lines where
redisplay is painfully slow.\n
:ALIASED-BY `goto-line-25%'\n
:SEE-ALSO `mon-goto-line-25%', `mon-goto-line-50%', `mon-goto-line-75%',
`mon-g2be', `goto-line', `line-number-at-pos', `count-lines'.\n►►►"
  (interactive)
 (line-number-at-pos (goto-char (/ (mon-g2be 1 t) 4))))
;;
(defun mon-goto-line-50% ()
  "Move point to character at 50% of `point-max' as if by `goto-char'.\n
Return `line-number-at-pos' moved to.\n
Like `goto-line' but point is not moved to `line-beginning-position'.\n
:EXAMPLE\n\n\(mon-goto-line-50%\)\n
:NOTE Useful for moving point in really large buffers with wrapped lines where
redisplay is painfully slow.\n
:ALIASED-BY `goto-line-50%'\n
:SEE-ALSO `mon-goto-line-25%', `mon-goto-line-50%', `mon-goto-line-75%',
`mon-g2be', `goto-line', `count-lines'.\n►►►"
  (interactive)
  (line-number-at-pos (goto-char (/ (mon-g2be 1 t) 2))))
;;
(defun mon-goto-line-75% ()
  "Move point to character at 75% of `point-max' as if by `goto-char'.\n
Return `line-number-at-pos' moved to.\n
Like `goto-line' but point is not moved to `line-beginning-position'.\n
:EXAMPLE\n\n\(mon-goto-line-75%\)\n
:NOTE Useful for moving point in really large buffers with wrapped lines where
redisplay is painfully slow.\n
:ALIASED-BY `goto-line-75%'\n
:SEE-ALSO `mon-goto-line-25%', `mon-goto-line-50%', `mon-g2be', `goto-line',
`count-lines'.\n►►►"
  (interactive)
  (let ((mgl7-chr-cnt (mon-g2be 1 t)))
    (line-number-at-pos (goto-char (- mgl7-chr-cnt (/ mgl7-chr-cnt 3))))))

;;; ==============================
(provide 'mon-line-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-line-utils.el ends here
;;; EOF
