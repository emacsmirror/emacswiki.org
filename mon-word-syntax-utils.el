;;; mon-word-syntax-utils.el --- counting things with word syntax
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-word-syntax-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-23T20:59:13-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS:  lisp, emacs, matching, extensions,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-word-syntax-utils provides procedures for counting things with word
;; syntax.
;;
;; FUNCTIONS:►►►
;; `mon-get-syntax-at', `mon-get-syntax-class-at', `mon-line-test-content',
;; `mon-word-get-next', `mon-word-get-list-in-buffer',
;; `mon-word-reverse-region', `mon-word-iterate-over', `mon-word-iterate-over',
;; `mon-word-count-analysis', `mon-word-count-occurrences',
;; `mon-word-count-region', `mon-word-count-chars-region',
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
;;
;; <PREFIX>-<QUALIFIED>                 <PREFIX>-<CORE-SYMBOL>
;; `mon-skip-whitespace'             -> `edebug-skip-whitespace'
;;
;;  <PREFIX>-<QUALIFIED>                 <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-buffer-get-word-count'       -> `mon-word-count-occurrences'
;; `mon-region-reverse-words'        -> `mon-word-reverse-region'
;; `mon-reverse-region-words'        -> `mon-word-reverse-region'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-get-syntax-at'                              <- mon-utils.el
;; `mon-get-syntax-class-at'                        <- mon-utils.el
;; `mon-line-test-content'                          <- mon-utils.el
;; `mon-word-get-next'                              <- mon-utils.el
;; `mon-word-get-list-in-buffer'                    <- mon-utils.el
;; `mon-word-reverse-region'                        <- mon-utils.el
;; `mon-word-iterate-over'                          <- mon-utils.el
;; `mon-word-iterate-over'                          <- mon-utils.el
;; `mon-word-count-analysis'                        <- mon-utils.el
;; `mon-word-count-occurrences'                     <- mon-utils.el
;; `mon-word-count-region'                          <- mon-utils.el
;; `mon-word-count-chars-region'                    <- mon-utils.el
;;
;; TODO:
;; :SEE `comint-word'
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-word-syntax-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-word-syntax-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-23T20:59:13-05:00Z}#{10472} - by MON KEY>
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
;;; :PREFIX "mgsa-"
;;; :CHANGESET 2180
;;; :CREATED <Timestamp: #{2010-10-10T09:17:24-04:00Z}#{10407} - by MON KEY>
(defun mon-get-syntax-at (&optional syntax-at-psn)
  "Get syntax at position. Default is position 1+ point.\n
When SYNTAX-AT-PSN an integer is non-nil examine that position from point.\n
:EXAMPLE\n\n\(mon-get-syntax-at\)\n
\(mon-get-syntax-at -8\)\n
:SEE-ALSO `mon-get-syntax-class-at'.\n►►►"
  (interactive)
  (let ((mgsa-at-pnt (if syntax-at-psn (+ (point) syntax-at-psn) (point))))
    (and (or (eobp)
             (and syntax-at-psn (or (< mgsa-at-pnt (mon-g2be -1 t))
                                    (>= mgsa-at-pnt (mon-g2be 1 t)))))
         (error (concat ":FUNCTION `mon-get-syntax-class-at' " 
                        "-- point or arg PSN exceed buffer-bounds, at point: %d")
                mgsa-at-pnt))
    `(:sytax-at  ,(syntax-after mgsa-at-pnt)
                 :char-at   ,(char-after mgsa-at-pnt)
                 :string-at ,(char-to-string (char-after mgsa-at-pnt))
                 ;; it isn't clear when/if this is relevant
                 ;; :syntax-class-at ,(mon-get-syntax-class-at mgsa-at-pnt)
                 :point-at     ,mgsa-at-pnt)))

;;; ==============================
;;; :PREFIX "mgsca-"
;;; :CHANGESET 1974
;;; :CREATED <Timestamp: #{2010-07-13T14:21:34-04:00Z}#{10282} - by MON KEY>
(defun mon-get-syntax-class-at (at-syntax-psn)
  "Return `syntax-class' at AT-SYNTAX-PSN.\n
This is just a combination of `syntax-after' and `syntax-class'.\n
 \(logand \(syntax-class \(char-syntax \(char-after \(point\)\)\)\)\n
:EXAMPLE\n\n(mon-get-syntax-class-at (1+ (point))) (\n
:SEE-ALSO `syntax-after', `syntax-class', `string-to-syntax', `char-syntax'
`mon-help-syntax-class', `mon-help-syntax-functions'.\n►►►"
  (let ((mgsca-aftr-syn 
         (progn 
           (unless (or (< at-syntax-psn  (mon-g2be -1 t))
                       (>= at-syntax-psn (mon-g2be 1 t)))
             (let ((mgsca-syn-tbl 
                    (and parse-sexp-lookup-properties
                         (get-char-property at-syntax-psn 'syntax-table))))
               (if (consp mgsca-syn-tbl) mgsca-syn-tbl
                 (aref (or mgsca-syn-tbl (syntax-table))
                       (char-after at-syntax-psn))))))))
    (and mgsca-aftr-syn (logand (car mgsca-aftr-syn) 65535))))
;;
;; This is neat:
;; (lsh 1 17) => 131072 
;; (logior 131072 255) => 131327  
;; (- 131327 131072) => 255 (#o377, #xff)

;; (lsh 4 17) ;=> 524288 
;; (logior 524288 255) ;=> 
;; (- 524543 524288) => 255

;;; ==============================
;;; :PREFIX "mltc-" rtrn-as-list
;;; :CREATED <Timestamp: Monday May 11, 2009 @ 05:07.49 PM - by MON KEY>
(defun mon-line-test-content (w-syntax-sym &optional w-return-lst)
  "Examine Syntax Location of W-SYNTAX-SYM from point.\n
When syntax W-SYNTAX-SYM is t advances point to end of syntax.
Return a formatted string describing syntax locations.
W-SYNTAX-SYM arg is a symbol of type: 'word 'whitespace or 'punctuation.
When W-RETURN-LST is non-nil returns as list.\n
:EXAMPLE\n
\(mon-line-test-content 'word)word =>
\"[line:413 word:word word-start:20267 word-end:20271]\"\n
\(mon-line-test-content 'word t\)word => 
\(413 word \"word\" 20269 20273\)
\(car cadr caddr cadddr cddddr\)
 line type found satart end\n
\(if \(> \(skip-syntax-forward \"^-\"\) 0\)
     \(mon-line-test-content 'whitespace t\)\)word more-word\n
:NOTE Function relies on current buffers local syntax table.\n
:SEE-ALSO `mon-get-syntax-class-at', `mon-get-text-properties-category', `mon-view-help-source',
`mon-help-syntax-class', `mon-help-syntax-functions'.\n►►►"
  (let* ((mltc-sytx-typ (cond ((eq w-syntax-sym 'word) 'word)
                              ((eq w-syntax-sym 'whitespace) 'whitespace)
                              ((eq w-syntax-sym 'punctuation) 'punctuation)))
	 (mltc-sytx-w-w-p (cond ((eq mltc-sytx-typ 'word)  
                                 '(mltc-sytx-typ "\w" "word-start:" "word-end:"))
                                ((eq mltc-sytx-typ 'whitespace) 
                                 '(mltc-sytx-typ "-" "spc-start:" "spc-end:"))
                                ((eq mltc-sytx-typ 'punctuation) 
                                 '(mltc-sytx-typ "." "punct-start:" "punct-end:"))))
	 (mltc-sytx-beg (caddr mltc-sytx-w-w-p))
	 (mltc-sytx-end (cadddr mltc-sytx-w-w-p))
         ;; These appear to be unused
	 ;; (starting) (ending)
         mltc-mrkr
         next)
    (setq mltc-mrkr (point-marker))
    (setq next (skip-syntax-forward (cadr mltc-sytx-w-w-p)))
    (let* ((mltc-lnap (line-number-at-pos))
	   (mltc-rng-beg (marker-position mltc-mrkr))
	   (mltc-rng-end (point))
	   (mltc-w-bfr-substr (mon-buffer-sub-no-prop mltc-rng-beg mltc-rng-end))
	   (mltc-sytx-mtch mltc-w-bfr-substr)
	   (mltc-sytx-is (cond ((and (eq mltc-sytx-typ 'whitespace) 
                                     (eq (string-match " " mltc-sytx-mtch) 0)) t)
                               ((and (eq mltc-sytx-typ 'whitespace)
                                     (not (eq (string-match " " mltc-sytx-mtch) 0))) nil)
                               ;; not testing for numbers add another case if thats whats wanted
                               ((and (eq mltc-sytx-typ 'word)
                                     (eq (string-match "[[:alpha:]]" mltc-sytx-mtch) 0)) t)
                               ((and (eq mltc-sytx-typ 'word) 
                                     (not (eq (string-match "[[:alpha:]]" mltc-sytx-mtch) 0))) nil)
                               ((and (eq mltc-sytx-typ 'punctuation)
                                     (eq (string-match "[[:punct:]]" mltc-sytx-mtch) 0)) t)
                               ((and (eq mltc-sytx-typ 'punctuation)
                                     (not (eq (string-match "[[:punct:]]" mltc-sytx-mtch) 0))) nil)))
	   (mltc-rslt-psn (cond (;; test word
                                 (and mltc-sytx-is (eq mltc-sytx-typ 'word))
                                 (format "[line:%d %s:%s %s%d %s%d]" 
                                         mltc-lnap mltc-sytx-typ mltc-sytx-mtch 
                                         mltc-sytx-beg mltc-rng-beg mltc-sytx-end mltc-rng-end))
                                ((and (not mltc-sytx-is) (eq mltc-sytx-typ 'word))
                                 (format "[line:%d %s:_no_ %s%d %s%d]"
                                         mltc-lnap mltc-sytx-typ  mltc-sytx-beg
                                         mltc-rng-beg  mltc-sytx-end mltc-rng-end))
                                (;; test whitespace
                                 (and mltc-sytx-is (eq mltc-sytx-typ 'whitespace)) 
                                 (format "[line:%d %s:_yes_ %s%d %s%d]" 
                                         mltc-lnap mltc-sytx-typ mltc-sytx-beg
                                         mltc-rng-beg mltc-sytx-end mltc-rng-end))
                                ((and (not mltc-sytx-is) (eq mltc-sytx-typ 'whitespace))
                                 (format "[line:%d %s:_no_ %s%d %s%d]"
                                         mltc-lnap mltc-sytx-typ mltc-sytx-beg
                                         mltc-rng-beg  mltc-sytx-end mltc-rng-end))
                                (;; test punctuation
                                 (and mltc-sytx-is (eq mltc-sytx-typ 'punctuation)) 
                                 (format "[line:%d %s:%s %s%d %s%d]" 
                                         mltc-lnap mltc-sytx-typ mltc-sytx-mtch
                                         mltc-sytx-beg mltc-rng-beg mltc-sytx-end mltc-rng-end))
                                ((and (not mltc-sytx-is) (eq mltc-sytx-typ 'punctuation))
                                 (format "[line:%d %s:_no_ %s%d %s%d]"
                                         mltc-lnap mltc-sytx-typ mltc-sytx-beg
                                         mltc-rng-beg  mltc-sytx-end mltc-rng-end))))
	   (mltc-rslt-lst-psn (cond (;; test word
                                     (and mltc-sytx-is (eq mltc-sytx-typ 'word))
                                     `(,mltc-lnap ,mltc-sytx-typ ,mltc-sytx-mtch
                                       ,mltc-rng-beg ,mltc-rng-end))
                                    ;; test whitespace
                                    ((and (not mltc-sytx-is) (eq mltc-sytx-typ 'word))
                                     `(,mltc-lnap ,mltc-sytx-typ
                                       nil ,mltc-rng-beg ,mltc-rng-end))
                                    ((and mltc-sytx-is (eq mltc-sytx-typ 'whitespace))	
                                     `(,mltc-lnap ,mltc-sytx-typ ,mltc-sytx-mtch
                                       ,mltc-rng-beg ,mltc-rng-end))
                                    ((and (not mltc-sytx-is) (eq mltc-sytx-typ 'whitespace))
                                     `(,mltc-lnap ,mltc-sytx-typ
                                       nil ,mltc-rng-beg ,mltc-rng-end))
                                    ;; test punctuation
                                    ((and mltc-sytx-is (eq mltc-sytx-typ 'punctuation)) 
                                     `(,mltc-lnap ,mltc-sytx-typ ,mltc-sytx-mtch
                                       ,mltc-rng-beg ,mltc-rng-end))
                                    ((and (not mltc-sytx-is) (eq mltc-sytx-typ 'punctuation))
                                     `(,mltc-lnap ,mltc-sytx-typ 
                                       nil ,mltc-rng-beg ,mltc-rng-end)))))
      (if w-return-lst
	  mltc-rslt-lst-psn
	mltc-rslt-psn))))
;;
;;; :TEST-ME (mon-line-test-content 'word t)this-word
;;; :TEST-ME (mon-line-test-content 'word)this-word
;;; :TEST-ME (mon-line-test-content 'word) this-word
;;; :TEST-ME (mon-line-test-content 'word t) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace t) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace)this-word
;;; :TEST-ME (mon-line-test-content 'whitespace t)this-word
;;; :TEST-ME (mon-line-test-content 'punctuation t),this-word
;;; :TEST-ME (mon-line-test-content 'punctuation),this-word
;;; :TEST-ME (mon-line-test-content 'punctuation t)this-word
;;; :TEST-ME (mon-line-test-content 'punctuation),his-word
;;; :TEST-ME (car (mon-line-test-content 'word t))word
;;; :TEST-ME (car (nthcdr 1 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 2 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 3 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 4 (mon-line-test-content 'word t)))word
;;; :TEST-ME (mon-line-test-content 'word)word => "[line:413 word:word word-start:20267 word-end:20271]"
;;; :TEST-ME (mon-line-test-content 'word t)word => (413 word "word" 20269 20273)
;;; :TEST-ME (car cadr caddr cadddr cddddr)
;;; :TEST-ME (if (> (skip-syntax-forward "^-") 0) (mon-line-test-content 'whitespace t))word more-word

;;; ==============================
;;; :PREFIX "mwgn-"
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :WAS `get-next-word' -> `mon-word-get-next'
(defun mon-word-get-next ()
  "Return the next 'word' in the buffer.\n
Point is left following the word.\n
When `eopb' return nil with point unchanged.\n
:NOTE Words motion is per `inhibit-field-text-motion' where a word is defined by
`forward-word' according to the syntax-table settings.\n
:SEE-ALSO `mon-line-get-next', `mon-word-get-list-in-buffer'.\n►►►"
  ;; :NOTE Should this let-bind a syntax-table per the `forward-word' motion?
  (let (mwgn-start mwgn-end)
    (if (eobp)
	nil
      (progn
	(setq mwgn-start (point))
	(forward-word 1)
	(setq mwgn-end (point))
	(forward-word -1)
        ;; Are we already past last word?
	(if (< (point) mwgn-start)           
	    (progn
	      (mon-g2be -1)
              nil)
	  (setq mwgn-start (point))
	  (goto-char mwgn-end)
          (mon-buffer-sub-no-prop mwgn-start mwgn-end))))))

;;; ==============================
;;; :PREFIX "mwglib-"
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :CREATED <Timestamp: #{2010-08-20T13:24:05-04:00Z}#{10335} - by MON KEY>
(defun mon-word-get-list-in-buffer (&optional intrp)
  "Convert entirety of current-buffer to list of `newline' separated \"words\".\n
A \"word\" is gathered with `forward-word' the heuristics of which are informed
by ther current syntax-table settings.\n
When called-interactively return and display results in a new buffer `\"*MON-WORD-LIST*\", 
:EXAMPLE\n\n(mon-word-get-list-in-buffer\)\n
\(mon-word-get-list-in-buffer t\)\n
:NOTE Apply `sort-lines', `unique-lines', etc. to obtain a list of all the
unique words in a buffer of document.\n
:ALIASED-BY `mon-buffer-get-word-list'\n
:SEE-ALSO `mon-word-count-occurrences', `mon-line-strings-to-list',
`mon-line-string-split', `mon-stringify-list', `mon-dropin-line-word',
`mon-insert-string-ify', `mon-word-count-analysis', `mon-word-count-region',
`mon-word-count-chars-region'.\n►►►"
  ;; :WAS
  ;; (interactive)
  ;; (let (word)
  ;;   (with-output-to-temp-buffer "*MON-WORD-LIST*"
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (while (setq word (mon-word-get-next))
  ;;         (princ (format "%s\n" word)))))))
  (interactive "p")
  (let* ((mwglib-MWL (get-buffer-create "*MON-WORD-LIST*"))
         ;; (generate-new-buffer-name "*MON-WORD-LIST*")
         ;; We use this buffer to accumlate results 
         (standard-output (get-buffer mwglib-MWL))
         mwglib-tmp)
    (with-current-buffer mwglib-MWL (erase-buffer))
    (save-excursion
      (mon-g2be -1) ;;(goto-char (point-min))
      (while (setq mwglib-tmp (mon-word-get-next))
        (princ (format "%s\n" mwglib-tmp))))
    (setq mwglib-tmp (buffer-name (current-buffer)))
    (with-current-buffer  mwglib-MWL
      (setq mwglib-MWL `(:BUFFER ,mwglib-tmp 
                         :TOTAL-WORDS ,(count-lines (buffer-end 0) (buffer-end 1))))
      (if intrp 

          (progn
            (mon-g2be -1) ;; (goto-char (buffer-end 0))
            (save-excursion            
              (insert  ";; :FUNCTION `mon-word-get-list-in-buffer'\n;; ")
              (princ mwglib-MWL (current-buffer))
              (insert "\n" (make-string 68 59) "\n"))
              (display-buffer (current-buffer) t))
        (progn
          (setq mwglib-MWL (append mwglib-MWL 
                                   (mon-line-strings-one-list (buffer-end 0) (buffer-end 1))))
          (kill-buffer (current-buffer)))))
    mwglib-MWL))

;;; ==============================
;;; :CHANGESET 2202 <Timestamp: #{2010-10-20T15:43:46-04:00Z}#{10423} - by MON KEY>
(defun mon-word-reverse-region (beg end &optional insrtp intrp)
  "Reverse the order of words in region from BEG to END.\n
When optional arg INSRTP is non-nil or called-interactively delete existing
region and replace with the reversed words. Does not move point.\n
Return value has the format:\n
 \(<REVERSED-STRING> \(<REVERSED-STRING-AS-LIST>\)
                     \(\(<BEG> <END>\) <ORIGINAL-STRING>\)\)\n
:EXAMPLE\n\n\(save-excursion \(forward-line 2\)
  \(mon-word-reverse-region \(line-beginning-position\) \(line-end-position\)\)\)\n
deleted maybe and reversed be will I\n
:ALIASED-BY `mon-region-reverse-words'\n
:ALIASED-BY `mon-reverse-region-words'\n
:SEE-ALSO `mon-region-reverse'.\n►►►"
  (interactive "r\ni\np")
  (let* ((mwrr-str   (mon-buffer-sub-no-prop beg end))
         (mwrr-rvrsd (save-match-data (split-string mwrr-str "\\b"))))
    (setq mwrr-rvrsd 
          `(,(mapconcat #'identity  (reverse mwrr-rvrsd) "")
            ,(nreverse mwrr-rvrsd)
            ((,beg ,end) ,mwrr-str)))
    (if (or insrtp intrp)
        (save-excursion
          (delete-region beg end)
          (insert (car mwrr-rvrsd))
          (prin1 mwrr-rvrsd))
      mwrr-rvrsd)))
;; 
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (equal 
;; |  (car (save-excursion 
;; |         (forward-line 1)
;; |         (mon-word-reverse-region (line-beginning-position) (line-end-position))))
;; |  "I will be reversed and maybe deleted")
;; | deleted maybe and reversed be will I
;; | ^--< column 0 
;; `----

;;; ==============================
;;; :PREFIX "mwio-"
;;; :COURTESY Jonathan Rockway :VERSION 2009-01-18
;;; :SEE (URL `http://blog.jrock.us/articles/Iterators%20in%20elisp.pod')
(defun mon-word-iterate-over (buffer)
  "Return an iterator that gets the next word in buffer.\n
Extract one word at a time by calling (funcall next-word).\n
:EXAMPLE For BUFFER test-buffer containing \"This is text.\"
\(setq next-word \(mon-word-iterate-over-in \(get-buffer \"test buffer\")))
The first time next-word is called, return \"This\".
The next time, retrun \" is\". Then, \" text.\". 
Finally, return nil forever.\n
:NOTE Uses lexical-let to close over over local vars mwio-bfr and mwio-psn.\n
:SEE-ALSO `mon-word-get-list-in-buffer'.\n►►►"
  (lexical-let ((mwio-bfr buffer)
                (mwio-psn 1))
    (lambda ()
      (save-excursion
        (let ((mwio-cur-bfr (current-buffer)) 
              mwio-rslt)
          (switch-to-buffer mwio-bfr)
          (goto-char mwio-psn)
          (forward-word)
          (let ((mwio-pnt (point)))
            (if (not (eq mwio-psn mwio-pnt))
                (progn 
                  (setq mwio-rslt (mon-buffer-sub-no-prop  mwio-psn mwio-pnt))
                  (setq mwio-psn mwio-pnt))))
          (switch-to-buffer mwio-cur-bfr) mwio-rslt)))))

;;; ==============================
;;; :PREFIX "mwca-"
;;; :CHANGESET 1973 <Timestamp: #{2010-07-12T20:20:45-04:00Z}#{10281} - by MON KEY>
(defun mon-word-count-analysis (start end &optional intrp)
  "Count number of times each word is used in the region.\n
Count anything with word syntax when `with-syntax-table' uses`standard-syntax-table'.\n
:EXAMPLE\n\n\(mon-word-count-analysis \(buffer-end 0\) \(buffer-end 1\)\)\n
\(mon-word-count-analysis \(buffer-end 0\) \(buffer-end 1\) t\)\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-chars-region',
`mon-word-count-occurrences', `mon-word-count-region', 
`mon-word-get-list-in-buffer'.\n►►►"
  (interactive "r\np")
  (let (mwca-wrds mwca-hshd mwca-oba)
    (setq mwca-oba (make-vector (/ (- end start) 2) 0))
    (setq mwca-wrds (make-hash-table :test #'eq :weakness 'key))
    (unwind-protect 
        (narrow-to-region start end)
      (save-excursion
        (mon-g2be -1)
        (with-syntax-table (syntax-table)
          (while (search-forward-regexp "\\w+" end t)
            (let* ((mwca-wrd (intern (match-string-no-properties 0) mwca-oba))
                   (mwca-cell (gethash mwca-wrd mwca-wrds)))
              (if (and (not (numberp mwca-wrd)) mwca-cell)
                  (puthash mwca-wrd (1+ mwca-cell) mwca-wrds)
                (puthash mwca-wrd 1 mwca-wrds))))))
      (widen))
    (maphash #'(lambda (mwca-k mwca-v) 
                 (push `(,mwca-k . ,mwca-v) mwca-hshd)) mwca-wrds)
    (setq mwca-hshd (nreverse mwca-hshd))
    (if intrp
        (message (concat ":FUNCTION `mon-word-count-analysis' "
                         "-- :WORDS-FOUND %S")
                 mwca-hshd)
      `(:WORDS-FOUND ,@mwca-hshd))))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el :WAS `ff/word-occurrences'
;;; :SEE (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :SEE git clone http://fleuret.org/git/elisp/ 
;;; Added `with-current-buffer', `message', `with-silent-modifications', `window-min-height'
;;; :CHANGESET 1969 <Timestamp: #{2010-07-12T13:40:08-04:00Z}#{10281} - by MON KEY>
;;; :CHANGESET 2316 <Timestamp: #{2010-11-12T22:21:45-05:00Z}#{10455} - by MON KEY>
(defun mon-word-count-occurrences (&optional intrp)
  "Display in a new buffer the list of words sorted by number of occurrences.\n
Count contains multiple occurences of words with > word-length 3 in buffer.\n
Return results and display in buffer named \"*WORD-COUNT*\".\n
:EXAMPLE\n\n\(mon-word-count-occurrences\)\n
\(mon-word-count-occurrences t\)\n
:ALIASED-BY `mon-buffer-get-word-count'\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-region', `mon-word-count-analysis',
`mon-word-count-chars-region', `mon-word-get-list-in-buffer'.\n►►►"
  (interactive "p")
  (let ((mwco-cnt-buf (when intrp (get-buffer-create "*WORD-COUNT*")))
        (mwco-dpt-buf (when intrp (current-buffer)))
        (mwco-dpt-win (when intrp (get-buffer-window (current-buffer))))
        (mwco-map (when intrp (make-sparse-keymap)))
        (mwco-nb (make-hash-table 
                  ;; split the difference we're looking for words > length 3
                  ;; avv Engrish word has length 5, 1+ for whitespce 
                  ;; So, 3.5 seems like a reasonable amount to divide by.
                  :size (mon-next-almost-prime (floor (/ (buffer-size) 3.5))) 
                  :weakness 'key))
        (mwco-st  (make-hash-table 
                   :size (mon-next-almost-prime (floor (/ (buffer-size) 3.5))) 
                   :weakness 'key))
        (mwco-wrds-chrs (mon-word-count-chars-region (mon-g2be -1 t) (mon-g2be 1 t)))
        mwco-rslt)
    ;; Collect all words into a pair of hash-tables.
    (save-excursion
      (mon-g2be -1)
      (with-syntax-table (standard-syntax-table)
        (while (search-forward-regexp "\\([\\-a-zA-Z\\\\]+\\)" nil t)
          (let* ((mwco-s (downcase (match-string-no-properties 1)))
                 (mwco-k1 (sxhash mwco-s)))
            (puthash mwco-k1 mwco-s mwco-st)
            (puthash mwco-k1 (1+ (gethash mwco-k1 mwco-nb 0)) mwco-nb)))))
    (if (<= (hash-table-count mwco-nb) 0)
        (if intrp 
            (message (concat ":FUNCTION `mon-word-count-occurrences' "
                             "-- did not find re-occurences for words "
                             "with > word-length 3 in buffer: %s") 
                     mwco-dpt-buf)
          `(,@mwco-wrds-chrs :WORDS-W-LEN-GT-3 0))
      (progn
        (maphash #'(lambda (mwco-k2 mwco-v)
                     (setq mwco-rslt (cons (cons mwco-v (gethash mwco-k2 mwco-st)) mwco-rslt)))
                 mwco-nb)
        ;; Find the longest string
        (if (not intrp)
            (progn (setq mwco-nb nil) (setq mwco-st nil))
          (progn
            (setq mwco-nb nil)
            (setq mwco-st '(0 . " "))
            (dolist (strl mwco-rslt (setq mwco-st (length (cdr mwco-st))))
              (let ((mwco-strl (length (cdr strl))))
                (when (> mwco-strl (car mwco-st)) 
                  (setq mwco-st `(,mwco-strl . ,(cdr strl))))))))
        (setq mwco-rslt (sort mwco-rslt #'(lambda (a b) (> (car a) (car b)))))
        (dolist (mwco-wc mwco-rslt (setq mwco-nb (nreverse mwco-nb)))
          (if (and (> (car mwco-wc) 1)
                   ;; No leading backslash and at least four characters.
                   (string-match "^[^\\]\\{4,\\}" (cdr mwco-wc)))
              (if intrp 
                  (push (concat (cdr mwco-wc) 
                                (if (< (length (cdr mwco-wc)) mwco-st)
                                    (make-string (- mwco-st (length (cdr mwco-wc))) 32)
                                  " ")
                                (number-to-string (car mwco-wc))) mwco-nb)
                (push `(,(cdr mwco-wc) . ,(car mwco-wc)) mwco-nb))))
        (if (not intrp)
            `(,@mwco-wrds-chrs :WORDS-W-LEN-GT-3 ,(length mwco-nb) (:WORDS-W-COUNTS ,@mwco-nb))
          ;; Create the mwco-rslt buffer if called-interactively
          (with-current-buffer mwco-cnt-buf
            (erase-buffer)
            (set (make-local-variable 'show-trailing-whitespace) nil)
            (save-excursion
              (insert ";; :W-FUNCTION     `mon-word-count-occurrences' \n"
                      ";; :IN-BUFFER       " (buffer-name mwco-dpt-buf) "\n"
                      ";; :W-SYNTAX-TABLE `standard-syntax-table'\n"
                      ";; " (mapconcat #'(lambda (wrdch) (format "%s" wrdch)) mwco-wrds-chrs " ") "\n"
                      ";; :NOTE To return to buffer counted type: \"C-c q\"\n;;\n"
                      (cond ((> mwco-st 8) (concat ";; :WORD"  (make-string (- mwco-st 9) 32) ":COUNT\n"))
                            ((< mwco-st 8) (concat ";; :WORD  :COUNT\n")))
                      (mapconcat #'identity mwco-nb "\n")))
            (define-key mwco-map "\C-cq" `(lambda () 
                                       (interactive)
                                       (if (buffer-live-p ,(buffer-name mwco-dpt-buf))
                                           (progn (switch-to-buffer ,(buffer-name mwco-dpt-buf))
                                                  (when (get-buffer "*WORD-COUNT*")
                                                    (kill-buffer (get-buffer "*WORD-COUNT*"))))
                                         (kill-this-buffer))))
            (use-local-map mwco-map)
            (display-buffer (current-buffer) t)
            (with-silent-modifications
              (let ((window-min-height 10)
                    (mwco-win-max-hgt-myb 
                     (let* ((mwco-wh (window-height 
                                      (if (equal mwco-dpt-buf (window-buffer mwco-dpt-win))
                                          mwco-dpt-win
                                        (other-window 1))))
                            (mwco-wh-2 (cond ((>= (length mwco-nb) 20)
                                              (+ mwco-wh (/ (length mwco-nb) 2)))
                                             ((>= (+ mwco-wh (length mwco-nb)) 20)
                                              (+ mwco-wh (length mwco-nb)))
                                             (t 12))))
                       mwco-wh-2)))
                (fit-window-to-buffer 
                 (get-buffer-window (current-buffer)) mwco-win-max-hgt-myb 10)))))))))

;;; =======================
(defun mon-word-count-region (start end &optional intrp)
  "Return the number of words in the region.\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-chars-region',
`mon-word-count-analysis', `mon-word-count-occurrences',
`mon-word-get-list-in-buffer'.\n►►►"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mon-g2be -1)
      (let ((mwcr-mtchd (count-matches "\\sw+")))
        (if intrp 
            (message (concat ":FUNCTION `mon-word-count-region' "
                             "-- :WORDS %d in the region") mwcr-mtchd)
          `(:WORDS ,mwcr-mtchd))))))

;;; ==============================
(defun mon-word-count-chars-region (beginning end &optional intrp)
  "Return message indicating the number of words and chars that are in a region.\n
:EXAMPLE\n\n\(mon-word-count-chars-region \(buffer-end 0\) \(buffer-end 1\)\)\n
\(mon-word-count-chars-region \(buffer-end 0\) \(buffer-end 1\) t\)\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-region',
`mon-word-count-analysis', `mon-word-count-occurrences', 
`mon-word-get-list-in-buffer', `mon-string-from-sequence'.\n►►►"
  (interactive "r\np")
  (let ((mwccr-wrd-cnt 0) 
        (mwccr-chr-cnt (- end beginning)))
    (save-excursion
      (goto-char beginning)
      (while (and (< (point) end) (search-forward-regexp "\\w+\\W*" end t))
        (incf mwccr-wrd-cnt))
      (if intrp 
          (message  (concat ":FUNCTION `mon-word-count-chars-region' "
                            "-- counted :WORDS %d :CHARS %d")
                    mwccr-wrd-cnt mwccr-chr-cnt)
        `(:WORDS ,mwccr-wrd-cnt :CHARS ,mwccr-chr-cnt)))))

;;; ==============================
;;; :COURTESY Henrik Enberg but prob. pulled out of:
;;; :COURTESY Stefan Reichor, stefan@xsteve.at :HIS xsteve-functions.el  
;;; :NOT-WORKING-AS-OF
;;; :CREATED <Timestamp: Tuesday February 17, 2009 @ 04:53.44 PM - by MON KEY>
;;; ==============================
;; (defun mon-query-remove-doubled-words (&optional force)
;;   "Find all doubled words and ask to remove them.
;; With optional arg FORCE remove them without asking."
;;   (interactive "P")
;;   (let ((case-fold-search t)
;; 	(del-counter 0))
;;     (while (re-search-forward
;; 	    "\\(\\<\\w\\{3,\\}\\>\\)[ \t\n]*\\(\\1\\)" nil t)
;;       (replace-highlight (match-beginning 2) (match-end 2))
;;       (unwind-protect
;; 	  (when (or force (y-or-n-p "Remove this doubled word? "))
;; 	    (delete-region (match-beginning 2) (match-end 2))
;; 	    (canonically-space-region (match-beginning 0) (match-end 0))
;; 	    (setq del-counter (1+ del-counter)))
;; 	(replace-dehighlight)))
;;     (if (> del-counter 0)
;; 	(message "Removed %d doubled %s." del-counter
;; 		 (if (< del-counter 1) "words" "word"))
;;       (message "No doubled words found or removed."))))
;;; =======================


;;; ==============================
(provide 'mon-word-syntax-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-word-syntax-utils.el ends here
;;; EOF
