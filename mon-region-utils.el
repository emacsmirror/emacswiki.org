;;; mon-region-utils.el --- region oriented procedures for mon-*utils features
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-region-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-23T17:48:50-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-region-utils provides region oriented procedures for mon-*utils features
;;
;; FUNCTIONS:►►►
;; `mon-wrap-with'
;; `mon-wrap-text'
;; `mon-wrap-selection'
;; `mon-region-reverse'
;; `mon-region-capitalize'
;; `mon-region-unfill'
;; `mon-region-indent-refill'
;; `mon-region-length'
;; `mon-region-position'
;; `mon-decode-coding-region-utf-8-unix'
;; `mon-region-split-commas' 
;; `mon-sha1-region'
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
;; <PREFIX>-<QUALIFIED>               <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-string-split-commas'         -> `mon-region-split-commas'
;; `mon-split-region-at-commas'      -> `mon-region-split-commas'
;; `mon-region-refill-indent'        -> `mon-region-indent-refill'
;; `mon-region-reverse-chars'        -> `mon-region-reverse'
;; `mon-indent-refill-region'        -> `mon-region-indent-refill'
;; `mon-indent-region-refill'        -> `mon-region-indent-refill'
;; `mon-capitalize-region'           -> `mon-region-capitalize'
;; `mon-region-wrap'                 -> `mon-wrap-selection'
;; `mon-region-count-regexp-matches' -> `comint-how-many-region'
;;
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-string-split-commas'                        -> `mon-region-split-commas' 
;; 
;;
;; MOVED:
;; `mon-wrap-with'                                   <- mon-utils.el
;; `mon-wrap-text'                                   <- mon-utils.el
;; `mon-wrap-selection'                              <- mon-utils.el
;; `mon-region-reverse'                              <- mon-utils.el
;; `mon-region-capitalize'                           <- mon-utils.el
;; `mon-region-unfill'                               <- mon-utils.el
;; `mon-region-indent-refill'                        <- mon-utils.el
;; `mon-region-length'                               <- mon-utils.el
;; `mon-region-position'                             <- mon-utils.el
;; `mon-decode-coding-region-utf-8-unix'             <- mon-utils.el
;; `mon-sha1-region'                                 <- mon-utils.el
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
;; URL: http://www.emacswiki.org/emacs/mon-region-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-25T02:27:00-05:00Z}#{10476} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-region-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-23T17:48:50-05:00Z}#{10472} - by MON KEY>
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
;;; :PREFIX "mrp-"
(defun mon-region-position (&optional insrtp intrp)
  "Return details of the postion of current region.\n
Return value is a list of key value pairs as returned by accessor functions:\n
  <KEY>             <VALUE>         ;;  <ACCESSOR>
 :USE-REGION-P     { t | nil }      ;; `use-region-p' 
 :REGION-ACTIVE-P  { t | nil }      ;; `region-active-p' 
 :REG-BEG          <INTEGER>        ;; `region-beginning'
 :REG-END          <INTEGER>        ;; `region-end'\n
When optional arg insrtp is non-nil or called interactively return value is a a
string prefixed by:\n
 \":FUNCTION `mon-region-position' -- current region \"\n
When insrtp is non-nil or called-interactively with a prefix arg the string is
inserted in current-buffer at point. Does not move point.\n
When called-interactively and insrtp is ommitted string is echod to the
minibuffer as if by `message'.\n
:EXAMPLE\n\n\(mon-region-position\)\n\n\(mon-region-position nil t)\n
\(save-excursion \(set-mark \(point\)\) \(forward-char 3\) \(mon-region-position nil t\)\)\n
\(save-excursion \(set-mark \(point\)\) \(forward-char 3\) \(mon-region-position\)\)\n
:SEE-ALSO `mon-region-indent-refill', `mon-region-unfill',
`mon-region-capitalize', `mon-region-reverse'.\n►►►"
  (interactive "P\np")
  (let ((mrp-rtn `(:USE-REGION-P    ,(use-region-p) 
                   :REGION-ACTIVE-P ,(region-active-p)  
                   :REG-BEG         ,(region-beginning)
                   :REG-END         ,(region-end))))
    (if (or insrtp intrp)
        (progn
          (setq mrp-rtn 
                (concat ":FUNCTION `mon-region-position' "
                         "-- current region " 
                         (mapconcat #'(lambda (mrp-L-1) (format "%s" mrp-L-1))
                                    mrp-rtn " ")))
          (cond (insrtp (save-excursion (insert mrp-rtn)))
                (intrp (message mrp-rtn))))
      mrp-rtn)))
         
;;; ==============================
;;; :PREFIX "mrgnl-"
;;; :CHANGESET 1788
;;; :CREATED <Timestamp: #{2010-05-28T14:44:04-04:00Z}#{10215} - by MON KEY>
(defun mon-region-length (&optional insrtp intrp)
  "Return the length of active region.\n
Signal an error if `region-active-p' is null.\n
When optional arg INSRTP is non-nil or called-interactively with prefix arg
insert length of region at region-end.\n
When called-interactively and INSRTP is ommitted message the region length.\n
:EXAMPLE\n\n\(progn\n \(push-mark \(line-beginning-position\) nil t\)
  \(mon-region-length nil t\)\)\n
:SEE-ALSO `mon-region-unfill', `mon-region-indent-refill',
`mon-region-capitalize', `mon-region-reverse'.\n►►►"
  (interactive "P\np")
  (unless (region-active-p)
    (error ":FUNCTION `mon-region-length' -- there is no active region"))
  (let* ((mrgnl-rbe (- (region-end) (region-beginning)))
         ;;(mrgnl-frmt  (format ":REGION-LENGTH %d" mrgnl)))
         (mrgnl-frmt (format (concat ":FUNCTION `mon-region-length' " 
                                    "-- :REGION-LENGTH %d") mrgnl-rbe)))
    (cond ((and intrp (not insrtp)) (message mrgnl-frmt))
           (insrtp (unless (eq (point) (region-end))
                     (goto-char (region-end)))
                   (insert mrgnl-frmt))
           (t mrgnl-rbe))))
;;
;;; :TEST-ME (progn (push-mark (line-beginning-position) nil t)
;;;                    (mon-region-length nil t))

;;; ==============================
;;; :CHANGESET 2117
;;; :CREATED <Timestamp: #{2010-09-10T13:54:02-04:00Z}#{10365} - by MON KEY>
(defun mon-decode-coding-region-utf-8-unix (start end &optional insrtp)
  "Revert region with `decode-coding-region' with CODING-SYSTEM arg utf-8-unix.\n
:EXAMPLE\n\n
:ALIASED-BY 
:SEE-ALSO `mon-region-indent-refill', `mon-region-unfill'.\n►►►"
  (interactive "r\np")
  ;; :NOTE the backqouting brouhaha is to allow passing nil nil unreservedly
  ;; without signaling an error
  (apply #'decode-coding-region
         `(,@(or (and (use-region-p) `(,start ,end))
              (and (null start) (null end)
                   `(,(region-beginning) ,(region-beginning))))
           ,'utf-8-unix ,(unless insrtp t))))

;;; ==============================
;;; :TODO Needs a keybinding
;;; :CHANGESET 2233
;;; :CREATED <Timestamp: #{2010-11-06T17:41:59-04:00Z}#{10446} - by MON KEY>
(defun mon-region-indent-refill (start end &optional w-fill-prefix w-fill-column
                                       w-active-rgn intrp)
  "Indent and refill the region from START to END.\n
W-FILL-PREFIX a string or character with which to dynamically bind `fill-prefix'.\n
When W-FILL-PREFIX is a string it should satisfy `mon-string-not-null-nor-zerop'.
When W-FILL-PREFIX is `characterp' it is used as the second arg to `make-string'
with the LENGTH arg fixed at 3 and one whitespace char 32 appended.
Default is \"    \".\n
When called-interactively with a prefix or W-FILL-COLUMN is non-nil value
satisfying `natnump' its value is used to dynamically bind `fill-column'.
Default is 70.\n
W-ACTIVE-RGN when non-nil and START and END are null and `use-region-p'
returns non-nil use region from `region-beginning' to `region-end' else signal an error.
W-ACTIVE-RGN is non-nil with START and END satisfying `number-or-marker-p' this
arg is ignored.\n
:ALIASED-BY `mon-indent-refill-region'
:ALIASED-BY `mon-indent-region-refill'
:ALIASED-BY `mon-region-refill-indent'\n
:SEE-ALSO `indent-region', `mon-region-unfill', `indent-tabs-mode.\n►►►"
  (interactive "r\ni\nP\ni\np")
  (let ((fill-column (or w-fill-column 70))
        (fill-prefix (or (and (mon-string-not-null-nor-zerop w-fill-prefix)
                              w-fill-prefix)
                         (and (characterp w-fill-prefix)
                              (concat (make-string 3 w-fill-prefix) " "))
                         (make-string 4 32)))
        (mrrfl-rgn (or (and intrp (cons start end))
                       (and w-active-rgn
                            (number-or-marker-p start)
                            (number-or-marker-p end)
                            (cons start end))
                       (and w-active-rgn 
                            (and (or (and (null start) (null end))
                                     (and start (null end))
                                     (and (null start) end))
                                 (or (use-region-p)
                                     (error (concat ":FUNCTION `mon-region-indent-refill' "
                                                    "-- with optional W-ACTIVE-RGN non-nil and START END null "
                                                    "could not satisfy `use-region-p'")))
                                 (cons (region-beginning) (region-end))))
                       (or (and (null start) (null end)
                                (error (concat ":FUNCTION `mon-region-indent-refill' "
                                               "-- args START and END `null'")))
                           (and (or (and (not (number-or-marker-p start))
                                         (error (concat ":FUNCTION `mon-region-indent-refill' "
                                                        "-- START not `number-or-marker-p', got %S"
                                                        " type-of: %S")
                                                start (type-of start)))
                                    (and (not (number-or-marker-p end))
                                         (error (concat ":FUNCTION `mon-region-indent-refill' "
                                                        "-- END not `number-or-marker-p', got: %S "
                                                        " type-of: %S")
                                                end (type-of end)))))))))
    (save-excursion
      (indent-region (car mrrfl-rgn) (cdr mrrfl-rgn))
      (fill-region (car mrrfl-rgn) (cdr mrrfl-rgn)))))

;;; ==============================
(defun mon-region-unfill (start end)
  "Do the opposite of `fill-region'.\n
Stuff all paragraphs paragraphs in the current region into long lines.\n
:SEE-ALSO `mon-region-indent-refill', `mon-line-strings-indent-to-col',
`mon-line-indent-from-to-col', `mon-string-fill-to-col',
`mon-comment-divide->col'.\n►►►"
  (interactive "r")
  (let ((fill-column 9000))
    (fill-region start end)))

;;; ==============================
;; :PREFIX "mrcap-"
(defun mon-region-capitalize (start end &optional insrtp intrp)
  "Return capitalized string in region START END.\n
Return three valued property list with the format:\n
 (:REGION-CAPITAL   <String-Capitalized>
  :REGION-ORIGINAL  <STRING-UNCAPITALIZED>
  :REGION          (<REGION-START> . <REGION-END>))\n
When optional arg INSRTP is non-nil split line at END and insert return
value. Does not move point.\n
When called-interactively replace content of region with capitalized string.
Does not move point.\n
:EXAMPLE\n\n\(mon-region-capitalize \(line-beginning-position 3\) \(line-end-position 3\)\)\n
lowercase string aNd UPERCASE STRING\n
\(mon-region-capitalize-TEST\)\n
:ALIASED-BY `mon-capitalize-region'\n
:SEE-ALSO `capitalize', `capitalize-region', `mon-region-unfill',
`mon-region-length', `mon-region-reverse', `mon-upcase-commented-lines'
`mon-downcase-regexp-region', `mon-upcase-regexp-region'.\n►►►"
  (interactive "r\ni\np")
  (let ((mrcap-pre (mon-buffer-sub-no-prop start end)))
    (setq mrcap-pre `(:REGION-CAPITAL  ,(capitalize mrcap-pre)
                      :REGION-ORIGINAL ,mrcap-pre
                      :REGION (,start . ,end)))
    (cond (intrp (save-excursion
                   (capitalize-region start end)
                   (prin1 mrcap-pre)))
          (insrtp (save-excursion
                    (goto-char end)
                    (print mrcap-pre (current-buffer))
                    (delete-char 1)))
          (t mrcap-pre))))
;;
;;; :TEST-ME (mon-region-capitalize-TEST)

;;; ==============================
(defun mon-region-reverse (reg-begin reg-end &optional insrtp intrp)
  "Reverse the characters in the region.\n
When called-interactively insert the reversed as with princ.
When INSRTP is non-nil insert the reversed as with princ.
Insertion does not move point. Insertion is whitespace agnostic.\n
:ALIASED-BY `mon-region-reverse-chars'\n
:SEE-ALSO `mon-word-reverse-region', `mon-region-unfill',
`mon-region-capitalize', `reverse-region'.\n►►►"
  (interactive "r\ni\np")
  (let ((m-reg-rev 
         (apply 'concat 
                (reverse 
                 (split-string 
                  ;; :WAS (buffer-substring-no-properties reg-begin reg-end)
                  (mon-buffer-sub-no-prop reg-begin reg-end)
                  "")))))
    (cond (intrp (save-excursion 
                   (delete-region reg-begin reg-end)
                   (princ m-reg-rev (current-buffer))))
          (insrtp (save-excursion 
                    (delete-region reg-begin reg-end)
                    (prin1 m-reg-rev (current-buffer))))
          (t m-reg-rev))))

;;; ==============================
;;; :COURTESY Stefan Reichor <stefan@xsteve.at> :HIS xsteve-functions.el
(defun mon-wrap-selection (&optional front-arg rear-arg)
  "Wraps contents of region with a FRONT-ARG and REAR-ARG delimeter.\n
:PROMPT-FOR 
 Front Delimiter:  <- Delmiter for beginning of region
 Rear Delimiter:   <- Delmiter for end of region\n\n
:EXAMPLE\n
 Point/Mark of region contain: My cats breath smells like cat food
 Front's prompt is provided: |[
 Rear's prompt is provided:  ]|
 Return: |[My cats breath smells like catfood]|\n
:ALIASED-BY `mon-region-wrap'\n
:SEE-ALSO `mon-wrap-url', `mon-wrap-span', `mon-wrap-text', `mon-wrap-with'.\n►►►"
  (interactive)
  (let* ((in-front (or front-arg 
                       (read-string (concat ":FUNCTION `mon-wrap-selection' " 
                                            "-- front delimiter: "))))
         ;; (concat ":FUNCTION `mon-wrap-selection' " "-- front delimiter: ")
         (in-rear (or rear-arg (read-string (concat ":FUNCTION `mon-wrap-selection' " 
                                                    "-- front delimiter: ")))))
    (if mark-active
        (progn
          (save-excursion
            (goto-char (region-beginning))
            (insert in-front))
          (save-excursion
            (goto-char (region-end))
            (insert in-rear)))
      (insert in-front)
      (save-excursion
        (insert in-rear)))))

;;; ==============================
;;; :NOTE To remind us where we're going:
;;; (defun mon-wrap-artist-name () "" (interactive)
;;;   (mon-wrap-text "\\@:artist[" "]")
;;; :PREFIX "mwt-"
(defun mon-wrap-text (wrap-a wrap-b &optional insrtp intrp)
  "Return current word with the string args WRAP-A and WRAP-B.\n
When optional arg INSRTP is non-nil or called-interactively insert wrapped word
at point. Does not move point.\n
:EXAMPLE\n\n\(mon-wrap-text \"\\\\@:artist[\" \"]\"\)Some-Name\n
:SEE-ALSO `mon-wrap-selection', `mon-wrap-url', `mon-wrap-span',
`mon-wrap-with'.\n►►►"
  (interactive "i\ni\ni\np")
  (save-excursion
    (let (mwt-pnt1 mwt-pnt2 mwt-wrap-wrd)
      (if (or (and transient-mark-mode mark-active)
              (use-region-p))
          (progn (setq mwt-pnt1 (region-beginning)) (setq mwt-pnt2 (region-end)))
        (progn
          ;;(skip-chars-backward "-A-Za-z")
          (skip-chars-backward "-A-Za-z")
          (setq mwt-pnt1 (point))
          ;; (skip-chars-forward "-A-Za-z")
          (skip-chars-forward "-A-Za-z")
          (setq mwt-pnt2 (point))))
      (setq mwt-wrap-wrd (mon-buffer-sub-no-prop mwt-pnt1 mwt-pnt2))
      (if (or insrtp intrp)
          (progn
            (goto-char mwt-pnt2) (insert wrap-b)
            (goto-char mwt-pnt1) (insert wrap-a))
        (concat wrap-a mwt-wrap-wrd wrap-b)))) )
;;
;;; :TEST-ME (mon-wrap-text "\\@:artist[" "]")Some-Name
;;; :TEST-ME (mon-wrap-text "\\@:artist[" "]" t)Some-Name

;;; ==============================
(defun mon-wrap-with (front-wrap back-wrap &optional insrtp intrp)
  "Wrap the current word or region with FRONT-WRAP and BACK-WRAP.\n
:SEE-ALSO `mon-wrap-selection', `mon-wrap-url', `mon-wrap-span',
`mon-wrap-text', `mon-wrap-with'.\n►►►"
  (interactive "sEnter string for front-wrap:\nsEnter String for back-wrap: \ni\np")
  (mon-wrap-text front-wrap back-wrap insrtp intrp))

;;; ==============================
;;; :PREFIX "mssc-"
;;; :NOTE Fashioned after `mail-parse-comma-list':SEE lisp/mail/mail-utils.el 
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-21T14:18:57-04:00Z}#{10382} - by MON KEY>
(defun mon-region-split-commas (start end &optional insrtp intrp)
  "Split the buffer-region into a list of tokens separated by commas.\n
Whitespace before or after tokens is ignored, but whitespace within tokens is
kept.\n
When optional INSRTP is non-nil or called-interactively insert list of results
at point.\n
:EXAMPLE\n\n\(with-temp-buffer 
  \(save-excursion 
    \(insert \(mapconcat #'identity
                       '\(\"  aiss,\\taiss s,s,s siss, fms,\"
                         \"aiss,aiss\\ts,s\\r,s siss, fms,\\n\"
                         \"aiss,aiss\\fs,s,s siss, fms,\"\) \"\\n\"\)\)\)
  \(mon-region-split-commas \(mon-g2be -1 t\) \(mon-g2be 1 t\) \)\)\n
:ALIASED-BY `mon-string-split-commas'
:ALIASED-BY `mon-split-region-at-commas'\n
:SEE-ALSO `mon-string-csv-regexp', `mon-string-csv-rotate', `mon-string-spread',
`mon-string-replace-char', `mon-string-chop-spaces', `mon-string-splice-sep',
`mon-string-split-and-unquote', `mon-string-upto-index'.\n►►►"
  (interactive "r\np")
  (let ((mssc-wspc (concat (reverse *mon-whitespace-chars*) ""))
        mssc-gthr mssc-beg)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        (skip-chars-forward mssc-wspc)
        (while (not (eobp))
          (setq mssc-beg (point))
          (skip-chars-forward "^,")
          (skip-chars-backward mssc-wspc)
          (push (mon-buffer-sub-no-prop mssc-beg (point)) mssc-gthr)
          (skip-chars-forward "^,")
          (skip-chars-forward (concat "," mssc-wspc))
          (widen)))
      (and (consp mssc-gthr) 
           (setq mssc-gthr (nreverse mssc-gthr)))
      (if (or insrtp intrp)
          (prin1 mssc-gthr (current-buffer))
        mssc-gthr))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |
;; | (with-temp-buffer 
;; |   (save-excursion 
;; |     (insert (mapconcat #'identity
;; |                        '("  aiss,\taiss s,s,s siss, fms,"
;; |                          "aiss,aiss\ts,s\r,s siss, fms,\n"
;; |                          "aiss,aiss\fs,s,s siss, fms,") "\n")))
;; |   (mon-region-split-commas (buffer-end 0) (buffer-end 1)))
;; |
;; `----

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-21T14:27:09-04:00Z}#{09433} - by MON KEY>
(defun mon-sha1-region (start end &optional insrtp intrp)
  "Return the sha1sum for contents of region.\n
When INSRTP is non-nil or called-interactively insert sha1 on newline.\n
Does not move point.\n
:SEE-ALSO `sha1-region', `sha1-string'.\n►►►."
  (interactive "r\ni\np")
  (eval-when-compile (require 'sha1))
  (let ((sha1-r (sha1-region start end)))
    ;; (sha1-string (buffer-substring-no-properties start end))))
    (if (or insrtp intrp)
        (save-excursion (newline) (princ sha1-r (current-buffer)))
        sha1-r)))

;;; ==============================
(provide 'mon-region-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-region-utils.el ends here
;;; EOF
