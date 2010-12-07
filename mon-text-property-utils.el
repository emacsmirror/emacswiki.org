;;; mon-text-property-utils.el --- extensions for text-properties/overlays
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-text-property-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-09-04T12:51:48-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, emacs, extensions, display,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-text-property-utils provides functions for working manipulating text properties
;;
;; FUNCTIONS:►►►
;; `mon-get-text-property-remove-all', `mon-get-next-face-property-change',
;; `mon-get-next-face-property-change-if', `mon-get-text-properties-region',
;; `mon-get-text-properties-print', `mon-get-text-properties-read-temp',
;; `mon-get-text-properties-elisp-string-pp',
;; `mon-get-text-properties-elisp-string',
;; `mon-get-text-properties-parse-prop-val-type-chk',
;; `mon-get-text-properties-parse-buffer', `mon-get-text-properties-parse-sym',
;; `mon-get-text-properties-parse-buffer-or-sym',
;; `mon-get-text-properties-map-ranges',
;; `mon-get-text-properties-map-ranges-string', `mon-get-text-property-bounds',
;; `mon-list-all-properties-in-buffer',
;; `mon-get-text-properties-region-to-kill-ring',
;; `mon-nuke-text-properties-buffer', `mon-remove-text-property',
;; `mon-get-face-at-posn', `mon-get-face-at-point',
;; `mon-remove-single-text-property', `mon-nuke-text-properties-region',
;; `mon-get-text-properties-category', `mon-nuke-overlay-buffer',
;; `mon-get-overlays-map-props', `mon-get-overlays-region',
;; `mon-get-overlays-region-map-props', `mon-get-overlays-buffer',
;; `mon-insert-w-text-properties', 
;; `mon-get-text-properties-region-prop-list', 
;; `mon-get-text-properties-region-prop', `mon-search-text-properties-prop',
;; `%mon-set-buffer-substring-no-properties', `%mon-set-buffer-substring',
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
;; ALIASED/ADVISED/SUBST'D:
;; :NOTE Aliases defined in :FILE mon-aliases.el
;;
;;  <PREFIX>-<QUALIFIED>                           <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-remove-text-with-property'             -> `mon-get-text-property-remove-all'
;; `mon-remove-text-properties-region-all'     -> `mon-nuke-text-properties-region'
;; `mon-remove-all-text-properties-region'     -> `mon-nuke-text-properties-region'
;; `mon-get-text-properties-region->kill-ring' -> `mon-get-text-properties-region-to-kill-ring'
;; `mon-kill-ring-save-w-props'                -> `mon-get-text-properties-region-to-kill-ring'
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-test-props'                            -> `mon-get-text-properties-category'
;; `mon-kill-ring-save-w-props'                -> `mon-get-text-properties-region-to-kill-ring'
;;
;; MOVED:
;; `mon-list-all-properties-in-buffer'               <- mon-utils.el
;; `mon-get-text-properties-region-to-kill-ring'     <- mon-utils.el
;; `mon-nuke-text-properties-buffer'                 <- mon-utils.el
;; `mon-remove-text-property'                        <- mon-utils.el 
;; `mon-get-face-at-posn'                            <- mon-utils.el
;; `mon-get-face-at-point'                           <- mon-utils.el
;; `mon-remove-single-text-property'                 <- mon-utils.el
;; `mon-nuke-text-properties-region'                 <- mon-utils.el
;; `mon-get-text-properties-category'                <- mon-utils.el
;; `mon-nuke-overlay-buffer'                         <- mon-utils.el
;; `mon-get-next-face-property-change'               <- mon-doc-help-utils.el
;; `mon-get-next-face-property-change-if'            <- mon-doc-help-utils.el
;; `mon-get-text-properties-region'                  <- mon-doc-help-utils.el
;; `mon-get-text-properties-print'                   <- mon-doc-help-utils.el
;; `mon-get-text-properties-read-temp'               <- mon-doc-help-utils.el
;; `mon-get-text-properties-elisp-string-pp'         <- mon-doc-help-utils.el 
;; `mon-get-text-properties-elisp-string'            <- mon-doc-help-utils.el
;; `mon-get-text-properties-parse-prop-val-type-chk' <- mon-doc-help-utils.el
;; `mon-get-text-properties-parse-buffer'            <- mon-doc-help-utils.el
;; `mon-get-text-properties-parse-sym'               <- mon-doc-help-utils.el
;; `mon-get-text-properties-parse-buffer-or-sym'     <- mon-doc-help-utils.el
;; `mon-get-text-properties-map-ranges'              <- mon-doc-help-utils.el
;; `mon-get-text-properties-map-ranges-string'       <- mon-doc-help-utils.el
;; `mon-get-text-property-bounds'                    <- mon-doc-help-utils.el
;; `mon-set-text-properies-region'                   -> mon-macs.el
;; `mon-get-face-at-posn'                            -> mon-macs.el
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
;; URL: http://www.emacswiki.org/emacs/mon-text-property-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-text-property-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-09-04T12:51:48-04:00Z}#{10356} - by MON KEY>
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

 
(eval-when-compile (require 'cl)
                   (require 'edebug))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

(require 'mon-plist-utils)

;;; ==============================
;;; :COURTESY slime.el :WAS `slime-insert-propertized'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-29T13:53:56-04:00Z}#{10393} - by MON KEY>
(defsubst mon-insert-w-text-properties (props &rest args)
  "Insert ARGS adding text-property PROPS to the inserted text.\n
:SEE-ALSO `mon-set-text-properies-region'.\n►►►"
  (mon-set-text-properies-region props (apply #'insert args)))

;;; ==============================
;;; :NOTE This is a verbatim copy of `cl-set-buffer-substring' 
;;; :FILE lisp/emacs-lisp/cl.el 
;;; The byte compiler whines about CL runtime nonsense (even though there is
;;; nothing CL related in the function.
;;; :CHANGESET 2283
;;; :CREATED <Timestamp: #{2010-11-05T16:19:14-04:00Z}#{10445} - by MON KEY>
(defun %mon-set-buffer-substring (start end val)
  "Helper function for `defsetf'.\n
Used with `%mon-set-buffer-substring-no-properties' to make
`buffer-substring-no-properties' `setf'able.\n
:EXAMPLE\n\n\(mon-with-inhibit-buffer-read-only
     \(%mon-set-buffer-substring 
      \(line-beginning-position 3\) \(line-end-position 3\)
      \(propertize \"I'm a bubba\" 'font-lock-face 'bold\)\)\)\n
I'm not a bubba\n
:NOTE this is a verbatim copy of `cl-set-buffer-substring', required so
byte-compiler will shut its yap about runtime warnings.\n
:SEE-ALSO `mon-set-buffer-substring-no-properties-TEST', `mon-insert-w-text-properties'.\n►►►" 
  (save-excursion (delete-region start end)
		  (goto-char start)
		  (insert val)
		  val))

;;; ==============================
;;; :NOTE The defsetf for `buffer-substring-no-properties' ought to exist already in 
;;; :FILE lisp/emacs-lisp/cl-macs.el  but it isn't there yet. 
;;;  If it should ever become available a function w/ a name similiar to 
;;; `cl-set-buffer-substring-no-properties' will most likely appear in 
;;; :FILE lisp/emacs-lisp/cl.el somewhere near `cl-set-buffer-substring'.
;;;  Until then, we need this.
;;; :CHANGESET 2283
;;; :CREATED <Timestamp: #{2010-11-05T16:19:18-04:00Z}#{10445} - by MON KEY>
(defun %mon-set-buffer-substring-no-properties (start end val)  
  "Helper for `defsetf' to make `buffer-substring-no-properties' `setf'able.\n
:EXAMPLE\n\n\(mon-set-buffer-substring-no-properties-TEST\)\n
:SEE-ALSO `%mon-set-buffer-substring', `cl-set-buffer-substring', `set-text-properties'.\n►►►"
  (set-text-properties 0 (length val) nil val)
  ;; (cl-set-buffer-substring start end val))
  (%mon-set-buffer-substring start end val))
;;
;; (defsetf buffer-substring-no-properties mon-set-buffer-substring-no-properties)
(defsetf buffer-substring-no-properties %mon-set-buffer-substring-no-properties)

;; (add-text-properties start end '(invisible t))

;;; ==============================
;;; :COURTESY  ../emacs/lisp/font-lock.el 
;;; :NOTE For completeness: this is to `remove-text-properties' as
;;; `put-text-property' ; is to `add-text-properties', etc. Included therein but
;;; commented out by SM as 'Additional text property functions' these may
;;; eventually become C builtins.
;;; For consistency: maybe this should be called `remove-single-property' like
;;; `next-single-property-change' (not `next-single-text-property-change'), etc.
;;; :WAS `remove-text-property'        -> ../emacs/lisp/font-lock.el
(defun mon-remove-text-property (start end property &optional object)
  "Remove a property from text from START to END.\n
Argument PROPERTY is the property to remove.\n
Optional argument OBJECT is the string or buffer containing the text.\n
Return t if the property was actually removed, nil otherwise.\n
:CALLED-BY `mon-remove-single-text-property'\n
:SEE-ALSO `mon-remove-single-text-property', `remove-text-properties',
`mon-nuke-text-properties-region', `add-text-properties', `put-text-property',
`next-single-property-change', `mon-list-all-properties-in-buffer',
`mon-nuke-overlay-buffer', `mon-help-text-property-functions', 
`mon-help-text-property-functions-ext'.\n►►►"
  (remove-text-properties start end (list property) object))
;;
;;; :WAS `remove-single-text-property' -> ../emacs/lisp/font-lock.el
(defun mon-remove-single-text-property (start end prop value &optional object)
 "Remove a specific property value from text from START to END.\n
Arguments PROP and VALUE specify the property and value to remove.\n
The resulting property values are not equal to VALUE nor lists containing VALUE.\n
Optional argument OBJECT is the string or buffer containing the text.\n
:SEE-ALSO `remove-text-property', `mon-nuke-text-properties-region',
`mon-nuke-overlay-buffer', `add-text-properties', `put-text-property',
`next-single-property-change', `mon-list-all-properties-in-buffer',
`mon-help-text-property-functions', `mon-help-text-property-functions-ext'.\n►►►"
 (let ((start (text-property-not-all start end prop nil object)) next prev)
   (while start
     (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
     (cond ((and (symbolp prev) (eq value prev))
	     (mon-remove-text-property start next prop object))
	    ((and (listp prev) (memq value prev))
	     (let ((new (delq value prev)))
	       (cond ((null new)
		      (mon-remove-text-property start next prop object))
		     ((= (length new) 1)
		      (put-text-property start next prop (car new) object))
		     (t
		      (put-text-property start next prop new object))))))
     (setq start (text-property-not-all next end prop nil object)))))

;;; ==============================
;;; :PREFIX "mntpr-"
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
(defun mon-nuke-text-properties-region (beg end)
  "Eliminate all text properties in current buffer from BEG to END.\n
:NOTE Only removes text properties, does not remove overlays.\n
:ALIASED-BY `mon-remove-text-properties-region-all' 
:ALIASED-BY `mon-remove-all-text-properties-region'\n
:SEE-ALSO `remove-text-property', `mon-remove-single-text-property',
`mon-nuke-overlay-buffer', `add-text-properties', `put-text-property',
`next-single-property-change', `mon-list-all-properties-in-buffer',
`mon-help-text-property-functions'.\n►►►"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (mon-g2be -1)
      (mon-with-inhibit-buffer-read-only
          (while (not (eobp))
            (let (;; (mntpr-cb (current-buffer))
                  (mntpr-pl (text-properties-at (point)))
                  (mntpr-nxt-chng (or (next-property-change (point) (current-buffer))
                                      (mon-g2be 1 t))))
              (remove-text-properties (point) mntpr-nxt-chng mntpr-pl (current-buffer))
              (goto-char mntpr-nxt-chng)))))))

;; ;;; ==============================
;; ;;; :RENAMED `mon-what-face' -> `mon-get-face-at-point'
;; ;;; :COURTESY Miles Bader :SOURCE (gnus.emacs.help)
;; ;;; :CHANGESET 1776 <Timestamp: #{2010-05-26T18:29:18-04:00Z}#{10213} - by MON KEY>
(defun mon-get-face-at-point (face-psn &optional describe-it)
  "Return the font-lock face information at FACE-PSN or point.\n
When optional arg DESCRIBE-IT is non-nil or called-interactively with prefix-arg
describe face.\n
:NOTE When more than one face is present at point return a list,
e.g. situtations where return value is:\n
 \(font-lock-constant-face font-lock-doc-face\)\n
and DESCRIBE-IT is non-nil describe the face at car of list.\n
:SEE-ALSO `mon-get-face-at-posn', `mon-help-faces', `mon-help-faces-basic',
`mon-help-faces-themes', `read-face-name'.\n►►►"
  (interactive "d\nP")
  (let ((mgfap-face 
         ;; :NOTE `get-char-property-and-overlay'
         (or (get-char-property (or face-psn (point)) 'read-face-name)
             (get-char-property (or face-psn (point)) 'face)
             (get-char-property (or face-psn (point)) 'font-lock-face))))
    (if mgfap-face
        (if describe-it
            (describe-face 
             (if (consp mgfap-face) (car mgfap-face) mgfap-face))
          ;;(message "Face: %s" mgfap-face))
          (message (concat ":FUNCTION `mon-get-face-at-point' " 
                           "-- Face: %s") mgfap-face)
          ;;(message "No face at %d" (or face-psn (point)))
          (message (concat ":FUNCTION `mon-get-face-at-point' " 
                           "-- no face at buffer position: %d") 
                   (or face-psn (point)))))))

;;; ==============================
;;; :COURTESY slime.el :WAS `slime-get-region-properties'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-29T14:25:49-04:00Z}#{10393} - by MON KEY>
(defun mon-get-text-properties-region-prop (prop start end)
  "Get occurences of prop in region from START to END.\n
Return a list of properties found. Return value has the form:\n
 ( ( <PROP> ( <PROP-START-PSN> . <PROP-END-PSN> ))* )\n
:EXAMPLE\n\n
:SEE-ALSO `mon-get-text-properties-region-prop-list', `mon-get-text-properties-region'.\n►►►"
  (loop for position = (if (get-text-property start prop)
                           start
                           (next-single-property-change start prop))
        then (next-single-property-change position prop)
        while (and position  (<= position end))
        when (get-text-property position prop)
        collect `(,(get-text-property position prop) 
                  (,position . ,(next-single-property-change position prop) ))))

;;; ==============================
;;; :COURTESY slime.el  :WAS `slime-get-properties'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-29T14:29:47-04:00Z}#{10393} - by MON KEY>
(defun mon-get-text-properties-region-prop-list (prop &optional no-region-from-psn)
  "Find region occurences of PROP. Return a list of properties found.\n
When the region is active (e.g. `use-region-p' is non-nil) and otpional arg
NO-REGION-FROM-PSN is omitted find occurences of PROP in region start to end as
if by `mon-get-text-properties-region-prop'.
When region is not active find PROP as if by `get-text-property' from `point'
unless optional arg NO-REGION-FROM-PSN is non-nil in which case text-properties
are located beginning with NO-REGION-FROM-PSN .\n
Return value has one of the the following two forms:\n
 ;; `use-region-p' non-nil and NO-REGION-FROM-PSN ommitted
 ( ( <PROP> ( <PROP-START-PSN> . <PROP-END-PSN> ))* )\n
 ;; from `point' or NO-REGION-FROM-PSN non-nil\n
 ( <PROP> ( <PROP-START-PSN> . <PROP-END-PSN> ))\n
:EXAMPLE\n\n
:SEE-ALSO `mon-get-text-properties-region-prop', `mon-get-text-properties-region' .\n►►►"
  (if (and (use-region-p) (not no-region-from-psn))
      (mon-get-text-properties-region-prop prop (region-beginning) (region-end))
    (let* ((frm-psn (or no-region-from-psn (point)))
           (mgtprpl-val (get-text-property frm-psn prop)))
      (when mgtprpl-val 
        `(,mgtprpl-val (,frm-psn . ,(next-single-property-change frm-psn prop)))))))

;;; ==============================
;;; :COURTESY slime.el :WAS `slime-search-property'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-29T15:00:42-04:00Z}#{10393} - by MON KEY>
(defun mon-search-text-properties-prop (prop &optional backward prop-value-fn)
  "Search the next text range where PROP is non-nil.\n
Return the value of PROP.
If BACKWARD is non-nil, search backward.
If PROP-VALUE-FN is non-nil use it to extract PROP's value.
:SEE-ALSO .\n►►►"
  (let ((next-candidate (if backward 
                            #'previous-single-char-property-change
                          #'next-single-char-property-change))
        (prop-value-fn  (or prop-value-fn
                            #'(lambda ()
                                (get-text-property (point) prop))))
        (start (point))
        prop-value)
    (while (progn 
             (goto-char (funcall next-candidate (point) prop))
             (not (or (setq prop-value (funcall prop-value-fn)) 
                      (eobp) 
                      (bobp)))))
    (cond (prop-value)
          (t (goto-char start) nil))))

;;; ==============================
;;: :PREFIX  "mgtprtkr-"str
;;; :CREATED: <Timestamp: #{2009-10-11T08:44:59-04:00Z}#{09417} - by MON KEY>
(defun mon-get-text-properties-region-to-kill-ring (start end &optional no-strip)
  "Copy region _with_ text-properties to kill-ring.\n
If a leading `#' is present in string strip it.\n
When NO-STRIP in non-nil or called-interactively with prefix arg do not strip
leading `#'.\n
:NOTE Function is yank-handler agnostic w/re to 2nd optional arg of `kill-new'.\n
:ALIASED-BY `mon-get-text-properties-region->kill-ring'
:ALIASED-BY `mon-kill-ring-save-w-props'\n
:SEE-ALSO `mon-get-text-properties-region',`mon-line-test-content',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer',
`mon-nuke-text-properties-region', `mon-remove-text-property',
`mon-remove-single-text-property', `mon-help-text-property-functions',
`mon-help-text-property-function-ext'.\n►►►"
  (interactive "r\nP")  
  (let (mgtprtkr-str) 
    (setq mgtprtkr-str (format "%S" (buffer-substring start end)))
    (kill-new 
     (substring mgtprtkr-str 
                (if no-strip 
                    0
                  (if (= (string-match-p "^#" mgtprtkr-str) 0)
                      1 
                    0))))))

;;; ==============================

;;; ==============================
;;; :RENAMED `mon-test-props' -> `mon-get-text-properties-category'
;;; :MODIFICATIONS <Timestamp: #{2010-09-04T13:11:28-04:00Z}#{10356} - by MON KEY>
(defun mon-get-text-properties-category ()
  "Test for category text-properties-at point.\n
:CALLED-BY `mon-view-help-source'\n
:SEE-ALSO `mon-get-text-properties-category', `mon-view-help-source',
`mon-line-test-content'.\n►►►"
  (let* ((to-view ((lambda () (text-properties-at (point)))))
	 (my-props `(,@to-view))
	 (prop-value (plist-get my-props 'category)))
    prop-value))

;;; (declare-function mon-plist-keys "mon-plist-utils"
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;;; :NOTE Keep with `mon-nuke-text-properties-buffer'
;;; :CHANGED `set-buffer' -> `with-current-buffer' 
;;; :CHANGED `delete-duplicates' -> `delete-dups'
;;; :ADDED (&optional start-range end-range buffer) :WAS (buffer)
;;; :MODIFICATIONS <Timestamp: #{2010-01-11T23:39:18-05:00Z}#{10021} - by MON KEY>
(defun mon-list-all-properties-in-buffer (&optional start-range end-range buffer)
  "List text-properties in current-buffer.\n
When BUFFER is non-nil list its text-properties instead.\n
:EXAMPLE\n\n\(mon-list-all-properties-in-buffer\)\n
:SEE-ALSO `mon-nuke-text-properties-buffer', `mon-plist-keys',
`mon-plist-remove', `mon-help-plist-functions',
`mon-help-text-property-functions'.\n►►►"
  (save-excursion
    (with-current-buffer 
        (if buffer (get-buffer buffer) (current-buffer))
      ;; :NOTE Don't remove commented `delete-duplicates' version below.
      ;;      `delete-dups' uses `equal' maybe we want to use CL features later.
      ;; :WAS (delete-duplicates 
      ;;       (loop for i 
      ;;        from (or start-range (point-min)) 
      ;;        to (or end-range (point-max))
      ;;       (delete-duplicates (mon-plist-keys (text-properties-at i nil))))))))
      ;;; (delete-duplicates 
      ;;;  (loop for i 
      ;;;     from (or start-range (point-min)) 
      ;;;     to (or end-range (point-max))
      ;;;     nconc (delete-duplicates (mon-plist-keys (text-properties-at i nil))))))))      
      (delete-dups
       ;;(loop for i from (or start-range (point-min)) to (or end-range (point-max)) ;; (mon-g2be 1 t)
       (loop for i from (or start-range (mon-g2be -1 t)) to (or end-range (mon-g2be 1 t))
             nconc (delete-dups (mon-plist-keys (text-properties-at i (current-buffer)))))))))
;;
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `remove-all-properties'
;;; :NOTE W/ significant modifications.
(defun mon-nuke-text-properties-buffer (&optional w-buffer)
  "Remove text-properites in `current-buffer'.\n
When optional arg W-BUFFER is non-nil remove text-properites in that buffer instead.\n
:EXAMPLE\n\n\(mon-with-inhibit-buffer-read-only \(mon-nuke-text-properties-buffer\)\)\n
:NOTE This won't always remove font-locked faces in modes which frob font-lock
faces at a low level e.g. `emacs-lisp-mode'.\n
:SEE-ALSO `mon-remove-text-property', `mon-remove-single-text-property',
`mon-nuke-text-properties-region', `mon-nuke-overlay-buffer',
`remove-list-of-text-properties', `mon-list-all-properties-in-buffer',
`mon-help-text-property-functions', `font-lock-defontify'.\n►►►"
  (interactive (list (and current-prefix-arg
                          (read-buffer 
                           (concat ":FUNCTION `mon-nuke-text-properties-buffer' "
                                   "-- buffer: " )
                           (current-buffer) 1))))
  (with-current-buffer (or (and w-buffer (get-buffer w-buffer)) (current-buffer))
    ;; (font-lock-defontify)                               ;; won't work
    ;; (set (make-local-variable 'font-lock-defaults) nil) ;; won't work
    (remove-list-of-text-properties (mon-g2be -1 t) (mon-g2be 1 t) 
                                    ;; (nconc '(font-lock-face) ;; won't work
                                    (mon-list-all-properties-in-buffer 
                                     (mon-g2be -1 t) (mon-g2be 1 t) (current-buffer))
                                    (current-buffer))))

;;; ==============================
;;; :PREFIX "mgnfpc-"
;;; :CREATED <Timestamp: #{2010-02-26T19:31:23-05:00Z}#{10086} - by MON KEY>
(defun mon-get-next-face-property-change (face-prop-val &optional from-posn)
  "Search for `next-single-property-change' with property 'face.\n
If face property has FACE-PROP-VAL push t onto list of return value.
Return a two element list formatted as:\n
 \(position \(t|nil face-prop-vals\)\)\n
When FROM-POSN is non-nil search for next face property change FROM-POSN.
Default is to search from point.\n
:EXAMPLE\n\n\(mon-get-next-face-property-change 'button \(mon-g2be -1 t\)\)\n
\(mon-get-next-face-property-change 
 'help-argument-name \(line-beginning-position -11\)\)\n
\(mon-get-next-face-property-change 'button \(point\)\)\n
:ALIASED-BY `mon-help-face-next-property-change'\n
:SEE-ALSO `mon-get-text-properties-region-to-kill-ring', `mon-get-text-properties-category',
`mon-line-test-content', `mon-get-next-face-property-change',
`mon-get-next-face-property-change-if', `mon-get-all-face-property-change'
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer',
`mon-nuke-text-properties-region', `mon-remove-text-property',
`mon-remove-single-text-property'.\n►►►"
  (let (mgnfpc-gthr)
     (setq mgnfpc-gthr (next-single-property-change (or from-posn (point)) 'face))
     (when mgnfpc-gthr
       (if (consp (get-text-property mgnfpc-gthr 'face))
           (setq mgnfpc-gthr `(,mgnfpc-gthr  ,(get-text-property mgnfpc-gthr 'face)))
           (setq mgnfpc-gthr `(,mgnfpc-gthr  (,(get-text-property mgnfpc-gthr 'face))))))
     (when (and mgnfpc-gthr (cadr mgnfpc-gthr))
       (if (memq face-prop-val (cadr mgnfpc-gthr))
           (push t (cadr mgnfpc-gthr))
           (push nil (cadr mgnfpc-gthr))))
     mgnfpc-gthr))

;;; ==============================
;;; :PREFIX "mgnfpci-"
;;; :CREATED <Timestamp: #{2010-02-27T19:58:23-05:00Z}#{10087} - by MON KEY>
(defun mon-get-next-face-property-change-if (test-face-symbol test-at-posn)
  "Test if the face we're looking for is at test-at-posn.\n
TEST-FACE-SYMBOL symbol naming a face to test.\n
TEST-AT-POSN is a buffer position to examine.\n
:SEE-ALSO `mon-get-text-properties-region-to-kill-ring',
`mon-get-text-properties-category', `mon-line-test-content',
`mon-get-next-face-property-change', `mon-get-next-face-property-change-if',
`mon-get-all-face-property-change' `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer', `mon-nuke-text-properties-region',
`mon-remove-text-property', `mon-remove-single-text-property'.\n►►►"
  (let ((mgnfpci-prp (plist-get (text-properties-at test-at-posn) 'face)))
    (when mgnfpci-prp 
      (cond ((consp mgnfpci-prp) (memq test-face-symbol mgnfpci-prp))
            (t (eq test-face-symbol mgnfpci-prp))))))
;;
;;; (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-comment-face)
;;; :TEST-ME (mon-get-next-face-property-change-if 'font-lock-comment-face (point)));;

;; :TODO unfinished test case
;; (with-current-buffer (generate-new-buffer-name 
;;                       (mapconcat 'number-to-string 
;;                                  (mon-list-nshuffle
;;                                   (number-sequence 0 9 (aref [1 2 3] (random 3))))
;;                                  "")
;;                       (mon-get-all-face-property-change)
;;   (emacs-lisp-mode)
;;   (save-excursion (insert ";;\n ()\n"))
;;   (with-syntax-table emacs-lisp-mode-syntax-table 
;;     (mon-get-next-face-property-change-if 'font-lock-comment-face (point))))
    
                    


;;; ==============================
;;; :PREFIX "mgtpr-" 
;;; :CREATED <Timestamp: #{2010-03-05T17:35:32-05:00Z}#{10095} - by MON KEY>
(defun mon-get-text-properties-region (start end)
  "Return region as a two elt list string and strings text properties.\n
:EXAMPLE\n\n\(let \(\(sbr \(save-excursion (goto-char (buffer-end 0))
             \(search-forward-regexp \"\(mon.*\)$\" nil t\)\)\)\)
  \(setq sbr `\(,\(match-beginning 0\) . ,\(match-end 0\)\)\)
  \(mon-get-text-properties-region \(car sbr\) \(cdr sbr\)\)\)\n
:NOTE Indexes are into string not buffer as with return value of:\n
 `mon-get-text-properties-print', `mon-get-text-properties-read-temp'\n
:SEE-ALSO `mon-get-text-properties-region-to-kill-ring'.\n►►►"
  (interactive "r\np")
  (let (mgtpr-bfr-str mgtpr-tp-lst) 
    (setq mgtpr-bfr-str (substring (format "%S" (buffer-substring start end)) 1))
    (setq mgtpr-bfr-str (car (read-from-string mgtpr-bfr-str)))
    (setq mgtpr-bfr-str `(,(car mgtpr-bfr-str) ,(cdr mgtpr-bfr-str)))
    (setq mgtpr-tp-lst (substring (format "%S" (cdr mgtpr-bfr-str)) 1 -1))
    (setq mgtpr-tp-lst (replace-regexp-in-string " ?\\([0-9]+ [0-9]+\\( (\\)\\)" ")(\\1" mgtpr-tp-lst t))
    (setq mgtpr-tp-lst (substring mgtpr-tp-lst 1))
    (setq mgtpr-tp-lst (concat "(" (substring mgtpr-tp-lst 1) ")"))
    ;; :NOTE Is there a better way to do this than `read-from-string' or at
    ;; least read with obarray elsewhere?
    (setq mgtpr-tp-lst (list (car (read-from-string mgtpr-tp-lst))))
    (setcdr mgtpr-bfr-str mgtpr-tp-lst)
    mgtpr-bfr-str))
;;
;;; :TEST-ME (let ((sfr (save-excursion (search-forward-regexp "(defun.*$"))))
;;;               (setq sfr `(,(match-beginning 0). ,(match-end 0)))
;;;               (mon-get-text-properties-region (car sfr) (cdr sfr)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-02T20:08:34-05:00Z}#{10093} - by MON KEY>
(defun mon-get-text-properties-print (start end tp-buff &optional intrp)
  "Return buffer-string START END with text-properties.\n
TP-BUFF is a buffer name to print to as with prin1.\n
When called-interactively insert at point. Moves point.\n
:SEE-ALSO `mon-get-text-properties-region', `mon-get-text-properties-print',
`mon-get-text-properties-read-temp', `mon-get-text-properties-elisp-string',
`mon-get-text-properties-elisp-string-pp',
`mon-get-text-properties-region-to-kill-ring'.\n►►►"
  (interactive "r\ni\np")
  (let* (mgtpfs-get
         (standard-output mgtpfs-get)
         mgtpfs)
    (setq mgtpfs (buffer-substring start end))
    (setq mgtpfs-get (prin1 mgtpfs mgtpfs-get))
    (if intrp 
        (prin1 mgtpfs-get (current-buffer))
        (prin1 mgtpfs-get tp-buff))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-02T20:08:44-05:00Z}#{10093} - by MON KEY>
(defun mon-get-text-properties-read-temp (&optional tp-buff)
  "Read list from `mon-get-text-properties-print' and strip leading #.\n
The car of return value is a new list formulated as with `read-from-string'.
The cdr is a list of index pairs and text-propery prop/val pairs e.g.:\n
 \(idx1 idx2 \(p1 p1-val p2 p2-val p3-val \(p3-lv1 p3-lv2 p3-lv3\)\)
  ;; { ... lots more here ... } 
  idx3 idx4 \(p1 p1-val p2 p2-val p3-val \(p3-lv1 p3-lv2 p3-lv3\)\)\)\n
When non-nil optional arg TP-BUFF names a buffer as required by
`mon-get-text-properties-elisp-string'.\n
:SEE-ALSO `mon-get-text-properties-region', `mon-get-text-properties-print',
`mon-get-text-properties-read-temp', `mon-get-text-properties-elisp-string',
`mon-get-text-properties-elisp-string-pp'.\n►►►"
  (let ((mgtprt-new 
         (if tp-buff tp-buff 
           (get-buffer-create "*MGTPRT-NEW*")))
        re-str 
        str-props)
    (with-current-buffer mgtprt-new
      (mon-g2be -1) ;; (goto-char (buffer-end 0))
      (delete-char 1)
      (setq re-str (read-from-string (car (sexp-at-point))))
      (setq str-props (cdr (sexp-at-point)))
      (setq re-str `(,re-str ,str-props)))
    (unless tp-buff (kill-buffer mgtprt-new))
    re-str))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-02T20:07:17-05:00Z}#{10093} - by MON KEY>
(defun mon-get-text-properties-elisp-string-pp (syn-list split-buff)
  "Pretty print the string and text property list extracted with
`mon-get-text-properties-elisp-string'.\n
:SEE-ALSO `mon-get-text-properties-region', `mon-get-text-properties-print',
`mon-get-text-properties-read-temp', `mon-get-text-properties-elisp-string'.\n►►►"
  (let* ((mgppespp-split (buffer-name split-buff))
         (mgppespp-buf2
          (concat 
           (substring mgppespp-split 0 (1- (length mgppespp-split))) "-STRING*")) 
         (mgppespp-syn-list
          (concat "(" 
                  (replace-regexp-in-string ") " ")) (" 
                                            (format "%S" (cadr syn-list)))
               ")"))
         chck-syn-list)
    (with-temp-buffer 
      (princ mgppespp-syn-list (current-buffer))
      (pp-buffer)
      (setq mgppespp-syn-list 
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (with-current-buffer split-buff
      (erase-buffer)
      (save-excursion (princ mgppespp-syn-list (current-buffer)))
      (princ (format ";; :IN-BUFFER %s\n;;\n" mgppespp-buf2) (current-buffer))
      (emacs-lisp-mode))
    ;; (mon-get-all-face-property-change 'font-lock-constant-face (buffer-end 0))
    (get-buffer-create mgppespp-buf2)
    (with-current-buffer mgppespp-buf2
      (prin1 (caar syn-list) (current-buffer))
       (emacs-lisp-mode))
    (display-buffer split-buff t)
    (display-buffer mgppespp-buf2 t)))
    
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-02T20:07:24-05:00Z}#{10093} - by MON KEY>
(defun mon-get-text-properties-elisp-string (&optional some-el-string)
  "Extract the text properties from the elisp SOME-EL-STRING.\n
:EXAMPLE\n\n\(mon-get-text-properties-elisp-string
 \(documentation 'mon-help-mon-help\)\)\n
\(mon-get-text-properties-elisp-string *mon-help-reference-keys*\)\n
:SEE-ALSO `mon-get-text-properties-region', `mon-get-text-properties-print',
`mon-get-text-properties-read-temp', `mon-get-text-properties-elisp-string',
`mon-get-text-properties-elisp-string-pp'.\n►►►"
  (let ((mgtpfes-buf (get-buffer-create "*MGTPFES*"))
        mgtpes-buf2
        mgtpfes-rd)
    (unless (stringp some-el-string)
      (error (concat ":FUNCTION `mon-get-text-properties-elisp-string' "
                     "-- arg SOME-EL-STRING is not")))
    (with-current-buffer mgtpfes-buf (erase-buffer))
    (with-temp-buffer 
      (save-excursion (print some-el-string (current-buffer)))
    (emacs-lisp-mode)
    (font-lock-fontify-syntactically-region  (buffer-end 0) (buffer-end 1))
    (font-lock-fontify-buffer)
    ;; (current-buffer)
    (mon-get-text-properties-print (buffer-end 0) (buffer-end 1) mgtpfes-buf))
    ;; (substring mgtpfes-buf 0 (1- (length mgtpfes-buf))) "-STRING*"))
    (setq mgtpfes-rd (mon-get-text-properties-read-temp mgtpfes-buf))
    (mon-get-text-properties-elisp-string-pp mgtpfes-rd mgtpfes-buf)))
;;
;;; :TEST-ME (mon-get-text-properties-elisp-string *mon-help-reference-keys*)
;;; :TEST-ME (mon-get-text-properties-elisp-string (documentation 'mon-help-mon-help))

;;; ==============================
;;; :TODO This should be generalized.
;;; :CREATED <Timestamp: #{2010-03-05T19:07:34-05:00Z}#{10096} - by MON KEY>
(defun mon-get-text-properties-parse-prop-val-type-chk (prop-val)
  "Check that PROP-VAL's type is suitable.\n
Return eq, eql, equal depending on type of PROP-VAL.\n
Signal an error if PROP-VAL is not of the type:\n
 string, integer, symbol, float, vector, or buffer\n
For use with:\n
 `mon-get-text-properties-parse-buffer'
 `mon-get-text-properties-parse-sym'
 `mon-get-text-properties-parse-buffer-or-sym'\n
:EXAMPLE\n
\(let \(\(bubba-type 
       `\(\"bubba\" bubba  8 8.8 [b u b b a] ,\(get-buffer \(current-buffer\)\)\)\)
      bubba-types\)
  \(dolist \(the-bubba bubba-type \(setq bubba-types \(nreverse bubba-types\)\)\)
    \(push `\(,the-bubba 
            . ,\(mon-get-text-properties-parse-prop-val-type-chk the-bubba\)\)
          bubba-types\)\)\)\n
:SEE-ALSO `mon-equality-for-type', `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties',
`typecase', `etypecase', `deftype', `typep', `type-of', `mon-function-object-p',
`mon-sequence-mappable-p', `mon-equality-or-predicate',
`*mon-equality-or-predicate-function-types*'.\n►►►"
  (typecase prop-val
    (string  'equal)           
    (integer 'eq)
    (symbol  'eq) 
    (float   'eql)
    (vector  'equal)
    (buffer  'eq)
    ;; cons can't happen '(a b c) 
    (t (error (concat ":FUNCTION `mon-get-text-properties-parse-sym' "
                      "-- arg PROPS-IN-SYM not string, integer, float, "
                      "vector, buffer, or symbol")))))
;;
;;; :TEST-ME 
;; (let ((bubba-type 
;;        `("bubba" bubba  8 8.8 [b u b b a] ,(get-buffer (current-buffer))))
;;       bubba-types)
;;   (dolist (the-bubba bubba-type (setq bubba-types (nreverse bubba-types)))
;;     (push `(,the-bubba 
;;             . ,(mon-get-text-properties-parse-prop-val-type-chk the-bubba))
;;           bubba-types)))

;;; ==============================
;;; (insert-buffer-substring "*MGTPFES*")
;;; :CREATED <Timestamp: #{2010-03-05T12:21:29-05:00Z}#{10095} - by MON KEY>
(defun mon-get-text-properties-parse-buffer (prop prop-val prop-buffer)
  "Filter the text-property list for sublists containing the text-property PROP
  and PROP-VAL.\n
PROP is a property to filter.
PROP-VAL is a property value of PROP to filter.
PROP-BUFFER names a buffer name from which to read from a list of sublists.
Sublists contain two index values and text-property plist of prop val pairs e.g.\n
 \(idx1 idx2 \(p1 p1-val p2 p2-val p3-val \(p3-lv1 p3-lv2 p3-lv3\)\)\)\n
:NOTE Reading begins from `point-min'. Reading does not move point.\n
:SEE `mon-get-text-properties-parse-buffer-or-sym' for usage example.
:SEE `mon-get-text-properties-parse-prop-val-type-chk' for PROP-VAL types.\n
:SEE-ALSO `mon-get-text-properties-region', `mon-get-text-properties-parse-sym',
`mon-get-text-properties-parse-buffer-or-sym', `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
  (unless (buffer-live-p (get-buffer prop-buffer))
    (error (concat ":FUNCTION `mon-get-text-properties-parse-buffer' "
                   "-- arg PROP-BUFFER does not exist")))
  (let ((rd-prop-marker (make-marker))
        (prop-st-marker (make-marker))
        (comp-type 
         (mon-get-text-properties-parse-prop-val-type-chk prop-val))
        rd-prop-times i-red im-reding)
    (with-current-buffer  prop-buffer
      (set-marker prop-st-marker (point))
      (set-marker rd-prop-marker (buffer-end 0))
      (unwind-protect
           (progn
             (goto-char (marker-position rd-prop-marker))
             (cond ((> (skip-syntax-forward "^(") 0)
                    (set-marker rd-prop-marker (point)))
                   ((bobp)
                    (set-marker rd-prop-marker (point)))
                    ;; Anything else is prob. funky
                   (t (error (concat ":FUNCTION `mon-get-text-properties-parse-buffer' "
                                     "-- bounds of sexp unknown"))))
             (if (eq (car (syntax-after (1+ (marker-position rd-prop-marker)))) 4)
                 (progn 
                   (setq rd-prop-times (length (sexp-at-point)))
                   (forward-char)
                   (set-marker rd-prop-marker (point)))
               (error (concat ":FUNCTION `mon-get-text-properties-parse-buffer' "
                              "-- bounds of sexp unknown")))
             ;;(marker-position rd-prop-marker)
             (dotimes (rd rd-prop-times (setq i-red (nreverse i-red)))
               (setq im-reding (read rd-prop-marker))
               (let* ((red-prop (plist-member (caddr im-reding) prop))
                      (red-prop-val (cadr red-prop)))
                 (when red-prop 
                   (cond ((consp red-prop-val)
                          (when (member prop-val red-prop-val)
                            (push im-reding i-red)))
                         ((and (not (null red-prop-val)) (atom red-prop-val))
                          (when (funcall comp-type prop-val red-prop-val)
                            (push im-reding i-red))))))))
        (goto-char prop-st-marker)))))
;;
;;; :TEST-ME (mon-get-text-properties-parse-buffer 'face 'font-lock-constant-face "*MGTPFES*")


;;; ==============================
;;; :PREFIX "mgtpps-"
;;; :CREATED <Timestamp: #{2010-03-05T14:10:07-05:00Z}#{10095} - by MON KEY>
(defun mon-get-text-properties-parse-sym (prop prop-val props-in-sym)
  "Filter text-property list for sublists containing PROP and PROP-VAL.\n
PROP is a property to filter.\n
PROP-VAL is a property value of PROP to filter.\n
It is one of the types:\n
 <STRING>, <INTEGER>, <SYMBOL>, <FLOAT>, <VECTOR>, <BUFFER>\n
PROPS-IN-SYM is a symbol to parse.\n
Format of PROPS-IN-SYM are as per `mon-get-text-properties-parse-buffer-or-sym'.\n
:SEE `mon-get-text-properties-parse-buffer-or-sym' for usage example.\n
:SEE `mon-get-text-properties-parse-prop-val-type-chk' for PROP-VAL types.\n
:SEE-ALSO `mon-get-text-properties-region', `mon-get-text-properties-parse-buffer',
`mon-help-text-property-functions-ext', `mon-help-text-property-functions', 
`mon-help-text-property-properties'.\n►►►"
  (let ((mgtpps-cmp-typ 
         (mon-get-text-properties-parse-prop-val-type-chk prop-val))
        mgtpps-got)
    (mapc #'(lambda (mgtpps-L-1)
              (let* ((mgtpps-prop-red (plist-member (caddr mgtpps-L-1) prop))
                     (mgtpps-prop-val (cadr mgtpps-prop-red)))
                (when mgtpps-prop-red 
                  (cond ((consp mgtpps-prop-val)
                         (when (member prop-val mgtpps-prop-val)
                           (push mgtpps-L-1 mgtpps-got)))
                        ((and (not (null mgtpps-prop-val)) (atom mgtpps-prop-val))
                         (when (funcall mgtpps-cmp-typ prop-val mgtpps-prop-val)
                           (push mgtpps-L-1 mgtpps-got)))))))
          props-in-sym)
    (setq mgtpps-got (nreverse mgtpps-got))))
;;
;;; :TEST-ME 
;;; (let ((mgtppb (mon-get-text-properties-parse-buffer 'face 'font-lock-constant-face "*MGTPFES*")))
;;;   (mon-get-text-properties-parse-sym 'face 'font-lock-string-face mgtppb))


;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-05T13:41:29-05:00Z}#{10095} - by MON KEY>
(defun* mon-get-text-properties-parse-buffer-or-sym (prop prop-val &key 
                                                          read-prop-sym 
                                                          read-prop-buffer)
  "Filter text-property list for sublists containing the PROP and PROP-VAL.\n
Return a two valued list.\n
The car is a list of conses of only the indexes for each matching sublist.\n
The cadr is a list of each each matching sublist.\n
PROP is a property to filter.\n
PROP-VAL is a property value of PROP to filter.\n
Keyword READ-PROP-SYM names a symbol to parse.\n
Keyword READ-PROP-BUFFER names a buffer to read from.\n
When keyword READ-PROP-BUFFER is non-nil reading begins from `point-min' does
not move point.\n
Contents of READ-PROP-SYM or READ-PROP-BUFFER should hold a list with sublists.\n
Sublists contain two index values and text-property plist of prop val pairs e.g.\n
 \(idx1 idx2 \(p1 p1-val p2 p2-val p3-val \(p3-lv1 p3-lv2 p3-lv3\)\)\)\n
:EXAMPLE\n\n\(let \(\(mgtppbos-example
       '\(\(34 35   \(fontified t hard t rear-nonsticky t face some-face\)\)
         \(idx1 idx2 \(p1 p1-val p2 p2-val p3-val \(p3-lv1 p3-lv2 p3-lv3\)\)\)
         \(388 391 \(some-boolean-prop t face \(some-first-face second-face\)\)\)
         ;; { ... lots more here ... }
         \(3862 3884 \(face \(font-lock-constant-face second-face\)\)\)\)\)\)
  \(mon-get-text-properties-parse-buffer-or-sym 
   'face 'second-face :read-prop-sym mgtppbos-example\)\)\n
\(let \(\(mgtppbos-example
       '\(\(34 35     \(fontified t p2 p2-val rear-nonsticky t face some-face\)\)
         \(idx1 idx2 \(p1 p1-val p2 p2-val p3-val \(p3-lv1 p3-lv2 p3-lv3\)\)\)
         \(388 391   \(some-boolean-prop t face \(some-first-face second-face\)\)\)
         ;; { ... lots more here ... }
         \(3862 3884 \(face \(font-lock-constant-face second-face\)\)\)\)\)\)
  \(prin1 mgtppbos-example \(get-buffer-create \"*MGTPPBOS-EXAMPLE*\"\)\)
  \(setq mgtppbos-example
        \(mon-get-text-properties-parse-buffer-or-sym 
         'p2 'p2-val :read-prop-buffer \"*MGTPPBOS-EXAMPLE*\"\)\)
  \(kill-buffer \"*MGTPPBOS-EXAMPLE*\"\)
  mgtppbos-example\)\n
:SEE-ALSO `mon-get-text-properties-parse-sym', `mon-get-text-properties-parse-buffer',
`mon-get-text-properties-region', `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
  (let (mgtpbos)
    (cond (read-prop-sym
           (setq mgtpbos (mon-get-text-properties-parse-sym 
                          prop prop-val read-prop-sym)))
          (read-prop-buffer
           (setq mgtpbos (mon-get-text-properties-parse-buffer 
                          prop prop-val read-prop-buffer))))
    (setq mgtpbos `(,(mon-get-text-properties-map-ranges mgtpbos) ,mgtpbos))))
;;
;;; :TEST-ME (mon-get-text-properties-parse-buffer-or-sym 
;;;               'face 'font-lock-constant-face :read-prop-buffer "*MGTPFES*")
;;; :TEST-ME (let ((mgtppb 
;;;                    (mon-get-text-properties-parse-buffer 
;;;                     'face 'font-lock-constant-face "*MGTPFES*")))
;;;               (mon-get-text-properties-parse-buffer-or-sym 
;;;                'face 'font-lock-string-face :read-prop-sym mgtppb))


;;; ==============================
;;; :PREFIX "mgtpmmr-"
;;; :CREATED <Timestamp: #{2010-03-05T19:04:53-05:00Z}#{10096} - by MON KEY>
(defun mon-get-text-properties-map-ranges (text-prop-list)
  "Map the indexes at head of each sublist of TEXT-PROP-LIST to a consed list.\n
Return value is a list of sublists of the form:\n
 ( (idx1a idx1b) (idx2a idx2b) (idx3a idx3b) { ... } )\n
:EXAMPLE\n
:CALLED-BY `mon-get-text-properties-parse-buffer-or-sym'.\n
:SEE-ALSO `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
  (let (mgtpmmr-rng)
    (setq mgtpmmr-rng
          (mapcar #'(lambda (mgtpmmr-L-1) 
                      (let ((bt (butlast mgtpmmr-L-1 1)))
                        (setq bt `(,(car bt) . ,(cadr bt)))))
                  text-prop-list))))
;;
;; (let ((mgtppb
;;        (mon-get-text-properties-parse-buffer 'face 'font-lock-constant-face "*MGTPFES*")))
;;   (setq mgtppb
;;         (mon-get-text-properties-parse-sym 'face 'font-lock-string-face mgtppb))
;;   (setq mgtppb `(,(mon-get-text-properties-map-ranges mgtppb) ,mgtppb)))

;;; ==============================
;;; :PREFIX "mgtpmrs-"
;;; :CREATED <Timestamp: #{2010-03-05T20:16:13-05:00Z}#{10096} - by MON KEY>
(defun mon-get-text-properties-map-ranges-string (string-range-buffer range-buffer)
  "Map a range text-properties in buffer STRING-RANGE-BUFFER.\n 
Return value inserted in RANGE-BUFFER.\n
:EXAMPLE\n\n
:SEE-ALSO `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
  (let (mgtpmrs-rr mgtpmrs-str
        ;; :NOTE `str-range` apears to be unused.
        str-range) 
    (setq mgtpmrs-rr 
          (with-current-buffer string-range-buffer ;;"*MGTPFES-STRING*"
            (mon-get-text-properties-region (mon-g2be -1 t) (mon-g2be 1 t)) ))
    (setq mgtpmrs-str (car mgtpmrs-rr))
    (setq mgtpmrs-rr  (cadr mgtpmrs-rr))
    (setq mgtpmrs-rr  (mon-get-text-properties-parse-buffer-or-sym 
                       'face 'font-lock-constant-face :read-prop-sym mgtpmrs-rr))
    (setq mgtpmrs-rr 
          (mapcar #'(lambda (mgtpmrs-idx-pair)
                      `(,mgtpmrs-idx-pair 
                        ,(substring mgtpmrs-str (car mgtpmrs-idx-pair) (cdr mgtpmrs-idx-pair))))
                  (car mgtpmrs-rr)))
    (princ mgtpmrs-rr (get-buffer range-buffer))))
;;
;; (mon-get-text-properties-map-ranges-string (current-buffer) (get-buffer-create "*MGTPFES-STRING*"))
;; (mon-get-text-properties-parse-buffer-or-sym 'face 'font-lock-keyword-face :read-prop-buffer "mon-text-property-utils.el")
;; (insert-buffer-substring "mon-text-property-utils.el" 52839 52870)

;;; ==============================
;;; :PREFIX "mgtpb-"
;;; :COURTESY slime.el :WAS `slime-property-bounds'
;;; :CHANGESET 2109
;;; :CREATED <Timestamp: #{2010-09-04T12:34:27-04:00Z}#{10356} - by MON KEY>
(defun mon-get-text-property-bounds (prop)
  "Return positions of changes to previous and next char property for PROP.\n
Return value is a two elt list of `previous-single-char-property-change' and
`next-single-char-property-change'.\n
PROP is the name of a text property.\n
:EXAMPLE\n\n\(with-temp-buffer 
  \(save-excursion \(insert \(propertize \"bubba\" 'bubba-props 'im-a-bubba\)\)\)
  \(mon-get-text-property-bounds 'bubba-props\)\)\n
:SEE-ALSO `mon-get-text-properties-elisp-string-pp',
`mon-get-text-properties-read-temp', `mon-get-text-properties-print',
`mon-get-text-properties-region', `mon-get-next-face-property-change-if',
`mon-get-next-face-property-change', `mon-get-text-properties-parse-sym',
`mon-get-text-properties-parse-buffer',
`mon-get-text-properties-parse-prop-val-type-chk',
`mon-get-text-properties-elisp-string',
`mon-get-text-properties-map-ranges-string',
`mon-get-text-properties-map-ranges',
`mon-get-text-properties-parse-buffer-or-sym',
`mon-help-text-property-functions-ext', `mon-help-text-property-functions',
`mon-help-text-property-properties', `mon-help-overlay-functions'.\n►►►"
  (assert (get-text-property (point) prop))  
  (let ((mgtpb-end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change mgtpb-end prop) mgtpb-end)))

;;; ==============================
;;; :PREFIX "mgtpra-"
;;; :COURTESY gnus-util.el :WAS `gnus-remove-text-with-property'
(defun mon-get-text-property-remove-all (txt-prop)
  "Delete all text in the current buffer with text property TXT-PROP.\n
:ALIASED-BY `mon-remove-text-with-property'\n
:SEE-ALSO .\n►►►"
  (let ((mgtpra-beg (mon-g2be -1 t))
	mgtpra-end)
    (unless (get-text-property mgtpra-beg txt-prop)
      (setq mgtpra-beg (next-single-property-change mgtpra-beg txt-prop)))
    (while mgtpra-beg
      (setq mgtpra-end (text-property-any mgtpra-beg (mon-g2be 1 t) txt-prop nil))
      (delete-region mgtpra-beg (or mgtpra-end (mon-g2be 1 t)))
      (setq mgtpra-beg (when mgtpra-end
                         (next-single-property-change mgtpra-beg txt-prop))))))

;;; ==============================
;;; :OVERLAYS
;;; ==============================

;;; ==============================
;;; :PREFIX "mgor-"
;;; :CHANGESET 2136
;;; :CREATED <Timestamp: #{2010-09-16T13:14:39-04:00Z}#{10374} - by MON KEY>
(defun mon-get-overlays-region (ov-start ov-end &optional intrp)
  "Return list of overlays in region from OV-START to OV-END.\n
When OV-END is ommitted default is the value of OV-START.\n
Return value has the format:\n
 \(:REGION-START <OV-START> 
  :REGION-END   <OV-END> 
  :OVERLAYS     \(#<OV-OBJECT> { ... } #<OV-OBJECT-N>\)\)\n
:EXAMPLE\n\n\(let \(\(mo \(make-overlay \(+ \(point\) 4\) \(+ \(point\) 11\)\)\)\)
  \(dolist \(oput '\(\(face . link\) \(evaporate . t\)\)
                \(mon-get-overlays-region \(+ \(point\) 4\) \(+ \(point\) 11\)\)\)
    \(overlay-put mo \(car oput\) \(cdr oput\)\)\)\) ;; overlay\n
:SEE-ALSO `mon-get-overlays-map-props', `mon-get-overlays-region',
`mon-get-overlays-region-map-props', `mon-get-overlays-buffer',
`mon-help-find-result-for-overlay', `mon-help-overlay-for-example',
`mon-help-overlay-on-region', `mon-help-overlay-result',
`mon-get-overlays-buffer', `mon-help-overlay-functions',
`mon-help-text-property-functions', `mon-help-text-property-properties',
`overlays-in'.\n►►►"
  (interactive "r\np")
  (let ((mgor-ov-rgn-p 
         ;; :NOTE `or'ing ov-end allows: 
         ;;  (mon-get-overlays-region (point) nil)
         (overlays-in ov-start (or ov-end ov-start))))
    (when mgor-ov-rgn-p 
      (setq mgor-ov-rgn-p `(:REGION-START ,ov-start 
                            :REGION-END   ,(or ov-end ov-start)
                            :OVERLAYS     ,mgor-ov-rgn-p))
      (if intrp
          (prin1 mgor-ov-rgn-p)
        mgor-ov-rgn-p))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |
;; | (let ((mo (make-overlay (+ (point) 4) (+ (point) 11))))
;; |   (dolist (oput '((face . link) (evaporate . t))
;; |                 (mon-get-overlays-region (+ (point) 4) (+ (point) 11)))
;; |     (overlay-put mo (car oput) (cdr oput)))) ;; overlay
;; |
;; `----


;;; ==============================
;;; :PREFIX "mgormp-"
;;; :CHANGESET 2136
;;; :CREATED <Timestamp: #{2010-09-16T14:38:31-04:00Z}#{10374} - by MON KEY>
(defun mon-get-overlays-region-map-props (ov-start ov-end)
  "Return a list of overlays in region and its properties.\n
OV-START OV-END are buffer positions.\n
Return value has the format:\n
\(:REGION-START <OV-START> :REGION-END <OV-END>
 :OVERLAYS \(\(:OVERLAY-ID #<OV-OBJECT>
             :OVERLAY-START <INTEGER> :OVERLAY-END <INTEGER>
             :OVERLAY-BUFFER #<BUFFER-OBJECT> 
             :OVERLAY-PROPS \(<PROP> <PVAL> { ... } <PROP-N> <PVAL-N>\)\)
             \(:OVERLAY-ID #<OV-OBJECT-N>  {...} \)\)\)
:EXAMPLE\n\n\(let \(\(inhibit-read-only t\)\)
  \(save-excursion
    \(apply #'make-button
           \(prog1 \(point\)  \(insert \"bubba\"\)\)
           \(point\) `\(label \"text-prop-button\" face link\)\)\)
  \(let \(\(mk-o \(make-overlay \(point\) \(+ \(point\) 5\)\)\)\)
    \(dolist \(oput '\(\(after-string . \" <- thatsa bubba!\"\) \(evaporate . t\)\)\)
      \(overlay-put mk-o \(car oput\) \(cdr oput\)\)\)\)
  \(mon-get-overlays-region-map-props \(point\) \(+ \(point\) 5\)\)\)\n
:SEE-ALSO `mon-get-overlays-map-props', `mon-get-overlays-region',
`mon-get-overlays-region-map-props', `mon-get-overlays-buffer',
`mon-help-find-result-for-overlay', `mon-help-overlay-for-example',
`mon-help-overlay-on-region', `mon-help-overlay-result',
`mon-get-overlays-buffer', `mon-help-overlay-functions',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
;;mgormp-o-rgn
  (let* ((mgormp-o-rgn (mon-get-overlays-region ov-start ov-end))
         (mgormp-ov-mapd (cadr (memq :OVERLAYS mgormp-o-rgn))))
    (when mgormp-ov-mapd
      (setf mgormp-ov-mapd (mon-get-overlays-map-props mgormp-ov-mapd)
            (car (last mgormp-o-rgn)) mgormp-ov-mapd))
    mgormp-o-rgn))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((inhibit-read-only t))
;; |   (save-excursion
;; |     (apply #'make-button
;; |            (prog1 (point)  (insert "bubba"))
;; |            (point) `(label "text-prop-button" face link)))
;; |   (let ((mk-o (make-overlay (point) (+ (point) 5))))
;; |     (dolist (oput '((after-string . " <- thatsa bubba!") (evaporate . t)))
;; |       (overlay-put mk-o (car oput) (cdr oput))))
;; |   (mon-get-overlays-region-map-props (point) (+ (point) 5)))
;; `----


;;; ==============================
;;; :PREFIX "mgomp-"
;;; :CHANGESET 2136
;;; :CREATED <Timestamp: #{2010-09-16T13:20:24-04:00Z}#{10374} - by MON KEY>
(defun mon-get-overlays-map-props (overlays-lst)
  "Return list of overlay properties for each overlay in OVERLAYS-LST.\n
OVERLAYS-LST is a list of overlays.\n
List elements of retrun value have the format:\n
  (:OVERLAY-ID #<OV-OBJECT>
   :OVERLAY-START <INTEGER> :OVERLAY-END <INTEGER>
   :OVERLAY-BUFFER #<BUFFER-OBJECT> 
   :OVERLAY-PROPS (<PROP> <PVAL> { ... } <PROP-N> <PVAL-N>))\n
:EXAMPLE\n\n\(mon-get-overlays-map-props \(mon-get-overlays-buffer\)\)\n
:SEE-ALSO `mon-get-overlays-map-props', `mon-get-overlays-region',
`mon-get-overlays-region-map-props', `mon-get-overlays-buffer',
`mon-button-get-plist', `mon-button-get-plist-props',
`mon-button-at-point-p', `mon-button-at-point-describe-button-plist',
`mon-help-find-result-for-overlay', `mon-help-overlay-for-example',
`mon-help-overlay-on-region', `mon-help-overlay-result',
`mon-get-overlays-buffer', `mon-help-overlay-functions',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
  (unless (null overlays-lst)
    (let (mgomp-op-gthr)
      (dolist (mgomp-D-1 overlays-lst (setq mgomp-op-gthr (nreverse mgomp-op-gthr)))
        ;; :NOTE Don't appear to be using the local variable `op-this`
        (let (op-this)
          (push 
           `(:OVERLAY-ID     ,mgomp-D-1
             :OVERLAY-START  ,(overlay-start mgomp-D-1)
             :OVERLAY-END    ,(overlay-end mgomp-D-1)
             :OVERLAY-BUFFER ,(overlay-buffer mgomp-D-1)
             :OVERLAY-PROPS  ,(overlay-properties mgomp-D-1))
           mgomp-op-gthr))))))
;;
;;; :TEST-ME (mon-get-overlays-map-props (mon-get-overlays-buffer))

;;; ==============================
;;; :PREFIX "mgob-"
;;; :CHANGESET 2136
;;; :CREATED <Timestamp: #{2010-09-16T14:30:16-04:00Z}#{10374} - by MON KEY>
(defun mon-get-overlays-buffer (&optional buffer-or-name)
  "Return list of overlays in current-buffer.\n
When BUFFER-OR-NAME is non-nil get its overlays.\n
Like `overlay-lists' but returns a flat list without consideration for the
overlay center.\n
:EXAMPLE\n\n\(mon-get-overlays-buffer\)\n
:SEE-ALSO `mon-get-overlays-map-props', `mon-get-overlays-region',
`mon-get-overlays-region-map-props', `mon-get-overlays-buffer',
`mon-help-find-result-for-overlay', `mon-help-overlay-for-example',
`mon-help-overlay-on-region', `mon-help-overlay-result',
`mon-get-overlays-buffer', `mon-help-overlay-functions',
`mon-help-text-property-functions', `mon-help-text-property-properties',.\n►►►"
  (let (mgob-ov-lsts)
    (with-current-buffer 
        (or (and buffer-or-name (get-buffer buffer-or-name))
            (current-buffer))
      (setq mgob-ov-lsts (overlay-lists)))
    (when mgob-ov-lsts `(,@(car mgob-ov-lsts) ,@(cdr mgob-ov-lsts)))))


;;; ==============================
;; :COURTESY lisp/progmodes/cpp.el :WAS `cpp-grow-overlay'
;; :NOTE Removed the AFTER argument and the optional LEN arg. 
;;       I can't see that these are actually used.
;;       :WAS  (overlay after start end &optional len)
;; :ADDED optional arg BUFFER-OR-NAME per `move-overlay's optional arg BUFFER.
(defun mon-set-overlay-range (mv-olay start-olay end-olay &optional buffer-or-name) 
  "Grow overlay MV-OLAY with `move-overlay' into range START-OLAY to END-OLAY.\n
MV-OLAY is an overlay object.\n
START-OLAY END-OLAY are overlay positions\n
Optional arg BUFFER-OR-NAME is as per `move-overlay's optional arg BUFFER.\n
:EXAMPLE\n\n
:SEE-ALSO .\n►►►"
  ;; :WAS (if after
  (move-overlay mv-olay
                (min start-olay (overlay-start mv-olay))
                (max end-olay   (overlay-end   mv-olay))
                (or buffer-or-name (current-buffer))))

;;; ==============================
;;; :TODO add a hook using to `kill-buffer-hook' or some such that removes the 
;;; `color-occur-face' correctly e.g.
;;;  (mon-nuke-overlay-buffer 'face 'color-occur-face)
;;; :NOTE First Implemented to zap linger `color-occur' overlays when occur buffer
;;;  is killed with kill-buffer as `color-occur-remove-overlays' doesn't get invoked.
;;; :CREATED <Timestamp: #{2010-02-09T11:11:52-05:00Z}#{10062} - by MON KEY>
(defun mon-nuke-overlay-buffer (overlay-prop overlay-val)
  "Remove all overlay props with OVERLAY-NAME and OVERLAY-VAL in current-buffer.\n
:EXAMPLE\n\n\(unwind-protect
     \(let* \(\(sb \(save-excursion \(search-forward-regexp \"^►.*◄$\"  nil t\)\)\)
            \(molay \(make-overlay \(match-beginning 0\) \(match-end 0\) \(current-buffer\)\)\)\)
       \(overlay-put molay 'face 'minibuffer-prompt\)
       \(sit-for 2\)\)
  \(mon-nuke-overlay-buffer 'face 'minibuffer-prompt\)\)\n
►I'm lingering◄\n
:SEE-ALSO `mon-get-overlays-map-props', `mon-get-overlays-region',
`mon-get-overlays-region-map-props', `mon-get-overlays-buffer',
`mon-help-find-result-for-overlay', `mon-help-overlay-for-example',
`mon-help-overlay-on-region', `mon-help-overlay-result',
`mon-get-overlays-buffer', `mon-help-overlay-functions',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
  (remove-overlays (mon-g2be -1 t) (mon-g2be 1 t) overlay-prop overlay-val))


;;; ==============================
;;; ==============================
;;; :TODO If possible figure out what is causing the peculiar return values w/re
;;; the 2nd :NOTE in docstring below and if there is a reasonable workaround.
;;;
;;; `mon-get-all-face-property-change' fails because of some weird. 
;;; return values when invoked in large (e)lisp-mode buffers and won't reliably find faces
;;; `font-lock-string-face' and `font-lock-comment-face' if these have been
;;; font-locked with font-lock syntactic voodo
;;; e.g. `lisp-font-lock-syntactic-face-function's conditional on (nth 3 state)
;;; vis a vis `font-lock-syntactic-face-function', `font-lock-syntactic-keywords'
;;; which the `jit-lock-*' fncns appear to be frobbing in complicated ways.
;;;
;;; Is it possible that the `looking-at' in `font-lock-syntactic-face-function'
;;; isn't `save-match-data'ing???  See at EOB for a redefinition of
;;; `lisp-font-lock-syntactic-face-function' to `save-match-data' as
;;; `mon-lisp-font-lock-syntactic-face-function'.
;;;
;;; NO. That isn't it... this problem is some bogus BULLSHIT! It is clear the
;;; face properties are set as one can immediately yank the text and _SEE_ that
;;; they are present throughout the buffer's string immediatley so I don't think
;;; this is a display problem.
;;;
;;;
;;; :CREATED <Timestamp: #{2010-02-27T19:38:29-05:00Z}#{10087} - by MON KEY>
;; (defun mon-get-all-face-property-change (search-face-symbol get-from-posn)
;;   "Find all start end locations of SEARCH-FACE-SYMBOL at or after get-from-posn.\n
;; SEARCH-FACE-SYMBOL symbol naming a face to find.\n
;; GET-FROM-POSN position to begin finding from.\n
;; :EXAMPLE\n\n\(mon-get-all-face-property-change 'help-argument-name \(buffer-end 0\)\)\n
;; :NOTE Won't find 'button faces in *Help*.\n
;; :NOTE Won't find 'button faces in *Help*.\n
;; :NOTE Invoked in large (e)lisp-mode buffers won't reliably find faces
;; `font-lock-string-face' and `font-lock-comment-face' if these have been
;; font-locked with font-lock syntactic voodo
;; e.g. `lisp-font-lock-syntactic-face-function's conditional on \(nth 3 state\)
;; vis a vis `font-lock-syntactic-face-function', `font-lock-syntactic-keywords'
;; which the `jit-lock-*' fncns appear to be frobbing in complicated ways.\n
;; :SEE-ALSO `mon-get-text-properties-region-to-kill-ring', `mon-get-text-properties-category',
;; `mon-line-test-content', `mon-get-next-face-property-change',
;; `mon-get-next-face-property-change-if', `mon-get-all-face-property-change'
;; `mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer',
;; `mon-nuke-text-properties-region', `mon-remove-text-property',
;; `mon-remove-single-text-property'.\n►►►"
;;   (save-excursion
;;     (let ((sfc search-face-symbol)
;;           top-st bot-st fc-bnds)
;;       (when (mon-get-next-face-property-change-if sfc get-from-posn)
;;         (setq top-st 
;;               (1- (previous-single-property-change 
;;                    (point) 'face nil (line-beginning-position)))))
;;       (if (integerp top-st)
;;           (setq top-st (mon-get-next-face-property-change sfc top-st))
;;           (setq top-st (mon-get-next-face-property-change sfc get-from-posn)))
;;       (while top-st
;;         (setq bot-st 
;;               (mon-get-next-face-property-change sfc (car top-st)))
;;         (when (caadr top-st)
;;           (push `(,(car top-st) . ,(car bot-st)) fc-bnds))
;;         (when bot-st 
;;           (setq top-st (mon-get-next-face-property-change sfc (car bot-st)))
;;           (goto-char (car bot-st)))
;;         (when (and (null (caadr top-st)) (null (caadr bot-st)))
;;           (while (and top-st 
;;                       (not (eq (cadadr 
;;                                 (mon-get-next-face-property-change 'font-lock-constant-face (point)))
;;                                font-lock-constant-face)))
;;             (if (car (mon-get-next-face-property-change 'font-lock-constant-face (point)))
;;                 (goto-char (car (mon-get-next-face-property-change 'font-lock-constant-face (point))))
;;                 (setq top-st nil)))))
;;           (setq fc-bnds (nreverse fc-bnds))
;;           ;; Uncomment when debug.
;;           ;; (goto-char (buffer-end 1)) (prin1 fc-bnds (current-buffer))
;;           )))

;;; ==============================
(provide 'mon-text-property-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-text-property-utils.el ends here
;;; EOF
