;;; mon-text-property-utils.el --- functions for working manipulating text properties
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
;; KEYWORDS: 

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
;; ALIASED/ADVISED/SUBST'D:
;; `mon-remove-text-with-property' -> `mon-get-text-property-remove-all'
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-test-props' -> `mon-get-text-properties-category'
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
;;
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

(eval-when-compile (require 'cl))

;;; ==============================
;;; :NOTE Keep with `mon-list-all-properties-in-buffer'.
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;;; `mon-nuke-text-properties-buffer'
(defun mon-plist-keys (plist)
  "Cons up a plist of keys with PLIST.\n
:EXAMPLE\n\(mon-plist-keys \(mon-alphabet-as-type 'plistD->num\)\)\n
:SEE-ALSO `mon-plist-remove', `mon-help-plist-functions',
`mon-map-obarray-symbol-plist-props', `mon-plist-remove-if',
`mon-plist-remove-consing', `remf', `remprop'.\n►►►"
  (if (null plist)
      plist
      (cons (car plist) (mon-plist-keys (cddr plist)))))

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
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
(defun mon-nuke-text-properties-region (beg end)
  "Eliminate all text properties in current buffer from BEG to END.\n
:NOTE Only removes text properties, does not remove overlays.\n
:SEE-ALSO `remove-text-property', `mon-remove-single-text-property',
`mon-nuke-overlay-buffer', `add-text-properties', `put-text-property',
`next-single-property-change', `mon-list-all-properties-in-buffer',
`mon-help-text-property-functions'.\n►►►"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((inhibit-read-only t)
              (plist (text-properties-at (point)))
              (next-change (or (next-property-change (point) (current-buffer))
                               (point-max))))
          (remove-text-properties (point) next-change plist (current-buffer))
          (goto-char next-change))))))

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
`mon-help-faces-themes'.\n►►►"
  (interactive "d\nP")
  (let ((mgfap-face (or (get-char-property (or face-psn (point)) 'read-face-name)
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
;;; :COURTESY :FILE gnus-util.el :WAS `gnus-faces-at'
;;; Removed the Xemacs conditional added a `gensym'.
;;; :CREATED <Timestamp: #{2010-02-04T14:03:31-05:00Z}#{10054} - by MON KEY>
(defmacro mon-get-face-at-posn (position)
  "Return a list of faces at POSITION.\n
:SEE-ALSO `mon-get-face-at-point', `mon-help-faces',
`mon-help-faces-basic', `mon-help-faces-themes'.\n►►►"
  (let ((mgfap-pos (make-symbol "mgfap-pos")))
    `(let ((,mgfap-pos ,position))
       (delq nil (cons (get-text-property ,mgfap-pos 'face)
		       (mapcar #'(lambda (overlay)
                                   (overlay-get overlay 'face))
                               (overlays-at ,mgfap-pos)))))))

;;; ==============================
;;; :RENAMED `mon-kill-ring-save-w-props' -> `mon-get-text-properties-region-to-kill-ring'
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
  (let (get-str) 
    (setq get-str (format "%S" (buffer-substring start end)))
    (kill-new 
     (substring 
      get-str 
      (if no-strip 
          0
          (if (= (string-match "^#" get-str) 0)
              1 
              0))))))
;;
(defalias 'mon-get-text-properties-region->kill-ring 'mon-get-text-properties-region-to-kill-ring)
;;
(defalias 'mon-kill-ring-save-w-props 'mon-get-text-properties-region-to-kill-ring)

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

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;;; :NOTE Keep with `mon-nuke-text-properties-buffer', `mon-plist-keys'
;;; :CHANGED `set-buffer' -> `with-current-buffer' 
;;; :CHANGED `delete-duplicates' -> `delete-dups'
;;; :ADDED (&optional start-range end-range buffer) :WAS (buffer)
;;; :MODIFICATIONS <Timestamp: #{2010-01-11T23:39:18-05:00Z}#{10021} - by MON KEY>
(defun mon-list-all-properties-in-buffer (&optional start-range end-range buffer)
  "List text-properties in current-buffer.\n
When BUFFER is non-nil list its text-properties instead.\n
:EXAMPLE\n(mon-list-all-properties-in-buffer)\n
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
      (loop for i from (or start-range (point-min)) to (or end-range (point-max))
         nconc (delete-dups (mon-plist-keys (text-properties-at i nil))))))))
;;
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;;; :NOTE Keep with `mon-list-all-properties-in-buffer', `mon-plist-keys'
(defun mon-nuke-text-properties-buffer ()
"Remove text-properites in buffer.\n
:SEE-ALSO `mon-remove-text-property', `mon-remove-single-text-property', 
`mon-nuke-text-properties-region', `mon-help-text-property-functions',
`mon-nuke-overlay-buffer'.\n►►►"
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max)
   (mon-list-all-properties-in-buffer)));; (buffer-name (current-buffer)))))


;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-26T19:31:23-05:00Z}#{10086} - by MON KEY>
(defun mon-get-next-face-property-change (face-prop-val &optional from-posn)
  "Search for `next-single-property-change' with property 'face.\n
If face property has FACE-PROP-VAL push t onto list of return value.
Return a two element list formatted as:\n
 \(position \(t|nil face-prop-vals\)\)\n
When FROM-POSN is non-nil search for next face property change FROM-POSN.
Default is to search from point.\n
:EXAMPLE\n\n\(mon-get-next-face-property-change 'button \(buffer-end 0\)\)\n
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
   (let (got-tp)
     (setq got-tp (next-single-property-change (or from-posn (point)) 'face))
     (when got-tp
       (if (consp (get-text-property got-tp 'face))
           (setq got-tp `(,got-tp  ,(get-text-property got-tp 'face)))
           (setq got-tp `(,got-tp  (,(get-text-property got-tp 'face))))))
     (when (and got-tp (cadr got-tp))
       (if (memq face-prop-val (cadr got-tp))
           (push t (cadr got-tp))
           (push nil (cadr got-tp))))
     got-tp))
;;
(defalias 'mon-help-face-next-property-change 'mon-get-next-face-property-change)


;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-27T19:58:23-05:00Z}#{10087} - by MON KEY>
(defun mon-get-next-face-property-change-if (test-face-symbol test-at-posn)
  "Text if the face we're looking for is at position.
TEST-FACE-SYMBOL symbol naming a face to test.\n
TEST-AT-POSN position to test at.\n
:SEE-ALSO `mon-get-text-properties-region-to-kill-ring', `mon-get-text-properties-category',
`mon-line-test-content', `mon-get-next-face-property-change',
`mon-get-next-face-property-change-if', `mon-get-all-face-property-change'
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer',
`mon-nuke-text-properties-region', `mon-remove-text-property',
`mon-remove-single-text-property'.\n►►►"
(let ((pg (plist-get (text-properties-at test-at-posn) 'face)))
  (when pg 
    (cond ((consp pg) (memq test-face-symbol pg))
          (t (eq test-face-symbol pg))))))
;;
;;; (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-comment-face)
;;; :TEST-ME (mon-get-next-face-property-change-if 'font-lock-comment-face (point));;


;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-05T17:35:32-05:00Z}#{10095} - by MON KEY>
(defun mon-get-text-properties-region (start end)
  "Return region as a two elt list string and strings text properties.\n
:EXAMPLE\n\n\(let \(\(sbr \(save-excursion (goto-char (buffer-end 0))
             \(search-forward-regexp \"\(mon.*\)$\" nil t\)\)\)\)
  \(setq sbr `\(,\(match-beginning 0\) . ,\(match-end 0\)\)\)
  \(mon-get-text-properties-region \(car sbr\) \(cdr sbr\)\)\)\n
:NOTE Indexes are into string not buffer as with return value of:
 `mon-get-text-properties-print' & `mon-get-text-properties-read-temp'.\n
:SEE-ALSO `mon-get-text-properties-region-to-kill-ring'.\n►►►"
  (interactive "r\np")
  (let (get-str nw-tl) 
    (setq get-str (substring (format "%S" (buffer-substring start end)) 1))
    (setq get-str (car (read-from-string get-str)))
    (setq get-str `(,(car get-str) ,(cdr get-str)))
    (setq nw-tl (substring (format "%S" (cdr get-str)) 1 -1))
    (setq nw-tl (replace-regexp-in-string " ?\\([0-9]+ [0-9]+\\( (\\)\\)" ")(\\1" nw-tl t))
    (setq nw-tl (substring nw-tl 1))
    (setq nw-tl (concat "(" (substring nw-tl 1) ")"))
    (setq nw-tl (list (car (read-from-string nw-tl))))
    (setcdr get-str nw-tl)
    get-str))
;;
;;; :TEST-ME (let ((sfr (save-excursion (search-forward-regexp "(defun.*$"))))
;;;               (setq sfr `(,(match-beginning 0). ,(match-end 0)))
;;;               (mon-get-text-properties-region (car sfr) (cdr sfr)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-02T20:08:34-05:00Z}#{10093} - by MON KEY>
(defun mon-get-text-properties-print (start end tp-buff &optional intrp)
  "Return buffer-string START END with text-properties.\n
TP-BUFF is a buffer name to print to as with prin1.\n
When called-interactively insert at point. Moves point.
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
      (goto-char (buffer-end 0))
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
:SEE-ALSO `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties',
`typecase', `etypecase', `deftype', `typep', `type-of'.\n►►►"
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
                    ;; Anything else if prob. funky
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
;;; :CREATED <Timestamp: #{2010-03-05T14:10:07-05:00Z}#{10095} - by MON KEY>
(defun mon-get-text-properties-parse-sym (prop prop-val props-in-sym)
  "Filter text-property list for sublists containing PROP and PROP-VAL.\n
PROP is a property to filter.
PROP-VAL is a property value of PROP to filter. 
It is one of the types:
 string, integer, symbol, float, vector, buffer
PROPS-IN-SYM is a symbol to parse.\n
Format of PROPS-IN-SYM are as per `mon-get-text-properties-parse-buffer-or-sym'.\n
:SEE `mon-get-text-properties-parse-buffer-or-sym' for usage example.\n
:SEE `mon-get-text-properties-parse-prop-val-type-chk' for PROP-VAL types.\n
:SEE-ALSO `mon-get-text-properties-region', `mon-get-text-properties-parse-buffer',
`mon-help-text-property-functions-ext', `mon-help-text-property-functions', 
`mon-help-text-property-properties'.\n►►►"
  (let ((comp-type 
         (mon-get-text-properties-parse-prop-val-type-chk prop-val))
        i-red)
    (mapc #'(lambda (im-reding)
              (let* ((red-prop (plist-member (caddr im-reding) prop))
                     (red-prop-val (cadr red-prop)))
                (when red-prop 
                  (cond ((consp red-prop-val)
                         (when (member prop-val red-prop-val)
                           (push im-reding i-red)))
                        ((and (not (null red-prop-val)) (atom red-prop-val))
                         (when (funcall comp-type prop-val red-prop-val)
                           (push im-reding i-red)))))))
          props-in-sym)
    (setq i-red (nreverse i-red))))
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
Return a two valued list. 
The car is a list of conses of only the indexes for each matching sublist.
The cadr is a list of each each matching sublist.
PROP is a property to filter.
PROP-VAL is a property value of PROP to filter.
Keyword READ-PROP-SYM names a symbol to parse.
Keyword READ-PROP-BUFFER names a buffer to read from.
When keyword READ-PROP-BUFFER is non-nil reading begins from `point-min' does
not move point.\n
Contents of READ-PROP-SYM or READ-PROP-BUFFER should hold a list with sublists.
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
;;; :CREATED <Timestamp: #{2010-03-05T19:04:53-05:00Z}#{10096} - by MON KEY>
(defun mon-get-text-properties-map-ranges (text-prop-list)
  "Map the indexes at head of each sublist of TEXT-PROP-LIST to a consed list.
Return value is a list of sublists of the form:
 ( (idx1a idx1b) (idx2a idx2b) (idx3a idx3b) { ... } )\n
:EXAMPLE
:CALLED-BY `mon-get-text-properties-parse-buffer-or-sym'.\n
:SEE-ALSO `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
  (let (mgtpmmr)
    (setq mgtpmmr
          (mapcar #'(lambda (top) 
                      (let ((bt (butlast top 1)))
                        (setq bt `(,(car bt) . ,(cadr bt)))))
                  text-prop-list))))
;;
;; (let ((mgtppb
;;        (mon-get-text-properties-parse-buffer 'face 'font-lock-constant-face "*MGTPFES*")))
;;   (setq mgtppb
;;         (mon-get-text-properties-parse-sym 'face 'font-lock-string-face mgtppb))
;;   (setq mgtppb `(,(mon-get-text-properties-map-ranges mgtppb) ,mgtppb)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-05T20:16:13-05:00Z}#{10096} - by MON KEY>
(defun mon-get-text-properties-map-ranges-string (string-range-buffer range-buffer)
  "Map a range text-properties in buffer STRING-RANGE-BUFFER.\n 
Return value inserted in RANGE-BUFFER.\n
:EXAMPLE\n\n
:SEE-ALSO `mon-help-text-property-functions-ext',
`mon-help-text-property-functions', `mon-help-text-property-properties'.\n►►►"
(let (rr the-str str-range)
  (setq rr (with-current-buffer string-range-buffer ;;"*MGTPFES-STRING*"
             (mon-get-text-properties-region (buffer-end 0) (buffer-end 1))))
  (setq the-str (car rr))
  (setq rr (cadr rr))
  (setq rr (mon-get-text-properties-parse-buffer-or-sym 
            'face 'font-lock-constant-face :read-prop-sym rr))
  (setq rr (mapcar #'(lambda (idx-pair)
                       `(,idx-pair ,(substring the-str (car idx-pair) (cdr idx-pair))))
                   (car rr)))
  (princ rr (get-buffer range-buffer))))
;;(current-buffer)))

;;; ==============================
;;; :COURTESY slime.el :WAS `slime-property-bounds'
;;; :CHANGESET 2109
;;; :CREATED <Timestamp: #{2010-09-04T12:34:27-04:00Z}#{10356} - by MON KEY>
(defun mon-get-text-property-bounds (prop)
  "Return positions of changes to previous and next char property for PROP.
Return value is a two elt list of `previous-single-char-property-change' and
`next-single-char-property-change'.\n
PROP is the name of a text property.
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
`mon-help-text-property-properties'.\n►►►"
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))


;; :COURTESY gnus-util.el :WAS `gnus-remove-text-with-property'
(defun mon-get-text-property-remove-all (txt-prop)
  "Delete all text in the current buffer with text property TXT-PROP.\n
:ALIASED-BY `mon-remove-text-with-property'\n
:SEE-ALSO .\n►►►"
  (let ((start (point-min))
	end)
    (unless (get-text-property start txt-prop)
      (setq start (next-single-property-change start txt-prop)))
    (while start
      (setq end (text-property-any start (point-max) txt-prop nil))
      (delete-region start (or end (point-max)))
      (setq start (when end
		    (next-single-property-change start txt-prop))))))
;;
(unless (and (intern-soft "mon-remove-text-with-property")
             (fboundp 'mon-remove-text-with-property))
  (defalias 'mon-remove-text-with-property 'mon-get-text-property-remove-all))


;;; ==============================
;;; OVERLAYS
;;; ==============================

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
:SEE-ALSO `mon-help-find-result-for-overlay', `mon-help-overlay-for-example',
`mon-help-overlay-on-region', `mon-help-overlay-result',
`mon-nuke-overlay-buffer'.\n►►►"
 (remove-overlays (buffer-end 0) (buffer-end 1) overlay-prop overlay-val))
;;




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
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-text-property-utils.el ends here
;;; EOF
