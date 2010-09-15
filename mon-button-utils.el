;;; mon-button-utils.el --- utilities for examining button properties
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-button-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-09-14T17:56:12-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: button, emacs, lisp

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-button-utils provides { some description here. }
;;
;; FUNCTIONS:►►►
;; `mon-button-get-plist', `mon-button-get-plist-props',
;; `mon-button-at-point-p', `mon-button-at-point-describe-button-plist',
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
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
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
;; URL: http://www.emacswiki.org/emacs/mon-button-utils.el
;; FIRST-PUBLISHED:<Timestamp: #{2010-09-14T20:58:02-04:00Z}#{10372} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-button-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-09-14T17:56:12-04:00Z}#{10372} - by MON KEY>
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
;;; :CHANGESET 2128
;;; :CREATED <Timestamp: #{2010-09-14T12:09:11-04:00Z}#{10372} - by MON KEY>
(defun mon-button-at-point-p (&optional putative-button-at)
  "Return non-nil when ther is a `button` property on char at point.\n
Arg PUTATIVE-BUTTON-AT is a buffer-position to check.
:EXAMPLE\n\n\(progn
  \(goto-char \(buffer-end 0\)\)
  \(while \(not \(get-char-property \(point\) 'button\)\) \(forward-char\)\)
  \(point\)\)\n
:SEE-ALSO `mon-button-get-plist', `mon-button-get-plist-props',
`mon-button-at-point-p', `mon-button-at-point-describe-button-plist'.\n►►►"
  (let ((gcp-btn (get-char-property (or putative-button-at (point)) 'button))
        hld-prps)
    (when gcp-btn
      (setplist hld-prps 
                (cond ((car-safe gcp-btn)
                       `(:BUTTON-CATEGORY-SYMBOL ,(get-char-property (or putative-button-at (point)) 'category) 
                         :BUTTON-TXTPROP t :BUTTON-OVERLAY nil))
                      ((overlayp gcp-btn)
                       `(:BUTTON-CATEGORY-SYMBOL ,(overlay-get gcp-btn 'category) 
                         :BUTTON-OVERLAY t :BUTTON-IS-TEXT nil))))
      (setq gcp-btn (symbol-plist (plist-get (symbol-plist hld-prps) :BUTTON-CATEGORY-SYMBOL)))
      ;;(setq hld-prps 
      (plist-put (symbol-plist hld-prps) :BUTTON-PLIST gcp-btn)
      (let ((btn-pl-prps (plist-get (symbol-plist hld-prps) :BUTTON-PLIST))
            invrt-bpp)
        (when (plist-get btn-pl-prps 'type)
          (plist-put (symbol-plist hld-prps) :BUTTON-TYPE (plist-get btn-pl-prps 'type)))
        (when (plist-get btn-pl-prps 'supertype)
          (plist-put (symbol-plist hld-prps) :BUTTON-SUPER (plist-get btn-pl-prps 'supertype))))
      (symbol-plist hld-prps))))


;;; ==============================
;;; This works except for the ugly symbol name handling:
;;; :CHANGESET 2133
;;; :CREATED <Timestamp: #{2010-09-14T20:17:15-04:00Z}#{10372} - by MON KEY>
(defun mon-button-at-point-describe-button-plist (&optional button-at-psn)
  "Return pp'd display of buttons current props as if by `apropos-describe-plist'.
:EXAMPLE\n\n
:SEE-ALSO `apropos-describe-plist' `mon-button-get-plist', `mon-button-get-plist-props',
`mon-button-at-point-p', `mon-button-at-point-describe-button-plist'.\n►►►"
  (interactive)
  (let ((mbapp (make-symbol "--temp-adp--"))) 
    (setplist mbapp 
              (mon-button-at-point-p (or button-at-psn (point))))
    (apropos-describe-plist mbapp))) 

;;; ==============================
;;; :CHANGESET 2128
;;; :CREATED <Timestamp: #{2010-09-14T11:41:00-04:00Z}#{10372} - by MON KEY>
(defun mon-button-get-plist (type-button)
  "Return a plist of enumerating properites for a button-type.\n
TYPE-BUTTON is a button constructed with `define-button-type'
Properties returned are those from the plist of symbol returned from
`button-category-symbol' with TYPE-BUTTON as arg.
:NOTE Directly accessing the button properties associated with the TYPE-BUTTON
symbol is not possible as that symbol does not actually hold the button
properties associated with it.\n
When `define-button-type' constructs a button it generates a separate uninterned
symbol having the form:\n
 <SYMBOL>-button|n
As such, accessors of button properties are indirected through this separate
uninterned symbol via its `category` property. Because the `category` property
is implemented by both overlays and text-properties this indirection allows: 
- flexible creation of buttons with `make-text-button', `make-button' or
  directly with `make-overlay', `overlay-put', `add-text-properties', etc.;\n
- guarding against inadverdent name clashes; 
- provision of a lightweight subertype <-> subtype inheritance hierarchy which
  can be shared/extended/modified across the entire regime of Eacs
  \"text-property\" frobbing facilites i.e. text-properties, overlays, widgets,
  faces, char-properties, keymaps. etc.
:EXAMPLE\n\n
:SEE-ALSO `mon-button-get-plist', `mon-button-get-plist-props',
`mon-button-at-point-p', `mon-button-at-point-describe-button-plist'.\n►►►"
  (let ((get-bcs (and (bound-and-true-p type-button)
                      (get type-button 'button-category-symbol))))
    (unless (null get-bcs)
      `(:BUTTON-TYPE ,type-button
        :BUTTON-CATEGORY-SYMBOL ,get-bcs
        :BUTTON-PLIST ,(symbol-plist get-bcs)))))

;;; ==============================
;;; :CHANGESET 2128
;;; :CREATED <Timestamp: #{2010-09-14T10:50:10-04:00Z}#{10372} - by MON KEY>
(defun mon-button-get-plist-props (type-of-button)
  "Return a plist properties for TYPE-OF-BUTTON.\n
TYPE-OF-BUTTON is a button constructed with `define-button-type'.\n
:EXAMPLE\n\n\(mon-button-get-plist-props 'button\)\n
:ALIASED-BY `mon-get-button-plist-props'\nn
:SEE-ALSO `mon-button-get-plist', `mon-button-get-plist-props',
`mon-button-at-point-p', `mon-button-at-point-describe-button-plist'.\n►►►"
  (let* ((cp-sypl (mon-button-get-plist type-of-button))
         (cp-pl   (unless (null cp-sypl) 
                    (plist-get cp-sypl :BUTTON-PLIST)))
        hds)
    (unless (null cp-pl)
      (while cp-pl
        (push (car cp-pl) hds)
        (setq cp-pl (cddr cp-pl)))
        (nreverse hds))))
;; 
(defalias 'mon-get-button-plist-props 'mon-button-get-plist-props)



;;; ==============================
(provide 'mon-button-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-button-utils.el ends here
;;; EOF
