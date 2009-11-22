;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-classes.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-classes provides {description here}.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES:
;;; See file: ./cedet-cvs/ede/ede.el for examples of defining big classes
;;;
;;; SNIPPETS:
;;; (mon-insert-naf-mode-class-template  t)
;;; (mon-insert-defclass-template)
;;;
;;; REQUIRES:
;;; `naf-mode-insertion-utils'
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-classes.el)
;;; FIRST-PUBLISHED: <Timestamp: #{2009-11-21T21:11:31-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-10-04T04:54:08-04:00Z}#{09407} - by MON>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:


;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T10:34:34-04:00Z}#{09407} - by MON>
(defclass naf-mode-base (eieio-persistent eieio-instance-tracker eieio-named)
  ((version 
    :initarg :version
    :initform "1.0"
    :type string
    ;:custom string
    :label "Version"
    ;;:group (default name)
    :documentation "The version number used of class."))
  "Base `naf-mode' class. naf-mode classes inherit from this class."
  :abstract t)

;; (describe-class naf-mode-base)
;; INSTANCE ALLOCATED SLOTS:
;; file
;; CLASS-ALLOCATED-SLOTS:
;; do-backups
;; file-header-line
;; extension
;; tracking-symbol

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T10:33:30-04:00Z}#{09407} - by MON>
(defclass naf-mode-obj-timestamp-abst-intrfc ()
  ((obj-stamp 
    :initarg  :obj-stamp
    :type      string ;needs its own type!
    :label "TIME-STAMP"
    :documentation "Time obj was created."))
  "Class for time-stamping an obect's instantiation."
  :abstract t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-12T20:20:12-04:00Z}#{09422} - by MON>
(defmethod naf-timestamp-on-obj  ((obj naf-mode-obj-uid-abst-intrfc))
  "Put a timestamp on object's obj-stamp slot."
  (setf (slt-value obj :obj-stamp) (mon-stamp)))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-12T17:19:23-04:00Z}#{09421} - by MON>
(defclass naf-mode-obj-uid-abst-intrfc ()
  ((obj-uid 
    :initarg  :obj-uid
    :type     string               ;needs its own type!
    :label "OBJECT-UNIQUE-IDENTIFIER"
    :documentation 
    "OBJECT UID is a 32 character alphanumeric string."))
  "Abstract class to generate a unique identifier for object at instantiation."
  :abstract t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-12T17:47:23-04:00Z}#{09421} - by MON>
(defmethod naf-uid-on-obj ((obj naf-mode-obj-uid-abst-intrfc))
  "Put a UID on object's obj-uid slot."
  (setf (slot-value obj :obj-uid)  (mon-generate-prand-id)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T10:32:27-04:00Z}#{09407} - by MON>
(defclass naf-mode-instance-doc-abst-intrfc ()
  ((instance-doc
    :initarg :instance-doc
    :type string
    :label "INSTANCE-DOCUMENTATION"))
  "Documentation slot for class instances."
  :abstract t)

;;; ==============================
;;; Is this needed here?
;;; CREATED: <Timestamp: #{2009-10-12T20:11:49-04:00Z}#{09422} - by MON>
(defclass naf-mode-xrefs-keyword-abst-intrfc ()
   ((xrefs-keywords
    :initarg :xrefs-keywords
    :type list
    :initform nil
    :label "KEYWORD XREFS"
    ;; :printer                       ; {function}
    :documentation "" ))
    "Class for xrefing `naf-mode' objects."
  :abstract t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-12T20:11:49-04:00Z}#{09422} - by MON>
(defclass naf-mode-xrefs-face-abst-intrfc ()
   ((xrefs-face
    :initarg :xrefs-face
    :type list
    ;; :initform nil
    :label "FACE XREFS"
    ;; :printer                       ; {function}
    :documentation ""))
    "Class for xrefing `naf-mode' objects."
  :abstract t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-12T20:11:49-04:00Z}#{09422} - by MON>
(defclass naf-mode-xrefs-function-abst-intrfc ()
((xrefs-function
    :initarg :xrefs-function
    :type list
    ;; :initform nil
    :label "FUNCTION XREFS"
    ;; :printer                       ; {function}
    :documentation ""))
    "Class for xrefing `naf-mode' objects."
  :abstract t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-12T20:11:49-04:00Z}#{09422} - by MON>
(defclass naf-mode-xrefs-constant-abst-intrfc ()
   ((xrefs-constant
    :initarg :xrefs-constant
    :type list
    ;; :initform nil
    :label "CONSTANT XREFS"
    ;; :printer                       ; {function}    
    :documentation ""))
    "Class for xrefing `naf-mode' objects."
  :abstract t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-12T20:11:49-04:00Z}#{09422} - by MON>
(defclass naf-mode-xrefs-variable-abst-intrfc ()
   ((xrefs-variable
    :initarg :xrefs-variable
    :type list
    ;; :initform nil
    :label "VARIABLE XREFS"
    ;; :printer                       ; {function}    
    :documentation ""))
    "Class for xrefing `naf-mode' objects."
  :abstract t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-12T20:11:49-04:00Z}#{09422} - by MON>
(defclass naf-mode-xrefs-package-abst-intrfc ()
      ((xrefs-package
        :initarg :xref-package
        :type list
        ;; :initform nil
        :label "PACKAGE XREFS"
        ;; :printer                       ; {function}
        :documentation ""))
    "Class for xrefing `naf-mode' objects."
  :abstract t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T10:32:52-04:00Z}#{09407} - by MON>
(defclass naf-mode-xrefs-abst-intrfc (naf-mode-xrefs-keyword-abst-intrfc
                                      naf-mode-xrefs-function-abst-intrfc
                                      naf-mode-xrefs-constant-abst-intrfc
                                      naf-mode-xrefs-variable-abst-intrfc
                                      naf-mode-xrefs-face-abst-intrfc
                                      naf-mode-xrefs-package-abst-intrfc) ;(naf-mode-base)
  ((xref-id
    :initarg :xref-id
    :initform nil
    ;; :accessor                      ; {generic-function-name}
    ;; :writer                        ; {generic-function-name}
    ;; :reader                        ; {generic-function-name}
    ;; :custom                        ; {string}
    :label "XREF IDENTIFIER"
    ;; :group                         ; {customization-group}
    ;; :custom-groups                 ; {list}
    ;; :printer                       ; {function}
    ;; :protection                    ; {:public, :protected, :private}
    :documentation "")
   (xrefs-package :initarg :xref-package)
   (xrefs-keywords :initarg :xrefs-keywords)
   (xrefs-variable :initarg :xrefs-variable)
   (xrefs-constant :initarg :xrefs-constant)
   (xrefs-face     :initarg :xrefs-face))
  "Class for Xrefing `naf-mode' keyword list objects."
   :abstract t)

;;; ==============================
;;; (defclass naf-mode--abst-intrfc ()
;;;   ((
;;;     ))
;;;     "Class for xrefing `naf-mode' objects."
;;;   :abstract t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T10:32:27-04:00Z}#{09407} - by MON>
(defclass naf-mode-keyword-abst-intrfc ()
  ((kwrd-list 
    :initarg :kwrd-list
    :type list ;; {t, null, symbol, list, function, string,integer, number, float}
    ;; :initform null
    ;; :accessor        ; {generic-function-name}
    ;; :writer          ; {generic-function-name}
    ;; :reader          ; {generic-function-name}
    ;; :custom          ; {string}
    :label "KEYWORD-LIST"            ; {string}
    ;; :group           ; {customization-group}
    ;; :custom-groups   ; {list}
    ;; :printer         ; {function}
    ;; :protection      ; {:public, :protected, :private}
    :documentation "")
   (kwrd-alist 
    :initarg :kwrd-alist
    :type list
    ;; :initform nil
    :label "KEYWORD-ALIST"
    :documentation  "alist of keywords.")
   (kwrd-plist 
    :initarg :kwrd-alist
    ;; :initform nil
    :type list
    :label "KEYWORD-PLIST"
    :documentation "plist of keywords")
   (kwrd-hash 
    :initarg :kwrd-hash
    ;; :initform nil
    :type hash-table ;hash-table-p 
    :label "KEYWORD-HASH"
    :documentation "hash-table of keywords.")
   (kwrd-array 
    :initarg :kwrd-array
    :type array  ;arrayp
    ;; :initform nil
    :label "KEYWORD-ARRAY"
    :documentation "array of keywords.")
   (kwrd-regexps 
    :initarg :kwrd-regexps
    ;; :initform nil
    :type string
    :label "KEYWORD-REGEXPS"
    :documentation "Regexps of keywords."))
  "Base class for building `naf-mode' keywords."
  :abstract t)

;;(mon-insert-naf-mode-class-template "key-w-base-fc" nil t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T12:05:07-04:00Z}#{09407} - by MON>
(defclass naf-mode-kwrd-face-abst-intrfc ()
  ((kwrd-face 
    :initarg :kwrd-face
    :type  face              ;symbol ;face should be a type e.g. facep
    :initform null
    :label "KEYWORD-BASE-FACE" ; {string}
    ;; :printer         ; {function}
    :documentation 
    "Face which xrefs with naf-mode keywords.")
   (kwrd-face-xrefs 
    :initarg :kwrd-face-xrefs
    :type list
    :initform  nil
    :label "KEYWORD-FACE-XREFS"
    :allocation :class
    ;; :printer         ; {function}
    :documentation 
    "List of keyword class objects which xref with this face.")
   )
  ""
  :abstract t)

;;;(mon-insert-naf-mode-class-template "key-w-base-fc" nil t)


;;; ==============================
(defclass naf-nation-english (naf-mode-kwrd-abst-intrfc 
                              naf-mode-obj-uid-abst-intrfc
                              naf-mode-instance-doc-abst-intrfc)
  ((obj-uid 
    :initarg  :obj-uid
    (kwrd-list 
     :initarg :kwrd-list)
   (kwrd-rgexp
    :initarg :kwrd-list)
   (instance-doc
    :initarg :instance-doc)

;;; ==============================
(provide 'naf-mode-classes)
;;; ==============================

;;; ================================================================
;;; naf-mode-classes.el ends here
;;; EOF
