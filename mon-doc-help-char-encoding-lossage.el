;;; mon-doc-help-char-encoding-lossage.el --- Show charset encoding lossage
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-doc-help-char-encoding-lossage.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2011-03-06T00:18:18-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-doc-help-char-encoding-lossage provides Show charset encoding lossage
;;; :EXAMPLE
;; (mon-get-encoding-point-hist-map-plists *cp1252-8859-15-lossage* *cp1252-8859-15-lossage-rules* t)
;; (mon-get-encoding-point-hist-map-plists *cp1252-8859-1-lossage* *cp1252-8859-1-lossage-rules* t)
;; (mon-get-encoding-point-hist-map-plists *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules* t)
;;
;; :SEE (URL `http://www.eki.ee/letter/')
;; :SEE (URL `http://en.wikipedia.org/wiki/Windows-1252')
;; :SEE (URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-15')
;; :SEE (URL `http://www.eki.ee/letter/chardata.cgi?cp=CP1252+%28Western%29&cp1=8859-1')
;; :SEE (URL `http://www.cs.tut.fi/~jkorpela/latin9.html')
;;
;; FUNCTIONS:►►►
;; `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
;; `mon-get-encoding-point-hist', `mon-get-encoding-map-results',
;; `mon-get-encoding-point-hist-map-plists',
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
;; `*cp1252-8859-15-lossage*', `*cp1252-8859-15-lossage-rules*',
;; `*cp1252-8859-1-lossage*', `*cp1252-8859-1-lossage-rules*',
;; `*8859-1-8859-15-lossage*', `*8859-1-8859-15-lossage-rules*',
;;
;; GROUPS:
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
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-char-encoding-lossage.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-doc-help-char-encoding-lossage. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2011-03-06T00:18:18-05:00Z}#{11097} - by MON KEY>
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
;; Copyright © 2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))


;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:36-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-codepoint (from-plist w-key &optional when-not-char &rest rest)
  (let ((mgfe-if (plist-get from-plist w-key)))
    (and mgfe-if 
         (or (and (= (length mgfe-if) 1)
                  (or (null when-not-char)
                      (stringp when-not-char)
                      (eq when-not-char t))
                  (cons t (format "%s :CODE-CHAR #\\%s :CHAR-CODE #x%04X"
                                  w-key mgfe-if (string-to-char mgfe-if))))
             (and when-not-char 
                  (stringp when-not-char)
                  (or (and (string-equal mgfe-if when-not-char)
                           (cons t (format "%s %S" w-key mgfe-if)))
                      (mon-format :w-fun #'error
                                  :w-spec '("Arg WHEN-NOT-CHAR was: %S\n"
                                            "but no match in arg FROM-PLIST got:\n %S\n")
                                  :w-args `(,when-not-char ,from-plist))))
             (and when-not-char 
                  (eq when-not-char t)
                  (equal (nth (- (length from-plist) 2) from-plist) w-key)
                  (equal (mon-list-last from-plist) mgfe-if)
                  (cons t (format "%s %S" w-key mgfe-if)))
             (cons t "")))))


;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:39-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-position-lossage (from-plist pos-key &optional w-raw-byte)
  (let ((mgp-if (plist-get from-plist pos-key)))
    (cons 
     (and mgp-if
          (or (and w-raw-byte mgp-if) t))
     (and mgp-if
          (format "%s :HEX #x%X :OCTAL #o%o :DECIMAL %d%s"
                  pos-key
                  mgp-if  mgp-if  mgp-if 
                  (or (and w-raw-byte " :RAW-BYTE ") ""))))))


;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:41-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-point-hist (w-plist rules &optional w-raw-byte); &rest rules)
  (let ((rstls '()))
    (dolist (r rules (nreverse rstls))
      (let ((rl-if (apply (car r) w-plist `(,@(cdr r) ,w-raw-byte))))
        (when (car rl-if) (push rl-if rstls))))))


;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:44-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-map-results (results w-buffer)
  (with-current-buffer w-buffer)
  (insert "\n" (make-string 5 45) "\n")
  (mapc #'(lambda (mgemr-L-0) 
            (let ((mgemr-hd (car mgemr-L-0)))
              (and mgemr-hd
                   (or (and (eq mgemr-hd t)
                            (or (insert (cdr mgemr-L-0) " ") t))
                       (and (integerp mgemr-hd)
                            (or (insert (cdr mgemr-L-0)) t)
                            (or (insert-byte mgemr-hd 1) t)
                            (terpri (current-buffer)))
                       ;;(terpri (current-buffer))
                       )))
            (unless (eql (line-beginning-position) (line-end-position)) 
              (terpri (current-buffer))))
        results))


;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:46-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-point-hist-map-plists (w-plists w-rules &optional w-raw-byte) ;; w-buffer-named
  (let ((mgephmp-bfr (get-buffer-create "*ENCODING-LOSSAGE*")))
    (with-current-buffer mgephmp-bfr
      (erase-buffer)
      (save-excursion 
        (mapc #'(lambda (wplst)
                  (mon-get-encoding-map-results 
                   (mon-get-encoding-point-hist wplst w-rules w-raw-byte)
                   (current-buffer)))
              w-plists))
      (whitespace-cleanup) 
      (display-buffer mgephmp-bfr t))))

;;; ==============================
;; Changes from ISO-8859-1 --> ISO-8859-15
;; :POSITION  #xA4   #xA6    #xA8    #xB4    #xB8    #xBC    #xBD    #xBE
;;   8859-1    ¤      ¦       ¨       ´       ¸       ¼       ½       ¾
;;   8859-15   €      Š       š       Ž       ž       Œ       œ       Ÿ
;; *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules*
(defvar *8859-1-8859-15-lossage*
      '((:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac")
        (:8859-1 "\u00a6"  :CODE-PSN-ORIG #xa6  :8859-15 "\u0160")
        (:8859-1 "\u00a8"  :CODE-PSN-ORIG #xa8  :8859-15 "\u0161")
        (:8859-1 "\u00b4"  :CODE-PSN-ORIG #xb4  :8859-15 "\u017D")
        (:8859-1 "\u00b8"  :CODE-PSN-ORIG #xb8  :8859-15 "\u017E")
        (:8859-1 "\u00bc"  :CODE-PSN-ORIG #xbc  :8859-15 "\u0152")
        (:8859-1 "\u00bd"  :CODE-PSN-ORIG #xbd  :8859-15 "\u0153")
        (:8859-1 "\u00be"  :CODE-PSN-ORIG #xbe  :8859-15 "\u0178")))
;;
(defvar *8859-1-8859-15-lossage-rules*
      '((mon-get-encoding-codepoint :8859-1)
        (mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
        (mon-get-encoding-codepoint :8859-15)))

;;; ==============================
;; http://www.eki.ee/letter/chardata.cgi?cp=CP1252+%28Western%29&cp1=8859-15
;; plists
;; *cp1252-8859-15-lossage* *cp1252-8859-15-lossage-rules*
(defvar *cp1252-8859-15-lossage* 
 '((:CP1252 "\u201A"  :CODE-PSN-ORIG #x82  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u0192"  :CODE-PSN-ORIG #x83  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u201E"  :CODE-PSN-ORIG #x84  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2026"  :CODE-PSN-ORIG #x85  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2020"  :CODE-PSN-ORIG #x86  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2021"  :CODE-PSN-ORIG #x87  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u02C6"  :CODE-PSN-ORIG #x88  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2030"  :CODE-PSN-ORIG #x89  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2039"  :CODE-PSN-ORIG #x8B  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2018"  :CODE-PSN-ORIG #x91  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2019"  :CODE-PSN-ORIG #x92  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u201C"  :CODE-PSN-ORIG #x93  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u201D"  :CODE-PSN-ORIG #x94  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2022"  :CODE-PSN-ORIG #x95  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2013"  :CODE-PSN-ORIG #x96  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2014"  :CODE-PSN-ORIG #x97  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u02DC"  :CODE-PSN-ORIG #x98  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u2122"  :CODE-PSN-ORIG #x99  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u203A"  :CODE-PSN-ORIG #x9B  :8859-15 "unassigned" :CHANGE   "lost")
   (:CP1252 "\u0161"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA8)
   (:CP1252 "\u00A8"  :CODE-PSN-ORIG #xA8  :8859-15 "\u0161"     :CHANGE   "lost")
   (:CP1252 "\u017D"  :CODE-PSN-ORIG #x8E  :8859-15 "unassigned" :CODE-PSN-MOVE  #xB4)
   (:CP1252 "\u00B4"  :CODE-PSN-ORIG #xB4  :8859-15 "\u017D"     :CHANGE   "lost")
   (:CP1252 "\u0152"  :CODE-PSN-ORIG #x8C  :8859-15 "unassigned" :CODE-PSN-MOVE  #xBC)
   (:CP1252 "\u00BC"  :CODE-PSN-ORIG #xBC  :8859-15 "\u0152"     :CHANGE   "lost")
   (:CP1252 "\u0153"  :CODE-PSN-ORIG #x9C  :8859-15 "unassigned" :CODE-PSN-MOVE  #xBD)
   (:CP1252 "\u00BD"  :CODE-PSN-ORIG #xBD  :8859-15 "\u0153"     :CHANGE   "lost")
   (:CP1252 "\u0160"  :CODE-PSN-ORIG #x8A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA6)
   (:CP1252 "\u00A6"  :CODE-PSN-ORIG #xA6  :8859-15 "\u0160"     :CHANGE   "lost")
   (:CP1252 "\u017E"  :CODE-PSN-ORIG #x9E  :8859-15 "unassigned" :CODE-PSN-MOVE  #xB8)
   (:CP1252 "\u00B8"  :CODE-PSN-ORIG #xB8  :8859-15 "\u017E"     :CHANGE   "lost")
   (:CP1252 "\u20AC"  :CODE-PSN-ORIG #x80  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA4)
   (:CP1252 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20AC"     :CHANGE   "lost")
   (:CP1252 "\u0178"  :CODE-PSN-ORIG #x9F  :8859-15 "unassigned" :CODE-PSN-MOVE  #xBE)
   (:CP1252 "\u00BE"  :CODE-PSN-ORIG #xBE  :8859-15 "\u0178"     :CHANGE   "lost")))
  ;; RULES
(defvar *cp1252-8859-15-lossage-rules*
      '((mon-get-encoding-codepoint :CP1252)
        (mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
        (mon-get-encoding-codepoint :8859-15 "unassigned")
        (mon-get-encoding-position-lossage   :CODE-PSN-MOVE)
        (mon-get-encoding-codepoint :CHANGE "lost")))

;;; ==============================
;; (URL `http://www.eki.ee/letter/chardata.cgi?cp=CP1252+%28Western%29&cp1=8859-1')
;; :CP1252 --> :8859-1
;; *cp1252-8859-1-lossage* *cp1252-8859-1-lossage-rules*
;;
(defvar *cp1252-8859-1-lossage*
      '((:CP1252 "\u20AC"  :CODE-PSN-ORIG #x80  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u201A"  :CODE-PSN-ORIG #x82  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u0192"  :CODE-PSN-ORIG #x83  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u201E"  :CODE-PSN-ORIG #x84  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2026"  :CODE-PSN-ORIG #x85  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2020"  :CODE-PSN-ORIG #x86  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2021"  :CODE-PSN-ORIG #x87  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u02C6"  :CODE-PSN-ORIG #x88  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2030"  :CODE-PSN-ORIG #x89  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u0160"  :CODE-PSN-ORIG #x8A  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2039"  :CODE-PSN-ORIG #x8B  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u0152"  :CODE-PSN-ORIG #x8C  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u017D"  :CODE-PSN-ORIG #x8E  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2018"  :CODE-PSN-ORIG #x91  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2019"  :CODE-PSN-ORIG #x92  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u201C"  :CODE-PSN-ORIG #x93  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u201D"  :CODE-PSN-ORIG #x94  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2022"  :CODE-PSN-ORIG #x95  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2013"  :CODE-PSN-ORIG #x96  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2014"  :CODE-PSN-ORIG #x97  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u02DC"  :CODE-PSN-ORIG #x98  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u2122"  :CODE-PSN-ORIG #x99  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u0161"  :CODE-PSN-ORIG #x9A  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u203A"  :CODE-PSN-ORIG #x9B  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u0153"  :CODE-PSN-ORIG #x9C  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u017E"  :CODE-PSN-ORIG #x9E  :8859-1 "unassigned" :CHANGE   "lost")
        (:CP1252 "\u0178"  :CODE-PSN-ORIG #x9F  :8859-1 "unassigned" :CHANGE   "lost")))
;; :RULES
(defvar *cp1252-8859-1-lossage-rules*
      '((mon-get-encoding-codepoint :CP1252)
        (mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
        (mon-get-encoding-codepoint :8859-1 "unassigned")
        (mon-get-encoding-position-lossage   :CODE-PSN-MOVE)
        (mon-get-encoding-codepoint :CHANGE "lost")))

 
;;; ==============================
;;
;; `mon-get-encoding-codepoint'
;;
;; (mon-get-encoding-codepoint '(:8859-1 "\u00A4"  :HEX-POSITION #xA4  :8859-15 "\u20ac") :8859-1 t)
;; (mon-get-encoding-codepoint '(:8859-1 "\u00A4"  :HEX-POSITION #xA4  :8859-15 "\u20ac") :8859-15)
;; (mon-get-encoding-codepoint '(:8859-1 "\u00A4"  :HEX-POSITION #xA4  :8859-15 "\u20ac") :8859-15 "unassigned")
;; (mon-get-encoding-codepoint '(:8859-1 "\u00A4"  :HEX-POSITION #xA4  :8859-15 "unassigned") :8859-15 "unassigned")
;; (mon-get-encoding-codepoint '(:CP1252 "\u00BE"  :HEX-POSITION #xBE  :8859-15 "\u0178"     :CHANGE   "lost") :CHANGE  "lost")
;; (mon-get-encoding-codepoint '(:CP1252 "\u00BE"  :HEX-POSITION #xBE  :8859-15 "\u0178"     :CHANGE   "lost") :CHANGE  t) 
;; (mon-get-encoding-codepoint '(:CP1252 "\u00BE"  :HEX-POSITION #xBE  :8859-15 "\u0178"     :CHANGE   "lost") :CHANGE "bubba")
;; (mon-get-encoding-codepoint '(:CP1252 "\u00BE"  :HEX-POSITION #xBE  :8859-15 "\u0178"     :CHANGE   "lost") :NOT-key "bubba")
;; (mon-get-encoding-codepoint '(:CP1252 "\u00BE"  :HEX-POSITION #xBE  :8859-15 "\u0178"     :CHANGE   "lost") :CHANGE)
;;
;; (let ((chk '(mon-get-encoding-codepoint :8859-1))
;;       (pl '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac")))
;;   (apply (car chk)  pl (cdr chk)))
;;; ==============================
;;
;; `mon-get-encoding-position-lossage'
;;
;; (mon-get-encoding-codepoint '(:CP1252 "\u00BE"  :HEX-POSITION #xBE  :8859-15 "\u0178"     :CHANGE   "lost") :CHANGE t)
;; (mon-get-encoding-position-lossage '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac") :CODE-PSN-ORIG )
;; (mon-get-encoding-position-lossage '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac") :CODE-PSN-ORIG t)
;; (mon-get-encoding-position-lossage '(:CP1252 "\u0161"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA8) :CODE-PSN-MOVE)
;; (mon-get-encoding-position-lossage '(:CP1252 "\u0161"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA8) :CODE-PSN-MOVE t)
;; (cdr (mon-get-encoding-position-lossage '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac") :CODE-PSN-ORIG  t))
;; (car (mon-get-encoding-position-lossage  '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac")  :NOT-A-PSN-ORIG t))
;;
;;; ==============================
;;
;; `mon-get-encoding-point-hist'
;;
;; (mon-get-encoding-point-hist
;;  ;; '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac")  
;;  ;; '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA8)
;;  '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CHANGE "lost")
;;  '((%mon-get-encoding-codepoint :8859-1)
;;    (mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
;;    (%mon-get-encoding-codepoint :8859-15 "unassigned")
;;    (mon-get-encoding-position-lossage   :CODE-PSN-MOVE)
;;    (mon-get-encoding-codepoint :CHANGE "lost"))
;;  t)
;;
;;; ==============================
;;
;; `mon-get-encoding-point-hist-map-plists'
;; (mon-get-encoding-point-hist-map-plists *cp1252-8859-15-lossage* *cp1252-8859-15-lossage-rules* t)
;; (mon-get-encoding-point-hist-map-plists *cp1252-8859-1-lossage* *cp1252-8859-1-lossage-rules* t)
;; (mon-get-encoding-point-hist-map-plists *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules* t)
;;; ==============================
;;
;;; ==============================
;;
;; `mon-get-encoding-map-results'
;; (mon-get-encoding-map-results 
;;  (mon-get-encoding-point-hist
;;   ;; '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac")  
;;   ;; '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA8)
;;   '(:8859-1 "\u00A4"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CHANGE "lost")
;;   t
;;   '(mon-get-encoding-codepoint :8859-1)
;;   '(mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
;;   '(mon-get-encoding-codepoint :8859-15 "unassigned")
;;   '(mon-get-encoding-position-lossage   :CODE-PSN-MOVE)
;;   '(mon-get-encoding-codepoint :CHANGE "lost"))
;;  (current-buffer))
;;
;;; ==============================


;;; ==============================
(provide 'mon-doc-help-char-encoding-lossage)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-doc-help-char-encoding-lossage.el ends here
;;; EOF
