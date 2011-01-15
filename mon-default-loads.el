;;; mon-default-loads.el --- Constants and vars for MON system portability
;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-

;;; ================================================================
;; Copyright © 2009-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-default-loads.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-08-09T04:29.20-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: local, environment, installation

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-default-loads provides default constants and vars for maintaining system
;; portability across MON systems.  Negotiates the startup loadpaths in prep
;; for mon-default-start-loads.el
;;
;; Functionality of this file requires that the procedure:
;; `mon-system-type-conditionals' be present. On MON systems it is 
;; defined in a private site local file along with other alists 
;; `*mon-emacsd*', `set-emacs-root' which encapsulate user data which needn't
;; be made availabe in public source.  These alists inform the following
;; constants defined which are bound here: `IS-BUG-P', `IS-BUG-P-REMOTE',
;; `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P', `IS-MON-SYSTEM-P', 
;; `IS-NOT-A-MON-SYSTEM'
;;
;; FUNCTIONS:►►►
;; `mon-get-mon-emacsd-paths',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;; `IS-W32-P', `IS-GNU-P', `IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-W32',
;; `IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-MON-P', `IS-NOT-A-MON-SYSTEM',
;;
;; `*mon-emacs-root*', `*mon-site-lisp-root*', `*mon-user-emacsd*',
;; `*mon-naf-mode-notes*', `*mon-naf-mode-root*', `*mon-ebay-tmplt-mode-root*',
;; `*mon-local-emacs-temp-dir*'
;;
;; VARIABLES:
;; `*IS-MON-OBARRAY*', `set-emacs-root', `*mon-default-start-loads-xrefs*',
;; 
;; GROUPS:
;; `mon-default-loads'
;;
;; AIASED/ADVISED/SUBST'D:
;; `win32p'     -> `IS-W32-P'
;; `gnu-linuxp' -> `IS-GNU-P'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; REQUIRES:
;;
;; TODO:
;;
;; NOTES:
;; The constant `IS-MON-SYSTEM-P' is evaluated at the top of:
;; :FILE mon-utils.el when bootstrapping the rest of MON's packages.
;;
;; SNIPPETS:
;;
;; THIRD PARTY CODE:
;;
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;;
;; URL: http://www.emacswiki.org/emacs/mon-default-loads.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T12:22:19-04:00Z}#{09393} - by MON>
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-08-09T04:29.20-04:00Z} - by MON KEY>
;; 
;; ================================================================

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
;; Copyright © 2009-2011 MON KEY 
;;; ==============================

;;; CODE:

 
;;; ==============================
(provide 'mon-default-loads)
(require 'mon-default-loads)

(eval-when-compile (require 'cl))

(unless (featurep 'mon-site-local-defaults)
  (require 'mon-site-local-defaults nil t))


;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T15:33:45-05:00Z}#{11022} - by MON KEY>
(defgroup mon-default-loads nil
  "Customization group for variables and functions of :FILE mon-default-loads.el\n
:SEE-ALSO .\n►►►"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE" "http://www.emacswiki.org/emacs/mon-default-loads.el")
  :link '(emacs-library-link "mon-default-loads.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2291
;;; :CREATED <Timestamp: #{2010-11-09T16:53:31-05:00Z}#{10452} - by MON KEY>
(defvar *IS-MON-OBARRAY* nil
  "Obarray for determining if a system is a MON system.\n
If *IS-MON-OBARRAY* is present in obarray following values will satisfy the form:\n
 \(and \(intern-soft <VALUE> *IS-MON-OBARRAY*\)
      \(bound-and-true-p <VALUE>\)\)\n
 `IS-NOT-A-MON-SYSTEM' `IS-MON-SYSTEM-P'
 `IS-MON-P-W32'        `IS-MON-P-GNU'
 `IS-BUG-P'            `IS-BUG-P-REMOTE'
 `IS-GNU-P'            `IS-W32-P'\n
:EXAMPLE\n\n
:SEE-ALSO `obarray'.\n►►►")
;;
(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
  (setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :CHANGESET 2291
;;; :CREATED <Timestamp: #{2010-11-09T18:48:47-05:00Z}#{10452} - by MON KEY>
(defvar *IS-MON-OBARRAY-SYMS* nil
  "List of symbols interned in obarray `*IS-MON-OBARRAY*'.\n
:SEE-ALSO .\n►►►")
;;
(unless (and (intern-soft "*IS-MON-OBARRAY-SYMS*" obarray)
             (bound-and-true-p *IS-MON-OBARRAY-SYMS*))
  (setq *IS-MON-OBARRAY-SYMS*
        '("IS-NOT-A-MON-SYSTEM" "IS-MON-SYSTEM-P"
          "IS-MON-P-W32"        "IS-MON-P-GNU"
          "IS-BUG-P"            "IS-BUG-P-REMOTE"
          "IS-GNU-P"            "IS-W32-P")))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T22:40:33-04:00Z}#{10136} - by MON KEY>
(defcustom *mon-default-loads-xrefs* nil
  "Xrefing list of symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-default-loads.el\n
:EXAMPLE\n\n\(symbol-value '*mon-default-start-loads-xrefs*\)\n
\(symbol-value \(nth 3 *mon-default-start-loads-xrefs*\)\)\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*naf-mode-xref-of-xrefs*'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-default-loads
  :group 'mon-xrefs)
;;
;; We try to ensure that the symbols contained of `*mon-default-loads-xrefs*'
;; don't exist at loadtime and delay binding t at top-level until after
;; "mon-default-start-loads" which only loads after a host of other dependencise
;; are up and running.
(eval-after-load "mon-default-loads"
  '(eval-after-load "mon-default-start-loads" 
     '(unless (and (intern-soft "*mon-default-loads-xrefs*" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p *mon-default-loads-xrefs*))
        (setq *mon-default-loads-xrefs*
              '(*mon-default-loads-xrefs* mon-get-mon-emacsd-paths
                set-emacs-root IS-W32-P IS-GNU-P IS-BUG-P IS-BUG-P-REMOTE
                IS-MON-P-W32 IS-MON-P-GNU IS-MON-P IS-NOT-A-MON-SYSTEM
                IS-MON-SYSTEM-P *IS-MON-OBARRAY-SYMS* *IS-MON-OBARRAY*
                *mon-emacs-root* *mon-site-lisp-root* *mon-naf-mode-root*
                *mon-naf-mode-notes* *mon-ebay-tmplt-mode-root*
                *mon-user-emacsd* *mon-local-emacs-temp-dir*))
        (custom-note-var-changed '*mon-default-start-loads-xrefs*))))

;;; ==============================
;;; :NOTE This gets compiled as `mon-get-emacsd-paths' in mon-utils.el
;;; :CREATED <Timestamp: #{2010-01-25T20:17:50-05:00Z}#{10042} - by MON KEY>
(defun mon-get-mon-emacsd-paths (&optional insrtp intrp)
  "Return pretty string mapping the current user's values of `*mon-emacsd*'.\n
When INSERTP is non-nil insert return value in buffer. Moves Point.\n
When called-interactively with prefix arg insert pp'd return value in
buffer followed by return value as list. Does not move point.\n
When called-interactively without prefix return value displayed in mini-buffer.\n
:EXAMPLE\n\n\(mon-get-mon-emacsd-paths\)\n
\(mon-get-mon-emacsd-paths nil t\)\n
:SEE-ALSO `mon-get-env-vars-emacs', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-user-system-conditionals-TEST',
`mon-build-misc-path-example', `*mon-emacs-root*', `*mon-site-lisp-root*',
`*mon-naf-mode-root*', `*mon-ebay-tmplt-mode-root*', `IS-MON-SYSTEM-P',
`IS-NOT-A-MON-SYSTEM', `IS-GNU-P', `IS-W32-P', `IS-MON-P', `IS-MON-P-W32',
`IS-MON-P-GNU', `IS-BUG-P', `IS-BUG-P-REMOTE'.\n►►►"
  (interactive "P\np")
  (if (and (intern-soft "*mon-emacsd*" obarray)
           (bound-and-true-p *mon-emacsd*))
      (let* ((chk-usr)
             (pths (assoc 
                    (dolist (u (cadr (assoc 5 *mon-emacsd*)) chk-usr)
                      (when (eval (car u))
                        (setq chk-usr (cadr u))))
                    *mon-emacsd*))
             gthr-pths)
        (dotimes (i (length (car *mon-emacsd*)) 
                    (progn 
                      (setq gthr-pths (nreverse gthr-pths))
                      (setq gthr-pths (mapconcat 'identity gthr-pths "\n"))
                      (setq gthr-pths (cons (assoc chk-usr *mon-emacsd*) gthr-pths))))
          (let ((this-nth-path (nth i pths)))
            (push (format "Value of nth %d is: %s" i 
                          (if (unless (or (numberp this-nth-path) 
                                          (null this-nth-path))
                                (file-name-absolute-p this-nth-path))
                              (file-truename this-nth-path)
                            this-nth-path)) gthr-pths)))
        (cond ((and intrp (not insrtp)) (message (cdr gthr-pths)))
              ((and insrtp intrp) (save-excursion 
                                    (newline)
                                    (princ (cdr gthr-pths) (current-buffer))
                                    (newline)
                                    (prin1 (car gthr-pths)(current-buffer))))
              (insrtp (prin1 gthr-pths (current-buffer)))
              (t (car gthr-pths))))
    ;; Only signal an error if this isn't a MONish, else warn.
    (if (and (featurep 'mon-site-local-defaults)
             (intern-soft "mon-user-name-conditionals" obarray)
             (let ((munc (mon-user-name-conditionals)))
               (cond ((string-equal munc "IS-MON-P-W32") t) 
                     ((string-equal munc "IS-MON-P-GNU") t)
                     ((string-equal munc "IS-BUG-P") t)
                     ((string-equal munc "IS-BUG-P-REMOTE") t))))
        (warn (concat ":VARIABLE `*mon-emacsd*' -- perversely bound or non-existent"
                      " :SEE :FILE mon-site-local-defaults.el"))
      (error (concat ":VARIABLE `*mon-emacsd*' -- not bound or non-existent"
                     " :SEE :FILE mon-site-local-defaults.el")))))
;;
;;; :TEST-ME (mon-get-mon-emacsd-paths)
;;; :TEST-ME (mon-get-mon-emacsd-paths nil t)
;;; :TEST-ME (mon-get-mon-emacsd-paths t t)
;;; :TEST-ME (call-interactively 'mon-get-mon-emacsd-paths)
;;; :TEST-ME (let ((IS-BUG-P t)) (mon-get-mon-emacsd-paths))

;;; ==============================
(defconst IS-W32-P
  (if (and (intern-soft "mon-system-type-conditionals" obarray)
           (fboundp 'mon-system-type-conditionals))
      (string-equal (mon-system-type-conditionals) "IS-W32-P")
    (eq system-type 'windows-nt))
  "*Return non-nil when current machine is running a win32 OS.\n
Used in conditional system type tests in lieu of:\n
 (equal system-type 'windows-nt).\n
:EXAMPLE\n\nIS-W32-P\n
:ALIASED-BY `win32p'\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-user-system-conditionals-TEST', `IS-GNU-P',
`IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-NOT-A-MON-SYSTEM',
`IS-MON-P', `IS-BUG-P', `IS-BUG-P-REMOTE', `*mon-site-lisp-root*',
`*mon-user-emacsd*', `*mon-emacs-root*', `*mon-naf-mode-root*',
`*mon-ebay-tmplt-mode-root*'.\n►►►")
;;
(when (and (intern-soft "IS-W32-P" obarray)
           (bound-and-true-p IS-W32-P))
  (unless (and  (and (intern-soft "win32p" obarray)
                     (bound-and-true-p win32p))))
(defvaralias 'win32p 'IS-W32-P))
;;
;;; :TEST-ME  IS-W32-P
;;
;;;(progn (makunbound 'IS-W32-P) (unintern "IS-W32-P" obarray) 
;;          (makunbound 'win32p) (unintern "win32p" obarray) )

;;; ==============================
(defconst IS-GNU-P
  (if (and (intern-soft "mon-system-type-conditionals" obarray)
           (fboundp 'mon-system-type-conditionals))
      (string-equal (mon-system-type-conditionals) "IS-GNU-P")
    (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
  "*Return non-nil if current machine is running on a GNU/Linux OS.\n
Used in conditional system type tests in lieu of:\n
 (equal system-type 'gnu/linux).\n
:EXAMPLE\n\nIS-GNU-P\n
:ALIASED-BY `gnu-linuxp'\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-user-system-conditionals-TEST', `IS-W32-P',
`IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-NOT-A-MON-SYSTEM',
`IS-MON-P', `IS-BUG-P', `IS-BUG-P-REMOTE', `*mon-site-lisp-root*',
`*mon-user-emacsd*', `*mon-emacs-root*' `*mon-naf-mode-root*',
`*mon-ebay-tmplt-mode-root*'.\n►►►")
;;
(when (and (intern-soft "IS-GNU-P" obarray)
           (bound-and-true-p IS-GNU-P))
  (unless (and (intern-soft "gnu-linuxp" obarray)
               (bound-and-true-p gnu-linuxp))
(defvaralias 'gnu-linuxp 'IS-GNU-P)))
;;
;;; :TEST-ME IS-GNU-P
;;
;;;(progn (makunbound 'IS-GNU-P) (unintern "IS-GNU-P" obarray)
;;;       (makunbound 'gnu-linuxp) (unintern "gnu-linuxp" obarray) )


;;; ==============================
(defconst IS-BUG-P (string-equal (mon-user-name-conditionals) "IS-BUG-P")
  "Return non-nil if this is a Buggy system and locally Bugged.\n
:EXAMPLE\n\nIS-BUG-P\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-user-system-conditionals-TEST', `IS-GNU-P',
`IS-W32-P', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-MON-P',
`IS-BUG-P-REMOTE', `*mon-site-lisp-root*', `*mon-user-emacsd*', `*mon-emacs-root*'
`*mon-naf-mode-root*', `*mon-ebay-tmplt-mode-root*'.\n►►►")
;;
;;; :TEST-ME IS-BUG-P
;;;(progn (makunbound 'IS-BUG-P) (unintern "IS-BUG-P" obarray) )

;;; ==============================
(defconst IS-BUG-P-REMOTE (string-equal (mon-user-name-conditionals) "IS-BUG-P-REMOTE")
  "Return non-nil if this is a Buggy system and remotely Bugged.\n
:EXAMPLE \nIS-BUG-P-REMOTE\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `IS-GNU-P', `IS-W32-P', `IS-MON-P-W32',
`IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-NOT-A-MON-SYSTEM', `IS-MON-P',
`IS-BUG-P', `*mon-site-lisp-root*', `*mon-user-emacsd*', `*mon-emacs-root*'
`*mon-naf-mode-root*', `*mon-ebay-tmplt-mode-root*'.\n►►►")
;;
;;; :TEST-ME IS-BUG-P-REMOTE
;;;(progn (makunbound 'IS-BUG-P-REMOTE) (unintern "IS-BUG-P-REMOTE" obarray) )

;;; ==============================
(defconst IS-MON-P-W32 (string-equal (mon-user-name-conditionals) "IS-MON-P-W32")
  "*Return non-nil if this a MON system and a win32 system.\n
:EXAMPLE\n\nIS-MON-P-W32\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-user-system-conditionals-TEST', `IS-BUG-P',
`IS-BUG-P-REMOTE', `IS-MON-P-GNU', `IS-MON-P', `IS-MON-SYSTEM-P',
`IS-NOT-A-MON-SYSTEM', `*mon-user-emacsd*', `*mon-emacs-root*', `IS-GNU-P',
`IS-W32-P', `*mon-site-lisp-root*', `*mon-naf-mode-root*',
`*mon-ebay-tmplt-mode-root*'.\n►►►")
;;
;;; :TEST-ME IS-MON-P-W32
;;;(progn (makunbound 'IS-MON-P-W32) (unintern "IS-MON-P-W32" obarray) )

;;; ==============================
(defconst IS-MON-P-GNU (string-equal (mon-user-name-conditionals) "IS-MON-P-GNU")
  "*Return non-nil if this a MON system and a GNU system.\n
:EXAMPLE\n\nIS-MON-P-GNU\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-user-system-conditionals-TEST', `IS-GNU-P',
`IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P-W32', `IS-MON-P', `IS-MON-SYSTEM-P',
`IS-NOT-A-MON-SYSTEM', `*mon-site-lisp-root*', `*mon-user-emacsd*', `*mon-emacs-root*'
`*mon-naf-mode-root*', `*mon-ebay-tmplt-mode-root*'.\n►►►")
;;
;;; :TEST-ME IS-MON-P-GNU
;;;(progn (makunbound 'IS-MON-P-GNU) (unintern "IS-MON-P-GNU" obarray) )

;;; ==============================
(defconst IS-MON-P 
  (or (and (intern-soft "IS-MON-P-GNU" obarray) (bound-and-true-p IS-MON-P-GNU))
      (and (intern-soft " IS-MON-P-W32" obarray) (bound-and-true-p  IS-MON-P-W32)))
  "*Return non-nil if this is a MON system?\n
:EXAMPLE\n\nIS-MON-P\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `*mon-site-lisp-root*', `*mon-naf-mode-root*',
`*mon-user-emacsd*', `*mon-ebay-tmplt-mode-root*', `*mon-emacs-root*', ', `IS-GNU-P',
`IS-W32-P', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P-W32', `IS-MON-P-GNU',
`IS-MON-SYSTEM-P', `IS-NOT-A-MON-SYSTEM'.\n►►►")
;;
;;; :TEST-ME IS-MON-P
;;;(progn (makunbound 'IS-MON-P) (unintern "IS-MON-P" obarray) ) 

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-10T15:53:10-05:00Z}#{10063} - by MON KEY>
(defconst IS-NOT-A-MON-SYSTEM
  (not ;; Then it IS-MON-SYSTEM-P, in which case invert to `nil'
   (and ;;  ^-- all tests passed
    ;; The symbol is only present when we put it there.
    (intern-soft "*mon-misc-path-alist*" obarray)  ;; *IS-MON-OBARRAY*
    ;; It isn't likely to be non-nil unless we bound it.
    (bound-and-true-p *mon-misc-path-alist*)
    ;; It most certainly won't associate this key unless we put it there.
    (let ((chk-assc (cdr (assoc 'the-only-if-its-a-mon-system *mon-misc-path-alist*))))
      ;; The _only_ valid return value is `t'.
      ;; Emacs lisps `booleanp' is a braindead implementation and
      ;; `mon-booleanp' isnt' loaded yet.
      (and (not (null chk-assc)) (car (memq chk-assc '(nil t)))))))
  "*Return non-nil if this `IS-NOT-A-MON-SYSTEM'.\n
When `IS-MON-SYSTEM-P' this should not evaluate true.\n
:SEE-ALSO `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-BUG-P',
`IS-BUG-P-REMOTE', `*mon-misc-path-alist*', `mon-get-mon-emacsd-paths',
`mon-system-type-conditionals', `mon-user-name-conditionals',
`mon-user-system-conditionals-TEST'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-13T12:57:44-05:00Z}#{10023} - by MON KEY>
(defconst IS-MON-SYSTEM-P
  (and (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
            (not (bound-and-true-p IS-NOT-A-MON-SYSTEM)))
       (and (intern-soft "*mon-misc-path-alist*" obarray)
            (bound-and-true-p *mon-misc-path-alist*)
            (cdr (assoc 'the-only-if-its-a-mon-system *mon-misc-path-alist*)))
       (or (and (intern-soft "IS-MON-P" obarray) 
                (bound-and-true-p IS-MON-P))
           (and (intern-soft "IS-MON-P-W32" obarray) 
                (bound-and-true-p IS-MON-P-W32))
           (and (intern-soft "IS-MON-P-GNU" obarray) 
                (bound-and-true-p IS-MON-P-GNU))
           (and (intern-soft "IS-BUG-P" obarray) 
                (bound-and-true-p IS-BUG-P))
           (and (intern-soft "IS-BUG-P-REMOTE" obarray) 
                (bound-and-true-p IS-BUG-P-REMOTE))
           (and (intern-soft "IS-MON-SYSTEM-P" obarray) 
                (bound-and-true-p IS-MON-SYSTEM-P))))
  "*Is this machine in MON's \"Circle of Trust\". :\)
Return non-nil this when any of the following evaluate to true:\n
 `IS-MON-P' `IS-MON-P-W32' `IS-MON-P-GNU' `IS-BUG-P' `IS-BUG-P-REMOTE'\n
:EXAMPLE\n\nIS-MON-SYSTEM-P\n
:CALLED-BY require in :FILE mon-utils.el to bootstrap the rest of MON packages.\n
:SEE-ALSO `IS-NOT-A-MON-SYSTEM', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-get-mon-emacsd-paths', `IS-W32-P',
`IS-GNU-P'.\n►►►")
;;
;;; :TEST-ME IS-MON-SYSTEM-P
;;;(progn (makunbound 'IS-MON-SYSTEM-P) (unintern "IS-MON-SYSTEM-P" obarray) )

;;; ==============================
;;; :NOTE Could also have been accomplished by frobbing return value of
;;; `load-file-name'.  Too bad we didn't know about it at the time... 
;;; Indeed, as of 2010-11-06 the variable remains undocumented in the manuals!
(defvar set-emacs-root nil
  "*This variable is bound at loadtime by `cd'ing to a directory derived from a
user conditionalized alist key lookup in variable `*mon-emacsd*'.\n
The 6th value \(0 indexed\) of that keyed association holds the directory path
we cd into at compile/loadtime which effictively reseats `default-directory'.\n
Because the majority of MON site-local paths are derived relative to the _new_
default-directory it is important that this variable be made known when
intializing our environment.\n
When compiling this library e.g. :FILE mon-default-loads.el this variable
i.e. `set-emacs-root' is bound to the value of an alist key in variable
`*mon-emacsd*'.  By making a series of indirections through evaluations of
`*mon-emacsd*' we are able to have a static value of the constant
`*mon-emacs-root*' hardwired in at compile time. This is so because
`*mon-emacs-root*' takes its value from `set-emacs-root' which is a dynamic value
\(e.g. its value changes from site to site\) derived from conditionalized tests
for particular user or system names.\n
With this approach `set-emacs-root' is the pivot point for bootstrapping MON
systems across multiple sites and helps to limit the introduction of multiple
points of failure.  Because only the pivot moves most other variables which
affect site local code can by locally byte-compiled while remaingin easily
tailored by simply adding/changing/removing key values in the `*mon-emacsd*'
variable rather than adjusting or tweaking multiple files and their variables,
constants and defcustoms. The is especially useful w/re defcustom'ed changes,
the cluster fucks they propogate, and the resulting nightmares encountered when
attempting to maintain and synchronize \(or unsynchronize as is more often the
case\) such changes across environments...\n
:USAGE Assume a conditional test for `IS-MON-P-GNU' returns non-nil. We
can quickly find that users system by examining the largest key of
`*mon-emacsd*'. This is so because the value of largest key in `*mon-emacsd*'
maps an index of the lesser keys and associated user-names. Assuming that the
length of the alist `*mon-emacsd*' is 5, then the 5th key will return a list of
user name conditionals and the respective keys which point to each users
site-local directory paths.\n
:EXAMPLE\n\n\(length \(mon-build-mon-emacsd-example\)\)\n ;=> 5\n
\(assoc 5 \(mon-build-mon-emacsd-example\)\)
 ;=> \(5 \(\(IS-USER-1-P 1\) \(IS-USER-2-P 2\) \(IS-USER-3-P 3\) \(IS-USER-4-P 4\)\)\)\n
\(cadr \(assoc 5 \(mon-build-mon-emacsd-example\)\)\)
 ;=> \(\(IS-USER-1-P 1\) \(IS-USER-2-P 2\) \(IS-USER-3-P 3\) \(IS-USER-4-P 4\)\)\n
\(assoc 'IS-USER-3-P \(cadr \(assoc 5 \(mon-build-mon-emacsd-example\)\)\)\)
 ;=> \(IS-USER-3-P 3\)\n
\(cadr \(assoc 'IS-USER-3-P \(cadr \(assoc 5 \(mon-build-mon-emacsd-example\)\)\)\)\)\n ;=> 3\n
\(assoc 3 \(mon-build-mon-emacsd-example\)\)
 ;=> \(3 \"<USER-3-EMACSD-PATH-Nth-1>\"
         ;; { ... OUTPUT-ELIDED ... }
         \"<USER-3-EMACSD-PATH-Nth-6>\"  ;; <- `*mon-emacs-root*', `set-emacs-root'
         ;; { ... OUTPUT-ELIDED ... }
         \"<USER-3-EMACSD-PATH-Nth-14>\"\)\n
\(nth 6 \(assoc 3 \(mon-build-mon-emacsd-example\)\)\)
 ;=> \"<USER-3-EMACSD-PATH-Nth-6>\"\n
So assuming the variable `*mon-emacsd*' is configured correctly
and IS-USER-3-P has the 3rd key in *mon-emacsd*, then the following:\n
\(nth 6 \(assoc\n        \(cadr\n         \(assoc 'IS-USER-3-P
                \(cadr \(assoc \(length *mon-emacsd*\) *mon-emacsd*\)\)\)\)
        *mon-emacsd*\)\)\n ;=> \"<USER-3-EMACSD-PATH-Nth-6>\"\n
Might be equivalently \(and more tersely\) expressed:\n
\(nth 6 \(assoc 3 *mon-emacsd*\)\)\n;=> \"<USER-3-EMACSD-PATH-Nth-6>\"\n
Likewise, assuming the environemt is configured and initialized correctly,
when a conditional for IS-USER-3-P evaluates non-nil `*mon-emacs-root*' will be
equivalent to \(nth 6 \(assoc  *mon-emacsd*\)\) e.g.:\n
\(equal \(nth 6 \(assoc 1 *mon-emacsd*\)\) *mon-emacs-root*\)\n ;=> t\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-system-type-conditionals',
`mon-user-name-conditionals', `mon-user-system-conditionals-TEST',
`mon-build-mon-emacsd-example', `mon-build-misc-path-example',
`mon-build-user-name-example', `mon-build-path-for-load-path', `*mon-emacsd*',
`*mon-emacs-root*', `*mon-site-lisp-root*', `*mon-naf-mode-root*', `*mon-user-emacsd*',
`*mon-local-emacs-temp-dir*', `*mon-ebay-tmplt-mode-root*', `*mon-emacs-root*', ',
`IS-GNU-P', `IS-W32-P', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P-W32',
`IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-NOT-A-MON-SYSTEM'.\n►►►")
;;
(when (and (and (intern-soft "IS-MON-SYSTEM-P" obarray)
                (bound-and-true-p IS-MON-SYSTEM-P))
           (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                (not (bound-and-true-p IS-NOT-A-MON-SYSTEM))))
  (eval-when (compile load eval)
    (let (this-df)
      (setq this-df default-directory)
      (unwind-protect
          (progn
            (cond ((and (intern-soft "IS-MON-P-GNU" obarray) (bound-and-true-p IS-MON-P-GNU))
                   (cd (nth 6 (assoc 2 *mon-emacsd*))))
                  ((and (intern-soft "IS-BUG-P-REMOTE" obarray)
                        (bound-and-true-p IS-BUG-P-REMOTE ))
                   (cd (nth 6 (assoc 4 *mon-emacsd*))))
                  ((and (intern-soft "IS-BUG-P" obarray) (bound-and-true-p IS-BUG-P))
                   (cd (nth 6 (assoc 3 *mon-emacsd*))))
                  ((and (intern-soft "IS-MON-P-W32" obarray) (bound-and-true-p IS-MON-P-W32))
                   (cd (nth 6 (assoc 1 *mon-emacsd*))))
                  ;; :NOTE This prob. isn't right for other users.
                  ;;       _REALLY_ needs to be defcustomized.
                  ((or (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                            (bound-and-true-p IS-NOT-A-MON-SYSTEM))
                       t)
                   (cd (directory-file-name (expand-file-name default-directory)))))
            (setq set-emacs-root 
                  ;;(convert-standard-filename 
                  (directory-file-name (expand-file-name default-directory))))
        (unless (or (and (intern-soft "IS-MON-P-GNU" obarray) (bound-and-true-p IS-MON-P-GNU))
                    (string-equal default-directory this-df))
          (cd this-df))))))

;;; ==============================
(defconst *mon-emacs-root* 
  (when (and (and (intern-soft "IS-MON-SYSTEM-P" obarray)
                  (bound-and-true-p IS-MON-SYSTEM-P))
             (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                  (not (bound-and-true-p IS-NOT-A-MON-SYSTEM))))
    (and set-emacs-root))
  "*Return a self expanding filename string to the root path.\n
Path is for all MON relevant site local Emacs files.\n
:EXAMPLE\n\n*mon-emacs-root*\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `*mon-HG-root-path*',
`*mon-site-lisp-root*', `*mon-naf-mode-root*', `*mon-naf-mode-notes*',
`*mon-ebay-tmplt-mode-root*', `*mon-user-emacsd*', `mon-system-type-conditionals',
`*mon-local-emacs-temp-dir*', `mon-user-name-conditionals', `IS-MON-SYSTEM-P',
`IS-NOT-A-MON-SYSTEM', `IS-GNU-P', `IS-W32-P', `IS-MON-P', `IS-MON-P-W32',
`IS-MON-P-GNU', `IS-BUG-P', `IS-BUG-P-REMOTE'.\n►►►")
;;
;;; :TEST-ME (file-exists-p *mon-emacs-root*)
;;;(progn (makunbound '*mon-emacs-root*) (unintern "*mon-emacs-root*" obarray) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON>
(defconst *mon-site-lisp-root* 
  (when (and (and (intern-soft "IS-MON-SYSTEM-P" obarray)
                  (bound-and-true-p IS-MON-SYSTEM-P))
             (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                  (not (bound-and-true-p IS-NOT-A-MON-SYSTEM))))  
    ;;(convert-standard-filename 
    (and *mon-emacs-root* (expand-file-name "site-lisp" *mon-emacs-root*)))
  "*Return a string to the load-path for the local site-lisp files.\n
:EXAMPLE\n\n*mon-site-lisp-root*\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `*mon-naf-mode-root*', `*mon-user-emacsd*',
`*mon-emacs-root*', `*mon-local-emacs-temp-dir*', `*mon-naf-mode-notes*',
`*mon-ebay-tmplt-mode-root*', `mon-system-type-conditionals',
`mon-user-name-conditionals', `IS-MON-SYSTEM-P', `IS-NOT-A-MON-SYSTEM',
`IS-GNU-P', `IS-W32-P', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P',
`IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►")
;;
;;; :TEST-ME (file-exists-p *mon-site-lisp-root*)
;;;(progn (makunbound '*mon-site-lisp-root*) (unintern "*mon-site-lisp-root*" obarray) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON>
(defconst *mon-naf-mode-root* 
  ;;(convert-standard-filename 
  (when (and (and (intern-soft "IS-MON-SYSTEM-P" obarray)
                  (bound-and-true-p IS-MON-SYSTEM-P))
             (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                  (not (bound-and-true-p IS-NOT-A-MON-SYSTEM))))
    (expand-file-name "naf-mode" *mon-emacs-root*))
  "*Return a string to the path for the `naf-mode' files.\n
Path used for to load all files for naf-mode.\n
:EXAMPLE\n\n*mon-naf-mode-root*\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `*mon-site-lisp-root*', `*mon-user-emacsd*',
`*mon-local-emacs-temp-dir*', `*mon-emacs-root*', `*mon-naf-mode-notes*',
`*mon-ebay-tmplt-mode-root*', `mon-system-type-conditionals',
`mon-user-name-conditionals', `IS-MON-SYSTEM-P' `IS-NOT-A-MON-SYSTEM',
`IS-GNU-P', `IS-W32-P',`IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P',
`IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►")
;;
;;; :TEST-ME (file-exists-p *mon-naf-mode-root*)
;;;(progn (makunbound '*mon-naf-mode-root*) (unintern "*mon-naf-mode-root*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-26T18:18:12-04:00Z}#{09353} - by MON>
(defconst *mon-naf-mode-notes* 
  (when (and (and (intern-soft "IS-MON-SYSTEM-P" obarray)
                   (bound-and-true-p IS-MON-SYSTEM-P))
              (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                   (not (bound-and-true-p IS-NOT-A-MON-SYSTEM))))
    (convert-standard-filename
     (concat (nth 5 (mon-get-mon-emacsd-paths)) "/mon-notes-HG")))
  "*Return a string to the path for the `notes' files.\n
Path used for to load all files for naf-mode notes.\n
:EXAMPLE\n\n*mon-naf-mode-notes*\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `*mon-site-lisp-root*',`*mon-user-emacsd*',
`*mon-local-emacs-temp-dir*', `*mon-emacs-root*', `*mon-ebay-tmplt-mode-root*',
`mon-system-type-conditionals', `mon-user-name-conditionals', `IS-MON-SYSTEM-P',
`IS-NOT-A-MON-SYSTEM', `IS-GNU-P', `IS-W32-P', `IS-BUG-P', `IS-BUG-P-REMOTE',
`IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►")
;;
;;; :TEST-ME (file-exists-p *mon-naf-mode-notes*)
;;;(progn (makunbound '*mon-naf-mode-notes*) (unintern "*mon-naf-mode-notes*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-19T14:26:35-04:00Z}#{09343} - by MON>
(defconst *mon-ebay-tmplt-mode-root* 
  (when (and (and (intern-soft "IS-MON-SYSTEM-P" obarray)
                  (bound-and-true-p IS-MON-SYSTEM-P))
             (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                  (not (bound-and-true-p IS-NOT-A-MON-SYSTEM))))
    (expand-file-name "ebay-template-mode" *mon-naf-mode-root*))
  "*Return a string to the path for the `naf-mode' files.\n
Path used for to load all files for `ebay-template-mode'.\n
:EXAMPLE\n\nmon-ebay-template-mode-root\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `*mon-site-lisp-root*', `*mon-user-emacsd*',
`*mon-local-emacs-temp-dir*', `*mon-emacs-root*', `*mon-naf-mode-root*', `IS-GNU-P', `IS-W32-P',
`mon-system-type-conditionals', `mon-user-name-conditionals', `IS-MON-SYSTEM-P'
`IS-NOT-A-MON-SYSTEM', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P',
`IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►")
;;
;;; :TEST-ME (file-exists-p *mon-ebay-tmplt-mode-root*)
;;;(progn (makunbound '*mon-ebay-tmplt-mode-root*) (unintern "*mon-ebay-tmplt-mode-root*" obarray) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-25T13:09:03-04:00Z}#{10124} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2010-02-10T16:08:38-05:00Z}#{10063} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON>
(defconst *mon-user-emacsd*
  (if (and (and (intern-soft "IS-MON-SYSTEM-P" obarray)
                (bound-and-true-p IS-MON-SYSTEM-P))
           (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)
                (not (bound-and-true-p IS-NOT-A-MON-SYSTEM)))
           (and (intern-soft "*mon-emacs-root*" obarray)
                (bound-and-true-p *mon-emacs-root*)))
      (expand-file-name (cadr (mon-get-mon-emacsd-paths)) *mon-emacs-root*)
    user-emacs-directory) ;; Probably ~/.emacs.d/
  "*Return a string to the path for the site-local .emacs.d path.\n
:EXAMPLE\n\n*mon-user-emacsd*\n
:NOTE bound in :FILE mon-default-start-loads.el to set `user-emacs-directory'.\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `*mon-emacs-root*', `*mon-site-lisp-root*',
`*mon-local-emacs-temp-dir*', `*mon-naf-mode-root*' `*mon-ebay-tmplt-mode-root*',
`mon-system-type-conditionals', `mon-user-name-conditionals', `IS-MON-SYSTEM-P',
`IS-NOT-A-MON-SYSTEM', `IS-GNU-P', `IS-W32-P', `IS-MON-P', `IS-MON-P-W32',
`IS-MON-P-GNU', `IS-BUG-P', `IS-BUG-P-REMOTE'.\n►►►")
;;
;;; :TEST-ME (file-exists-p *mon-user-emacsd*)
;;;(progn (makunbound '*mon-user-emacsd*) (unintern "*mon-user-emacsd*" obarray) )

;;; ==============================
;;; :NOTE BZR revno: 102144 2010-10-28 removed `temporary-file-directory' from lisp/files.el
;;; and added its customomization to lisp/cus-start.el
;;; If something breaks around here in emacs-24 start there.
;;; :CHANGESET 1743 <Timestamp: #{2010-05-19T19:41:29-04:00Z}#{10203} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-04-02T22:15:10-04:00Z}#{10136} - by MON KEY>
(defconst *mon-local-emacs-temp-dir*
  (cond ((and (intern-soft "IS-MON-P" obarray) (bound-and-true-p IS-MON-P))
         (concat *mon-emacs-root* 
                 (cadr (assoc 'the-local-emacs-temp *mon-misc-path-alist*))))
        ((or (and (intern-soft "IS-BUG-P" obarray) (bound-and-true-p IS-BUG-P))
             (and (intern-soft "IS-BUG-P-REMOTE" obarray) (bound-and-true-p IS-BUG-P-REMOTE)))
         (let ((make-local-emacs-temp 
                (concat (file-truename (getenv "HOME")) "/.emacs.d"
                        (cadr (assoc 'the-local-emacs-temp *mon-misc-path-alist*)))))
           (unless (file-directory-p make-local-emacs-temp)
             (mkdir make-local-emacs-temp))
           make-local-emacs-temp))
        ((or (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)  ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-NOT-A-MON-SYSTEM))
             t)
         temporary-file-directory))
  "The base directory for temporary files/dir Emacs generates locally.\n
:NOTE This is essentially `temporary-file-directory', but where Emacs is
extended with third party coded it isn't always particularly sane w/re to where
and how it stashes away `temp' files esp. on W32 and it can be difficult to
track this on one system let alone across mulitple machines.
Easier to store it all in a local directory hierarchy we have our eye on.\n
:SEE-ALSO `IS-W32-P', `IS-GNU-P', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P-W32',
`IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-MON-P', `IS-NOT-A-MON-SYSTEM',
`*mon-emacs-root*', `*mon-site-lisp-root*', `*mon-user-emacsd*', `*mon-naf-mode-notes*',
`*mon-naf-mode-root*', `*mon-ebay-tmplt-mode-root*'.\n►►►")
;;
;;; :TEST-ME (file-exists-p *mon-local-emacs-temp-dir*)
;;;(progn (makunbound '*mon-local-emacs-temp-dir*) (unintern "*mon-local-emacs-temp-dir*" obarray) )

;;; ==============================
(provide 'mon-default-loads)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; ==============================
;;; mon-default-loads.el ends here
;;; EOF
