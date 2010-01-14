;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-
;;; this is mon-default-loads.el
;;; ================================================================
;;;    ___ _ __ ___   __ _  ___ ___
;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;  |  __/ | | | | | (_| | (__\__ \
;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;
;;; ==============================
;;; DESCRIPTION:
;;; mon-default-loads provides default constants for maintaining system
;;; portability across MON systems.  Negotiates the startup loadpaths in prep
;;; for mon-default-start-loads.el
;;;
;;; Functionality of this file requires that the procedure:
;;; `mon-system-type-conditionals' be present. On MON systems it is 
;;; defined in a private site local file along with other alists 
;;; `*mon-emacsd*', `set-emacs-root' which encapsulate user data which needn't
;;; be made availabe in public source.  These alists inform the following
;;; constants defined which are bound here: `IS-BUG-P', `IS-BUG-P-REMOTE',
;;; `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P', `IS-MON-SYSTEM-P'.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;; `win32p', `gnu-linuxp', `IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-W32',
;;; `IS-MON-P-GNU', `IS-MON-SYSTEM-P'
;;; `IS-MON-P', `mon-emacs-root', `mon-site-lisp-root', `mon-user-emacsd', 
;;; `mon-naf-mode-notes', `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'
;;;
;;; VARIABLES:
;;;
;;; AIASED/ADVISED/SUBST'D:
;;; `IS-W32-P' -> `win32p'
;;; `IS-GNU-P' -> `gnu-linuxp'
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;;
;;; NOTES:
;;; The constant `IS-MON-SYSTEM-P' is evaluated at the top of:
;;; :FILE mon-utils.el when bootstrapping the rest of MON's packages.
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-default-loads.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T12:22:19-04:00Z}#{09393} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Sunday August 09, 2009 @ 04:29.20 AM - by MON KEY>
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
(provide 'default-loads)
(require 'default-loads)

;;; ==============================
(defconst win32p
  (if (fboundp 'mon-system-type-conditionals)
      (equal (mon-system-type-conditionals) "IS-W32")
    (eq system-type 'windows-nt))
  "*Are we running on a WinTel system? Evaluates to t if we are, else nil.
Used in conditional system type tests in lieu of (equal system-type 'windows-nt).\n
:SEE-ALSO `mon-system-type-conditionals', `mon-user-name-conditionals',
`gnu-linuxp', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-MON-P',
`IS-BUG-P', `IS-BUG-P-REMOTE', `mon-site-lisp-root', `mon-user-emacsd',
`mon-emacs-root' `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'.\n►►►")
;;
(defvaralias 'IS-W32-P 'win32p)
;;
;;; :TEST-ME  win32p
;;; :TEST-ME  IS-W32-p
;;
;;;(progn (makunbound 'win32p) (unintern 'win32p))

;;; ==============================
(defconst gnu-linuxp  
  (if (fboundp 'mon-system-type-conditionals)
      (equal (mon-system-type-conditionals) "IS-GNU")
    (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
  "*Are we running on a GNU/Linux system? Evaluates to t if we are, else nil.
Used in conditional system type tests in lieu of: (equal system-type 'gnu/linux).\n
:SEE-ALSO `mon-system-type-conditionals', `mon-user-name-conditionals',
`win32p', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P', `IS-MON-P',
`IS-BUG-P', `IS-BUG-P-REMOTE', `mon-site-lisp-root', `mon-user-emacsd',
`mon-emacs-root' `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'.\n►►►")
;;
(defvaralias 'IS-GNU-P 'gnu-linuxp)
;;
;;; :TEST-ME gnu-linuxp
;;; :TEST-ME IS-GNU-P
;;
;;;(progn (makunbound 'gnu-linuxp) (unintern 'gnu-linuxp))

;;; ==============================
(defconst IS-BUG-P (equal (mon-user-name-conditionals) "IS-BUG-P")
  "Return non-nil if this is a Buggy system - t if Bugged, else nil.\n
:EXAMPLE\nIS-BUG-P\n
:SEE-ALSO `mon-system-type-conditionals', `mon-user-name-conditionals',
`gnu-linuxp', `win32p', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P',
`IS-MON-P', `IS-BUG-P-REMOTE', `mon-site-lisp-root', `mon-user-emacsd',
`mon-emacs-root' `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'.\n►►►")
;;
;;; :TEST-ME IS-BUG-P
;;;(progn (makunbound 'IS-BUG-P) (unintern 'IS-BUG-P))

;;; ==============================
(defconst IS-BUG-P-REMOTE (equal (mon-user-name-conditionals) "IS-BUG-P-REMOTE")
  "Return non-nil if this is a Buggy system and remote - t if it Bugged, else nil.\n
:EXAMPLE \nIS-BUG-P-REMOTE\n
:SEE-ALSO `mon-system-type-conditionals', `mon-user-name-conditionals',
`gnu-linuxp', `win32p', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P',
`IS-MON-P', `IS-BUG-P', `mon-site-lisp-root', `mon-user-emacsd',
`mon-emacs-root' `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'.\n►►►")
;;
;;; :TEST-ME IS-BUG-P-REMOTE
;;;(progn (makunbound 'IS-BUG-P-REMOTE) (unintern 'IS-BUG-P-REMOTE))

;;; ==============================
(defconst IS-MON-P-W32 (equal (mon-user-name-conditionals) "IS-MON-P-W32")
  "*Return non-nil if this a MON win32 system.\n
When MONishly w32 t, else nil.\n
:SEE-ALSO `mon-system-type-conditionals', `mon-user-name-conditionals',
`IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-GNU', `IS-MON-P', `IS-MON-SYSTEM-P'
`mon-user-emacsd', `mon-emacs-root', `gnu-linuxp', `win32p',
`mon-site-lisp-root', `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'.\n►►►")
;;
;;; :TEST-ME IS-MON-P-W32
;;;(progn (makunbound 'IS-MON-P-W32) (unintern 'IS-MON-P-W32))

;;; ==============================
(defconst IS-MON-P-GNU (equal (mon-user-name-conditionals) "IS-MON-P-GNU")
  "*Return non-nil if this a MON GNU system.\n
t when current system is MONishly GNU, else nil.\n
:EXAMPLE\nIS-MON-P-GNU\n
:SEE-ALSO `mon-system-type-conditionals', `mon-user-name-conditionals',
`gnu-linuxp', `IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-W32', `IS-MON-P',
`IS-MON-SYSTEM-P', `mon-site-lisp-root', `mon-user-emacsd', `mon-emacs-root'
`mon-naf-mode-root', `mon-ebay-tmplt-mode-root'.\n►►►")
;;
;;; :TEST-ME IS-MON-P-GNU
;;;(progn (makunbound 'IS-MON-P-GNU) (unintern 'IS-MON-P-GNU))

;;; ==============================
(defconst IS-MON-P (or IS-MON-P-GNU IS-MON-P-W32)
  "*Return non-nil if this is a MON system? t if it MONish, else nil.\n
:SEE-ALSO `mon-site-lisp-root', `mon-naf-mode-root', `mon-user-emacsd',
`mon-ebay-tmplt-mode-root', `mon-emacs-root', `mon-system-type-conditionals',
`mon-user-name-conditionals', `gnu-linuxp', `win32p', `IS-BUG-P',
`IS-BUG-P-REMOTE', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-SYSTEM-P'.\n►►►")
;;
;;; :TEST-ME IS-MON-P
;;;(progn (makunbound 'IS-MON-P) (unintern 'IS-MON-P)) 

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-13T12:57:44-05:00Z}#{10023} - by MON KEY>
(defconst IS-MON-SYSTEM-P (or IS-MON-P IS-MON-P-W32 IS-MON-P-GNU 
                              IS-BUG-P IS-BUG-P-REMOTE)
"*Return non-nil if any of the following evaluate to t: `IS-MON-P',
:CALLED-BY require in :FILE mon-utils.el to bootstrap the rest of MON packages.\n
 :SEE-ALSO  `mon-system-type-conditionals', `mon-user-name-conditionals', 
`IS-MON-P-W32' `IS-MON-P-GNU' `IS-BUG-P' `IS-BUG-P-REMOTE',`IS-W32-P',
`IS-GNU-P'.\n►►►")
;;
;;; :TEST-ME IS-MON-SYSTEM-P
;;;(progn (makunbound 'IS-MON-SYSTEM-P) (unintern 'IS-MON-SYSTEM-P))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON>
;;; Given the vile-local-variable :) We're not compiling are we??
(eval-when-compile
  (cond (IS-MON-P-W32    (cd (nth 6 (assoc 1 *mon-emacsd*))))
        (IS-MON-P-GNU    (cd (nth 6 (assoc 2 *mon-emacsd*))))
        (IS-BUG-P-REMOTE (cd (nth 6 (assoc 4 *mon-emacsd*))))
        (IS-BUG-P        (cd (nth 6 (assoc 3 *mon-emacsd*)))))
  (setq set-emacs-root (directory-file-name (expand-file-name default-directory))))

;;; ==============================
(defconst mon-emacs-root set-emacs-root
  "*Build a self expanding filename string to the root path.\n
Path is for all relevant site local emacs files.\n
:EXAMPLE\nmon-emacs-root\n
:SEE-ALSO `*mon-HG-root-path*', `mon-site-lisp-root', `mon-naf-mode-root',
`mon-naf-mode-notes', `mon-ebay-tmplt-mode-root', `mon-user-emacsd',
`mon-system-type-conditionals', `mon-user-name-conditionals', `IS-MON-SYSTEM-P'
`gnu-linuxp', `win32p', `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-BUG-P',
`IS-BUG-P-REMOTE'.\n►►►")

;;; ==============================
;;; (setq set-site-lisp-root 
;;; (concat (expand-file-name "site-lisp" mon-emacs-root))) ;;set-emacs-root)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON>
(defconst mon-site-lisp-root (expand-file-name "site-lisp" mon-emacs-root)
  "*Build a string to the load-path for the local site-lisp files.\n
:EXAMPLE\nmon-site-lisp-root\n
:SEE-ALSO `mon-naf-mode-root', `mon-user-emacsd', `mon-emacs-root',
`mon-naf-mode-notes', `mon-ebay-tmplt-mode-root',
`mon-system-type-conditionals', `mon-user-name-conditionals', `IS-MON-SYSTEM-P',
`gnu-linuxp', `win32p', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P',
`IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►")
;;
;;; :TEST-ME  mon-site-lisp-root
;;;(progn (makunbound 'mon-site-lisp-root) (unintern 'mon-site-lisp-root))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON>
(defconst mon-naf-mode-root (expand-file-name "naf-mode" mon-emacs-root)
  "*Build a string to the path for the `naf-mode' files.
Path used for to load all files for naf-mode.\n
:EXAMPLE\nmon-naf-mode-root\n
:SEE-ALSO `mon-site-lisp-root',`mon-user-emacsd', `mon-emacs-root',
`mon-naf-mode-notes', `mon-ebay-tmplt-mode-root',
`mon-system-type-conditionals', `mon-user-name-conditionals', `IS-MON-SYSTEM-P'
`gnu-linuxp', `win32p',`IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P',
`IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►")
;;
;;; :TEST-ME  mon-naf-mode-root
;;;(progn (makunbound 'mon-naf-mode-root) (unintern 'mon-naf-mode-root))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-26T18:18:12-04:00Z}#{09353} - by MON>
(defconst mon-naf-mode-notes (expand-file-name "notes" mon-naf-mode-root)
  "*Build a string to the path for the `notes' files.
Path used for to load all files for naf-mode notes.\n
:EXAMPLE\nmon-naf-mode-notes\n
:SEE-ALSO `mon-site-lisp-root',`mon-user-emacsd', `mon-emacs-root',
`mon-ebay-tmplt-mode-root', `mon-system-type-conditionals',
`mon-user-name-conditionals', `IS-MON-SYSTEM-P' `gnu-linuxp', `win32p',
`IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►"
)
;;
;;; :TEST-ME  mon-naf-mode-notes
;;;(progn (makunbound 'mon-naf-mode-notes) (unintern 'mon-naf-mode-notes))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-19T14:26:35-04:00Z}#{09343} - by MON>
(defconst mon-ebay-tmplt-mode-root (expand-file-name "ebay-template-mode" mon-naf-mode-root)
  "*Build a string to the path for the `naf-mode' files.
Path used for to load all files for `ebay-template-mode'.\n
:EXAMPLE\nmon-ebay-template-mode-root\n
:SEE-ALSO `mon-site-lisp-root',`mon-user-emacsd', `mon-emacs-root',
`mon-naf-mode-root', `gnu-linuxp', `win32p', `mon-system-type-conditionals',
`mon-user-name-conditionals', `IS-MON-SYSTEM-P' `IS-BUG-P', `IS-BUG-P-REMOTE',
`IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU'.\n►►►")
;;
;;; :TEST-ME  mon-ebay-tmplt-mode-root
;;;(progn (makunbound 'mon-ebay-tmplt-mode-root) (unintern 'mon-ebay-tmplt-mode-root))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON>
(defconst mon-user-emacsd
  (expand-file-name 
   (cadr (assoc (cond (IS-MON-P-W32 1)
                      (IS-MON-P-GNU 2)
                      (IS-BUG-P 3)) 
                *mon-emacsd*)) mon-emacs-root)
  "*Build a string to the path for the site-local .emacs.d path.\n
:EXAMPLE\nmon-user-emacsd\n
:SEE-ALSO `mon-emacs-root', `mon-site-lisp-root', `mon-naf-mode-root'
`mon-ebay-tmplt-mode-root', `mon-system-type-conditionals',
`mon-user-name-conditionals', `IS-MON-SYSTEM-P', `gnu-linuxp', `win32p',
`IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-BUG-P',
`IS-BUG-P-REMOTE'.\n►►►")
;;
;;; :TEST-ME mon-user-emacsd
;;;(progn (makunbound 'mon-user-emacsd) (unintern 'mon-user-emacsd))

;;; ==============================

;;; ==============================
(provide 'mon-default-loads)
;;; ==============================

;;; ==============================
;;; mon-default-loads.el ends here
;;; EOF
