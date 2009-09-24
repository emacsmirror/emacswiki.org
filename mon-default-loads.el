;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-
;;; this is default-loads.el.el
;;; ================================================================
;;;    ___ _ __ ___   __ _  ___ ___
;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;  |  __/ | | | | | (_| | (__\__ \
;;; (_)___|_| |_| |_|\__,_|\___|___/
;;;
;;; ==============================
;;; DESCRIPTION:
;;; Provides the defaults for system-operability across systems.
;;; Negotiates the startup loadpaths in prep for default-start-loads.el
;;; Functionality of this file requires that the procedure:
;;; `mon-system-type-conditionals' be present. On MON systems it is 
;;; defined in a private site local file along with other alists 
;;; `*mon-emacsd*', `set-emacs-root'
;;; which 
;;; encapsulate user data which needn't be made availabe in public source.
;;; these alists inform the following constants defined which are bound here:
;;; `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P'
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `win32p', `gnu-linuxp', `IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-W32', `IS-MON-P-GNU',
;;; `IS-MON-P', `mon-emacs-root', `mon-site-lisp-root', `mon-user-emacsd', 
;;; `mon-naf-mode-notes', `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'
;;;
;;; VARIABLES:
;;;
;;;
;;; AIASED/ADVISED/SUBST'D:
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
;;; ©opyright (C) MON KEY - 2009
;;; ============================
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
See also; `gnu-linuxp', `mon-site-lisp-root', `mon-naf-mode-root', 
`mon-user-emacsd', `mon-emacs-root', `mon-system-conditionals', 
`IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P'.")

;;;test-me; win32p
;;;(progn (makunbound 'win32p) (unintern 'win32p))

;;; ==============================
(defconst gnu-linuxp  
  (if (fboundp 'mon-system-type-conditionals)
      (equal (mon-system-type-conditionals) "IS-GNU")
          (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
  "*Are we running on a GNU/Linux system? Evaluates to t if we are, else nil.
Used in conditional system type tests in lieu of: (equal system-type 'gnu/linux).\n
 See also; `mon-site-lisp-root', `mon-naf-mode-root', `mon-user-emacsd',
`mon-ebay-tmplt-mode-root', `mon-emacs-root', `win32p', `mon-system-conditionals',
`IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P'.")

;;;test-me; gnu-linuxp
;;;(progn (makunbound 'gnu-linuxp) (unintern 'gnu-linuxp))

;;; ==============================
(defconst IS-BUG-P (equal (mon-user-name-conditionals) "IS-BUG-P")
  "*Test if this is a Buggy system - t if Bugged, else nil.\n
EXAMPLE:\nIS-BUG-P\n
See also; `mon-site-lisp-root', `mon-naf-mode-root', `mon-ebay-tmplt-mode-root',
`mon-user-emacsd', `mon-emacs-root', `gnu-linuxp', `win32p', 
`mon-system-conditionals', `IS-BUG-P-remote', `IS-MON-P-W32',`IS-MON-P-GNU',
`IS-MON-P'.")

;;;test-me; IS-BUG-P
;;;(progn (makunbound 'IS-BUG-P) (unintern 'IS-BUG-P))

;;; ==============================
(defconst IS-BUG-P-REMOTE (equal (mon-user-name-conditionals) "IS-BUG-P-REMOTE")
  "*Test if this is a Buggy system and remote - t if it Bugged, else nil.\n
EXAMPLE:\nIS-BUG-P-REMOTE\n
See also; `mon-site-lisp-root', `mon-naf-mode-root', `mon-ebay-tmplt-mode-root',
`mon-user-emacsd', `mon-emacs-root', `gnu-linuxp', `win32p', 
`mon-system-conditionals', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-MON-P',
`IS-BUG-P'.")

;;;test-me; IS-BUG-P-REMOTE
;;;(progn (makunbound 'IS-BUG-P-REMOTE) (unintern 'IS-BUG-P-REMOTE))

;;; ==============================
(defconst IS-MON-P-W32 (equal (mon-user-name-conditionals) "IS-MON-P-W32")
  "*Test if this a Mon win32 system - t if Monishly w32, else nil.\n
See also; `mon-site-lisp-root', `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'
`mon-user-emacsd', `mon-emacs-root', `gnu-linuxp', `win32p', 
`mon-system-conditionals', `IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-GNU',
`IS-MON-P'.")

;;;test-me; IS-MON-P-W32
;;;(progn (makunbound 'IS-MON-P-W32) (unintern 'IS-MON-P-W32))

;;; ==============================
(defconst IS-MON-P-GNU (equal (mon-user-name-conditionals) "IS-MON-P-GNU")
  "*Test if this a Mon GNU system. t if Monishly Gnu, else nil.\n
EXAMPLE:\nIS-MON-P-GNU\n
See also; `mon-site-lisp-root', `mon-naf-mode-root', `mon-user-emacsd',
`mon-ebay-tmplt-mode-root', `mon-emacs-root', `mon-system-conditionals',
`gnu-linuxp', `IS-BUG-P', `IS-BUG-P-remote', `IS-MON-P-W32', `IS-MON-P'.")

;;;test-me; IS-MON-P-GNU
;;;(progn (makunbound 'IS-MON-P-GNU) (unintern 'IS-MON-P-GNU))

;;; ==============================
(defconst IS-MON-P (or IS-MON-P-GNU IS-MON-P-W32)
  "*Test if this is a Mon system? t if it Monish, else nil.\n
See also; `mon-site-lisp-root', `mon-naf-mode-root', `mon-user-emacsd',
`mon-ebay-tmplt-mode-root', `mon-emacs-root', `mon-system-conditionals',
`gnu-linuxp', `win32p', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P-W32',
`IS-MON-P-GNU'.")

;;;test-me; IS-MON-P
;;;(progn (makunbound 'IS-MON-P) (unintern 'IS-MON-P)) 

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON KEY>
(eval-when-compile
    (cond (IS-MON-P-W32    (cd (nth 6 (assoc 1 *mon-emacsd*))))
          (IS-MON-P-GNU    (cd (nth 6 (assoc 2 *mon-emacsd*))))
          (IS-BUG-P-REMOTE (cd (nth 6 (assoc 4 *mon-emacsd*))))
          (IS-BUG-P        (cd (nth 6 (assoc 3 *mon-emacsd*)))))
    (setq set-emacs-root (directory-file-name (expand-file-name default-directory))))

;;; ==============================
(defconst mon-emacs-root set-emacs-root
  "*Build a self expanding filename string to the root path. 
Path is for all relevant site local emacs files.\n
EXAMPLE:\nmon-emacs-root\n
See also; `*mon-HG-root-path*', `mon-site-lisp-root', `mon-naf-mode-root',
`mon-naf-mode-notes', `mon-ebay-tmplt-mode-root', `mon-user-emacsd',
`mon-system-conditionals', `gnu-linuxp', `win32p', `IS-BUG-P',
`IS-BUG-P-REMOTE', `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU'.")

;;; ==============================
;;; (setq set-site-lisp-root 
;;; (concat (expand-file-name "site-lisp" mon-emacs-root))) ;;set-emacs-root)))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON KEY>
(defconst mon-site-lisp-root (expand-file-name "site-lisp" mon-emacs-root)
  "*Build a string to the load-path for the local site-lisp files.\n
EXAMPLE:\nmon-site-lisp-root\n
See also; `mon-naf-mode-root', `mon-user-emacsd', `mon-emacs-root',
`mon-naf-mode-notes', `mon-ebay-tmplt-mode-root', `mon-system-conditionals',
`gnu-linuxp', `win32p', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P', `IS-MON-P-W32',
`IS-MON-P-GNU'.")

;;;test-me; mon-site-lisp-root
;;;(progn (makunbound 'mon-site-lisp-root) (unintern 'mon-site-lisp-root))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON KEY>
(defconst mon-naf-mode-root (expand-file-name "naf-mode" mon-emacs-root)
  "*Build a string to the path for the `naf-mode' files.
Path used for to load all files for naf-mode.\n
EXAMPLE:\nmon-naf-mode-root\n
See also; `mon-site-lisp-root',`mon-user-emacsd', `mon-emacs-root',
`mon-naf-mode-notes', `mon-ebay-tmplt-mode-root', `mon-system-conditionals',
`gnu-linuxp', `win32p',`IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P',
`IS-MON-P-W32', `IS-MON-P-GNU'.")

;;;test-me; mon-naf-mode-root
;;;(progn (makunbound 'mon-naf-mode-root) (unintern 'mon-naf-mode-root))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-26T18:18:12-04:00Z}#{09353} - by MON KEY>
(defconst mon-naf-mode-notes (expand-file-name "notes" mon-naf-mode-root)
  "*Build a string to the path for the `notes' files.
Path used for to load all files for naf-mode notes.\n
EXAMPLE:\nmon-naf-mode-notes\n
See also; `mon-site-lisp-root',`mon-user-emacsd', `mon-emacs-root',
`mon-ebay-tmplt-mode-root', `mon-system-conditionals',
`gnu-linuxp', `win32p', `IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P',
`IS-MON-P-W32', `IS-MON-P-GNU'." )

;;;test-me; mon-naf-mode-notes
;;;(progn (makunbound 'mon-naf-mode-notes) (unintern 'mon-naf-mode-notes))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-19T14:26:35-04:00Z}#{09343} - by MON KEY>
(defconst mon-ebay-tmplt-mode-root (expand-file-name "ebay-template-mode" mon-naf-mode-root)
  "*Build a string to the path for the `naf-mode' files.
Path used for to load all files for `ebay-template-mode'.\n
EXAMPLE:\nmon-ebay-template-mode-root\n
See also; `mon-site-lisp-root',`mon-user-emacsd', `mon-emacs-root',
`mon-naf-mode-root', `mon-system-conditionals', `gnu-linuxp', `win32p',
`IS-BUG-P', `IS-BUG-P-REMOTE', `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU'.")

;;;test-me; mon-ebay-tmplt-mode-root
;;;(progn (makunbound 'mon-ebay-tmplt-mode-root) (unintern 'mon-ebay-tmplt-mode-root))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-11T18:54:22-04:00Z}#{09332} - by MON KEY>
(defconst mon-user-emacsd
  (expand-file-name 
   (cadr (assoc (cond (IS-MON-P-W32 1)
                      (IS-MON-P-GNU 2)
                      (IS-BUG-P 3)) 
                *mon-emacsd*)) mon-emacs-root)
  "*Build a string to the path for the site-local .emacs.d path.\n
EXAMPLE:\nmon-user-emacsd\n
See also; `mon-emacs-root', `mon-site-lisp-root', `mon-naf-mode-root'
`mon-ebay-tmplt-mode-root', `mon-system-conditionals', `gnu-linuxp',
`win32p', `IS-MON-P', `IS-MON-P-W32', `IS-MON-P-GNU', `IS-BUG-P',
`IS-BUG-P-REMOTE'.")

;;; test-me; mon-user-emacsd
;;;(progn (makunbound 'mon-user-emacsd) (unintern 'mon-user-emacsd))

;;; ==============================

;;; ==============================
(provide 'mon-default-loads)
;;; ==============================

;;; ==============================
;;; default-loads.el ends here
;;; EOF
