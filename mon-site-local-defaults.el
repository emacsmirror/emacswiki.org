;;; this is mon-site-local-defaults.el
;;; ================================================================
;;; DESCRIPTION: Example configuration of mon-site-local-defaults file.
;;; mon-site-local-defaults provides functions and vars that encapsulate 
;;; user data that is site local but used in public functions and across 
;;; systems.
;;;
;;; FUNCTIONS:►►►
;;; `mon-system-type-conditionals', `mon-user-name-conditionals'
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `*mon-emacsd*', `*MON-NAME*'
;;;
;;; VARIABLES:
;;; `*mon-misc-path-alist*'
;;; 
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK:
;;; (URL `http://www.emacswiki.org/emacs/mon-site-local-defaults.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T14:44:53-04:00Z}#{09393} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-11T16:48:54-04:00Z}#{09332} - by MON KEY>
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
;;; Copyright (C) 2009 by MON KEY
;;; =============================
;;; CODE:

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T19:47:14-04:00Z}#{09332} - by MON KEY>
(defun mon-user-name-conditionals ()
  "Return value for the current system-type's constant.
For debugging startup system name conditions. 
Returns one of:
\"IS-MON-P-W32\", \"IS-BUG-P\", \"IS-BUG-P-REMOTE\", \"win32p\"
\"IS-MON-P-GNU\", \"gnu-linuxp\"
EXAMPLE:\n\(mon-user-name-conditionals\)\n
See also; `mon-system-type-conditionals', `gnu-linuxp', `win32p'."
  (let ((is-w32 (eq system-type 'windows-nt))
        (is-gnu (or (eq system-type 'gnu/linux) (eq system-type 'linux))))
    (cond (is-w32
         (cond           
          ((equal user-real-login-name "<LOCAL-W32-SYSTEM-LOGIN-NAME>") "IS-MON-P-W32")
          ((equal user-real-login-name "<LOCAL-w32-SYSTEM-LOGIN-NAME-N>") 
           (if (file-directory-p "<CONDITIONAL-LOCAL-PATH-TEST>")
               "IS-BUG-P" "IS-BUG-P-REMOTE"))
          (t "win32p")))
          (is-gnu 
           (cond ((equal user-real-login-name "<LOCAL-GNU-SYSTEM-LOGIN-NAME>") "IS-MON-P-GNU")
                 (t "gnu-linuxp"))))))

;;;test-me;(mon-user-name-conditionals)

;;; ==============================
;;; This can't be loaded from mon-default-start-loads.
;;; CREATED: <Timestamp: #{2009-08-11T19:46:53-04:00Z}#{09332} - by MON KEY>
(defun mon-system-type-conditionals ()
  "Return the local system type as either 'win32p' or 'gnu-linuxp'.\n
Value is conditional on `mon-user-name-conditionals' return value.
EXAMPLE:\n\(mon-system-type-conditionals\)\n
NOTE: return value depends on constants bound in mon-default-loads.el e.g.:
`IS-MON-P-W32' `IS-BUG-P' `IS-BUG-P-REMOTE' `win32p' `IS-MON-P-GNU' `gnu-linuxp'"
  (let ((get-sys)) 
        (setq get-sys (mon-user-name-conditionals))
        (cond 
         ((equal get-sys   "IS-MON-P-W32")     "IS-W32")
         ((equal get-sys   "IS-BUG-P")         "IS-W32")
         ((equal get-sys   "IS-BUG-P-REMOTE")  "IS-W32")
         ((equal get-sys   "win32p")           "IS-W32")
         ((equal get-sys   "IS-MON-P-GNU")     "IS-GNU")
         ((equal get-sys   "gnu-linuxp")       "IS-GNU"))))

;;;test-me;(mon-system-type-conditionals)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-11T19:47:24-04:00Z}#{09332} - by MON KEY>
(defconst *mon-emacsd* 
  '((1                 ;; nth 0
     "<PATHSTRING-1>"  ;; nth 1
     "<PATHSTRING-2>"  ;; nth 2
     "<PATHSTRING-3>"  ;; nth 3
     "<PATHSTRING-4>"  ;; nth 4
     "<PATHSTRING-5>"  ;; nth 5
     "<PATHSTRING-6>") ;; nth 6
    (2                 ;; nth 0
     "<PATHSTRING-1>"  ;; nth 1
     "<PATHSTRING-2>"  ;; nth 2
     "<PATHSTRING-3>"  ;; nth 3
     "<PATHSTRING-4>"  ;; nth 4
     "<PATHSTRING-5>"  ;; nth 5
     "<PATHSTRING-6>") ;; nth 6
    )                  ;;{...etc...}
  "*Alist to encapusulate common site local and and default paths.
alist contains numbered keys (a key per system). 
cdr of each associated key maps to an Nary elt list where each elt points to a
site-local path, default value, string, etc.\n
EXAMPLE:\n\(assoc 1 *mon-emacsd*\)")

;;;test-me(assoc  1 *mon-emacsd*)
;;;test-me;(assoc 2 *mon-emacsd*)
;;;test-me;(assoc 3 *mon-emacsd*)
;;;test-me;(nth 6 (assoc 3 *mon-emacsd*))
;;;test-me;(nth 7 (assoc 3 *mon-emacsd*))
;;;test-me:
;;; (let ((list-val (number-sequence 1 3)))
;;;   (while list-val
;;;     (let* ((lv (pop list-val))
;;;            (this-user (assoc lv *mon-emacsd*)))
;;;       (mapc (lambda (x) (newline) (princ x (current-buffer))) this-user))))
;;
;;;(progn (makunbound '*mon-emacsd*) (unintern '*mon-emacsd*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-13T17:41:02-04:00Z}#{09334} - by MON KEY>
(defconst *MON-NAME*
  '((1 "<NAMEFORM-1>")(2 "<NAMEFORM-2>")(3 "<NAMEFORM-3>"));; {...etc..})
  "*Alist to encapsulate MON name across public functions.\n
Different system's and user's keep an alist of their preferred
Nameforms stored in a constant symbol of the form `*SOME-NAME*'
Numbered keys in alist (1 indexed) map to Nameforms - typically a string.
EXAMPLE:\n(assoc 1 *MON-NAME*) ;-> \"<NAMEFORM-1>\"")

;;;test-me; *MON-NAME*
;;;test-me; (assoc 1 *MON-NAME*)
;;;test-me; (cadr (assoc 2 *MON-NAME*)) 
;;
;;;(progn (makunbound '*MON-NAME*) (unintern '*MON-NAME*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-14T13:28:29-04:00Z}#{09335} - by MON>
(defvar *mon-misc-path-alist* nil
  "*Misc. path alist for paths not available on all systems.
Paths and values which don't need assignement to a dedicated variable.
All keys in this list should get a 'the-' prefix to help distinguish when they
will be used to assign a global var with a similar name. 
EXAMPLE:\n\(assoc 'the-1-path *mon-misc-path-alist*\) -> \"<PATHSTRING-1>\"")
;;
(when (not (bound-and-true-p *mon-misc-path-alist*))
  (setq *mon-misc-path-alist*
        '((the-1-path "<PATHSTRING-1>")
          (the-2-path "<PATHSTRING-2>")
          (the-3-path "<PATHSTRING-3>")
          (the-4-path "<PATHSTRING-4>"))))

;;;test-me; *mon-misc-path-alist*
;;;test-me;(assoc 'the-1-path *mon-misc-path-alist*)
;;
;;;(progn (makunbound '*mon-misc-path-alist*) (unintern '*mon-misc-path-alist*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-26T11:12:06-04:00Z}#{09353} - by MON KEY>
;;; TODO: 
;;; For browsing local urls, Consider building some pathname translations for
;;; browsing local `cached' URLS using `browse-url-of-buffer'.
;;; This could be useful for example when editing ebay-template files. 
;;; 1) Capture the html region 
;;; 2) spit it to a temp-buffer (or just write it to a temp file!)
;;; 3) browse it in a conkeror/ffx 
;;; SEE: (URL `http://www.emacswiki.org/emacs/BrowseUrl#toc3')
;;; (add-to-list 'browse-url-filename-alist
;;;              '("/var/www/cgi/files/" . "http://my.website.com/cgi?"))

;;; ==============================
(provide 'mon-site-local-defaults)
;;; ==============================

;;; ================================================================
;;; mon-site-local-defaults.el ends here
;;; EOF
