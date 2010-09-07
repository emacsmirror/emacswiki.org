;;; mon-cifs-utils.el --- utilities for mounting CIFS domain using auth-source
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-cifs-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-01-06T15:49:01-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: GnuPG, SAMBA, CIFS, environment, files, external, execute

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-cifs-utils provides utilities for mounting domain using auth-source to
;; access CIFS (Cats in Fat Suits) AKA Common Internet File System protocol,
;; e.g. successor to the SMB (Server Message Block) protocol and the Samba.
;; :SEE (man "mount.cifs") :SEE (man "umount.cifs") :SEE (man "samba")
;;
;; FUNCTIONS:►►►
;; `mon-map-cifs-domain->local-mount', `mon-get-cifs-credentials',
;; `mon-build-cifs-credentials', `mon-get-cifs-mount-points',
;; `mon-mount-cifs', `mon-unmount-cifs', `mon-bind-cifs-vars-at-loadtime',
;; `mon-inform-cifs-credentials-unbound', `mon-verify-CIFS-credentials',
;; `mon-spit-auth-source-debug->var',
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
;; `*mon-CIFS-mount-points*', `*mon-CIFS-auth-protocol*',
;; `*mon-CIFS-domain*', `*mon-CIFS-pass*',
;; `*mon-CIFS-user*', `*mon-CIFS-mount-root*',
;; `*mon-CIFS-vars-unbound*',
;; `*mon-auth-source-debug-var*'
;; `*mon-auth-source-cache-debug-var*'
;; `*mon-misc-path-alist*' <- (CONDITIONAL)
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
;; The function `mon-mount-cifs' invokes 'mount -t cifs {...}' via sudo. 
;; Change the shell command concatenations in that file if that is not what you
;; want.  mount.cifs command may be used only by root, unless installed
;; setuid, in which case the noeexec and nosuid mount flags are enabled.
;; You may need to adjust your /etc/fstab accordingly.
;; :SEE (man "fstab")
;; :SEE (man "getmntent")
;;
;; Below MON uses the `IS-MON-P' conditional from:
;; :FILE mon-default-loads.el to test the current system type.
;; FTMP, IS-MON-P =: 
;;     (and (equal (user-login-name) "<MON-USER-LOGIN-NAME>")
;;          (or (eq system-type 'gnu/linux)
;;              (eq system-type 'windows-nt)))
;
;; However, MON has various association keys ferreted away in the
;; :VARIABLE `*mon-misc-path-alist*' these are only evaluated per the above
;; conditional and shouldn't be on your system.
;; This variable is inlined here as `*mon-CIFS-misc-path-alist*' and included
;; with example values. To use this library _you_ should either:
;;  a) Provide the appropriate values for the `*mon-misc-path-alist*' keys if
;;     you are already using that variable with another of MON's other packages.
;;  b) Provide the appropriate values for the `*mon-CIFS-misc-path-alist*' keys.
;;  c) Directly bind the following variables:
;;     `*mon-CIFS-domain*', `*mon-CIFS-mount-root*', `*mon-CIFS-mount-points*'
;;
;; And no, MON won't be encapsulating these local variables in defcustom forms.
;; Code relying on the auth-source facility has no business _whatsoever_
;; interacting with defcustom's dark voodo.
;; 
;; SNIPPETS:
;;
;; :NOTE If you have an Emacs running on a w32 system to find these values
;; evaluate the following on the w32 machine:
;;  (getenv "USERDOMAIN")
;;  (getenv "LOGONSERVER")
;;
;; REQUIRES:
;; auth-source.el
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-cifs-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-01-05T21:41:27-05:00Z}#{10013} - by MON>
;;
;; EMACSWIKI: (URL `http://www.emacswiki.org/emacs/MonCifsUtils')
;; FIRST-PUBLISHED: <Timestamp: #{2010-01-06T15:49:01-05:00Z}#{10013} - by MON>
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-01-06T15:49:01-05:00Z}#{10013} - by MON KEY>
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
                   (require 'auth-source)
                   (require 'mon-cl-compat)
                   ;; :NOTE See header for details.
                   (unless (bound-and-true-p IS-MON-P)
                     (setq IS-MON-P nil)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-11T13:53:07-05:00Z}#{10021} - by MON KEY>
(defvar *mon-CIFS-misc-path-alist*
  (if (or (bound-and-true-p *mon-misc-path-alist*)
          IS-MON-P)
      *mon-misc-path-alist*
      nil)
  "*An alist mapping miscellaneous paths paths not available on all systems.
Paths and values which don't need assignement to a dedicated variable.
All keys in this list should get a 'the-' prefix to help distinguish when they
will be used to assign a global var with a similar name e.g.\n
 'the-mnt-prfx -> `*mon-CIFS-domain*'\n
:EXAMPLE\n(assoc 'the-mnt-prfx *mon-CIFS-misc-path-alist*)\n
:NOTE This variable is normally bound in a separate file on MON systems. It is
provided here with sample key values as a compatibily feature for the
mon-cifs-utils package. Populate the alist keys `the-shr-prfx', `the-mnt-prfx',
`the-mnt-maps' with appropriate values if you wish to bind CIFS paths in the
default fashion at load time e.g. with `mon-bind-cifs-vars-at-loadtime'
_without_ the optinal ARG NO-MAP-MOUNT-POINTS. Additional details regarding
possible formats for the alist key `the-mnt-maps' are in docstrings of:
:SEE `mon-mount-cifs'\n:SEE `mon-map-cifs-domain->local-mount'.
:CALLED-BY `*mon-CIFS-domain*',`*mon-CIFS-mount-root*',`*mon-CIFS-mount-points*'.
:SEE-ALSO `mon-unmount-cifs'.\n►►►")
;;
(unless (bound-and-true-p *mon-CIFS-misc-path-alist*)
  ;; Build some same safe dummy values for `*mon-CIFS-misc-path-alist*' if not present.
  (setq *mon-CIFS-misc-path-alist*
        '((the-shr-prfx ;; :CALLED-BY `*mon-CIFS-domain*'
           "//<PROVIDE-DOMAIN-HERE>")     
          (the-mnt-prfx ;; :CALLED-BY `*mon-CIFS-mount-root*'
           "/<PROVIDE-MNT>/<PREFIX-HERE>") ;; e.g. /mnt/some-dir
          (the-mnt-maps ;; :CALLED-BY `*mon-CIFS-mount-points*'
           ((<EXAMPLE-SHARE1-KEY> 
             "<EXAMPLE-REMOTE-SHARE1-PATH>" "<EXAMPLE-LOCAL-SHARE1-PATH>")
            (<EXAMPLE-SHARE2-KEY> 
             "<EXAMPLE-REMOTE-SHARE2-PATH>" "<EXAMPLE-LOCAL-SHARE2-PATH>")
            (<EXAMPLE-SHARE3-KEY> 
             "<EXAMPLE-REMOTE-SHARE3-PATH>" "<EXAMPLE-LOCAL-SHARE3-PATH>"))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T16:26:10-05:00Z}#{10012} - by MON>
(defvar *mon-CIFS-auth-protocol* nil
  "*A protocol list of CIFS elements for `auth-source-protocols'.\n
List has the form '\(key str-val str-num ... ) where key is the symbol `cifs'
and value is a string nameing a protocol or port number apropos your site local
CIFS configuration.\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs', `mon-get-cifs-credentials',
`mon-bind-cifs-vars-at-loadtime', `*mon-CIFS-domain*', `*mon-CIFS-user*',
`*mon-CIFS-pass*'.\n►►►")
;;
;;; :TEST-ME (assoc 'cifs auth-source-protocols)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T16:22:38-05:00Z}#{10012} - by MON>
(defvar *mon-CIFS-domain* nil
  "*CIFS domain to mount with `*mon-CIFS-user*' using `*mon-CIFS-pass*'.\n
This is the the CIFS `workgroup' or `domain' you wish to connect with.
:NOTE If you have an Emacs running on a w32 system one way to find this value is
to evaluate the following on the w32 machine:\n
 \(getenv \"USERDOMAIN\"\)\n \(getenv \"LOGONSERVER\"\)\n
:SEE \(man \"mount.cifs\")\n:SEE \(man \"samba\"\)
:SEE `*mon-CIFS-mount-points*' for additional discussion.
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs',
`mon-bind-cifs-vars-at-loadtime', `mon-get-cifs-credentials',
`*mon-CIFS-misc-path-alist*',`*mon-CIFS-auth-protocol*'.\n►►►")
;;
;;; :TEST-ME *mon-CIFS-domain*
;;
;;;(progn (makunbound '*mon-CIFS-domain*) (unintern '*mon-CIFS-domain*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T16:22:38-05:00Z}#{10012} - by MON>
(defvar *mon-CIFS-user* nil
  "*CIFS username to access `*mon-CIFS-domain*' using `*mon-CIFS-pass*'.\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs', `mon-get-cifs-credentials',
`mon-bind-cifs-vars-at-loadtime', `*mon-CIFS-auth-protocol*',
`*mon-CIFS-pass*'.\n►►►")
;;
;;; :TEST-ME *mon-CIFS-user*
;;
;;;(progn (makunbound '*mon-CIFS-user*) (unintern '*mon-CIFS-user*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T16:22:38-05:00Z}#{10012} - by MON>
(defvar *mon-CIFS-pass* nil
  "*CIFS password of `*mon-CIFS-user*' for accessing `*mon-CIFS-domain*'.\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs', `mon-get-cifs-credentials',
`mon-bind-cifs-vars-at-loadtime', `*mon-CIFS-misc-path-alist*'.\n►►►")
;;
;;; :TEST-ME *mon-CIFS-pass*
;;
;;;(progn (makunbound '*mon-CIFS-pass*) (unintern '*mon-CIFS-pass*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T12:52:36-05:00Z}#{10012} - by MON KEY>
(defvar *mon-CIFS-mount-root* nil
"*Local mount point for networked SMBFS/CIFS shares.\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs',
`mon-map-cifs-domain->local-mount', `mon-bind-cifs-vars-at-loadtime',
`*mon-CIFS-misc-path-alist*', `*mon-CIFS-domain*', `*mon-CIFS-user*',
`*mon-CIFS-pass*'.\n►►►")
;;
;;; :TEST-ME *mon-CIFS-mount-root*
;;
;;;(progn (makunbound '*mon-CIFS-mount-root*) (unintern '*mon-CIFS-mount-root*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-11T16:21:52-05:00Z}#{10021} - by MON KEY>
(defvar *mon-CIFS-vars-unbound* nil
  "*A buffer name for displaying status of mon-cifs-utils variables.\n
When value is a string the buffer will take this name.\n
:NOTE Setting value to a non-nil string value overides the default behavior of
`mon-inform-cifs-credentials-unbound' which prints a message to the buffer named
 \"*MON-CIFS-VARS-UNBOUND*\".\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs', `mon-verify-cifs-credentials',
`mon-bind-cifs-vars-at-loadtime', `mon-get-cifs-credentials',
`mon-build-cifs-credentials', `*mon-CIFS-user*', `*mon-CIFS-pass*'.\n►►►")
;;
(unless (bound-and-true-p *mon-CIFS-vars-unbound*)
  (setq *mon-CIFS-vars-unbound*
        (upcase (symbol-name '*mon-CIFS-vars-unbound*))))
;;;
;;; :TEST-ME *mon-CIFS-vars-unbound*
;;; :TEST-ME (stringp *mon-CIFS-vars-unbound*)
;;
;;;(progn (makunbound '*mon-CIFS-vars-unbound*) (unintern '*mon-CIFS-vars-unbound*) )

;;; ==============================
;;; :NOTE CIFS paths with spaces need escapes with `\40' so double the `\' here.
;;;       For example, `Some Spacey Path' -> `Some\\40Spacey\\40Path'
;;; :CREATED <Timestamp: #{2010-01-05T12:54:30-05:00Z}#{10012} - by MON KEY>
(defvar *mon-CIFS-mount-points* nil
  "*An alist mapping local mount points to networked shares.
Alist entries have one of the following the forms:\n
 \(symbol \"//<CIFS-DOMAIN-IP>/<SOME-CIFS-SHARE>\" \"/LOCAL/MOUNT/<MNT-POINT>\"\)\n
 \(symbol \"//<CIFS-DOMAIN-NAME>/<SOME-CIFS-SHARE>\" \"/LOCAL/MOUNT/<MNT-POINT>\"\)\n
Where //<CIFS-DOMAIN-NAME> is the domain or workgroup and <SOME-CIFS-SHARE> is
one of its shares and where /LOCAL/MOUNT/ is the local systems common mount
directory e.g. /mnt, /media, etc. and <MNT-POINT> is a directory beneath.  
:NOTE The first two forms above are not needed, and indeed should not be used,
where each of your shares are located beneath `*mon-CIFS-domain*' and where each
of your local mount points are beneath `*mon-CIFS-mount-root*' as, by default,
these are programmatically updated with `mon-map-cifs-domain->local-mount'.
Instead, you should provide alist entries with the `//CIFS-DOMAIN-*' and root
`/LOCAL/MOUNT/' portions omitted by using the form:\n
 \(symbol \"<SOME-CIFS-SHARE>\" \"MNT-POINT\"\)\n
A more detailed example of the preferred format can be seen evaluating and
examining the output of the the example code provided after the `:EXAMPLE'
section of that `mon-map-cifs-domain->local-mount's docstring.\n
:NOTE To find CIFS shares and/or test access to a domain do as follows:\n
 shell> smbclient -L <DOMAIN> -W windom -U <USERNAME>
        [Provide password when prompted]\n
:SEE \(man \"samba\"\)\n:SEE \(man \"fstab\"\)\n:SEE \(man \"getmntent\"\)\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs', `mon-get-cifs-credentials',
`mon-bind-cifs-vars-at-loadtime', `mon-build-cifs-credentials',
`mon-get-cifs-mount-points', `*mon-CIFS-misc-path-alist*', `*mon-CIFS-mount-points*',
`*mon-CIFS-auth-protocol*', `*mon-CIFS-user*', `*mon-CIFS-pass*'.\n►►►")
;;
;;; :TEST-ME *mon-CIFS-mount-points*
;;; :TEST-ME (cdr (assoc 'HG-Repos *mon-CIFS-mount-points*))
;;
;;;(progn (makunbound '*mon-CIFS-mount-points*) (unintern '*mon-CIFS-mount-points*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T19:13:39-05:00Z}#{10012} - by MON KEY>
(defun mon-map-cifs-domain->local-mount (&optional mount-point-list 
                                         local-mount-point cifs-domain)
  "Map common domain and mount points across the path values in cdr's of
`*mon-CIFS-mount-points*'. Assumes that the domain and base-mount point are
consistent across all the elements of `*mon-CIFS-mount-points*' and tack on
the path prefixes here.\n
Optional args MOUNT-POINT-LIST, LOCAL-MOUNT-POINT, and CIFS-DOMAIN should have
similar values as those of their respective defaults values in:\n
`*mon-CIFS-mount-points*', `*mon-CIFS-mount-root*', and `*mon-CIFS-domain*'.\n
:NOTE In the default configuration this function assumes that all of your local
mount points reside under (for example):\n
 /mnt/some-base/path\n
And that all of your CIFS shares reside under a common domain (for example):\n
 //CIFS-DOMAIN-NAME\n
This approach provides a degree of portability, allows reasonable inter-system
mirroring of file trees, and aids in keeping path variables,symlinks, alias,
etc. to a minimum.\n
:EXAMPLE\n\n\(mon-map-cifs-domain->local-mount
 '\(\(PLN-BUBBA \"REMOTE-pln-bubba\" \"LOCAL-pln-bubba\"\)
   \(LIL-BUBBA \"REMOTE-lil-bubba\" \"LOCAL-lil-bubba\"\)
   \(BIG-BUBBA \"REMOTE-big-bubba\" \"LOCAL-big-bubba\"\)\) ;<- MOUNT-POINT-LIST
 \"/mnt/local-bubba\"                                  ;<- LOCAL-MOUNT-POINT
 \"BUBBAS-CIFS\"\)                                      ;<- CIFS-DOMAIN\n
If this above format does not reflect your current configuration, one could
repeatedly evaluate this loop substituting the optional args where needed and
`add-to-list'ing the return values to `*mon-CIFS-mount-points*' variable.\n
:SEE \(man \"mount.cifs\"\)\n:SEE \(man \"getmntent\")\n:SEE \(man \"fstab\")\n
:SEE-ALSO `mon-get-cifs-credentials' `mon-build-cifs-credentials',
`mon-get-cifs-mount-points', `mon-bind-cifs-vars-at-loadtime', `mon-mount-cifs',
`mon-unmount-cifs', `*mon-CIFS-misc-path-alist*', `*mon-CIFS-auth-protocol*',
`*mon-CIFS-pass*', `*mon-CIFS-user*'.\n►►►"
  (let (map-mnt )
    (dolist (i (or mount-point-list *mon-CIFS-mount-points*) (nreverse map-mnt))
      (push `(,(car i) 
               ,(concat "//" (or cifs-domain *mon-CIFS-domain*) "/" (cadr i))
               ,(concat (or local-mount-point  *mon-CIFS-mount-root*) "/" (caddr i)))
            map-mnt))))
;;
;;; :TEST-ME (mon-map-cifs-domain->local-mount)

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS `anything-delicious.el' 
;;; :WAS `anything-delicious-authentify'
;;; :NOTE When debugging set: 
;;;      (setq auth-source-debug nil) ;nil
;;;      (setq auth-source-hide-passwords t) ;t 
;;; :CREATED <Timestamp: #{2010-01-05T16:21:52-05:00Z}#{10012} - by MON>
(defun mon-get-cifs-credentials ()
  "Set CIFS user and password values.\n
The value of variables `*mon-CIFS-user*' and  `*mon-CIFS-pass*' are bound but
are not displayed.\n
Evaluate the variable for side effects if this is what you you want.\n
For use as authentication values when accessing/mounting `*mon-CIFS-domain*'
using the local ~/.authinfo.gpg file instead of a dedicated credentials file.\n
:NOTE Requires `auth-sources' and a properly configured .authinfo that 
contains pre-existing CIFS authentication entry formatted in netrc format as per
`auth-source's requirments. CIFS values in .authinfo files should be as below:\n
 machine protocol login password ;<- netrc style
 domain username password        ;<- _NOT_ CIFS style\n
So, if you normally use /etc/fstab or mount with the option:\n
  crenditals=<SOME-FILE>\n
where <SOME-FILE> contains the key/value pairs:\n
 domain=<SOME-DOMAIN> username=<SOME-USER> password=<SOME-PASS>\n
your ~/.authinfo.gpg file should contain a line with these pairs:\n
 machine <SOME-MACHINE> port cifs login <SOME-LOGIN> password <SOME-PASS>\n
IOW, to make a CIFS credentials format congruent with  ~/.authinfo.gpg you would:\n
 o Add a protocol entry using `port cifs';
 o Substitute `domain' with `machine';
 o Substitute `username' with `login';\n
:NOTE Depending on the current netbios config `domain' is prob. the `workgroup'.
:SEE \(man \"mount.cifs\"\) for additional details\n
:SEE-ALSO `auth-source-user-or-password',`mon-map-cifs-domain->local-mount',
`mon-build-cifs-credentials', `mon-get-cifs-mount-points', `mon-mount-cifs',
`mon-unmount-cifs', `mon-bind-cifs-vars-at-loadtime',
`*mon-CIFS-misc-path-alist*', `*mon-CIFS-mount-points*',
`*mon-CIFS-auth-protocol*',`*mon-CIFS-mount-root*'.\n►►►"
  (interactive)
  (let ((cifs-auth (auth-source-user-or-password  '("login" "password")
                                                  *mon-CIFS-domain*
                                                  "cifs")))
    (when cifs-auth
      (setq *mon-CIFS-user* (car cifs-auth)
            *mon-CIFS-pass* (cadr cifs-auth))
      nil)))
;;
;;; :TEST-ME (mon-get-cifs-credentials)
;;; :TEST-ME *mon-CIFS-user*
;;; :TEST-ME *mon-CIFS-pass*

;;(declare-function cl::pairlis "mon-cl-compat" t t)
(declare-function mon-mapcar "mon-utils")
;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T13:55:59-05:00Z}#{10012} - by MON KEY>
(defun mon-build-cifs-credentials (&optional credentials-file)
  "Return a credentials string for mounting CIFS.
Default is to concatenate the credentials values of
`*mon-CIFS-user*', `*mon-CIFS-domain*', `*mon-CIFS-pass*' 
As per CIFS specs return value has the form:\n
domain=<DOMAIN>,username=<USERNAME>,password=<PASSWORD>\n
When optional arg CREDENTIALS-FILE is non-nil returns:\n
 credentials=CREDENTIALS-FILE\n
:SEE \(man \"mount.cifs\"\) for additional specifications.\n
:SEE-ALSO `mon-map-cifs-domain->local-mount', `mon-build-cifs-credentials',
`mon-verify-cifs-credentials', `mon-get-cifs-mount-points', `mon-mount-cifs',
`mon-unmount-cifs', `mon-bind-cifs-vars-at-loadtime'
`*mon-CIFS-misc-path-alist*', `*mon-CIFS-mount-points*',
`*mon-CIFS-auth-protocol*', `*mon-CIFS-mount-root*'.\n►►►"
  (let (creds)
    (if (and (not (null credentials-file))
             (file-exists-p credentials-file))
        (setq creds  (concat "credentials=" credentials-file))
        ;; Mapping -- use `\n' not `,' if we decide to spit string to file instead.
        (setq creds (mapconcat 
                     #'(lambda (p) (concat (car p) (cdr p)))
                     ;;(pairlis '("domain=" "username=" "password=")
                     ;; cl::pairlis '("domain=" "username=" "password=")
                     (mon-mapcar 'cons
                                  '("domain=" "username=" "password=")
                                  `(,*mon-CIFS-domain* ,*mon-CIFS-user* ,*mon-CIFS-pass*))
                     ",")))))
;;
;;; :TEST-ME (mon-build-cifs-credentials)
;;; :TEST-ME (mon-build-cifs-credentials (buffer-file-name))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T12:44:53-05:00Z}#{10012} - by MON KEY>
(defun mon-get-cifs-mount-points (&optional mount-point intrp)
  "Return alist element of key MOUNT-POINT in `*mon-CIFS-mount-points*'.\n
When MOUNT-POINT is nil or called-interactively prompt for MOUNT-POINT key.\n
:CALLED-BY `mon-mount-cifs', `mon-unmount-cifs'\n
:SEE-ALSO `mon-map-cifs-domain->local-mount', `mon-get-cifs-credentials',
`mon-build-cifs-credentials', `mon-verify-cifs-credentials',
`mon-bind-cifs-vars-at-loadtime', `*mon-CIFS-mount-points*',
`*mon-CIFS-auth-protocol*', `*mon-CIFS-misc-path-alist*', `*mon-CIFS-domain*',
`*mon-CIFS-pass*', `*mon-CIFS-user*', `*mon-CIFS-mount-root*'.\n►►►"
  (interactive "p")
  (let (;;(mnt-root *mon-CIFS-mount-root*)
        (mnt-pnts *mon-CIFS-mount-points*))
    (setq mnt-pnts 
          (assoc 
           (if (or intrp (null mount-point))
               (car (read-from-string 
                     (completing-read "Which mount point (TAB completes): "
                                      mnt-pnts
                                      nil t)))
               mount-point)
           mnt-pnts))))
;;
;;; :NOTE Tests assume req'd vars are bound already.
;;
;;: :TEST-ME (mon-get-cifs-mount-points (car (nth 1 *mon-CIFS-mount-points*)))
;;: :TEST-ME (mon-get-cifs-mount-points) 

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T13:14:17-05:00Z}#{10012} - by MON KEY>
(defun mon-mount-cifs (the-mount &optional unmount credential-file as-string-no-shell)
  "Mount a remote CIFS designated by alist key THE-MOUNT.
Elements of THE-MOUNT are mapped to local mount points in fstab and retrieved
with `mon-get-cifs-mount-points' from alist in `*mon-CIFS-mount-points*'.\n
When THE-MOUNT is nil or called-interactively prompt for THE-MOUNT.\n
When optional arg UNMOUNT is non-nil or called-interactively with prefix arg
unmont THE-MOUNT.\n
When CREDENTIAL-FILE is non-nil and mounting read credentials from file.\n
:NOTE CREDENTIAL-FILE must be readable by current user as `mount' is invoked
with sudo.\n
When optional AS-STRING-NO-SHELL is non-nil return shell-command as string and
do not execute.\n
:EXAMPLE\n\(let* \(\(*mon-CIFS-user* \"BUBBA\"\)
       \(*mon-CIFS-pass* \"BUBBAS-PASS\"\)
       \(*mon-CIFS-mount-root* \"/mnt/local-bubba\" \)
       \(*mon-CIFS-domain* \"BUBBAS-CIFS\"\)
       \(*mon-CIFS-mount-points*
        \(mon-map-cifs-domain->local-mount
         '\(\(LIL-BUBBA \"REMOTE-lil-bubba\" \"LOCAL-lil-bubba\"\)
           \(BIG-BUBBA \"REMOTE-big-bubba\" \"LOCAL-big-bubba\"\)\)
         \"/mnt/local-bubba\" \"BUBBAS-CIFS\"\)\)
       \(mn-str \(mon-mount-cifs 'LIL-BUBBA nil nil t\)\)
       \(um-str \(mon-mount-cifs 'BIG-BUBBA t nil t\)\)
       \(mn-str-creds 
        \(mon-mount-cifs 'BIG-BUBBA nil 
                        \(directory-file-name default-directory\) t\)\)
       \(lbep \(line-beginning-position 2\)\)\)
  \(setq mn-str 
        \(format 
         \(concat 
          \"\\nSample MOUNT for shell-command:\\n\\n shell> %s\\n\\n\"
          \"Sample UMOUNT string for shell-command:\\n\\n  shell> %s\\n\\n\"
          \"Sample MOUNT string using credntials file:\\n\\n  shell> %s\\n\\n\"\)
         mn-str um-str mn-str-creds\)\)
  \(mon-help-overlay-result lbep \(+ \(length mn-str\) lbep\) 81 mn-str\)\)\n
:SEE \(man \"mount.cifs\"\)\n:SEE \(man \"mount\")
:SEE \(man \"umount.cifs\"\)\n:SEE \(man \"samba\"\)\n:SEE \(man \"sudo\"\)\n
:SEE-ALSO `mon-unmount-cifs', `mon-map-cifs-domain->local-mount',
`mon-get-cifs-credentials', `mon-build-cifs-credentials',
`mon-get-cifs-mount-points', `mon-bind-cifs-vars-at-loadtime',
`mon-verify-cifs-credentials', `mon-inform-cifs-credentials-unbound'
`*mon-CIFS-misc-path-alist*', `*mon-CIFS-mount-points*', `*mon-CIFS-pass*',
`*mon-CIFS-user*', `*mon-CIFS-mount-root*', `*mon-CIFS-domain*',
`*mon-CIFS-vars-unbound*', `*mon-CIFS-auth-protocol*',
`*mon-CIFS-mount-root*'.\n►►►"
  (interactive "i\nP") 
  (let ((mp (mon-get-cifs-mount-points the-mount)))
    (setq mp (if unmount
                 (concat "sudo /sbin/umount -t cifs " (nth 1 mp))
                 (concat "sudo /sbin/mount -t cifs " (nth 1 mp) 
                         ;;" " *mon-CIFS-mount-root* "/" (nth 2 mp) 
                         " " (nth 2 mp) 
                         " -o " (mon-build-cifs-credentials credential-file)
                         ",iocharset=utf8,owner,user,uid="
                         (number-to-string (user-uid)))))
    (if as-string-no-shell mp
        (shell-command mp))))
;;
;;; :TEST-ME (mon-mount-cifs (car (nth 1 *mon-CIFS-mount-points*)))
;;; :TEST-ME (mon-mount-cifs (car (nth 1 *mon-CIFS-mount-points*)) t)
;;; :TEST-ME (mon-mount-cifs (car (nth 1 *mon-CIFS-mount-points*)) nil (buffer-file-name))
;;; :TEST-ME (mon-mount-cifs (car (nth 1 *mon-CIFS-mount-points*)) t)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-09T12:53:00-05:00Z}#{10102} - by MON KEY>
(defun mon-unmount-cifs (the-mount)
  "Unount a remote CIFS designated by alist key THE-MOUNT.\n
Like `mon-mount-cifs' but only unmounts the CIFS.\n
:EXAMPLE\n\n\(mon-unmount-cifs nil\)\n
:SEE-ALSO `mon-map-cifs-domain->local-mount', `mon-get-cifs-credentials',
`mon-build-cifs-credentials', `mon-get-cifs-mount-points', 
`mon-bind-cifs-vars-at-loadtime', `mon-verify-cifs-credentials',
`mon-inform-cifs-credentials-unbound' `*mon-CIFS-misc-path-alist*',
`*mon-CIFS-mount-points*', `*mon-CIFS-pass*', `*mon-CIFS-user*',
`*mon-CIFS-mount-root*', `*mon-CIFS-domain*', `*mon-CIFS-vars-unbound*',
`*mon-CIFS-auth-protocol*', `*mon-CIFS-mount-root*'.\n►►►"
 (interactive "i")
 (shell-command (mon-mount-cifs (or the-mount) t nil t)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-11T15:26:59-05:00Z}#{10021} - by MON KEY>
(defun mon-verify-cifs-credentials ()
  "Check if the variables `*mon-CIFS-user*' `*mon-CIFS-pass*' are non-nil.\n
When these are bound message user that this is so.\n
When these are not bound, interactively prompt user if they should be, and if so
make it so, else message user that `mon-mount-cifs' featuers will be available
after evaluating `mon-verify-cifs-credentials'.\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs', `mon-get-cifs-credentials',
`mon-build-cifs-credentials', `mon-bind-cifs-vars-at-loadtime'.\n►►►"
  (interactive)
  (if (and *mon-CIFS-user*  *mon-CIFS-pass*) 
      (message "The variables `*mon-CIFS-user*' and `*mon-CIFS-pass*' are set")
      (if (yes-or-no-p 
           (concat 
            "The variables `*mon-CIFS-user*' `*mon-CIFS-pass*' are _NOT_ set.\n"
            "Would you like to set them now? "))
          (progn     
            (mon-get-cifs-credentials)
            (mon-verify-cifs-credentials))
          (message 
           (concat
            "Set variables `*mon-CIFS-user*' and `*mon-CIFS-pass*' with the command\n"
            "`mon-verify-cifs-credentials' when you are ready to use `mon-mount-cifs'.")))))
;;    
;; :TEST-ME (progn (setq *mon-CIFS-user* nil *mon-CIFS-pass* nil)
;;                 (mon-verify-cifs-credentials))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-11T15:59:58-05:00Z}#{10021} - by MON KEY>
(defun mon-inform-cifs-credentials-unbound ()
  "Inform that `*mon-CIFS-user*' and `*mon-CIFS-pass*' were void at load time.\n
Write a message to buffer *MON-CIFS-VARS-UNBOUND* and display it.\n
:NOTE This procedure is evaluated at load-time and will write its message to the
buffer specifed by the variable `*mon-CIFS-vars-unbound*' each time Emacs is
loaded. This behavior is useful when one wishes to be reminded to load in the
CIFS user and password values. By default these values are accessed and bound
with `mon-get-cifs-credentials' which evaluates `auth-source-user-or-password'.\n 
Whenever one is using ~/authinfo.gpg \(or equivalent\) as the value for
`auth-sources', the auth-source interface will (in an attempt to preserve a
degree of security) require that users first provide a gpg password before it
will parse the auth-sources file.\n
At present MON is not able to find any reasonable way to prompt the user for
this information at load-time, moreover it isn't entirely clear whether we
should do this.\n
As such, it is left up to the user to evaluate `mon-verify-cifs-credentials'
which initializes the auth-source interface, prompts the user for password
details and sets the necessary CIFS credentials.\n
However, because it is easy to forget to intialize one's auth-source cache we
write an informative message to a big ugly buffer name which one is not likely
to miss esp. when switching among a new Emacs session's intial set of buffers.\n
If this amount of load-time verbosity is not what is wanted it can be disabled by
setting the value of `*mon-CIFS-vars-unbound*' to something other than a string,
in which case we simply signal the following message at load time:\n
\"*MON-CIFS-VARS-UNBOUND*\"\n
:SEE-ALSO `mon-mount-cifs', `mon-unmount-cifs',
`mon-map-cifs-domain->local-mount', `mon-get-cifs-credentials',
`mon-build-cifs-credentials', `mon-get-cifs-mount-points',
`mon-bind-cifs-vars-at-loadtime', `mon-inform-cifs-credentials-unbound',
`mon-verify-cifs-credentials', `*mon-CIFS-mount-points*',
`*mon-CIFS-auth-protocol*', `*mon-CIFS-domain*', `*mon-CIFS-mount-root*',
`*mon-CIFS-vars-unbound*', `*mon-misc-path-alist*'.\n►►►"
  (let ((mcvu *mon-CIFS-vars-unbound*)
        (mcvu-msg 
         (concat 
          ";;; Buffer written: " (format-time-string "%Y-%m-%dT%T%z")
          "\n\nThe variables `*mon-CIFS-user*' and `*mon-CIFS-pass*' were void at load time.\n\n"
          "This is a good thing! :)\n\n"
          "That said, these variables are required for use of the package mon-cifs-utils.\n\n"
          "Once you are ready to use mon-cifs-utils features, bind each of variable to\n"
          "make them availabe for your current Emacs session.\n\n"
          "This can be done been by evaluating the following form:\n\n"
          " (mon-verify-cifs-credentials)\n\n"
          "Or, by invoking the command:\n\n"
          " M-x mon-verify-cifs-credentials\n\n"
          "For additional details about mon-cifs-utils and its primary interface\n"
          ":SEE `mon-mount-cifs' <- (describe-function 'mon-mount-cifs)\n")))
    (if (stringp *mon-CIFS-vars-unbound*)
        (progn
          (setq mcvu (get-buffer-create mcvu))
          (with-current-buffer mcvu 
            (let ((inhibit-read-only t))
              (save-excursion (princ mcvu-msg mcvu)))
            ;;(goto-char (buffer-end 0))
            (set (make-local-variable 'buffer-read-only) t))
            (display-buffer mcvu t))
        (message (upcase (symbol-name '*mon-CIFS-vars-unbound*))))))
;;
;;; :TEST-ME (mon-inform-cifs-credentials-unbound)
;;; :TEST-ME (let ((*mon-CIFS-vars-unbound* 'no-show-buffer))
;;;            (mon-inform-cifs-credentials-unbound))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-11T12:32:09-05:00Z}#{10021} - by MON KEY>
(defun mon-bind-cifs-vars-at-loadtime (&optional no-misc-path no-map-mount-points)
  "Called at load time to \(re\)bind the variables for mon-cifs-utils.\n
Performs the following tasks:\n
o Require the `auth-source.el' if it isn not already present.\n
o Add the value of `*mon-CIFS-auth-protocol*' to `auth-source-protocols'.\n
o Evaluate `mon-get-cifs-credentials' which binds the values of: 
  `*mon-CIFS-user*' and  `*mon-CIFS-pass*'\n
o Bind or rebind the following variables:\n
  `*mon-CIFS-domain*', `*mon-CIFS-mount-root*', `*mon-CIFS-mount-points*',\n
o When the variable `*mon-CIFS-misc-path-alist*' is not present bind it to a
  symbol holding an alist of safe dummy values which present examples for the
  alist keys 'the-shr-prfix', 'the-mnt-prfx', and 'the-mnt-maps' which are
  evaluated elsewhere in with this procedure.\n
 The `*mon-CIFS-misc-path-alist*' keys map to variables:\n
  'the-shr-prfix' <- `*mon-CIFS-domain*'
  'the-mnt-prfx'  <- `*mon-CIFS-mount-root*'
  'the-mnt-maps'  <- `*mon-CIFS-mount-points*'\n
o When optional arg NO-MISC-PATH is non-nil does not map the domain and mount
  points specified by the alist key values 'the-mnt-prfx' and 'the-mnt-maps' in
  `*mon-CIFS-misc-path-alist*'; instead uses the user symbol-values of
  `*mon-CIFS-domain*' and `*mon-CIFS-mount-root*'.\n
o Evaluate `mon-map-cifs-domain->local-mount' which maps the values of
  `*mon-CIFS-domain*' and `*mon-CIFS-mount-root*' by prepending these onto the
  cadr and caddr of each element of `*mon-CIFS-mount-points*'.\n
o When optional arg NO-MAP-MOUNT-POINTS is non-nil do not evaluate
  `mon-map-cifs-domain->local-mount' at loadtime. This prevents mapping domains
  and mount points when either of the values for `*mon-CIFS-domain*'
  `*mon-CIFS-mount-root*' are not the same for each list element of
  `*mon-CIFS-mount-points*'. :SEE `mon-map-cifs-domain->local-mount' for
  additional details regarding this mapping heuristic:\n
:SEE-ALSO `mon-set-register-tags-loadtime', `mon-bind-iptables-vars-at-loadtime'
`mon-mount-cifs', `mon-unmount-cifs', `mon-get-cifs-mount-points',
`mon-build-cifs-credentials', `mon-get-cifs-credentials',
`mon-verify-cifs-credentials', `mon-inform-cifs-credentials-unbound',
`*mon-CIFS-vars-unbound*'.\n►►►"
  ;; Make sure auth-source is a present feature.
  (unless (featurep 'auth-source) (require 'auth-source))
  ;; Add a new protocol to `auth-source-protocols'
  (setq *mon-CIFS-auth-protocol* '(cifs "cifs"))
  (add-to-list 'auth-source-protocols *mon-CIFS-auth-protocol*)
  ;; Add a value for the local CIFS domain.
  ;; If domain was supplied with leading/trailing "//" or "/" remove them now.
  (if (not no-misc-path)
      (setq *mon-CIFS-domain*
        (upcase 
         (substring 
          (cadr (assoc 'the-shr-prfx *mon-CIFS-misc-path-alist*))
          2 
          (1- (length (cadr (assoc 'the-shr-prfx *mon-CIFS-misc-path-alist*)))))))
      (setq *mon-CIFS-domain* 
            (replace-regexp-in-string "/" "" *mon-CIFS-domain*)))
  ;; (re)bind values for `*mon-CIFS-user*' user and `*mon-CIFS-pass*' if present.
  ;; Else, spit a message the the buffer named by `*mon-CIFS-vars-unbound*'
  (if (and *mon-CIFS-user* *mon-CIFS-pass*)
      (mon-get-cifs-credentials)
      (ignore-errors (mon-inform-cifs-credentials-unbound)))
  ;; Bind value of `*mon-CIFS-mount-root*' to base dir for local CIFS mount point.
  (setq *mon-CIFS-mount-root* 
        (cadr (assoc 'the-mnt-prfx *mon-CIFS-misc-path-alist*)))
  ;; First form binds/rebind `*mon-CIFS-mount-points*', alist mapping local
  ;; mount points to shares. Unless NO-MAP-MAP-POINTS is non-nil, second form
  ;; maps values of `*mon-CIFS-domain*' and `*mon-CIFS-mount-root*' across each
  ;; element of `*mon-CIFS-mount-points*'. :SEE `mon-map-cifs-domain->local-mount'
  (if (not no-misc-path)
      (setq *mon-CIFS-mount-points*  (cadr (assoc 'the-mnt-maps *mon-CIFS-misc-path-alist*)))
      ;; This is tricky, in order for `mon-map-cifs-domain->local-mount' to do
      ;; its job we need to reset the value of `*mon-CIFS-mount-points*' to
      ;; whatever it was first bound to originally, else we double up the domain
      ;; and local mount points.
      (progn
        (setq *mon-CIFS-misc-path-alist*            
              (remove 
               (assoc 'the-mnt-maps *mon-CIFS-misc-path-alist*) 
               *mon-CIFS-misc-path-alist*))
        (push *mon-CIFS-mount-points* *mon-CIFS-misc-path-alist*)
        (setq *mon-CIFS-mount-points*
              (cadr (assoc 'the-mnt-maps *mon-CIFS-misc-path-alist*)))))
  (unless no-map-mount-points
    (setq *mon-CIFS-mount-points* (mon-map-cifs-domain->local-mount))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-14T19:21:38-05:00Z}#{10025} - by MON>
(defvar *mon-auth-source-debug-var* nil
  "*Symbol to store debug messages when `auth-source-debug' is t and bound to
the function `mon-spit-auth-source-debug->var'.\n
Also, does double duty as the name of the buffer name that debug logs are
displayed in.\n
:WARNING Authentication passwords and login information will persist for this
Emacs session depending on the value of `*mon-auth-source-cache-debug-var*'.\n
:SEE-ALSO `auth-source-do-debug', `auth-source-hide-passwords'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-14T22:53:44-05:00Z}#{10024} - by MON>
(defvar *mon-auth-source-cache-debug-var*  nil
"*When non-nil maintain a persistent log of auth-source authentications.
When nil clear `*mon-auth-source-debug-var*' after each invocation of 
`mon-spit-auth-source-debug->var', this is the default value.\n
:SEE-ALSO `auth-source-do-debug',`auth-source-hide-passwords'.\n►►►")

;;; ==============================
;;; :NOTE The local variables `tail-msg' and `fmt-msg' are conditional on the
;;; value of the arg `DEBUG-MSG'. 
;;; The cdr of `DEBUG-MSG' has one three forms:
;;;
;;; (("login" "password") HOST PROTOCOL)    
;;; (("login" "password") "SECRET" HOST PROTOCOL))
;;; (("login" "password") ("LOGIN" "PASSWORD") HOST PROTOCOL)))
;;;
;;; These correpsond to the format string and args for the variable `fmt-msg':
;;;
;;; "auth-source-user-or-password: get %s for %s (%s)" mode host protocol)
;;; auth-source-user-or-password: get (login password) for <HOST> (PROTOCOL)
;;;
;;; "auth-source-user-or-password: cached %s=%s for %s (%s)" mode (list|SECRET) host protocol
;;; auth-source-user-or-password: cached (login password)=(LOGIN PASS) for <HOST> (PROTOCOL) ;(mode)=(clear)
;;; auth-source-user-or-password: cached (login password)=SECRET for <HOST> (PROTOCOL) ;(mode)=SECRET
;;;
;;; "auth-source-user-or-password: found %s=%s for %s (%s)" mode (list|SECRET) host protocol
;;; auth-source-user-or-password: found (login password)=(LOG PASS) for <HOST> (PROTOCOL) ;(mode)=(clear)
;;; auth-source-user-or-password: found (login password)=SECRET for <HOST> (PROTOCOL) ;(mode)=SECRET
;;;
;;; :CREATED <Timestamp: #{2010-01-12T19:37:03-05:00Z}#{10023} - by MON>
(defun mon-spit-auth-source-debug->var (&rest debug-msg)
  "Concat DEBUG-MSG of `auth-source-do-debug' onto `*mon-auth-source-debug-var*'.
Return the buffer named *MON-AUTH-SOURCE-DEBUG-VAR* with details of recent
values returned by `auth-source-user-or-password'.\n
When arg DEBUG-MSG is bogus, signal an error.\n
:WARNING Authentication passwords and login information may persist in variable
`*mon-auth-source-debug-var*' depending on value of `*mon-auth-source-cache-debug-var*'.\n
Also, the return buffer may contain authentication information in the clear when
`auth-source-hide-passwords' is non-nil.\n
When you are finished debugging, make sure to kill the buffer named
*MON-AUTH-SOURCE-DEBUG-VAR* and set *mon-auth-source-debug-var* to nil.\n
:EXAMPLE\n\n\(dotimes \(i 3 \(progn \(setq *mon-auth-source-debug-var*\)
                     \(kill-buffer \"*MON-AUTH-SOURCE-DEBUG-VAR*\"\)\)\)
  \(let \(\(auth-source-debug  'mon-spit-auth-source-debug->var\)
        \(*mon-auth-source-cache-debug-var* t\)\)
    \(mon-spit-auth-source-debug->var
     \"auth-source-user-or-password: found %s=%s for %s \(%s\)\" 
     '\(\"login\" \"password\"\) '\(\"MY-LOGIN\" \"MY-PASSWORD\"\) 'MY-HOST 'HER-PROTOCOL\)
    \(sit-for 1.5\)\)\)\n
:SEE-ALSO `netrc-machine-user-or-password'.\n►►►"
  (unwind-protect
       (if auth-source-debug
           (let ((dbg-buf (get-buffer-create 
                           (upcase (symbol-name '*mon-auth-source-debug-var*))))
                 (sprtr (make-string 30 45))
                 (fmt-msg (car debug-msg))
                 (tail-msg (cdr debug-msg)))
             (setq *mon-auth-source-debug-var*
                   (concat *mon-auth-source-debug-var*
                           (cond (;; case1 mode not cached
                                  ;; (("login" "password") HOST PROTOCOL)
                                  (and (= (length tail-msg) 3) (listp (car tail-msg)))
                                  (apply 'format                
                                         (concat
                                          "GET MODE = %s\n"   ;;GET MODE  = (login password)
                                          "    HOST = %s\n"   ;;    HOST  = <DOMAIN>
                                          "PROTOCOL = %s\n")  ;; PROTOCOL = <PROTOCOL/PORT>
                                         (car    tail-msg) ;; -> (login password)
                                         (cadr  tail-msg)  ;; -> HOST
                                         (cddr tail-msg))) ;; -> PROTOCOL
                                 (;; Don't check `auth-source-hide-passwords's value.
                                  ;; (("login" "password") "SECRET" HOST PROTOCOL)))
                                  (and (= (length tail-msg) 4) (stringp (cadr tail-msg)))
                                  (apply 'format                
                                         (concat
                                          "    MODE = %s\n" ;;    MODE  = (login password)
                                          "   CLEAR = %s\n" ;;   CLEAR  = SECRET
                                          "    HOST = %s\n" ;;    HOST  = <DOMAIN>
                                          "PROTOCOL = %s\n") ;; PROTOCOL = <PROTOCOL/PORT>
                                         (car tail-msg) ;; ("login" "password")
                                         (cadr tail-msg) ;; -> SECRET
                                         (caddr tail-msg) ;; -> HOST       
                                         (cdddr tail-msg))) ;; -> PROTOCOL
                                 (;; Fall through, groking the mode pair in the clear.
                                  ;; (("login" "password") ("LOGIN" "PASSWORD") HOST PROTOCOL)
                                  (= (length tail-msg) 4)
                                  (apply 'format                
                                         (concat
                                          "    MODE = %s\n" ;;    MODE = (login password)
                                          "   MODEL = %s\n" ;;    MODEL = LOGIN
                                          "   MODEP = %s\n" ;;    MODEP = PASSWORD
                                          "  CACHED = %s\n" ;;  CACHED = yes|no
                                          "    HOST = %s\n" ;;     HOST = <DOMAIN>
                                          "PROTOCOL = %s\n") ;; PROTOCOL = <PROTOCOL/PORT>
                                         (car tail-msg) ;; -> ("login" "password")
                                         (caadr tail-msg)  ;; -> "LOGIN"
                                         (cadadr tail-msg) ;; -> "PASSWORD"
                                         (if (string= (substring fmt-msg 30 36) "cached")
                                             "YES" "NO")
                                         (caddr tail-msg)  ;; -> HOST
                                         (cdddr tail-msg))) ;; -> PROTOCOL
                                 (t (error "Something amiss, check format string of `auth-source-user-or-password'")))
                           sprtr "\n\n"))
             (with-current-buffer (buffer-name (get-buffer dbg-buf))
               (erase-buffer)
               (insert *mon-auth-source-debug-var*))
             (display-buffer dbg-buf t))
           (error "auth-source-debug is nil bind it to t first"))
    (unless *mon-auth-source-cache-debug-var*
      (setq *mon-auth-source-debug-var* nil))))

;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (progn
;; |   (auth-source-forget-all-cached)
;; |   (let (;; (auth-source-debug t)
;; |         (auth-source-debug  'mon-spit-auth-source-debug->var)
;; |         ;; (auth-source-hide-passwords t)
;; |         (auth-source-hide-passwords nil)
;; |         ;; (*mon-auth-source-cache-debug-var* t))
;; |         (*mon-auth-source-cache-debug-var* nil))
;; |     (auth-source-user-or-password  '("login" "password")
;; |                                    "<SOME-HOST>"<SOME-PROTOCOL>)))
;; `----


;;; ==============================
;;; :NOTE Now evaluated with `mon-after-mon-utils-loadtime' :SEE :FILE mon-utils.el
;;;
;;; :NOTE Unless the domain and mount-point for each element of
;;; `*mon-CIFS-mount-points*' match the values of `*mon-CIFS-domain*' and
;;; `*mon-CIFS-mount-root*' you should not map the mount points instead comment
;;; out the first form and uncomment the second form below. 
;;; :SEE `mon-map-cifs-domain->local-mount' and `mon-bind-cifs-vars-at-loadtime'

;; (eval-after-load "mon-cifs-utils" '(mon-bind-cifs-vars-at-loadtime))
;; (eval-after-load 'mon-cifs-utils '(mon-bind-cifs-vars-at-loadtime nil t)) 

;; ,---- :UNCOMMENT-BELOW-WHEN-TESTING
;; | (progn 
;; |   (makunbound '*mon-CIFS-domain*) (unintern '*mon-CIFS-domain*) 
;; |   (makunbound '*mon-CIFS-user*) (unintern '*mon-CIFS-user*)
;; |   (makunbound '*mon-CIFS-pass*) (unintern '*mon-CIFS-pass*)
;; |   (makunbound '*mon-CIFS-mount-root*) (unintern '*mon-CIFS-mount-root*)
;; |   (makunbound '*mon-CIFS-vars-unbound*) (unintern '*mon-CIFS-vars-unbound*)
;; |   (makunbound '*mon-CIFS-mount-points*) (unintern '*mon-CIFS-mount-points*)
;; |   (auth-source-forget-all-cached)
;; |   (message "All `*mon-CIFS-*' vars unbound auth-source forgot her cache."))
;; `----

;;; ==============================
(provide 'mon-cifs-utils)
;;; ==============================

;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ================================================================
;;; mon-cifs-utils.el ends here
;;; EOF
