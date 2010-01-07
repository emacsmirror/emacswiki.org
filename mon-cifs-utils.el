;;; this is mon-cifs-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-cifs-utils provides utilities for mounting domain using auth-source to
;;; access CIFS (Cats in Fat Suits) AKA Common Internet File System protocol,
;;; e.g. successor to the SMB (Server Message Block) protocol and the Samba.
;;; :SEE (man "mount.cifs") :SEE (man "umount.cifs") :SEE (man "samba")
;;;
;;; FUNCTIONS:►►►
;;; `mon-map-cifs-domain->local-mount', `mon-get-cifs-credentials'
;;; `mon-build-cifs-credentials', `mon-get-cifs-mount-points'
;;; `mon-mount-cifs'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*mon-CIFS-mount-points*', `*mon-CIFS-auth-protocol*'
;;; `*mon-CIFS-domain*', `*mon-CIFS-pass*'
;;; `*mon-CIFS-user*', `*mon-CIFS-mount-root*'
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
;;; The function `mon-mount-cifs' invokes 'mount -t cifs {...}' via sudo. 
;;; Change the shell command concatenations in that file if that is not what you
;;; want.  mount.cifs command may be used only by root, unless installed
;;; setuid, in which case the noeexec and nosuid mount flags are enabled.
;;; You may need to adjust your /etc/fstab accordingly.
;;; :SEE (man "fstab")
;;; :SEE (man "getmntent")
;;;
;;; Below MON uses the `IS-MON-P' conditional from:
;;; :FILE mon-default-loads.el to test the current system type.
;;; FTMP, IS-MON-P-GNU =: (eq system-type 'gnu/linux) 
;;; However, MON has various association keys ferreted away in the
;;; :VARIABLE `*mon-misc-path-alist*' these are only evaluated per the above
;;; conditional. IOW, _you_ should provide or bind values for the following
;;; `*mon-CIFS-domain*', `*mon-CIFS-mount-root*', `*mon-CIFS-mount-points*'
;;; as defined herein and disregard all the eval-when IS-MON-P rigmarole.
;;; And no, MON won't be putting those local variable in a defcustom
;;; form... code relying on the auth-source facility has no business
;;; _whatsoever_ interacting with defcustom's dark voodo.
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;; auth-source
;;;
;;; THIRD-PARTY-CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-cifs-utils.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2010-01-05T21:41:27-05:00Z}#{10013} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2010-01-05T21:33:06-05:00Z}#{10012} - by MON>
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
;;; Copyright © 2010 MON KEY 
;;; ==============================
;;; CODE:

(eval-when-compile (require 'cl))

;; :NOTE See header.
(eval-when-compile
  (unless (bound-and-true-p IS-MON-P)
    (setq IS-MON-P nil)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T16:26:10-05:00Z}#{10012} - by MON>
(defvar *mon-CIFS-auth-protocol* nil
  "*A protocol list of CIFS elements for `auth-source-protocols'.\n
List has the form '\(key str-val str-num ... ) where key is the symbol `cifs'
and value is a string nameing a protocol or port number apropos your site local
CIFS configuration.\n
:SEE-ALSO  `mon-get-cifs-credentials', `*mon-CIFS-domain*',
`*mon-CIFS-user*', `*mon-CIFS-pass*'.\n►►►")
;;
(eval-when (compile load)
  (unless (bound-and-true-p *mon-CIFS-auth-protocol*)
    (setq *mon-CIFS-auth-protocol* '(cifs "cifs")))
  (require 'auth-source)
  (add-to-list 'auth-source-protocols *mon-CIFS-auth-protocol*))
;;
;;; :TEST-ME (assoc 'cifs auth-source-protocols)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T16:22:38-05:00Z}#{10012} - by MON>
(defvar *mon-CIFS-domain* nil
  "*CIFS domain to mount with `*mon-CIFS-user*' using `*mon-CIFS-pass*'.
This is the the CIFS `workgroup' or `domain' you wish to connect with.
:SEE \(man \"mount.cifs\")\n:SEE \(man \"samba\"\)
:SEE `*mon-CIFS-mount-points*' for additional discussion.
:SEE-ALSO `mon-get-cifs-credentials',`*mon-CIFS-auth-protocol*'.\n►►►")
;;
(eval-when-compile
  (if IS-MON-P
      (unless (bound-and-true-p *mon-CIFS-domain*)
        (setq *mon-CIFS-domain*
              (upcase (substring 
                       (cadr (assoc 'the-shr-prfx *mon-misc-path-alist*))
                       2 (1- (length (cadr (assoc 'the-shr-prfx *mon-misc-path-alist*))))))))))
;;      
(defvar *mon-CIFS-user* nil
  "*CIFS username to access `*mon-CIFS-domain*' using `*mon-CIFS-pass*'.\n
:SEE-ALSO `*mon-CIFS-pass*', `mon-get-cifs-credentials',
`*mon-CIFS-auth-protocol*'.\n►►►")
;;
(defvar *mon-CIFS-pass* nil
  "*CIFS password of `*mon-CIFS-user*' for accessing `*mon-CIFS-domain*'.\n
:SEE-ALSO `mon-get-cifs-credentials',`*mon-CIFS-auth-protocol*'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T12:52:36-05:00Z}#{10012} - by MON KEY>
(defvar *mon-CIFS-mount-root* nil
"*Local mount point for networked SMBFS/CIFS shares.\n
:SEE-ALSO `*mon-CIFS-domain*',`*mon-CIFS-user*',`*mon-CIFS-pass*'.\n►►►")
(eval-when-compile
  (if IS-MON-P
      (unless (bound-and-true-p *mon-CIFS-mount-root*)
        (setq *mon-CIFS-mount-root*
              (cadr (assoc 'the-mnt-prfx *mon-misc-path-alist*))))))
;;
;;; :TEST-ME *mon-CIFS-mount-root*
;;;(progn (makunbound '*mon-CIFS-mount-root*) (unintern '*mon-CIFS-mount-root*) )

;;; ==============================
;;; :NOTE CIFS paths with spaces need escapes with `\40' so double the `\' here.
;;;       For example, `Some Spacey Path' -> `Some\\40Spacey\\40Path'
;;; :CREATED <Timestamp: #{2010-01-05T12:54:30-05:00Z}#{10012} - by MON KEY>
(eval-when-compile
(defvar *mon-CIFS-mount-points* nil
  "*An alist mapping local mount points to networked shares.
Alist entries have one of the following the forms:\n
 \(symbol \"//<CIFS-DOMAIN-IP>/<SOME-CIFS-SHARE>\" \"/LOCAL/MOUNT/<MNT-POINT>\"\)\n
 \(symbol \"//<CIFS-DOMAIN-NAME>/<SOME-CIFS-SHARE>\" \"/LOCAL/MOUNT/<MNT-POINT>\"\)\n
Where //<CIFS-DOMAIN-NAME> is the domain or workgroup and <SOME-CIFS-SHARE> is
one of its shares and where /LOCAL/MOUNT/ is the local systems common mount
directory e.g. /mnt, /media, etc. and <MNT-POINT> is a directory beneath.  :NOTE
The first two forms above are not needed, and indeed should not be used, where
each of your shares are located beneath `*mon-CIFS-domain*' and where each of
your local mount points are beneath `*mon-CIFS-mount-root*' as, by default,
these are programmatically updated with `mon-map-cifs-domain->local-mount'.
Instead, you should provide alist entries with the `//CIFS-DOMAIN-*' and root
`/LOCAL/MOUNT/' portions omitted by using the form:\n
 \(symbol \"<SOME-CIFS-SHARE>\" \"MNT-POINT\"\)\n
A more detailed example of the preferred format can be seen evaluating and
examining the output of the the example code provided after the `:EXAMPLE'
section of that function's docstring.\n
:NOTE To find CIFS shares and/or test access to a domain do as follows:\n
 shell> smbclient -L <DOMAIN> -W windom -U <USERNAME>
        [Provide password when prompted]\n
:SEE \(man \"samba\"\)\n:SEE \(man \"fstab\"\)\n:SEE \(man \"getmntent\"\)\n
:SEE-ALSO `mon-get-cifs-credentials', `mon-build-cifs-credentials', 
`mon-get-cifs-mount-points', `mon-mount-cifs', `*mon-CIFS-mount-points*', 
`*mon-CIFS-auth-protocol*',`*mon-CIFS-domain*', `*mon-CIFS-pass*', 
`*mon-CIFS-user*', `*mon-CIFS-mount-root*'.\n►►►")
;;
(if IS-MON-P
    (unless (bound-and-true-p *mon-CIFS-mount-points*)
      (setq *mon-CIFS-mount-points*
            (cadr (assoc 'the-mnt-maps *mon-misc-path-alist*))))))
;;
;;; :TEST-ME *mon-CIFS-mount-points*
;;; :TEST-ME (cdr (assoc 'HG-Repos *mon-CIFS-mount-points*))
;;
;;;(progn (makunbound '*mon-CIFS-mount-points*) (unintern '*mon-CIFS-mount-points*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T19:13:39-05:00Z}#{10012} - by MON KEY>
(eval-when-compile
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
`mon-get-cifs-mount-points', `mon-mount-cifs' `*mon-CIFS-auth-protocol*',
`*mon-CIFS-pass*', `*mon-CIFS-user*'.\n►►►"
  (let (map-mnt )
    (dolist (i (or mount-point-list *mon-CIFS-mount-points*) (nreverse map-mnt))
      (push `(,(car i) 
               ,(concat "//" (or cifs-domain *mon-CIFS-domain*) "/" (cadr i)) ;;(caddr i)))
               ,(concat (or local-mount-point  *mon-CIFS-mount-root*) "/" (caddr i))) ;;(cadr i))
            map-mnt))))
) ;; :CLOSE  eval-when
;;
;;; :TEST-ME (mon-map-cifs-domain->local-mount)

;;; ==============================
;;; :NOTE See docstring of `mon-map-cifs-domain->local-mount'.  If standard 
;;;       arrangmenet is is not what you want or doesn't reflect your needs then
;;;       hand code a list for _ALL_ of the paths bound by the variable
;;;       `*mon-CIFS-mount-points*' and comment out the binding below.
;;;       If you do this, be sure to include the "//CIFS-DOMAIN/" and
;;;       "/LOCAL/MOUNT/POINT" for each element in the `*mon-CIFS-mount-points*'
;;;       list.
;;(eval-when (load)
(eval-when-compile
  (setq *mon-CIFS-mount-points* (mon-map-cifs-domain->local-mount)))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS `anything-delicious.el' 
;;; :WAS `anything-delicious-authentify'
;;; :NOTE When debugging set: 
;;;      (setq auth-source-debug nil) ;nil
;;;      (setq auth-source-hide-passwords t) ;t 
;;; :CREATED <Timestamp: #{2010-01-05T16:21:52-05:00Z}#{10012} - by MON>
(defun mon-get-cifs-credentials ()
  "Return and set CIFS user and password values.
For use as authentication values when accessing/mounting `*mon-CIFS-domain*'
using the local ~/.authinfo.gpg file instead of a dedicated credentials file.
Sets the value of variables `*mon-CIFS-user*' and  `*mon-CIFS-pass*'.\n
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
 machine <SOME-MACHINE> port cifs login <SOME-LOGIN> password <SOME-PASS>
IOW, to make a CIFS credentials format congruent with  ~/.authinfo.gpg you would:
 o Add a protocol entry using `port cifs';
 o Substitute `domain' with `machine';
 o Substitute `username' with `login';\n
:NOTE Depending on the current netbios config `domain' is prob. the `workgroup'.
:SEE \(man \"mount.cifs\"\) for additional details\n
:SEE-ALSO `auth-source-user-or-password',`mon-map-cifs-domain->local-mount',
`mon-build-cifs-credentials', `mon-get-cifs-mount-points', `mon-mount-cifs'
`*mon-CIFS-mount-points*', `*mon-CIFS-auth-protocol*',`*mon-CIFS-mount-root*'.
►►►"
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

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-05T13:55:59-05:00Z}#{10012} - by MON KEY>
(defun mon-build-cifs-credentials (&optional credentials-file)
  "Return a credentials string for mounting CIFS.
By default concatenates the credentials values as per CIFS specs.
:VARIABLES `*mon-CIFS-user*', `*mon-CIFS-domain*', `*mon-CIFS-pass*' 
return value has the form:\n
domain=<DOMAIN>,username=<USERNAME>,password=<PASSWORD>\n
When optional arg CREDENTIALS-FILE is non-nil returns:\n
 credentials=CREDENTIALS-FILE\n
:SEE \(man \"mount.cifs\"\) for additional specifications.\n
:SEE-ALSO `mon-map-cifs-domain->local-mount', `mon-build-cifs-credentials',
`mon-get-cifs-mount-points', `mon-mount-cifs',`*mon-CIFS-mount-points*',
`*mon-CIFS-auth-protocol*', `*mon-CIFS-mount-root*'\n►►►"
  (let (creds)
    (if (and (not (null credentials-file))
             (file-exists-p credentials-file))
        (setq creds  (concat "credentials=" credentials-file))
        ;; Mapping -- use `\n' not `,' if we decide to spit string to file instead.
        (setq creds (mapconcat 
                     #'(lambda (p) (concat (car p) (cdr p)))
                     (pairlis '("domain=" "username=" "password=")
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
:CALLED-BY `mon-mount-cifs'\n
:SEE-ALSO `mon-map-cifs-domain->local-mount', `mon-get-cifs-credentials', 
`mon-build-cifs-credentials',`*mon-CIFS-mount-points*',`*mon-CIFS-auth-protocol*',
`*mon-CIFS-domain*',`*mon-CIFS-pass*',`*mon-CIFS-user*', `*mon-CIFS-mount-root*'.
►►►"
  (interactive "p")
  (let (;;(mnt-root *mon-CIFS-mount-root*)
        (mnt-pnts *mon-CIFS-mount-points*))
    (setq mnt-pnts 
          (assoc 
           (if (or intrp (null mount-point))
               (car (read-from-string 
                     (completing-read "Which mount point (TAB completes) :"
                                      mnt-pnts
                                      nil t)))
               mount-point)
           mnt-pnts))))
;;
;;; :NOTE Tests assume req'd vars are bound already.
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
                        \(directory-file-name default-directory\) t\)\)\)
  \(setq mn-str 
        \(format 
         \(concat 
          \"\\nSample MOUNT for shell-command:\\n\\n shell> %s\\n\\n\"
          \"Sample UMOUNT string for shell-command:\\n\\n  shell> %s\\n\\n\"
          \"Sample MOUNT string using credntials file:\\n\\n  shell> %s\\n\\n\"\)
         mn-str um-str mn-str-creds\)\)
  \(momentary-string-display mn-str \(line-beginning-position 2\)\)\)\n
:SEE \(man \"mount.cifs\"\)\n:SEE \(man \"mount\")
:SEE \(man \"umount.cifs\"\)\n:SEE \(man \"samba\"\)\n:SEE \(man \"sudo\"\)\n
:SEE-ALSO `mon-map-cifs-domain->local-mount', `mon-get-cifs-credentials'
`mon-build-cifs-credentials', `mon-get-cifs-mount-points', `mon-mount-cifs'
`*mon-CIFS-mount-points*', `*mon-CIFS-auth-protocol*',`*mon-CIFS-domain*',
`*mon-CIFS-pass*',`*mon-CIFS-user*',`*mon-CIFS-mount-root*',
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
(provide 'mon-cifs-utils)
;;; ==============================

;;; ================================================================
;;; mon-cifs-utils.el ends here
;;; EOF
