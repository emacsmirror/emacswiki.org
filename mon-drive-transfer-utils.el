;;; mon-drive-transfer-utils.el --- tools for transferring backing up larg harddrives.
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-drive-transfer-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-05-06T21:21:43-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: data, exectue, files, hardware, processes, tools

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-drive-transfer-utils provides cmd-line template for migrating hardrives,
;; moving large amounts of data, and backing up before backing up.
;; See docstring `mon-cln-drive-transfer-template' for discussion.
;;
;; FUNCTIONS:►►►
;; `mon-cln-drive-transfer-template', `mon-insert-drive-transfer-template'
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
;; `*mon-drive-transfer-template*'
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
;; URL: http://www.emacswiki.org/emacs/mon-drive-transfer-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-05-06T21:27:28-04:00Z}#{10184} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-drive-transfer-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-05-06T21:21:43-04:00Z}#{10184} - by MON KEY>
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
;;; :CREATED <Timestamp: #{2010-05-06T19:37:03-04:00Z}#{10184} - by MON>
(defvar *mon-drive-transfer-template* nil
  ":SEE-ALSO `mon-insert-drive-transfer-template',`mon-cln-drive-transfer-template'.\n►►►")
;;
(unless (bound-and-true-p *mon-drive-transfer-template*)
  (setq *mon-drive-transfer-template*
        (mapconcat 'identity
         '(";;; <FROM-MOUNT-POINT> -> <TO-MOUNT-POINT>"
          "script <TRANSFER-NOTES-DIR>/TRANSFER-LOG-<DATE>"
          "mount <TO-MOUNT-POINT>"
          "cd <FROM-MOUNT-POINT>"
          "du -h --max-depth=1 <FROM-MOUNT-POINT>"
          "ls -laR | grep -v ^[.lt] | grep -v ^$ | wc -l"
          "ls -laR | grep -v ^[.dlt] | grep -v ^$ | wc -l"
          ";;; :MOVED"
          " (cd <FROM-MOUNT-POINT> && tar Scpf - .) | (cd <TO-MOUNT-POINT> && tar Sxvpf - )"
          "du -h --max-depth=1 <TO-MOUNT-POINT>"
          "ls -laR | grep -v ^[.lt] | grep -v ^$ | wc -l"
          "ls -laR | grep -v ^[.dlt] | grep -v ^$ | wc -l"
          "umount <TO-MOUNT-POINT>"
          "ls -alh <TO-MOUNT-POINT>"
          "mount <TO-MOUNT-POINT>"
          "ls -alh <TO-MOUNT-POINT>"
          "rm-force -r <FROM-MOUNT-POINT>"
          "exit"
          "chown -R <USER-ID>:<GROUP-ID> <TRANSFER-NOTES-DIR>"
          "cp <TRANSFER-NOTES-DIR> <TO-MOUNT-POINT>"
          "chown -R <USER-ID>:<GROUP-ID> <TO-MOUNT-POINT>") "\n\n")))
;; 
;;; :TEST-ME *mon-drive-transfer-template*
;;;(progn (makunbound '*mon-drive-transfer-template*) (unintern '*mon-drive-transfer-template*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-06T19:40:28-04:00Z}#{10184} - by MON>
(defun mon-insert-drive-transfer-template (&optional clnd-template)
  "Insert the contents of variable `*mon-drive-transfer-template*'.\n
Does not move point.\n
When optional arg CLND-TEMPLATE is non-nil it is a string as generated by
`mon-insert-drive-transfer-template'.\n
:SEE `mon-cln-drive-transfer-template' for usage.\n►►►
:SEE-ALSO .\n►►►"
      (save-excursion 
        (newline)
        (if clnd-template
            (insert clnd-template)
            (insert *mon-drive-transfer-template*))))
;;
;;; :TEST-ME (mon-insert-drive-transfer-template)


        
;;; ==============================
;;; :NOTE We currently use the regexps to frob variables. Could likewise build a
;;; string up w/ format and parameters... this approach is what happens when I
;;; have to grok POSIX & cmd-line :(  -- Worse is worse - I'm loosing big!!!
;;; :CREATED <Timestamp: #{2010-05-06T19:12:51-04:00Z}#{10184} - by MON>
(defun mon-cln-drive-transfer-template (from-moint-point to-moint-point transfer-note-dir 
                                                   &optional to-mount-point-mnt user group)
  "Insert a cmd-line template for performing manual hard-drive backup/transfer.\n
FROM-MOINT-POINT   -- Where to move the files/dirs from. Strip trailing slash.
TO-MOINT-POINT     -- Where to move the files/dirs to. Strip trailing slash.
TRANSFER-NOTE-DIR  -- Where to save the TRANSFER-LOG. Strip trailing slash.
TO-MOUNT-POINT-MNT -- If TO-MOINT-POINT is a dir this is its mount point.
USER               -- chown username for TO-MOINT-POINT and TRANSFER-NOTE-DIR.
GROUP              -- chown groupname for TO-MOINT-POINT and TRANSFER-NOTE-DIR.\n
See value of `*mon-drive-transfer-template*', and below for discussion.\n
:EXAMPLE\n\n\(with-current-buffer 
    \(get-buffer-create \"*MON-CLN-DRIVE-TRANSFER-TEMPLATE-EXAMPLE*\"\)
  \(erase-buffer\)
  \(mon-insert-drive-transfer-template\)
  \(display-buffer \(current-buffer\) t\)\)\n
\(with-current-buffer 
    \(get-buffer-create \"*MON-CLN-DRIVE-TRANSFER-TEMPLATE-EXAMPLE*\"\)
  \(erase-buffer\)
  \(mon-cln-drive-transfer-template  
   \"/mnt/frm-this-drv\" 
   \"/mnt/to-this-other-drv/subdir\"
   \"~/log-this-to-here\" 
   \"/mnt/to-this-other-drv\" 
   \"user-bubba\" \"bubba-group\"\)
  \(display-buffer \(current-buffer\) t\)\)\n\n
;;; :DISCUSSION\n
A Template for interactively moving large directory trees drives and mount points.

Template is meant to be invoked manually as root. It is generally not safe to
execute these types of commands in a scripted environment when backups are not
available. Moreover, the following types of routines are those typically used to
initiate an initial backup/filesystem transfer when moving data across non-like
filesystems, partitions, drives, etc. Once such a transition occurs it is
obviously easier, safer, more robust to use a tool like rsync for subsequent
operations. Indeed, rsync may be implemented to accomplish the intial transfer
as well. This said, there are situations where rsync may not be desired
esp. w/re some subtle errors and/or bit-rot which may arise or be introduced
when moving across file system types with different internal representations of
charset, filename length, etc. For such situations `tar' while more conservative
may be more appropriate, hence this template :\)

Each delimited `<VARIABLE>' below is replaced by the equivalent values of the
parameter args enumerated.  These template variables appear unmodified in the
`*mon-drive-transfer-template*' and may be inserted for manual
editing/examination with `mon-insert-drive-transfer-template'.

<FROM-MOUNT-POINT> -- The mounted directory or subdir to move;

<TO-MOUNT-POINT>   -- A mounted directory or subdir to move <FROM-MOUNT-POINT> into; 

<TRANSFER-NOTES-DIR> -- A directory with files related to the transfer 
   			These include files named:

                        --- <FROM-MOUNT-POINT>-transfer-notes
    		        A record of shell commands used to execute the transfer

                        --- TRANSFER-LOG-<TRANSFER-DATE>
                            A file name to store the output of `script'
                            command.  This file will contain a list the
                            commands executed along with a complete list of
                            all files moved into <TO-MOUNT-POINT> and is
                            useful for error checking an future replay.

<USER-ID>:<GROUP-ID> -- The uid and gid to `chown' <TRANSFER-NOTES-DIR> and
                        <TO-MOUNT-POINT> upon completion;

We executute the following as root user.

- The `script' command records the output;

- The `mount' command ensures that to mount point is mounted. Most likely this
  is redundant _HOWEVER_ if we don't do this then we wind up moving all the
  files into a directory, the contents of which won't appear when the drive is
  mounted. This happened to me once and it is a real PITA b/c it requires
  re-executing the entire Nn GB transfer again... This prevents that from
  happening. 
  :NOTE if <TO-MOUNT-POINT> is a subdirectory of a mount point you will need
  to adjust the `mount'/`unmount' commnads accordingly.

- The `du' command is executed twice once on the <FROM-MOUNT-POINT> side, and
  once on the <TO-MOUNT-POINT> side. Assuming everthing transferred correctly
  these should match. (:NOTE :SEE email correspondence w/re why this doesn't
  actually happen.)

- The `ls' piped command is executed twice: 

   -- once to get the file _and_ directory count;

   -- once to get teh file count _without_ directories;

  This happens on both the <FROM-MOUNT-POINT> and <TO-MOUNT-POINT> sides. 
  Used to verify that all files/dirs transferred correctly. 
  :NOTE the ext4 file system has an extra directory `lost+found' so the the
  counts will always differ by +/- 3 when the <FROM-MOUNT-POINT> was from a
  filesystem that doesn't utilize this feature, e.g. VFAT.

- The `tar' command is piped per file/dir moving these `sparse' format
  preserving permissions etc. the long form is as follows:

    tar --sparse --create --gzip --preserve-permissions --file

  :NOTE on the output side of the pipe the --verbose flag is provided this
  allows the `script' command to record the transfer on a per file/dir basis.

  To move the entire directory as an archive do:

   (cd <FROM-MOUNT-POINT> && tar Scpvf <TO-MOUNT-POINT><TAR-FILE-NAME>.tar .)

  :NOTE In this configuration we add the --verbose flag on the
  <FROM-MOUNT-POINT> side as there is no per file/dir decompression to
  <TO-MOUNT-POINT> side instead we move everthing to a single tar file.  

  Also, note that tar doesn't reliably handle _BIG_ fils and may choke in
  situations where (> (total-size <FROM-MOUNT-POINT>) 8GB)

- The sequential invocation of `umount' and `ls' followed by `mount' and `ls' is
  made as a second verification that <TO-MOUNT-POINT> was mounted before the
  transfer (see above).

- The `rm-force -R' is executed to clean out the contents of <FROM-MOUNT-POINT>
  We alias `rm' in ~/.bashrc to `rm -i' to prevent accidents esp. recursive
  removal during interactive shell use. When we know we want to rm-force we use
  that.  Following is from our ~/.bashrc and /root/.bashrc files:

  ,----
  | alias rm=\"rm -i\"
  | alias mv=\"mv -i\"
  | alias mv-force=\"mv -f\"
  | alias rm-force=\"rm -f\"
  `----

  So, assuming everything is kosher the above will recursively remove the
   contents of <FROM-MOUNT-POINT>. There is no good way to \"undo\" such a thing
   You should be careful to verify that this is indeed what is wanted
   _before_ clobbering a filesystem esp. as we're in our backup routine!!!

- The `exit' command stops `script' and writes a session transcript to the file:
  <TRANSFER-NOTES-DIR>/TRANSFER-LOG-<DATE>

- The `chown' command on <TRANSFER-NOTES-DIR> is invoked because we are root
  (or running w/ root priveleges). Set <USER-ID>:<GROUP-ID> as needed.

- The `cp' moves a log of our transfer into <TO-MOUNT-POINT> for future reference.

- The second chown sets the permissions of all files/dir in <TO-MOUNT-POINT> to
  something sensible.

- Now we're done.

 \"Le Roi est mort, vive le Roi!\"\n \"Ext3 is dead, all hail the Ext4!\"\n

:SEE-ALSO `*mon-drive-transfer-template*', `mon-cln-drive-transfer-template',
`mon-insert-drive-transfer-template'.\n►►►"
  (let (;; :NOTE Using marker in case we decide not to dump ;;
        ;; `*mon-drive-transfer-template*' to temp-buffer i.e. adding an INTRP arg
        ;; further up.
        (pre-frm-to (make-marker)) 
        ;; Where to move the files/dirs from. :NOTE Omit trailing slash.
        (mctt-fmp (directory-file-name (file-name-as-directory from-moint-point)))
        ;; Where to move the files/dirs to. Strip trailing slash.
        (mctt-2mp (directory-file-name (file-name-as-directory to-moint-point)))
        ;; If TO-MOINT-POINT is a dir this is its mount point.
        (mctt-2mpm (directory-file-name (file-name-as-directory to-mount-point-mnt)))
        ;; Where to save the TRANSFER-LOG. :NOTE Omit trailing slash.      
        (mctt-tnd (directory-file-name (file-name-as-directory transfer-note-dir))))
    ;; user group
    (with-temp-buffer 
      (set-marker pre-frm-to (point))
      (mon-insert-drive-transfer-template)
      (when to-mount-point-mnt
        (if (string-match-p (substring mctt-2mp 0 (length mctt-2mpm)) mctt-2mpm)
            (progn (search-forward-regexp "^mount <TO-MOUNT-POINT>" nil t)
                   (replace-match (concat "mount " mctt-2mpm) t)
                   (goto-char pre-frm-to))
          (error (concat ":FUNCTION `mon-cln-drive-transfer-template'"
                         "-- arg TO-MOUNT-POINT not a subdir of mount moint TO-MOUNT-POINT-MNT"))))
      (while (search-forward-regexp "<FROM-MOUNT-POINT>" nil t)
        (replace-match mctt-fmp)) 
      (goto-char pre-frm-to)
      (while (search-forward-regexp "<TO-MOUNT-POINT>" nil t)
        (replace-match mctt-2mp t)) 
      (goto-char pre-frm-to)
      (while (search-forward-regexp "<TRANSFER-NOTES-DIR>" nil t)
        (replace-match mctt-tnd t)) 
      (goto-char pre-frm-to)
      (while (search-forward-regexp "<DATE>" nil t)
        (replace-match (mon-date-stamp :as-string t)))
      (goto-char pre-frm-to)
      (when group
        (while (search-forward-regexp "<GROUP-ID>" nil t)
          (replace-match group t))
        (goto-char pre-frm-to))
      (when user
        (while (search-forward-regexp "<USER-ID>" nil t)
          (replace-match user t))
        (goto-char pre-frm-to))
      ;; Done w/ marker pre-frm-to, put bffr-str on itfor `mon-insert-drive-transfer-template'.
      (setq pre-frm-to (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (mon-insert-drive-transfer-template pre-frm-to)))
;;
;;; :TEST-ME (mon-cln-drive-transfer-template  "/mnt/frm-this-drv" "/mnt/to-this-other-drv/subdir"
;;;                            "~/log-this-to-here" "/mnt/to-this-other-drv" 
;;;                            "user-bubba" "bubba-group")

;;; ==============================
(provide 'mon-drive-transfer-utils)
;;; ==============================

;;; ====================================================================
;;; mon-drive-transfer-utils.el ends here
;;; EOF
