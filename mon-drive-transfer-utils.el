;;; mon-drive-transfer-utils.el --- tools for transferring backing up large harddrives.
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
;; See docstring `mon-drive-transfer-template-cln-all' for discussion.
;;
;; FUNCTIONS:►►►
;; `mon-drive-transfer-template-cln-all', `mon-insert-drive-transfer-template',
;; `mon-drive-transfer-template-cln-log-dest',
;; `mon-drive-transfer-template-subst-src-dest-log',
;; `mon-drive-transfer-template-TEST',
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
;; `*mon-drive-transfer-template*',
;; `*mon-drive-transfer-template-src-dest-log*',
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
;;; :CHANGESET 1732
;;; :CREATED <Timestamp: #{2010-05-15T08:32:52-04:00Z}#{10196} - by MON KEY>
(defvar *mon-drive-transfer-template-src-dest-log* nil
  "*A subcomponent of the template returned by `*mon-drive-transfer-template*'.
This holds the `ls' and `grep' templates for deriving file and directory counts in 
<FROM-MOUNT-POINT> and <TO-MOUNT-POINT>.\n
:NOTE Template defaults to <*SRC-LOG> text variables.
      The <*DEST-LOG> variables are substititued in with the function
      `mon-drive-transfer-template-subst-src-dest-log'.\n
:NOTE The `printf' statments have newlines escaped for
      `mon-insert-drive-transfer-template' which invokes `insert'.\n
:SEE-ALSO .\n►►►")
;;
(unless (bound-and-true-p *mon-drive-transfer-template-src-dest-log*)
  (setq *mon-drive-transfer-template-src-dest-log*
        (mapconcat 
         'identity
         `(
           ;; :NOTE The `du' can take an `--exclude-from' to filter "^lost+found$". Though, 
           ;; it isn't safe to default to this behaviour unless we know that filesystem of
           ;; <FROM-MOUNT-POINT> differs from <TO-MOUNT-POINT> such that <TO-MOUNT-POINT>
           ;; contains a top-level lost+found directory where <FROM-MOUNT-POINT> doesn't,
           ;; e.g. a transfer such as vfat -> ext4 prob. should filter whereas the inverse
           ;; ext4 -> vfat shouldn't...  The point is, if you get mismatches +/-3 in your
           ;; file/dir counts these types of top-level dirs are a good place to look when
           ;; reconciling differences.
           ,(concat "\ndu --human-readable --one-file-system --max-depth=1 <FROM-MOUNT-POINT> "
                    "| tee <TRANSFER-NOTES-DIR>/<DU-SRC-LOG>")

           ;; Don't echo the ls output to transcript we only care about the grepd file/dir counts.
           ;; Read the ls logfile if that is what is wanted :)
           "ls -laR <FROM-MOUNT-POINT>  > <TRANSFER-NOTES-DIR>/<SRC-LOG>"  

           ;; Record the grep command used to generate the counts into the log-file
           ,(concat
             "printf \"\\n### grep --invert-match ^[.lt]  <TRANSFER-NOTES-DIR>/<SRC-LOG> "
             "| grep --invert-match ^$ | wc -l \\n\""
             " >> <TRANSFER-NOTES-DIR>/<SRC-LOG>")
           ;; Now execute teh command and echo the output to log file. 
           ;; :NOTE We use `tee' to ensure that the transcript also catches this output.  
           ,(concat
             "grep --invert-match ^[.lt]  <TRANSFER-NOTES-DIR>/<SRC-LOG> "
             "| grep --invert-match ^$ | wc -l "
             "| tee --append <TRANSFER-NOTES-DIR>/<SRC-LOG>")
           ;; This is nearly identical to above except we invert matches for
           ;; directories as well, e.g. `d'.
           ,(concat
             "printf \"\\n###grep --invert-match ^[.dlt]  <TRANSFER-NOTES-DIR>/<SRC-LOG> "
             "| grep --invert-match ^$ | wc -l \\n\""
             " >> <TRANSFER-NOTES-DIR>/<SRC-LOG>")
           ,(concat
             "grep --invert-match ^[.dlt]  <TRANSFER-NOTES-DIR>/<SRC-LOG> "
             "| grep --invert-match ^$ | wc -l "
             "| tee --append <TRANSFER-NOTES-DIR>/<SRC-LOG>"))
         "\n\n")) )
;;
;;; :TEST-ME *mon-drive-transfer-template-src-dest-log*
;;
;;;(progn (makunbound '*mon-drive-transfer-template-src-dest-log*)
;;;       (unintern    '*mon-drive-transfer-template-src-dest-log*) )

;;; ==============================
;;; :CHANGESET 1732
;;; :CREATED <Timestamp: #{2010-05-15T08:37:41-04:00Z}#{10196} - by MON KEY>
(defun mon-drive-transfer-template-subst-src-dest-log (&optional subst-dest)
  "Return value of `*mon-drive-transfer-template-src-dest-log*' variable.\n
When optional arg SUBST-DEST is non-nil substitute all occurences of 
<SRC-LOG> with <DEST-LOG>.
:EXAMPLE\n\n\(mon-drive-transfer-template-subst-src-dest-log\)\n
\(mon-drive-transfer-template-subst-src-dest-log t\)\n
:SEE-ALSO `*mon-drive-transfer-template*'.\n►►►"
  (let (mdttssdl)
    (setq mdttssdl *mon-drive-transfer-template-src-dest-log*)
    (when subst-dest
      (setq mdttssdl
            (replace-regexp-in-string "SRC-LOG" "DEST-LOG" mdttssdl t))
      (setq mdttssdl
            (replace-regexp-in-string "<FROM-MOUNT-POINT>" "<TO-MOUNT-POINT>" mdttssdl t)))
    mdttssdl))
;;
;;; :TEST-ME (mon-drive-transfer-template-subst-src-dest-log)
;;; :TEST-ME (mon-drive-transfer-template-subst-src-dest-log t)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-06T19:37:03-04:00Z}#{10184} - by MON>
(defvar *mon-drive-transfer-template* nil
  "*A Template for interactively moving large directory trees drives and mount points.\n
:NOTE Potentially destructive commands are prefixed with by \"#\" at BOL.\n
:SEE Invocation example of `mon-insert-drive-transfer-template' in docstring of
`mon-drive-transfer-template-cln-all' for usage.\n
:SEE-ALSO `*mon-drive-transfer-template-src-dest-log*',
`mon-drive-transfer-template-subst-src-dest-log',
`mon-drive-transfer-template-cln-log-dest', 
`mon-drive-transfer-template-TEST'.\n►►►")
;;
(unless (bound-and-true-p *mon-drive-transfer-template*)
  (setq *mon-drive-transfer-template*
        (mapconcat 'identity
         `("# :MOVED <FROM-MOUNT-POINT> -> <TO-MOUNT-POINT>"
           "script <TRANSFER-NOTES-DIR>/TRANSFER-LOG-<DATE>"
           
           ;; Make sure to mount is mounted and record the fact to transcript.
           "mount <TO-MOUNT-POINT>"

           ;; Make sure we start in the SRC directory.
           "cd <FROM-MOUNT-POINT>"
           
           ;; Begin the du, ls, and grep templates for SRC-LOG
           ,(mon-drive-transfer-template-subst-src-dest-log)

           ;; Record that this is the command we used to perform the transfer.
           "# :MOVED-WITH"

           ;; :NOTE Following `tar' command could just as well have been:
           ;;       tar -C <FROM-MOUNT-POINT> -Scpf - . | tar -C <TO-MOUNT-POINT> -Sxvpf - 
           ;;
           ;; Which FWIW info says keeps us from using a new subshell. :SEE (info "(tar)Applications")
           ;; However, following has a bit more clarity and the use of `-' and
           ;; `.' seem error prone if elided or misapplied. Wrapping everthing
           ;; in a paren'd block might help guard against that.
           "# (cd <FROM-MOUNT-POINT> && tar Scpf - .) | (cd <TO-MOUNT-POINT> && tar Sxvpf - )"

           ;; The cd to DEST isn't really needed when scripted but it can be
           ;; helpful to be there when running these commands interactively.
           "cd <TO-MOUNT-POINT>"

           ;; Begin the du, ls, and grep templates for SRC-LOG
           ,(mon-drive-transfer-template-subst-src-dest-log t)
           
           ;; Make sure we aren't in DEST's mount point before attempting to `unmount'
           "cd <TRANSFER-NOTES-DIR>"
           "umount <TO-MOUNT-POINT>"
           "ls -alh <TO-MOUNT-POINT>"
           "mount <TO-MOUNT-POINT>"
           "ls -alh <TO-MOUNT-POINT>"
           "# rm-force --recursive --one-file-system <FROM-MOUNT-POINT>"
           "exit"
           "chown -R <USER-ID>:<GROUP-ID> <TRANSFER-NOTES-DIR>"
           "cp -R <TRANSFER-NOTES-DIR> <TO-MOUNT-POINT>"
           "chown -R <USER-ID>:<GROUP-ID> <TO-MOUNT-POINT>") "\n\n")))
;; 
;;; :TEST-ME *mon-drive-transfer-template*
;;
;;;(progn (makunbound '*mon-drive-transfer-template*)
;;;       (unintern '*mon-drive-transfer-template*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-06T19:40:28-04:00Z}#{10184} - by MON>
(defun mon-insert-drive-transfer-template (&optional clnd-template)
  "Insert the contents of variable `*mon-drive-transfer-template*'.\n
Does not move point.\n
When optional arg CLND-TEMPLATE is non-nil it is a string as generated by
`mon-drive-transfer-template-cln-all'.\n
:SEE Example invocation of `mon-drive-transfer-template-cln-all' for usage.\n
:SEE-ALSO `mon-drive-transfer-template-cln-log-dest', 
`*mon-drive-transfer-template-src-dest-log*',
`mon-drive-transfer-template-subst-src-dest-log',
`mon-drive-transfer-template-TEST'.\n►►►"
  (save-excursion 
    (newline)
    (if clnd-template
        (insert clnd-template)
      (insert *mon-drive-transfer-template*))))
;;
;;; :TEST-ME (mon-insert-drive-transfer-template)

;;; ==============================
;;; :CHANGESET 1726
;;; :CREATED <Timestamp: #{2010-05-13T12:46:20-04:00Z}#{10194} - by MON KEY>
(defun mon-drive-transfer-template-cln-log-dest (mnt-src mnt-dest log-dir) 
  "Replace `du',`ls', `grep' text variables with MNT-SRC, MNT-DEST, and LOG-DIR.\n
Helper function for `mon-drive-transfer-template-cln-all'.\n
Regexps of this function specific to template elements held in the variable
`*mon-drive-transfer-template*'.\n
The args MNT-SRC, MNT-DEST, and LOG-DIR are equivalent to first three args to
`mon-drive-transfer-template-cln-all'. They map as follows:\n
 MNT-SRC  <- from-mount-point
 MNT-DEST <- to-mount-point
 LOG-DIR  <- transfer-note-dir\n
Following template elements are replaced:\n 
 <TRANSFER-NOTES-DIR>/<DU-SRC-LOG> 
 <TRANSFER-NOTES-DIR>/<SRC-LOG> 
 <TRANSFER-NOTES-DIR>/<DU-DEST-LOG>\n
 <TRANSFER-NOTES-DIR>/<DEST-LOG>\n
With filenames:\n
 /TRANSFER-NOTES-DIR/source-mount-path--du-dump--SRC-LOG-YYYY-MM-DD
 /TRANSFER-NOTES-DIR/source-mount-path--ls-alR--SRC-LOG-YYYY-MM-DD
 /TRANSFER-NOTES-DIR/source-mount-path--du-dump--DEST-LOG-YYYY-MM-DD
 /TRANSFER-NOTES-DIR/destn-mount-path--ls-alR--DEST-LOG-YYYY-MM-DD
:EXAMPLE\n\n(mon-drive-transfer-template-TEST t)\n
:SEE-ALSO `mon-drive-transfer-template-TEST',
`*mon-drive-transfer-template-src-dest-log*',
`mon-drive-transfer-template-subst-src-dest-log'.\n►►►"
  (let ((subst-ds #'(lambda (dest-src)
                      (let ((new-ds dest-src))
                        ;; :NOTE Passing INPLACE non-nil to `subst-char-in-string' problematic.
                        (setq new-ds (subst-char-in-string 47 45 new-ds))
                      (if (aref new-ds 0)
                          (setq new-ds (substring new-ds 1))
                        new-ds)))))
    (while ;; :WAS (search-forward-regexp "\\(<TRANSFER-NOTES-DIR>/<\\)\\(SRC\\|DEST\\)\\(-LOG>\\)*" nil t)
        (search-forward-regexp "\\(<TRANSFER-NOTES-DIR>/<\\)\\(DU-SRC\\|SRC\\|DU-DEST\\|DEST\\)\\(-LOG>\\)*" nil t)
      (let ((msnp2 (match-string-no-properties 2)))
        (cond 
         ((equal msnp2 "DU-SRC")
          (setq msnp2
                (concat log-dir "/" (funcall subst-ds mnt-src) 
                        "--du-dump--SRC-LOG-" (mon-date-stamp :as-string t))))
         ((equal msnp2 "SRC")
          (setq msnp2
                (concat log-dir "/" (funcall subst-ds mnt-src) 
                        "--ls-alR--SRC-LOG-" (mon-date-stamp :as-string t))))
         ((equal msnp2 "DU-DEST")
          (setq msnp2
                (concat log-dir "/" (funcall subst-ds mnt-dest) 
                        "--du-dump--DEST-LOG-" (mon-date-stamp :as-string t))))
         ((equal msnp2 "DEST") 
          (setq msnp2 
                (concat log-dir "/" (funcall subst-ds mnt-dest) 
                        "--ls-alR--DEST-LOG-" (mon-date-stamp :as-string t))))
         ;; This can't actually happen.
         (t (error (concat ":FUNCTION `mon-drive-transfer-template-cln-all' --\n" 
                           "regexp cant find template <DEST-LOG> or <SRC-LOG>"))))
        (replace-match msnp2 t)))))
;;
;;; :TEST-ME (mon-drive-transfer-template-TEST t)

;;; ==============================
;;; :CHANGESET 1727
;;; :CREATED <Timestamp: #{2010-05-13T13:14:12-04:00Z}#{10194} - by MON KEY>
(defun mon-drive-transfer-template-TEST (&optional log-dest-test)
  "Test function for `mon-drive-transfer-template-cln'.\n
Return results in new buffer-name'd \"*MON-DRIVE-TRANSFER-CLN-TEST*\".
When optional arg LOG-DEST-TEST is non-nil return results of evaluating
`mon-drive-transfer-template-cln-log-dest' instead.\n
:SEE-ALSO `mon-drive-transfer-template-cln-all', `mon-insert-drive-transfer-template',
`*mon-drive-transfer-template*',`*mon-drive-transfer-template-src-dest-log*',
`mon-drive-transfer-template-subst-src-dest-log'.\n►►►"
  (with-current-buffer (get-buffer-create "*MON-DRIVE-TRANSFER-CLN-TEST*")
    (erase-buffer)
    (if log-dest-test
        (let ((mdtcle (concat (mon-drive-transfer-template-subst-src-dest-log)
                              (mon-drive-transfer-template-subst-src-dest-log t))))
          (save-excursion (insert mdtcle))
          (mon-drive-transfer-template-cln-log-dest
           "/mnt/frm-this-drv" "/mnt/to-this-other-drv/subdir" "/home/me/log-this-to-here"))
      (mon-drive-transfer-template-cln-all  "/mnt/frm-this-drv" "/mnt/to-this-other-drv/subdir"  
                                            "/home/me/log-this-to-here" "/mnt/to-this-other-drv" 
                                            "bubba-user" "bubba-group"))
    (display-buffer (current-buffer) t)))
;;
;;; :TEST-ME (mon-drive-transfer-template-TEST)
;;; :TEST-ME (mon-drive-transfer-template-TEST t)

;;; ==============================
;;; :NOTE We currently use the regexps to frob variables. Obv. could likewise
;;; build a string up w/ format and lisp parameters... However, the current
;;; approach is what happens when I have to grok POSIX & cmd-line :(
;;; IOW, "Hello *NIX; where `worse is better' means loosing big!!!"
;;; :CREATED <Timestamp: #{2010-05-06T19:12:51-04:00Z}#{10184} - by MON>
(defun mon-drive-transfer-template-cln-all (from-mount-point to-mount-point transfer-note-dir 
                                                   &optional to-mount-point-mnt user group)
  "Insert a cmd-line template for performing manual hard-drive backup/transfer.\n
FROM-MOUNT-POINT   -- Where to move the files/dirs from. Strip trailing slash.\n
TO-MOUNT-POINT     -- Where to move the files/dirs to. Strip trailing slash.\n
TRANSFER-NOTE-DIR  -- Where to save the a transfer-log. Strip trailing slash.\n
TO-MOUNT-POINT-MNT -- If TO-MOUNT-POINT is a dir this is its mount point.\n
USER               -- Will `chown' USER for TO-MOUNT-POINT and TRANSFER-NOTE-DIR.
                      When optional arg is ommitted default is taken from output
                      of shell command: `id -nu'\n
GROUP              -- chown groupname for TO-MOUNT-POINT and TRANSFER-NOTE-DIR.\n
                      When optional arg is ommitted default is taken from output
                      of shell command: `id -ng'\n
See value of `*mon-drive-transfer-template*', and below for discussion.\n
:EXAMPLE\n\n(mon-drive-transfer-template-TEST)\n\n
;; :TEMPLATE-DISCUSSION\n
A Template for interactively moving large directory trees drives and mount points.

Template is meant to be invoked manually as root as it is generally not safe to
execute these types of commands in a scripted environment when backups are not
available. This is esp. so where these template routines are those typically
used to initialize a _first_ instance of a filesystem backup/transfer involving
the movement of data across non-like filesystems, partitions, drives, etc. Once
such a transition occurs it is obviously easier, safer, more robust to use a
tool like rsync for subsequent operations. Indeed, rsync may be implemented to
accomplish the intial transfer as well. This said, there are situations where
rsync may not be desired esp. w/re some subtle errors and/or bit-rot which may
arise or be introduced when moving across file system types with different
internal representations of charset, filename length, etc. For such situations
`tar' while more conservative may be more appropriate, hence this template :\)

Each delimited `<VARIABLE>' below is replaced by the equivalent values of the
parameter args to `mon-drive-transfer-template-cln-all' as enumerated above.  The
template variables outlined below appear unmodified in the elisp variable
`*mon-drive-transfer-template*' and may be inserted to current-buffer for manual
editing/examination with `mon-insert-drive-transfer-template'.

<FROM-MOUNT-POINT> -- The mounted directory or subdir to move;
                      Occurences are replaced by FROM-MOUNT-POINT arg above.

<TO-MOUNT-POINT>   -- A mounted directory or subdir to move <FROM-MOUNT-POINT> into; 
                      Occurences are replaced by TO-MOUNT-POINT arg above.
                      :NOTE When <TO-MOUNT-POINT> is a sub-dir of a mount point
                      Pass the TO-MOUNT-POINT-MNT arg in addition to TO-MOUNT-POINT.

<TRANSFER-NOTES-DIR> -- A directory with files related to the transfer 
                        Occurences are replaced by TRANSFER-NOTE-DIR arg above.

   			These include files named:

                        --- <FROM-MOUNT-POINT>-transfer-notes
    		        A record of shell commands used to execute the transfer

                        --- TRANSFER-LOG-<TRANSFER-DATE>
                            A file name to store the output of `script'
                            command.  This file will contain a list the
                            commands executed along with a complete list of
                            all files moved into <TO-MOUNT-POINT> and is
                            useful for error checking and/or future replay.

                       --- <TRANSFER-NOTES-DIR>/<SRC-LOG>
                           <TRANSFER-NOTES-DIR>/<DEST-LOG>
                           File names to store the output of `ls -alR' for
                           subsequent processing with `grep' command to ascertein
                           file/dir counts. IOW each of these logs contains a
                           list of the files and directoris for both
                           <FROM-MOUNT-POINT> and <TO-MOUNT-POINT> and are
                           rendant in that TRANSFER-LOG-<TRANSFER-DATE> also
                           retains such a record. However, these logs are a more
                           granular in their enumeration and may serve as useful
                           in identifying any disconnetcs between SRC<-->DEST.

<USER-ID>:<GROUP-ID> -- The uid and gid to `chown' <TRANSFER-NOTES-DIR> and
                        <TO-MOUNT-POINT> upon completion of transfer
                        Occurences are replaced by USER and GROUP args above.

We executute the following as root user.

- The `script' command records the output;

- The `mount' command ensures that <TO-MOUNT-POINT> is mounted. Most likely this
  is redundant _HOWEVER_ if we don't do this then we wind up moving all the
  files into a directory, the contents of which won't appear when the drive is
  mounted. This happened to me once and it is a real PITA b/c it requires
  re-executing the entire Nn GB transfer again... This prevents that from
  happening. 

  :NOTE if <TO-MOUNT-POINT> is a subdirectory of a mount point and you are
  manually frobbing paths (e.g. the \"un-cleaned\" output returned by
  `mon-insert-drive-transfer-template' / `*mon-drive-transfer-template*') you
  should adjust the `mount'/`unmount' commnads accordingly. When using the
  output of `mon-drive-transfer-template-cln-all' by passing the optional arg
  TO-MOUNT-POINT-MNT the difference in paths is automatically adjusted.

- The `du' command is executed twice once on the <FROM-MOUNT-POINT> side, and
  once on the <TO-MOUNT-POINT> side. Assuming everthing transferred correctly
  these should match.

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

   (cd <FROM-MOUNT-POINT> && tar Scpvf <TO-MOUNT-POINT><TAR-FILE-NAME>.tar .)\n
   Or if you prefer:\n
   tar -C <FROM-MOUNT-POINT> -Scpvf <TO-MOUNT-POINT><TAR-FILE-NAME>.tar .

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
  removal during interactive shell use. When we know we want to explicitly force
  a `rm' invoking `rm-force' with the `--recrusive' flag. Following is from our
  ~/.bashrc and /root/.bashrc files:

  ,----
  | # Optionally set `--interactive=once' to only prompt once when `--recursive'
  | alias rm=\"rm --interactive\" 
  | alias rm-force=\"rm --force\"
  `----

  So, assuming everything is kosher and the aliases above are visible to your
  interactive shell the above `rm-force -R' will recursively remove the contents
  of <FROM-MOUNT-POINT>. 

  :NOTE There is no good way to \"undo\" such a big `rm'. You should take care
  to verify that this is indeed what is wanted _before_ clobbering a filesystem
  esp. as we're in our backup routine!!!

- The `exit' command stops `script' and writes a session transcript to the file:
  <TRANSFER-NOTES-DIR>/TRANSFER-LOG-<DATE>

- The `chown' command on <TRANSFER-NOTES-DIR> is invoked because we are root
  (or running w/ root priveleges). Set <USER-ID>:<GROUP-ID> as needed.

- The `cp' moves a log of our transfer into <TO-MOUNT-POINT> for future reference.

- The second `chown' sets the permissions of all files/dir in <TO-MOUNT-POINT> to
  something sensible.

- Now we're done.

 \"Le Roi est mort, vive le Roi!\"\n \"Ext3 is dead, all hail the Ext4!\"\n

:SEE-ALSO `*mon-drive-transfer-template*', `mon-drive-transfer-template-cln-log-dest',
`mon-insert-drive-transfer-template', `*mon-drive-transfer-template-src-dest-log*',
`mon-drive-transfer-template-subst-src-dest-log'.\n►►►"
  (let (;; :NOTE Using marker in case we decide not to dump ;;
        ;; `*mon-drive-transfer-template*' to temp-buffer i.e. adding an INTRP arg
        ;; further up.
        (pre-frm-to (make-marker)) 
        ;; Where to move the files/dirs from. :NOTE Omits trailing slash.
        (mctt-fmp (directory-file-name (file-name-as-directory from-mount-point)))
        ;; Where to move the files/dirs to. :NOTE Omits trailing slash.
        (mctt-2mp (directory-file-name (file-name-as-directory to-mount-point)))
        ;; If TO-MOUNT-POINT is a dir this is its mount point.
        (mctt-2mpm (when to-mount-point-mnt
                     (directory-file-name (file-name-as-directory to-mount-point-mnt))))
        ;; Where to save the TRANSFER-LOG. :NOTE Omits trailing slash.      
        (mctt-tnd (directory-file-name (file-name-as-directory transfer-note-dir)))
        (mctt-user (if user  
                       user
                     ;; Consider adding an `or' w/ (getenv "USER")
                     (replace-regexp-in-string "^\\(.*\\)\n*" "\\1"
                                               (shell-command-to-string "id -un"))))
        (mctt-group (if group
                       group
                     (replace-regexp-in-string "^\\(.*\\)\n*" "\\1"
                                               (shell-command-to-string "id -ng")))))
    (with-temp-buffer 
      (set-marker pre-frm-to (point))
      (mon-insert-drive-transfer-template)
      ;; Make sure `mount' and `umount' get the value of arg TO-MOUNT-POINT-MNT.
      (when to-mount-point-mnt
        (if (string-match-p (substring mctt-2mp 0 (length mctt-2mpm)) mctt-2mpm)
            (progn 
              (while (search-forward-regexp "^\\([u]?mount[[:blank:]]+\\)<TO-MOUNT-POINT>" nil t)
                (replace-match (concat (match-string-no-properties 1) mctt-2mpm) t))
              (goto-char pre-frm-to))
          (error (concat ":FUNCTION `mon-drive-transfer-template-cln-all'"
                         "-- arg TO-MOUNT-POINT not a subdir of mount point TO-MOUNT-POINT-MNT"))))
      ;; By calling `mon-drive-transfer-template-cln-log-dest' we leave the
      ;; regexp below for "<TRANSFER-NOTES-DIR>" as a fall-through.      
      (goto-char pre-frm-to)
      (mon-drive-transfer-template-cln-log-dest mctt-fmp mctt-2mp mctt-tnd) 
      (goto-char pre-frm-to)
      (while (search-forward-regexp "<FROM-MOUNT-POINT>" nil t)
          (replace-match (concat mctt-fmp " ") t))
      (goto-char pre-frm-to)
      ;; Any remaining occurences of "<TO-MOUNT-POINT>" aren't going to `[u]mount' commands,
      ;; now safe to  use value of arg TO-MOUNT-POINT.
      (while (search-forward-regexp "<TO-MOUNT-POINT>" nil t)
        (replace-match (concat mctt-2mp " ") t)) 
      (goto-char pre-frm-to)
      (while (search-forward-regexp "<TRANSFER-NOTES-DIR>" nil t)
        (replace-match mctt-tnd t)) 
      (goto-char pre-frm-to)
      (while (search-forward-regexp "<DATE>" nil t)
        (replace-match (mon-date-stamp :as-string t)))
      (goto-char pre-frm-to)
      (when mctt-group
        (while (search-forward-regexp "<GROUP-ID>" nil t)
          (replace-match mctt-group t))
        (goto-char pre-frm-to))
      (when mctt-user
        (while (search-forward-regexp "<USER-ID>" nil t)
          (replace-match mctt-user t))
        (goto-char pre-frm-to))
      ;; Done w/ marker pre-frm-to, put bffr-str on itfor `mon-insert-drive-transfer-template'.
      (setq pre-frm-to (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (mon-insert-drive-transfer-template pre-frm-to)))
;;
;; :TEST-ME (mon-drive-transfer-template-cln-all  
;;           "/mnt/frm-this-drv" "/mnt/to-this-other-drv/subdir"
;;           "~/log-this-to-here" "/mnt/to-this-other-drv" 
;;           "user-bubba" "bubba-group")
;;
;; :TEST-ME (mon-drive-transfer-template-TEST)

;;; ==============================

;;; ==============================
;;; :NOTE From :FILE mon-time-utils.el @ :CHANGESET 1734
(unless (fboundp 'mon-date-stamp)
;;; ==============================
;;; :CHANGESET 1711 <Timestamp: #{2010-05-06T15:28:04-04:00Z}#{10184} - by MON KEY>
;;; :RENAMED :FUNCTION `mon-today' -> `mon-date-stamp' 
;;; :ADDED keywords INSRTP and INTRP
;;; :CREATED <Timestamp: #{2009-10-23T20:57:47-04:00Z}#{09436} - by MON KEY>
(defun* mon-date-stamp (&key as-string as-symbol as-list-str as-list-num
                             as-vec-str as-vec-num insrtp intrp)
  "Return today's date as YYYY-MM-DD.\n
When keyword AS-STRING is non-nil return date as a string.
When keyword INTRP is non-nil or called-interactivley a string at point.\n
When keyword AS-SYMBOL is non-nil return date as a symbol.
When keyword AS-LIST-STR is non-nil return date as a list of three strings.
When keyword AS-LIST-NUM is non-nil return date as a list of three numbers.
When keyword AS-VEC-STR is non-nil return date as a vector of three strings.
When keyword AS-VEC-NUM is non-nil return date as a vector of three numbers.\n
When keyword INSRTP is non-nil insert retrun value at point as if with `prin1'\n
:NOTE the AS-*-NUM keywords return in decimal \(radix 10\).
:EXAMPLE\n\(mon-date-stamp :as-string t\)\n\(mon-date-stamp :as-symbol t\)\n
\(mon-date-stamp :as-list-str t\)\n\(mon-date-stamp :as-list-num t\)
\(mon-date-stamp :as-vec-str t\)\(mon-date-stamp :as-vec-num t\)\n
:ALIASED :BY `mon-stamp-date-only'
:ALIASED :BY `mon-today-stamp'\n
:SEE-ALSO `mon-get-current-year', `mon-format-iso-8601-time', `mon-stamp',
`mon-accessed-stamp', `mon-timestamp', `mon-lisp-stamp', `mon-file-stamp',
`mon-file-stamp-buffer-filename', `mon-file-stamp-minibuffer'.\n►►►"
  (interactive (list :intrp t))
  (let ((mds-2day (format-time-string "%Y-%m-%d")))
    (setq mds-2day
          (cond ((or intrp as-string) mds-2day)
                (as-symbol 
                 (eval-when (compile load eval)
                   (if (fboundp 'mon-string-to-symbol)
                       (mon-string-to-symbol mds-2day)
                     (car (read-from-string mds-2day)))))
                (as-list-str  `(,(substring mds-2day 0 4)
                                ,(substring mds-2day 5 7)
                                ,(substring mds-2day 8 10)))
                (as-list-num  (mapcar 'string-to-number
                                      (mon-date-stamp :as-list-str t)))
                (as-vec-str (apply 'vector (mon-date-stamp :as-list-str t)))
                (as-vec-num (apply 'vector (mon-date-stamp :as-list-num t)))))
    
    (cond (intrp (insert mds-2day))
          (insrtp (prin1 mds-2day (current-buffer)))
          (t mds-2day))))
;;
;;; :TEST-ME (mon-date-stamp :intrp t)
;;; :TEST-ME (mon-date-stamp :as-string t)
;;; :TEST-ME (mon-date-stamp :as-string t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-symbol t)
;;; :TEST-ME (mon-date-stamp :as-symbol t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-list-str t )
;;; :TEST-ME (mon-date-stamp :as-list-str t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-list-num t)
;;; :TEST-ME (mon-date-stamp :as-list-num t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-vec-str t)
;;; :TEST-ME (mon-date-stamp :as-vec-str t :insrtp t)
;;; :TEST-ME (mon-date-stamp :as-vec-num t)
;;; :TEST-ME (mon-date-stamp :as-vec-num t :insrtp t)
;;; ==============================
) ;; :CLOSE unless


;;; ==============================
(provide 'mon-drive-transfer-utils)
;;; ==============================

;;; ====================================================================
;;; mon-drive-transfer-utils.el ends here
;;; EOF
