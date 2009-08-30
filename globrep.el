;;; globrep.el
;;
;; From gamin@carette.Foo.COM  Fri Jun 27 05:34:38 1997
;; Return-Path: <gamin@carette.Foo.COM>
;; Received: from relay.lehman.com by batman.lehman.com (4.1/Lehman Bros. V1.6)
;;  id AA21870; Fri, 27 Jun 97 05:34:38 EDT
;; Received: from lehman.Lehman.COM by relay.lehman.com (4.1/LB-0.6)
;;  id AA15021; Fri, 27 Jun 97 05:34:37 EDT
;; Received: (from smap@localhost) by lehman.Lehman.COM (8.8.5/8.6.12) id FAA13815 for <flee@lehman.com>; Fri, 27 Jun 1997 05:33:20 -0400 (EDT)
;; Received: from bizet.videotron.net(205.151.222.75) by lehman via smap (V1.3)
;;  id tmp013812; Fri Jun 27 05:33:12 1997
;; Received: from carette.Foo.COM (ppp143.103.mmtl.videotron.net [207.253.103.143]) by bizet.videotron.net (8.8.5/8.8.2) with ESMTP id FAA03499 for <flee@lehman.com>; Fri, 27 Jun 1997 05:33:03 -0400 (EDT)
;; Received: (from gamin@localhost) by carette.Foo.COM (8.7.6/8.7.3) id FAA07422; Fri, 27 Jun 1997 05:32:11 -0400
;; Date: Fri, 27 Jun 1997 05:32:11 -0400
;; Message-Id: <199706270932.FAA07422@carette.Foo.COM>
;; To: flee@lehman.com (Franklin Lee)
;; Subject: Re: Global Replace modifications: context diff
;; References: <1068-Thu26Jun1997233127-0400-flee@lehman.com>
;; From: Martin Boyer <gamin@videotron.ca>
;; Status: RO

;; >I was able to find what appears to be the 'original' version of
;; >global-replace from which I made the modifications.  The context diff
;; >is below.

;; Cool!

;; It turns out that I only had to merge the "win32 drive:/path" part.
;; The rest is only relevant to very old versions of Emacs and XEmacs.

;; >I had difficulties with global-replace in this regard because of how
;; >tightly it was coupled to features in compile.el.

;; That is the main difference between the version you had and the new
;; version.  The new version is no longer so incestuous.  It also has a
;; new name (globrep.el).

;; Let me know if this works for you (a context diff between what you
;; have and the new version would be too big).

;; Martin


;------------------------------------------------------------------------------
;
; Nom     : (global-replace-lines)
;       (global-replace <string> <replacement>)
;       (global-grep-and-replace
;                           <string> <replacement> <files> <grep-command>)
; Fonction: Search and replace strings in multiple files.
; Fichiers: globrep.el
; Notes   : This version is known to work on XEmacs 19.14.
;
; Cr..    : 20 avril 90 --------- Martin Boyer <mboyer@ireq-robot.uucp>
; Modifi. : 27 juin 97 -------10- Martin Boyer <gamin@videotron.ca>
;   $Id$
;
; Historique:
;   $Log$
;  21 Jun 2004 Claus Brunzema
;   Added first and last comment lines to enable upload to
;   http://www.emacswiki.org. Replaced some strange french characters with
;   dots. Version 2.2.1
;
;  3 ao.t 94 --------7- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;   Renamed the file as globrep.el to accomodate broken systems that
;   allow only 14 characters in a file name.  Used query-replace-map.
;   Version 2.1.
;
; 26 juillet 94 -----6- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;   Added VC support.  Used `compilation-next-error-locus' and
;   `compilation-goto-locus' instead of local hacks.  Version 2.0.
;
; 20 mars 94 --------5- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;   Added code to preformat a "ChangeLog" entry.  Version 1.2.
;
;  7 mars 94 --------4- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;   Updated the documentation after initial comments from the net
;   and the FSF.  This is version 1.1.
;
;  1 mars 94 --------3- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;   Version 1.0: Prepared for distribution through LCD.
;
; 23 novembre 93 ----2- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;   Ported to version 19.
;   No longer requires a modified version of compile.el.
;------------------------------------------------------------------------------

;;; COPYRIGHT NOTICE
;;;
;; Copyright (C) 1990, 1994 Martin Boyer and Hydro-Quebec.
;; Copyright (C) 1994 Free Software Foundation
;; Copyright (C) 1997 Martin Boyer
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; author of this program <mboyer@ireq-robot.hydro.qc.ca> or to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Send bug reports to the author, Martin Boyer <gamin@videotron.ca>.

;;;; Contributors
;;
;; Thanks to Justin Gordon <justin@mcm.com> for doing the conversion
;; to XEmacs (with the help of Ben Wing):
;;
;;   Here are the converted emacs files. Special thanks to Ben who
;;   provided me the XEmacs equivalents of read-event and
;;   listify-key-sequence: Use `next-command-event' for read-event and
;;   `character-to-event' for listify-key-sequence. character-to-event
;;   converts a single key chord.  This function might not be
;;   necessary, however, because `next-command-event' returns more
;;   sensible things than `read-event' does.
;;
;; Thanks to Gary Oberbrunner <garyo@avs.com> for providing the
;; global-replace-use-vc variable:
;;
;;   I don't know if you are still maintaining globrep.el, but I made
;;   some changes to it to allow it to ignore VC.  This is important
;;   when the VC backend is CVS, since CVS doesn't need checkouts.
;;   Without these changes, globrep confuses CVS quite badly.
;;
;; Thanks to Franklin Lee <flee@lehman.com> for providing the idea for
;; the NT port:
;;
;; Dear M. Boyer,
;;
;;   Please excuse my lack of French in thanking you for writing
;;   global-replace!
;;
;;   I am using it with Win NT/95 emacs v 19.34, and patched it a bit
;;   to comply with the version of compile.el.  I also discovered
;;   that, due to the idiotic filesystem conventions those
;;   dos-descendants, that the version of global replace doesn't work
;;   without another patch!  so...  I have included below my version
;;   of global-replace which works with v 19.34 on sun AND winnt
;;   versions of emacs.  In particular, the patch to
;;   global-replace-lines marked 'win32' takes care of the extra ':'
;;   character for drive letters in Windows pathnames, e.g.,
;;
;;       g:/devtools.32/lisp/foo.el: 34: blah blah
;;
;;   doesn't get completely chewed up!


;;; DESCRIPTION AND USAGE
;;;
;; This program can be used to search for and replace strings in
;; multiple files.  The idea is to find, in a set of files, all the
;; lines that contain a string to be changed.  Then, one can edit
;; the result of this search, and the program will take care of the
;; tedious task of splicing the changes back into the original files.
;;
;; The search operation can be done separately, before calling any
;; command in this file.  The output of the search has be be in a
;; special format and, indeed, there is an emacs command that does
;; that for you; see the description of the grep command (C-h f grep
;; [RET]).
;;
;; Once you have the list of interesting lines in a "grep" buffer, you
;; can edit it in any way you like, under the following conditions:
;;
;;      1. The file names and line numbers are not edited.
;;      2. Lines can be deleted altogether, in which case
;;     the program will not attempt to put them back in
;;     the original files.
;;      3. A single line can be replaced by multiple lines if carriage
;;     returns (^M) are used to delimit the end of a line.  In
;;     other words, the program replaces all '\r' by '\n' just
;;     before putting the edited lines back in place.
;;
;; The last step is to call global-replace-lines.  This command will
;; prompt you for every line in the "grep" buffer -- even those that
;; you didn't edit, asking if you want to carry to the original file
;; the change you made on that line.  You will also be offerred to
;; quit or to perform all replacements without confirmation.  Since a
;; large number of files may be involved, the command will also ask
;; you if you want to dispose of a buffer after all processing has
;; been completed on that buffer.
;;
;; Finally, the command can also present a preformatted "ChangeLog"
;; entry, saving you some time in keeping the ChangeLog file up to
;; date.
;;
;; If you prefer, there are two commands that can call grep for you,
;; and then proceed to carry out the changes.  global-grep-and-replace
;; will prompt you for original and replacement strings, and for a
;; list of files.  It will then call grep, allow you to inspect and
;; modify the result, and then will preform the actual replacements.
;; global-replace is similar, but will skip the inspection stage.
;;
;; You may wish to examine the documentation of the query.el library.
;; But if you don't, here's the short story: whenever you are prompted
;; and a set of possible answers appear between [brackets], the first
;; is always the default.
;;
;;
;; This program differs from tags-query-replace in the following ways:
;;
;; 1. tags-query-replace requires a TAGS file, and not all files can
;;    be processed by etags.
;;
;; 2. Shell wildcards are more flexible that TAGS files to specify
;;    which files to process.  Anyhow, some variants of grep can take
;;    a list a files from a file, so the TAGS functionality can be
;;    duplicated.
;;
;; 3. Mainly, global-replace is more flexible in the way the
;;    replacements are done; the list of lines to be changed, along
;;    with the name of the file where they appear, is directly
;;    available in an emacs buffer.  From then on, the user can elect
;;    not to change lines from certain files, or to process certain
;;    files differently, etc.
;;
;; 4. Finally, I find it faster and easier to use, since I specify all
;;    the replacements in one operation (editing the result from
;;    grep), and then tell emacs to do the tedious job of actually
;;    putting the changes back in the files.


;;; INSTALLATION
;;;
;; Put this file somewhere in your load-path and byte-compile it.
;; Retrieve and install query.el in the same fashion.
;; Put the following lines in your .emacs:
;;
;; (autoload 'global-replace-lines "globrep"
;;           "Put back grepped lines" t)
;; (autoload 'global-replace "globrep"
;;           "query-replace across files" t)
;; (autoload 'global-grep-and-replace "globrep"
;;           "grep and query-replace across files" t)


;;; LCD Archive Entry:
;;;
;; global-replace|Martin Boyer|gamin@videotron.ca|
;; Search and replace strings in multiple files|
;; 26-jun-97|2.2|~/packages/globrep.el.Z|


;;; BUGS
;;;
;; `global-replace-lines' should try to preserve attributes
;; of the files it visits:
;;    don't offer to kill the buffer if it existed before the global-replace
;;    don't call vc-checkin if the checkout wasn't done by global-replace
;;
;; There should be a way to say "replace all occurrences in this buffer" or
;; "skip the rest of this buffer".  Perhaps it should even be the default.
;;
;; I don't understand very well the code in vc.el.  Consequently, VC
;; support is probably buggy.


(require 'compile)
(require 'query)
(require 'cl)
(require 'vc)

(provide 'globrep)

(autoload 'change-log-fill-paragraph "add-log")

(defconst global-replace-version "2.2.1"
  "The version number of the `global-replace' library.")

(defvar global-replace-use-vc nil
  "If non-nil, global-replace checks files out if they are managed by vc.")

(defvar globrep-xemacs  (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running XEmacs.  Otherwise, assume we are running Emacs.")

(defvar globrep-win32 (eq system-type 'windows-nt)
  "Non-nil if we are in Win95, Windows NT 3.5+, Windows NT 4.0+.")

(defvar globrep-chglog-prompt "Do you want to create a ChangeLog entry? "
  "prompt string used several places within globrep.el.  don't touch!")

(defmacro globrep-confirm (prompt)
  (` (if globrep-win32
         (yes-or-no-p (, prompt))
       (confirm (, prompt)))))

(defmacro globrep-read-event ()
  (` (if globrep-xemacs
         (next-command-event)
       (read-event))))


(defun global-replace-lines (&optional rescan)
  "Splice back to their original files the (possibly edited) output of grep.
This is a type of query-replace in which the original lines (in the
files) are replaced by those currently in the grep-output buffer.  You
will be asked to replace (y), don't replace (n), quit (q), or replace all.

When all changes are done in a file, you will be asked to
either save and kill the buffer, simply save it, or leave it as is.
In the replacement text, all ^M are changed to newlines, to allow a
single line to be replaced by multiple lines.

Global-replace will check out files automatically when needed (and
will also offer to steal a lock as a last resort).  Files checked out
this way are automatically checked in."
  (interactive "P")
  (let ((comment nil)
    (continue 'start)
    (loops -1)
    next-error
    (count 0)
    action
    (modified nil)          ;did we modify the current file?
    (modified-files '())        ;the list of modified files
    buf b e
    next-buf
    file-under-vc           ;nil or the name of the current file
    file-editable
    replacement-line
    (message
     (substitute-command-keys
      "Globally replacing lines: (\\<query-replace-map>\\[help] for help) "))
    )

    (if global-replace-use-vc
    (if (and (interactive-p)
         (globrep-confirm "Do you want to create a VC comment? "))
        (setq comment (g-r-format-vc-comment))))
    (or comment
    (setq comment ""))      ;VC gets very sick if there's no comment

    (while continue
      (incf loops)
      (setq next-error (compilation-next-error-locus nil rescan t))
      (setq rescan nil)         ;We are not just starting anymore
      (if (or (eq continue 'exit) (null next-error))
      (setq continue nil)
    (setq next-buf (marker-buffer (cdr next-error))))
      (if (null next-error)
      (setq next-buf nil))
      (setq action
        (if (or (eq continue 'start) (eq next-buf buf))
        "no"
          (switch-to-buffer buf)
          (if (not modified)
          ;; Was it modified by something other than global-replace?
          (if (buffer-modified-p)
              (query-string "Done with this buffer (no replacements), dispose?"
                    '("save" "no"))
            (query-string "Done with this buffer (unmodified), dispose?"
                  '("kill" "no")))
        (setq modified nil)
        (setq modified-files
              (nconc modified-files (list (buffer-file-name buf))))
        (query-string "Done with this buffer, dispose?"
                  '("kill (after saving)" "save" "no")))))
      ;; Dispose of the buffer according to user's choice.
      ;; Do nothing if action is "no".
      ;; Because global replaces in multiple files may create a large
      ;; number of uninteresting buffers, it is useful to remove them
      ;; after the replace.
      (when (or (string= action "save")
        (string= action "kill (after saving)"))
    (set-buffer buf)
    (save-buffer nil))
      (when (string-match "^kill" action)
    ;; Files automatically checked out by global-replace are
    ;; automatically checked in.
    (if (and global-replace-use-vc file-under-vc)
        (vc-checkin file-under-vc nil comment))
    (kill-buffer buf))

      (when continue
    (if (eq continue 'start)
        (setq continue t))      ;We are not just starting anymore
    (compilation-goto-locus next-error)
    (unless (eq next-buf buf)
      ;; This is a new file.  See if it's under VC
      (setq file-under-vc (buffer-file-name))
      (if (or (not global-replace-use-vc)
          (not (vc-name file-under-vc)))
          (setq file-under-vc nil
            file-editable t)
        (let ((owner (vc-locking-user file-under-vc)))
          (if (and owner
               (not (string-equal owner (user-login-name))))
          ;; I can't see how I can wait for the lock to be stolen
          ;; without using a recursive edit.  vc-steal-lock doesn't
          ;; block.
          (if (not (globrep-confirm
                "Do you wish to steal the lock on the current file? "))
              (setq file-editable nil)
            (message
             (substitute-command-keys
              "Type \\[exit-recursive-edit] after taking the lock."))
            (recursive-edit)
            ;; Verify that the lock was indeed granted.
            (setq action
              (not (string-equal (vc-locking-user file-under-vc)
                         (user-login-name)))))
        ;; The file is registered under VC, but either
        ;; there is no lock on it or we own the lock
        (setq file-editable t))
          (if (and file-editable buffer-read-only)
          (vc-checkout-writable-buffer file-under-vc)
        ;; Remember that we don't need to check it in
        (setq file-under-vc nil))))
      (setq buf (current-buffer))   ;Where the grep hit occurred
      )
    (let ((done nil)        ;when done with a line
          (replaced nil)        ;when replacement did take place
          key)
      ;; Given that buffers may be killed, is this reasonable?
      (unless (eq continue 'automatic)
        (undo-boundary))

      ;; Loop reading commands until one of them sets done,
      ;; which means it has finished handling this occurrence.
      (while (not done)
        (if file-editable
        (unless (eq continue 'automatic)
          (message message)
          (setq key (vector (globrep-read-event)))
          (setq action (lookup-key query-replace-map key)))
          (message
           "According to VC, you can't edit this file.  Skipping this line.  (Hit RET)")
          (setq key (vector (globrep-read-event)))
          (setq action 'skip))

        ;; Answers that affect the main loop
        (cond ((eq action 'automatic)
           (setq continue 'automatic))
          ((or (eq action 'exit)
               (eq action 'act-and-exit))
           (setq continue 'exit)))

        (cond ((or (eq action 'act)
               (eq action 'act-and-exit)
               (eq action 'act-and-show)
               (eq continue 'automatic))
           (unless replaced
             (incf count)
             (setq modified t)
             (beginning-of-line) (setq b (point))
             (end-of-line) (setq e (point))
             (delete-region b e)
             ;; Go to the error message
             (set-buffer (marker-buffer (car next-error)))
             (goto-char (car next-error))
             (end-of-line) (setq e (point))
             (beginning-of-line)
             ;; If we're in win32, there might be an extra
             ;; colon after the drive letter, as in "drive:/path".
             ;; So we have to skip the colon if it's present.
             (when (and (eq system-type 'windows-nt)
                (looking-at "[a-zA-Z]:/"))
               (forward-char 2))
             (skip-chars-forward "^:" e) (forward-char)
             (skip-chars-forward "^:" e) (forward-char)
             (setq b (point))
             (narrow-to-region b e)
             (replace-string "\r" "\n")
             (widen)
             (setq replacement-line (buffer-substring b e))
             (set-buffer buf)
             (insert replacement-line)
             (setq replaced t))
           (setq done (not (eq action 'act-and-show))))
          ((eq action 'skip)
           (setq done t))   ;don't replace this occurrence
          ((eq action 'exit)
           (setq done t))
          ((eq action 'edit)
           (save-excursion (recursive-edit)))
          ((eq action 'delete-and-edit)
           (save-excursion
             (beginning-of-line)
             (setq b (point))
             (end-of-line)
             (delete-region b (point))
             (recursive-edit))
           (setq done t))
          ((eq action 'recenter)
           (recenter nil))
          ((eq action 'help)
           (with-output-to-temp-buffer "*Help*"
             (princ
              (concat "Globally replacing lines.\n\n"
                  (substitute-command-keys
                   query-replace-help)))))
          ((eq action 'backup)
           (message "It is not possible to backup in global-replace.")
           (sit-for 1))
          (t
           (setq continue 'exit)
           (and globrep-xemacs
                (setq unread-command-events
                      (append (character-to-event key)
                              unread-command-events)))
           (setq done t))
          )
        )
      )
    )
      )

    (if (not (interactive-p))
        (list count loops modified-files)
      (if (and (> count 0)
               (globrep-confirm globrep-chglog-prompt))
           (g-r-format-changelog-entry modified-files))
      (message "%d replacements (of %d) done." count loops))
    )
  )



(defun g-r-format-vc-comment (&optional text)
  "Prepare a comment that will be used in VC log entries.  Return it as a string.
Optional argument is the description of the change."
  (let ((buffer (get-buffer-create "*GR-log*"))
    comment)
    (pop-to-buffer buffer)
    (setq mode-name (substitute-command-keys
             "Type \\[exit-recursive-edit] when done"))
    (erase-buffer)
    (if text (insert text))
    (message "Enter VC comment describing this global-replace.")
    (recursive-edit)
    (set-buffer buffer)
    (setq comment (buffer-substring (point-min) (point-max)))
    (kill-buffer buffer)
    comment))


(defun g-r-format-changelog-entry (files &optional text)
  "Format and present to the user a ChangeLog entry after a global-replace.
First argument is the list of files where the change occurred.
Second optional argument is the description of the change."
  (add-change-log-entry)
  (beginning-of-line)
  (skip-chars-forward " \t")
  (delete-region (point) (progn (end-of-line) (point)))
  (insert "* ")
  (let ((logfile-dir
     (concat "^" (regexp-quote (file-name-directory (buffer-file-name))))))
    (dolist (file files)
      (setq file (if (string-match logfile-dir (file-name-directory file))
             (substring file (match-end 0))
           file))
      (insert " " file)))
  (change-log-fill-paragraph nil)
  (insert ": ")
  (when text (insert text)
    (change-log-fill-paragraph nil)))



(defun global-replace (string replacement &optional rescan)
  "From a grep output buffer, query replace STRING by REPLACEMENT.
Prefix argument or optional RESCAN forces rescanning of the *compilation*
buffer.  See also global-replace-lines for a more flexible approach."
  (interactive "sGlobal replace: \nsGlobal replace %s by: \nP")
  (set-buffer (compilation-find-buffer))
  (if (not (string-match "grep" mode-name))
      (error "The last compilation was not a grep!"))
  (goto-char (point-min))
  (setq buffer-read-only nil)
  (replace-string string replacement)
  (let ((ret (global-replace-lines rescan)))
    (if (not (interactive-p))
    ret
      (if (globrep-confirm globrep-chglog-prompt)
          (g-r-format-changelog-entry
       (nth 2 ret)
       (format "Replaced `%s' by `%s'." string replacement)))
      (message "%d replacements (of %d) done." (nth 0 ret) (nth 1 ret)))))


(defun global-grep-and-replace (string replacement files grp-cmd)
  "Query replace STRING by REPLACEMENT in FILES, using GRP-CMD to find STRING.
global-replace-lines is used to perform the actual replacement.
Before that, however, the user is given a chance to edit the grep output."
  (interactive
   (let* ((s (read-string "Global replace: "))
      (r (read-string (concat "Global replace " s " by: ")))
      (f (read-string (concat "Searching for " s " in files: ")))
      (l (read-string "grep command: " grep-command)))
     (list s r f l)))
  (grep (concat grp-cmd " " string " " files))
  (let* ((status 'run)
     (compilation-buffer (compilation-find-buffer))
     (compilation-process (get-buffer-process compilation-buffer))
     action)
    (while (eq status 'run)
      (message "running...") (sit-for 0)
      (sleep-for 1)
      (message "") (sit-for 0)
      (sleep-for 1)
      (setq status (process-status compilation-process)))
    (if (not (eq status 'exit))
    (error "Grep process exited abnormally"))
    (setq action
      (query-string "Do you want to"
            '("replace" "edit search" "quit")))
    (cond ((string= action "replace")
       (message "On with the replace!")
       (sit-for 0)
       (set-buffer compilation-buffer)
       (goto-char (point-min))
       (setq buffer-read-only nil)
       (replace-string string replacement))
      ((string= action "edit search")
       (message "Entering recursive edit, exit with C-M-c, abort with C-]")
       (recursive-edit))
      ((string= action "quit")
       (error "Aborted."))
      )
    )
  (let ((ret (global-replace-lines)))
    (if (not (interactive-p))
    ret
      (if (globrep-confirm globrep-chglog-prompt)
      (g-r-format-changelog-entry
       (nth 2 ret)
       (format "Replaced `%s' by `%s'." string replacement)))
      (message "%d replacements (of %d) done." (nth 0 ret) (nth 1 ret)))))

;;; globrep.el ends here.
