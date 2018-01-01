;;; appt.el --- Notification of appointments from your diary file.
;; 
;; Filename: appt.el
;; Description: Notification of appointments from your diary file.
;; Author: Neil Mager <neilm@juliet.ll.mit.edu>
;;      Drew Adams
;; Maintainer: Drew Adams 
;; Copyright (C) 1989, 1990, 1994, 1998 Free Software Foundation, Inc.
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Thu Aug  3 18:14:22 1995
;; Version: 0
;; Last-Updated: Mon Jan  1 09:17:52 2018 (-0800)
;;           By: dradams
;;     Update #: 697
;; URL: https://www.emacswiki.org/emacs/download/appt.el
;; Keywords: calendar, local
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `cal-dst', `cal-julian', `cal-menu', `cal-persia',
;;   `calendar', `calendar+', `cl', `diary-lib', `easymenu', `faces',
;;   `frame-cmds', `frame-fns', `lisp-float-type', `misc-fns',
;;   `solar', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Notification of today's appointments from your diary file.
;;
;; Note: This code is quite old, and is no doubt obsolete now.  You
;;       might find it useful in some way - or not. ;-)
;;
;; -------------------------------------------------------------------
;;
;; This is an "enhanced" version of `appt.el' with modifications by
;; Drew Adams.  The original code was written by Neil Mager
;; <neilm@juliet.ll.mit.edu>.  See description of differences from the
;; original GNU (Mager) version, below.
;;
;; Visible and/or audible notification of today's appointments from
;; your diary file (variable `diary-file' (see file `calendar+.el').
;;
;; Reminder messages may optionally be displayed in the mode line, in
;; the message area, or in a separate buffer.
;;
;; Variable `appt-message-warning-time' determines how much notice is
;; given before each appointment reminder.  Variable `appt-checking-p'
;; specifies whether or not to notify you of pending appointments.
;;
;; In order to use this, you need to have a `diary-file' and invoke
;; `calendar'.  The following should be in your `~/.emacs':
;;
;; (setq appt-checking-p t)
;; (add-hook 'diary-hook 'appt-make-list)
;;
;;  This is an example of two reminders in your `diary-file':
;; Monday
;;   9:30am Coffee break
;;  12:00pm Lunch        
;; 
;; With the above lines in files `~/.emacs' and `diary-file', the
;; calendar and diary are displayed when you enter invoke `calendar',
;; and your appointments list is automatically be created.  You are
;; reminded before 9:30am about your coffee break and before noon to
;; go to lunch.
;;
;; This appointments reminder facility works with a copy of today's
;; appointments, gleaned from your diary (`diary-file').  It generally
;; does not change the diary itself.  This copy, the appointments
;; list, is called `appt-time-msg-list', and it is updated each time
;; `appt-make-list' is called.  This is normally whenever the function
;; `diary' is called (via `list-diary-entries', via `diary-hook').

;; The function `appt-check' checks `appt-time-msg-list' periodically,
;; and notifies you of upcoming appointments.
;;
;; You can add or delete appointment reminders from today's list with
;; `appt-add' and `appt-delete'.  Such additions are normally *not*
;; additions to the diary itself, but only to `appt-time-msg-list'.
;; They concern today only, and they are *volatile*: they do not
;; persist beyond your Emacs session.  Likewise, such deletions are
;; *not* deletions from the diary itself (even if the diary was the
;; source of the appointment for `appt-time-msg-list').
;;
;; An exception to this is the following: If you give a prefix
;; argument to `appt-add', it will also add an entry to your diary for
;; the appointment.  If the prefix is negative, the new diary entry is
;; non-marking; otherwise, it is marking.
;;
;; The appointments list is recreated automatically at 12:01am (by a
;; call to function `diary'), if Emacs is running at that time.
;;
;; If you change your diary file, the changes will not be taken into
;; account by this reminder facility, unless you reexecute the
;; function `diary' (`appt-make-list').  Remember, however, that
;; reexecuting `diary' will reinitialize `appt-time-msg-list', wiping
;; out any volatile changes you may have made via `appt-add' and
;; `appt-delete'.
;;
;; You can change the way the appointment reminders window is created
;; and deleted by setting the variables `appt-disp-window-function'
;; and `appt-delete-window-function'.  For instance, these can be set
;; to functions that display appointments in pop-up frames, which are
;; lowered or iconified after `appt-display-interval' seconds.
;;
;;
;; Main changes here from the original GNU version (Mager's):
;; ---------------------------------------------------------
;;
;; 1. Added functions:
;;    `appt', `appt-delete-past-appts', `appt-hide-reminders',
;;    `appt-insert-anniversary-diary-entry',
;;    `appt-insert-cyclic-diary-entry', `appt-insert-diary-entry',
;;    `appt-insert-monthly-diary-entry',
;;    `appt-insert-weekly-diary-entry',
;;    `appt-insert-yearly-diary-entry', `appt-remind',
;;    `appt-remove-past-appts', `clear-appointments',
;;    `minutes-past-midnight', `reminders-mode', `save-reminders',
;;    `show-reminders', `show-reminders-reminder'.
;; 2. Removed functions:
;;    `appt-disp-window', `appt-delete-window',
;;    `appt-select-lowest-window' (called only by `appt-disp-window').
;; 3. Variable renamings: `appt-issue-message' -> `appt-checking-p',
;;    `appt-message-warning-time' -> `appt-msg-warning-time'.
;; 4. Added variables:
;;    `appt-time+msg-regexp', `reminders-mode-map',
;;    `view-appointments-initially'.
;; 5. Removed variable `appt-issue-message'.
;; 6. Variable `appt-disp-window-function' has default value
;;    `appt-remind' (not `appt-disp-window').  Variable
;;    `appt-delete-window-function' has default value
;;    `appt-hide-reminders'(not `appt-delete-window').
;; 7. More flexibility in input time expressions
;;    (`appt-time+msg-regexp').
;; 8. `appt-check':  
;;    a. Remove past appointments first, even if not `appt-checking-p'.
;;    b. Call `appt-delete-window-function' only if
;;       `appt-display-duration' is a number.
;;    c. Don't remove message when `min-to-app' = 0.
;;    d. Show reminders only if only removed past appointments.
;;    e. Even when there are no messages, update the buffer.
;;    f. Fixed bug in calculation of appointment just before midnight.
;; 9. `appt-add':
;;    a. With prefix arg, also adds appointment to diary.
;;    b. New appointment must be later than now.
;;    c. Calls `appt-disp-window-function' when done.
;; 10. `appt-delete': Calls `appt-disp-window-function' when done.
;; 11. `appt-make-list':
;;    a. Add appointment help to mode line.
;;    b. Sort diary entries (via `sort-diary-entries').
;;    c. Remove past appointments from list.
;;    d. Display appointments.
;;
;;  The following bindings are made here for mode `reminders-mode':
;;
;;    `C-c C-s'        `save-reminders'
;;    `C-c d'          `clear-appointments'
;;    `C-c i a'        `appt-insert-anniversary-diary-entry'
;;    `C-c i c'        `appt-insert-cyclic-diary-entry'
;;    `C-c i d'        `appt-insert-diary-entry'
;;    `C-c i m'        `appt-insert-monthly-diary-entry'
;;    `C-c i w'        `appt-insert-weekly-diary-entry'
;;    `C-c i y'        `appt-insert-yearly-diary-entry'
;;    `C-c j'          `appt-add'
;;    `C-c k'          `appt-delete'
;;    `C-c s'          `show-all-diary-entries'
;;
;;  The following bindings are made here for mode `calendar-mode'.
;;  These commands are also added to the Diary menu bar menu.
;;
;;    `j'              `appt-add'
;;    `k'              `appt-delete'
;;    `r'              `appt-disp-window-function'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2011/01/03 dadams
;;     Removed autoload cookies from defvars and non-interactive functions.
;; 2010/12/04 dadams
;;     Removed make-local-hook call.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2005/12/18 dadams
;;     Use minibuffer-prompt face.  Removed require of def-face-const.el.
;; 2005/10/03 dadams
;;     Removed require of icomplete+.el (no longer redefines read-from-minibuffer).
;; 2005/05/25 dadams
;;     string-to-int -> string-to-number everywhere.
;; 2005/05/15 dadams
;;     Renamed: flash-ding-minibuffer-frame to 1on1-flash-ding-minibuffer-frame.
;; 2005/01/25 dadams
;;     Changed defcustom for appt-display-duration to allow a sexp.
;;     NOTE: I haven't used or tested this library for years. This minor fix
;;           doesn't imply that the rest works with the latest versions of calendar.
;; RCS $Log: appt.el,v $
;; RCS Revision 1.8  2001/01/08 22:14:20  dadams
;; RCS Adapted file header for Emacs Lisp Archive.
;; RCS
;; RCS Revision 1.7  2001/01/08 19:42:30  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.6  2001/01/03 00:25:18  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.5  2001/01/02 22:47:03  dadams
;; RCS 1. More optional require's via 3rd arg=t now.
;; RCS 2. appt-remind: protect show-a-frame-on via fboundp.
;; RCS
;; RCS Revision 1.4  2001/01/02 22:35:14  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.3  2000/11/28 19:13:09  dadams
;; RCS Optional require's via 3rd arg=t now.
;; RCS
;; RCS Revision 1.2  2000/09/27 21:31:57  dadams
;; RCS Updated for Emacs 20.7:
;; RCS 1. appt-msg-warning-time -> appt-message-warning-time (original name).
;; RCS 2. appt-delete-window-function, view-appointments-initially:
;; RCS    Corrected doc string.
;; RCS 3. defvar's -> defcustom's
;; RCS 4. Added: appt-mode-string, appt-now-displayed, appt-prev-comp-time,
;; RCS    appt-timer.
;; RCS 5. Removed: add-hook display-time-hook.
;; RCS 6. appt-check: updated from 20.7 version.
;; RCS 7. appt-delete: while -> dolist.
;; RCS 8. Added: remove-hook temp-buffer-show-hook: help-mode-finish.
;; RCS 9. show-reminders: use variable-length underline.
;; RCS
;; RCS Revision 1.1  2000/09/13 20:01:49  dadams
;; RCS Initial revision
;; RCS
;
; Revision 1.5  1999/10/26  11:09:07  dadams
; 1. Use calendar-mouse-3-menu-items, not calendar-mouse-3-map.
; 2. Moved provide to end (because of featurep test).
;
; Revision 1.4  1999/10/25  11:21:35  dadams
; appt-insert-diary-entry: Append " :" when european-calendar-style.
;
; Revision 1.3  1999/10/25  08:18:06  dadams
; *** empty log message ***
;
; Revision 1.2  1999/10/07  09:41:02  dadams
; 1. Require def-face-const.el when compile, instead of require std-faces.el.
; 2. calendar-goto-info-node, not describe-calendar-mode, in mode-line.
; 3. save-reminders: buffer-live-p -> live-buffer-name.
; 4. show-reminders: current-date-string -> calendar-date-string.
;
; Revision 1.1  1997/03/19  16:55:53  dadams
; Initial revision
;
; Revision 1.45  1996/06/20  11:45:34  dadams
; Renamed: time+msg-regexp->appt-time+msg-regexp, max-time->appt-max-time.
;
; Revision 1.44  1996/06/17  09:31:44  dadams
; File header Commentary: Explained diffs from GNU version of this file.
;
; Revision 1.43  1996/06/12  12:31:59  dadams
; (trivial)
;
; Revision 1.42  1996/06/06  12:18:15  dadams
; 1. Update of file dependency comments (e.g. "Autoloaded from...").
; 2. Require frame-cmds.el.
;
; Revision 1.41  1996/04/26  08:24:56  dadams
; Put escaped newlines on long-line strings.
;
; Revision 1.40  1996/03/18  13:16:54  dadams
; appt-insert-diary-entry, appt-insert-weekly-diary-entry,
; appt-insert-monthly-diary-entry, appt-insert-yearly-diary-entry, appt:
;    defsubst -> defun.
;
; Revision 1.39  1996/03/14  10:56:41  dadams
; minutes-past-midnight, appt-delete-past-appts, appt-sort-list,
; appt-insert-diary-entry, appt-insert-weekly-diary-entry,
; appt-insert-monthly-diary-entry, appt-insert-yearly-diary-entry,
; show-reminders-reminder, appt-hide-reminders, appt: defun -> defsubst.
;
; Revision 1.38  1996/03/06  12:38:08  dadams
; (trivial: faces+ -> std-faces)
;
; Revision 1.37  1996/03/06  08:13:38  dadams
; 1. Copyright.
; 2. drew-cal.el -> calendar+.el, 'drew-faces.el -> std-faces.el.
;
; Revision 1.36  1996/02/27  10:15:56  dadams
; No longer use special-display-buffer-names.
; (Rely on special-display-regexps.)
;
; Revision 1.35  1996/02/12  09:00:08  dadams
; Updated header keywords (for finder).
;
; Revision 1.34  1996/02/06  10:37:40  dadams
; Put variable-interactive property on appropriate user option vars.
;
; Revision 1.33  1996/01/29  16:10:34  dadams
; appt-remind: Show (raise) frame.
;
; Revision 1.32  1996/01/09  09:07:03  dadams
; (trivial)
;
; Revision 1.31  1996/01/08  13:34:55  dadams
; 1. Removed requires for drew-util-19.el, drew-windows.el, drew-misc-19.el,
;    since autoloaded now.
; 2. Require drew-faces.el.
; 3. save-reminders: message -> display-in-minibuffer.
;
; Revision 1.30  1995/11/28  12:40:48  dadams
; Put define-key's in first column, so imenu picks them up (cosmetic).
;
; Revision 1.29  1995/11/22  15:14:27  dadams
; Require drew-windows.el.
;
; Revision 1.28  1995/11/22  08:45:28  dadams
; appt-add: Add `:' if european-calendar-style.
;
; Revision 1.27  1995/11/16  12:57:02  dadams
; 1) Require drew-cal.el.  Don't autoload diary-ins.el or diary-lib.el.
; 2) appt-buffer-name is now a special-display buffer.
; 3) Corrected mode-line.
;
; Revision 1.26  1995/11/14  16:24:42  dadams
; (minor - removed time in calendar-mode-line-format)
;
; Revision 1.25  1995/11/09  17:34:16  dadams
; Use update-calendar-mode-line (in appt-make-list too).
;
; Revision 1.24  1995/11/09  16:24:18  dadams
; appt-make-list: Don't call appt-disp-window-function unless today is
;                 in range of dates `diary' is considering.
;
; Revision 1.23  1995/11/09  14:52:24  dadams
; 1) Removed to drew-cal.el: removal of Moon from menu bar.
; 2) Update *Calendar* mode line here to reflect key bindings.
;
; Revision 1.22  1995/10/30  13:06:14  dadams
; 1) time+msg-regexp: Use whole line, including part to left of time.
; 2) appt-add: Add TIME to end of entry too.
; 3) reminders-mode: Updated comment to reflect these mods.
;
; Revision 1.21  1995/10/24  12:49:24  dadams
; appt-add: Improved checking/correcting of input times.
;
; Revision 1.20  1995/09/04  13:20:50  dadams
; Changed header to GNU std.
;
; Revision 1.19  1995/09/01  15:11:33  dadams
; When add entry to appt-time-msg-list, put it at end.  That way, diary
; order is respected between entries whenever the times are the same.
;
; Revision 1.18  1995/09/01  14:45:30  dadams
; appt-make-list and appt-delete: Removed prin1-to-string.
; Use string already there, without escaping it.
;
; Revision 1.17  1995/09/01  07:57:13  dadams
; 1) Autoload sort-diary-entries.
; 2) Changed time+msg-regexp: a) Begins on word boundary.
;    b) separate hr, min, msg.  c) minutes optional.
; 3) Use C-c prefix for reminders-mode-map.
; 4) appt-remove-past-appts -> appt-delete-past-appts.
;    Added new, non-destructive version of appt-remove-past-appts too.
; 5) appt-add: a) Error before read msg if bad Time.
;              b) Add ":00" if implicit min.
; 6) appt-make-list: a) Factored code.  b) Use sort-diary-entries.
;                    c) appt-time: Gather matched parts.
; 7) save-reminders:
;    a) Save even if buffer unmodified (no error), bc could be from undo.
;    b) Check for bad Time format.  c) Warn if removed past reminders.
; 8) appt-convert-time:  a) Use single time+msg-regexp match (gather parts).
;                        b) 26h00 -> 2h00.
; 9) reminders-mode:  Updated doc string.
;
; Revision 1.16  1995/08/31  09:36:51  dadams
; appt-add: Added prefix arg (persistent), to allow adding diary entry.
;
; Revision 1.15  1995/08/30  15:29:21  dadams
; Put (require calendar) in eval-when-compile.
;
; Revision 1.14  1995/08/30  08:07:36  dadams
; 1) appt-display-duration:  Non-number => Indefinitely.
; 2) Improved doc strings, especially reminders-mode.
; 3) Removed time-regexp.  Use only time+msg-regexp.
;    (In appt-make-list, appt-add, save-reminders).
; 4) Entries in appt-time-msg-list are dotted lists: (t . m), not (t m).
; 5) appt-check:
;    a) remove-past-appts first, even if not appt-checking-p.
;    b) appt-delete-window-function only if appt-display-duration is number.
;    c) Don't remove msg when min-to-app = 0.
;    d) show-reminders only if only removed past appts.
; 6) Added appt-remove-past-appts.  Use it each time just before sort list.
; 7) appt-make-list: See #3.  Removed string-match inside while
;    string-match.  Use appt-convert-time on whole time-string.
; 8) save-reminders:
;    a) See #3.  appt-time-string just gets whole match, directly.
;    b) show-reminders.
; 9) Simplified: appt-delete, save-reminders.
;
; Revision 1.13  1995/08/25  12:49:07  dadams
; To get current time as HH:MM, use substring of current-time-string,
; instead of string-matching display-time-string.
;
; Revision 1.12  1995/08/25  12:13:15  dadams
; 1) appt-issue-message -> appt-checking-p.
; 2) Added view-appointments-initially, minutes-past-midnight,
;    clear-appointments.
; 3) show-reminders split in two:  Added appt-remind to replace old.
;    New show-reminders just updates buffer.
; 4) Corrected time-regexp to end on word boundary.
; 5) Use appt-disp-window-function everywhere, instead of literal appt-remind.
; 6) appt-check: Even when no msgs, update buffer (w/o displaying).
; 7) appt-add: Warn, not of afterhours appt, but if appt time already past.
; 8) appt-delete: Redisplay, even if user does (e.g.) C-g.
; 9) appt-make-list: If view-appointments-initially, show buffer, w/o beeping.
; 10) Allow 14h30, as well as 14:00 time formats.
; 11) appt-hide-reminders: Fixed bug: Was trying to iconify window, not frame.
;
; Revision 1.11  1995/08/24  13:59:50  dadams
; 1) Require: date.el, calendar.el, diary-ins.el.
; 2) Added: reminders-mode-map, time-regexp, save-reminders,
;    appt-hide-reminders, reminders-mode, appt-insert-*-diary-entry,
;    show-reminders-reminder.
; 3) Removed appt-delete-reminder (Replaced by appt-hide-reminders.)
; 4) Require for confirmation of early morning appointments.
; 5) show-reminders: Display buffer even if list is empty.  Use reminders-mode.
; 6) Removed commented-out old, unused stuff.
;
; Revision 1.10  1995/08/22  14:48:36  dadams
; 1) Added title in reminders buffer (appt-buffer-name).
; 2) Added show-reminders-reminder.
;
; Revision 1.9  1995/08/22  06:31:46  dadams
; appt-add: Prompt now tells current time.
;
; Revision 1.8  1995/08/18  14:34:10  dadams
; (trivial)
;
; Revision 1.7  1995/08/18  06:15:43  dadams
; 1) Bound appt-add, appt-delete, show-reminders in calendar-mode-map.
;    Unbound appt.  Require calendar.el for calendar-mode-map.
; 2) show-reminders: No longer impose calendar-mode-map here.
; 3) Changed default for appt-buffer-name.
;
; Revision 1.6  1995/08/11  14:08:12  dadams
; Better comments.
;
; Revision 1.5  1995/08/11  09:54:01  dadams
; 1) More cleanup.
; 2) Rewrote appt-sort-list (call sort*).
; 3) New default values of appt-disp-window and appt-delete-window.
; 4) Commented out: appt-select-lowest-window, appt-disp-window, and
;    appt-delete-window.
;
; Revision 1.4  1995/08/11  08:17:04  dadams
; 1) Moved appt+.el code here (everything): show-reminders,
;    appt-delete-reminder, appt, and key binding for `t' in calendar-mode-map.
; 2) Require cl.el, drew-util-19.el, drew-misc-19.el.
; 3) Cleaned up code and comments.
; 4) appt-check: Fixed bug in calculation of appt just before midnight.
; 5) Renamings: appt-message-warning-time -> appt-msg-warning-time,
;    cur-comp-time -> cur-time, appt-comp-time -> appt-time.
;
; Revision 1.3  1995/08/11  06:27:20  dadams
; 1) appt-add, appt-delete: show-reminders at end.
; 2) appt-make-list: Put whole time-string, not appt-time-string, in
; time-msg.
;
; Revision 1.2  1995/08/04  14:45:42  dadams
; Require calendar.el.  Provide this.  Added header.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comments, corrections, and improvements should be sent to Drew
;; Adams. The original author is Neil M. Mager
;; <neilm@juliet.ll.mit.edu>, tel. (617) 981-4803
;;
;; Thanks to  Edward M. Reingold for much help and many suggestions, 
;; And to many others for bug fixes and suggestions. - N. Mager
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cl) ;; multiple-value-bind, incf, delete-if, pushnew
              ;; caar, caaar, caddr, cdadr, setf, dolist

(require 'calendar) ;; calendar-current-date, calendar-mode-map,
                    ;; calendar-date-compare,
                    ;; calendar-read, calendar-gregorian-from-absolute,
                    ;; calendar-absolute-from-gregorian, calendar-date-equal,
                    ;; calendar-date-string, calendar-day-name,
                    ;; calendar-date-display-form, european-calendar-style
                    ;; sexp-diary-entry-symbol, update-calendar-mode-line

(require 'calendar+ nil t) ;; (no error if not found): calendar-mouse-3-menu-items
(require 'frame-cmds nil t) ;; (no error if not found): show-a-frame-on
(require 'misc-fns nil t) ;; (no error if not found): 
                          ;; current-line, live-buffer-name

;; Free variables referenced here:
;; ORIGINAL-DATE, NUMBER, DIARY-ENTRIES-LIST, DISPLAYED-MONTH, DISPLAYED-YEAR.

;;;;;;;;;;;;;;;;;;;;;


;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :group 'basic-faces))

(defvar view-appointments-initially nil
  "*Non-nil => Display today's appointments whenever `appt-make-list' is \
called.
If `appt-make-list' is used as a `diary-hook', this will be whenever
`diary' is called.")

;;;###autoload
(defcustom appt-checking-p t
  "*Non-nil means check for appointments in the diary buffer.
To be detected, the diary entry must have the time
as the first thing on a line."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-message-warning-time 15
  "*Number of minutes before an appointment to start reminding."
  :type 'integer
  :group 'appt)

;;;###autoload
(defcustom appt-audible t
  "*Non-nil => Beep to indicate appointment."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-visible t
  "*Non-nil => Appointment msgs in echo area, unless appt-msg-window."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-display-mode-line t
  "*Non-nil => Display minutes to appointment and time in mode-line."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-msg-window t
  "*Non-nil => Display appointment message in another window."
  :type 'boolean
  :group 'appt)

;;;###autoload
(defcustom appt-display-duration 10
  "*Seconds to display appointments window.  Non-number => indefinitely."
  :type 'sexp
  :group 'appt)

;;;###autoload
(defcustom appt-display-diary t
  "*Non-nil => Display the next day's diary on the screen. 
This will occur at midnight when the appointment list is updated."
  :type 'boolean
  :group 'appt)

(defvar appt-time-msg-list nil
  "List of today's appointments reminders.
The list is generated from today's `diary-entries-list'.
Use `appt-add' and `appt-delete' to add and delete appointments.
Each appointment reminder in the list is of the form ((TIME) . MSG),
where TIME is the time of the appointment in minutes past midnight,
and MSG is the appointment reminder message.")

(defconst appt-max-time 1439
  "11:59pm, in minutes - number of minutes in a day, minus 1.")

(defcustom appt-display-interval 3
  "*Number of minutes to wait between checking the appointment list.
Also, the interval between repetitions of the same reminder."
  :type 'integer
  :group 'appt)
  
(defvar appt-buffer-name "*Today's Appointments*"
  "*Name of appointments buffer.")
(put 'appt-buffer-name 'variable-interactive "sName of appointments buffer: ")

(defvar appt-disp-window-function 'appt-remind
  "*Function called to display appointments window.")
(put 'appt-disp-window-function 'variable-interactive
     "aFunction to display appointments window: ")

(defvar appt-delete-window-function 'appt-hide-reminders
  "*Function called to remove appointments window.")
(put 'appt-delete-window-function 'variable-interactive
     "aFunction to remove appointments window: ")

(defvar appt-mode-string nil
  "String displayed in mode line to say you have an appointment.
It includes the amount of time until the appointment.")

(defvar appt-prev-comp-time nil
  "Time of day (mins since midnight) when appointments last checked.")

(defvar appt-now-displayed nil
  "Non-nil when we have started notifying about a appointment that is near.")

(defvar appt-display-count nil)

(defconst appt-time+msg-regexp
 "^.*\\b\\([0-2]?[0-9]\\)[:h]\\([0-5][0-9]\\)?\\(am\\|pm\\)?\\b\\(.*\\)"
  "Regexp for appointment reminder: appointment time plus reminder message.
Examples of allowed time formats: 2:15, 02:15, 2:15am, 02:15am,
2h15, 02h15, 2h15am, and 02h15am are all allowed and equivalent.")

;;;;;;;;;;;;;;;;

;; Keymap for appt-buffer-name buffer.
(defvar reminders-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'save-reminders)
    (define-key map "\C-cj" 'appt-add)
    (define-key map "\C-ck" 'appt-delete)
    ;; From calendar-mode-map, (with C-c prefix):
    (define-key map "\C-cd" 'clear-appointments)
    (define-key map "\C-cs" 'show-all-diary-entries)
    (define-key map "\C-cid" 'appt-insert-diary-entry)
    (define-key map "\C-ciw" 'appt-insert-weekly-diary-entry)
    (define-key map "\C-cim" 'appt-insert-monthly-diary-entry)
    (define-key map "\C-ciy" 'appt-insert-yearly-diary-entry)
    (define-key map "\C-cia" 'appt-insert-anniversary-diary-entry)
    (define-key map "\C-cic" 'appt-insert-cyclic-diary-entry)
    map)
  "Keymap for `reminders-mode'.")

;; Keymap for *Calendar* buffer.
(when (and (boundp 'calendar-mode-map) (keymapp calendar-mode-map))
  (define-key calendar-mode-map "r" appt-disp-window-function)
  (define-key calendar-mode-map "j" 'appt-add)
  (define-key calendar-mode-map "k" 'appt-delete)
  (define-key calendar-mode-map [menu-bar diary appt-separator] '("--"))
  (define-key calendar-mode-map [menu-bar diary appt-delete]
    '("Delete Appointment" . appt-delete))
  (define-key calendar-mode-map [menu-bar diary appt-add]
    '("Add Appointment" . appt-add))
  (define-key calendar-mode-map [menu-bar diary appt-remind]
    (cons "Show Appointments" appt-disp-window-function)))

;; Add items to Mouse 3 popup menu:
(when (and (not (featurep 'appt))       ; Only do it once.
           (boundp 'calendar-mouse-3-menu-items) ; Defined in `calendar+.el'.
           (consp calendar-mouse-3-menu-items)
           (consp (cadr calendar-mouse-3-menu-items)))
  (setf (cdadr calendar-mouse-3-menu-items)
        (nconc (cdadr calendar-mouse-3-menu-items)
               (list '("--")
                     (cons "Show Appointments" appt-disp-window-function)
                     '("Add appointment" . appt-add)
                     '("Delete appointment" . appt-delete)))))
              
;;;;;;;;;;;;;;;;

;; Update mode line for *Calendar* buffer.
(setq cal-mode-line-key-help            ; Defined in calendar+.el
      (concat (and (boundp 'cal-mode-line-key-help)
                   cal-mode-line-key-help)
              " \\<calendar-mode-map>\\[appt-add]\\[appt-delete]\
\\[appt-remind]"))
(setq calendar-mode-line-format         ; Defined in `calendar+.el'
      (list
       (substitute-command-keys
        "<=\\<calendar-mode-map>\\[scroll-calendar-right-three-months]")
       (substitute-command-keys (concat "[" cal-mode-line-key-help "]"))
       (substitute-command-keys
        "(\\<calendar-mode-map>\\[calendar-goto-info-node] = help)")
       (calendar-date-string (calendar-current-date) t)
       (substitute-command-keys
        "\\<calendar-mode-map>\\[scroll-calendar-left-three-months]=>")))
(update-calendar-mode-line)

(defsubst minutes-past-midnight ()
  "Returns the number of minutes past midnight, at the current time."
  (let ((time (current-time-string)))
    (+ (* 60 (string-to-number (substring time 11 13))) ; Current hour X 60.
       (string-to-number (substring time 14 16))))) ; Minutes past the hour.

(defsubst appt-delete-past-appts ()
  "Delete past appointments from APPT-TIME-MSG-LIST, destructively."
  (let ((cur-time (minutes-past-midnight)))
    (setq appt-time-msg-list
          (delete-if (function (lambda (ap) (< (caar ap) cur-time)))
                     appt-time-msg-list))))

(defun appt-check ()
  "Check for an appointment now (or soon).
An appointment time must come first in a diary line.
The format of the time can be either 24 hour or am/pm.
Example: 
               02/23/89
                 18:00 Dinner
            
              Thursday
                11:45am Lunch meeting.

The following variables control appointment notification:
 `appt-checking-p'
        Non-nil => The diary buffer is checked for appointments.
 `appt-message-warning-time'
        Used to determine if appointment message should be displayed.
 `appt-audible'
        Non-nil (default) => Appointment is signalled by a beep.
 `appt-visible'
        Non-nil (default) => Appointment msg is in echo area.
 `appt-msg-window'
        Non-nil (default) => Appointment msg is in another window.
        Overrides `appt-visible'.
 `appt-display-duration'
        Number of seconds to display appointments in another window.
        If anything other than a number, display window indefinitely. 
 `appt-display-interval'
        Number of minutes to wait between checking appointments list.
        Also, the interval between repetitions of the same reminder.
 `appt-disp-window-function' 
        Function called to display appointments window.
 `appt-delete-window-function' 
        Function called to remove appointments window."
  (let* ((min-to-app -1)
         (prev-appt-mode-string appt-mode-string)
         (prev-appt-display-count (or appt-display-count 0))
         ;; Non-nil means do a full check for pending appointments
         ;; and display in whatever ways the user has selected.
         ;; When no appointment is being displayed,
         ;; we always do a full check.
         (full-check (or (not appt-now-displayed)
                         ;; This is true every appt-display-interval minutes.
                         (= 0 (mod prev-appt-display-count appt-display-interval))))
         ;; Non-nil means only update the interval displayed in the mode line.
         (mode-line-only (and (not full-check) appt-now-displayed)))
    (when (or full-check mode-line-only)
      (save-excursion
        (let ((cur-time (minutes-past-midnight))
              (msgs-before-checking-p appt-time-msg-list)) ; Some may be past.
          ;; At the first check in any given day, update our 
          ;; appointments to today's list.
          (if (or (null appt-prev-comp-time)
                  (< cur-time appt-prev-comp-time))
              (condition-case nil
                  (progn
                    (if (and view-diary-entries-initially appt-display-diary)
                        (diary)
                      (let ((diary-display-hook 'appt-make-list))
                        (diary))))
                (error nil)))
          (setq appt-prev-comp-time cur-time)
          (setq appt-mode-string nil)
          (setq appt-display-count nil)

          (appt-delete-past-appts)      ; Remove past appointments from list.

          ;; If `appt-checking-p', and there are still `appt-time-msg-list'
          ;; entries, get first time from `appt-time-msg-list' and calculate
          ;; number of minutes until appointment.
          (if (and appt-checking-p appt-time-msg-list)
              (let ((appt-time (caaar appt-time-msg-list)))
                (setq min-to-app (- appt-time cur-time))
                ;; If there is an appointment between midnight and
                ;; `appt-message-warning-time' minutes after midnight,
                ;; begin to issue a message before midnight.
                ;; Midnight is considered 0 minutes and 11:59pm is
                ;; 1439 minutes.  Must recalculate the number of minutes
                ;; to the appointment (`min-to-app').  It is the number of 
                ;; minutes before midnight, plus the number of 
                ;; minutes past midnight of the appointment.
                (when (and (< appt-time appt-message-warning-time)
                           (> (+ cur-time appt-message-warning-time) appt-max-time))
                  (setq min-to-app (+ (- (1+ appt-max-time) cur-time)
                                      appt-time)))
                ;; Remind, if appointment is within `appt-msg-warning' time.
                (when (and (<= min-to-app appt-message-warning-time)
                           (>= min-to-app 0))
                  (setq appt-now-displayed t)
                  (setq appt-display-count (1+ prev-appt-display-count))
                  (unless mode-line-only
                    (cond (appt-msg-window
                           (funcall appt-disp-window-function min-to-app
                                    ;; WAS: (format-time-string "%a %b %e "
                                    ;;                          (current-time))
                                    (substring (current-time-string) 11 16)
                                    ;; WAS: cadar, not cdar
                                    (cdar appt-time-msg-list)) ; Msg.
                           (when (numberp appt-display-duration)
                             (run-at-time (format "%d sec" appt-display-duration)
                                          nil
                                          appt-delete-window-function)))
                          (t
                           (when appt-visible
                             ;; WAS: cadar, not cdar
                             (message "%s" (cdar appt-time-msg-list))) ; Msg.
                           (when appt-audible (beep 1)))))
                  (when appt-display-mode-line
                    (setq appt-mode-string
                          (concat "Appointment in " min-to-app " min. "
                                  (substring (current-time-string) 11 16) " "))
                    (force-mode-line-update))
                  ;; When an appointment is reached,
                  ;; delete it from the list.
                  ;; Reset the count to 0 in case we display another
                  ;; appointment on the next cycle.
                  (when (= min-to-app 0)
                    (pop appt-time-msg-list)
                    (setq appt-display-count nil))))
            ;; There were only past appointments, which have been removed.
            ;; Update reminders buffer, but do not display it.
            (when (and appt-checking-p msgs-before-checking-p)
              (let ((temp-buffer-show-function 'identity))
                (show-reminders))))
          ;; If we have changed the mode line string,
          ;; redisplay all mode lines.
          (when (and appt-display-mode-line
                     (not (equal appt-mode-string
                                 prev-appt-mode-string)))
            (force-mode-line-update t)
            ;; If the string now has a notification,
            ;; redisplay right now.
            (when appt-mode-string (sit-for 0))))))))



;; Sort appointments list chronologically.  Destructively modifies APPT-LIST.
(defsubst appt-sort-list (appt-list) (sort* appt-list '< :key 'caar))

;;;###autoload
(defun appt-add-to-diary (new-appt-time new-appt-msg)
  "Add appointment at TIME to diary. Second arg is REMINDER to add.
The TIME must be in either 24 hour format or am/pm format."
  (interactive
   (list
    (let* ((time (read-from-minibuffer "Appointment time (HH:MM[am/pm]): "))
           (time-regexp
            "\\b\\([0-2]?[0-9]\\)\\([:h]\\)?\\([0-5][0-9]\\)?\
\\(am\\|pm\\)?\\'")
           (time-match (and (string-match time-regexp time) (match-data)))
           (hr (and (nth 2 time-match) (substring time (nth 2 time-match)
                                                  (nth 3 time-match))))
           (am/pm (and (nth 8 time-match) (substring time (nth 8 time-match)
                                                     (nth 9 time-match)))))
      (unless (nth 6 time-match) (setq time (concat hr ":00" am/pm)))
      (unless (and time-match (string-match time-regexp time)) ; Again, w/ 00.
        (error
         "Time must be of form: 9:45pm, 9h45pm, 21:45, 21h45, 9h, 9hpm, 21h."))
      (when am/pm                       ; Explicit "am" or "pm".
        (if (string-match "\\(1[3-9]\\|2[0-9]\\)" hr) ; After noon -- 15h, 21h.
            (if (string= "am" am/pm)    ; "am" input with hour after noon.
                (error "Hour `%s' is incompatible with \"am\" (morning)." hr)
              (setq time (substring time 0 (- (length time) 2)))))) ; Remove pm
      time)
    (read-from-minibuffer "Appointment reminder text: ")))
  (insert-diary-entry
   (concat new-appt-time " " new-appt-msg [?\t] "-- " new-appt-time)))

;;;###autoload
(defun appt-add (new-appt-time new-appt-msg &optional persistent-p)
  "Add appointment for today at TIME.  Second arg is the reminder MESSAGE.
The TIME must be in either 24 hour format or am/pm format.

With prefix arg, also adds (persistent) appointment to your diary.
If the prefix argument is negative, this diary entry is non-marking.
Otherwise, it is marking."
  (interactive
   (list
    (let* ((time (read-from-minibuffer
                 (format "It is now %s.  New appointment time (HH:MM[am/pm]): "
                         (substring (current-time-string) 11 16))))
           (time-regexp
            "\\b\\([0-2]?[0-9]\\)\\([:h]\\)?\\([0-5][0-9]\\)?\
\\(am\\|pm\\)?\\'")
           (time-match (and (string-match time-regexp time) (match-data)))
           (hr (and (nth 2 time-match) (substring time (nth 2 time-match)
                                                  (nth 3 time-match))))
           (am/pm (and (nth 8 time-match) (substring time (nth 8 time-match)
                                                     (nth 9 time-match)))))
      (unless (nth 6 time-match) (setq time (concat hr ":00" am/pm)))
      (unless (and time-match (string-match time-regexp time)) ; Again, w/ 00.
        (error
         "Time must be of form: 9:45pm, 9h45pm, 21:45, 21h45, 9h, 9hpm, 21h."))
      (when am/pm                       ; Explicit "am" or "pm".
        (if (string-match "\\(1[3-9]\\|2[0-9]\\)" hr) ; After noon -- 15h, 21h.
            (if (string= "am" am/pm)    ; "am" input with hour after noon.
                (error "Hour `%s' is incompatible with \"am\" (morning)." hr)
              (setq time (substring time 0 (- (length time) 2)))))) ; Remove pm
      time)
    (read-from-minibuffer "Reminder text: ")
    current-prefix-arg))
  
  (let* ((appt-time-string (concat new-appt-time " " new-appt-msg
                                   [?\t] "-- " new-appt-time)) ; Time rightmost
         (appt-time (list (appt-convert-time new-appt-time)))
         (time+msg (cons appt-time appt-time-string)))
    (when (< (car appt-time) (minutes-past-midnight))
      (error
       "It is already past %s.  Appointment must be later than now (%s%s)."
       new-appt-time (substring (current-time-string) 11 16)
       (if (string< (substring (current-time-string) 11 13) "13") "am" "")))
    (setq appt-time-msg-list
          (appt-sort-list (nconc appt-time-msg-list (list time+msg))))
    (when persistent-p
      (let ((marking-p (natnump (prefix-numeric-value persistent-p))))
        (save-excursion
          (set-buffer (find-file-noselect (substitute-in-file-name diary-file)))
          (goto-char (point-max))
          (insert (if (bolp) "" "\n")
                  (if marking-p "" diary-nonmarking-symbol)
                  (concat (calendar-date-string (calendar-current-date) t t)
                          (and european-calendar-style ":")
                          " " appt-time-string "\n"))
          (save-buffer))
        (message "Appointment has also been recorded in diary%s."
                 (if marking-p "" " (but not marked)")))))
  (funcall appt-disp-window-function))

;;;###autoload
(defun appt-delete ()
  "Delete appointments from the list of today's appointments.
You are prompted to confirm each deletion."
  (interactive)
  (unwind-protect
      (let ((tmp-msg-list appt-time-msg-list))
        (dolist (entry tmp-msg-list)
          (when (y-or-n-p (concat "Delete `" (cdr entry)
                                  "' from today's appointments? "))
            (setq appt-time-msg-list (delq entry appt-time-msg-list))))
        (message ""))
    (funcall appt-disp-window-function)))
                 
;;;###autoload
(defun clear-appointments ()
  "Clear all added (volatile) appointments from the today's appointments,
and reinitialize the list from your diary."
  (interactive)
  (when (or (null appt-time-msg-list)
            (progn (and (fboundp '1on1-flash-ding-minibuffer-frame)
                        (1on1-flash-ding-minibuffer-frame))
                   (yes-or-no-p "Do you really want to clear today's appointment \
list?  \"Yes\" will reinitialize it using your diary. ")))
    (diary)
    (funcall appt-disp-window-function)))


;; Create the appointments list from today's diary buffer. 
;; Examples:
;;             02/23/89
;;               12:00pm lunch
;;
;;             Wednesday
;;               10:00am group meeting.
;;
;; This function is intended as a value for `diary-hook', which is
;; called from `list-diary-entries'.  It is assumed here that the
;; variables `date' and `number' hold the arguments that
;; `list-diary-entries' received.  They specify the range of dates
;; that the diary is being processed for.  It is also assumed that
;; `diary-entries-list' and `original-date' are correctly bound (in
;; `list-diary-entries').
(defun appt-make-list ()
  "Create appointment reminders list from today's diary buffer.
This function is intended as a value for `diary-hook', which is
called from `list-diary-entries'."
  (update-calendar-mode-line) ; Ensure mode-line shows appt help.
  (let* ((today (calendar-current-date))
         (today-cons (list today)))
    ;; Need act if range of dates diary is considering includes current date.
    (when (and (not (calendar-date-compare today-cons (list original-date)))
               (calendar-date-compare
                today-cons
                (list (calendar-gregorian-from-absolute
                       (+ (calendar-absolute-from-gregorian original-date)
                          number)))))   ; `number' is a free var here.
      (save-excursion
        ;; Clear the appointments list, then fill it in from the diary.
        (setq appt-time-msg-list nil)
        (when diary-entries-list
          
          ;; Cycle through the entry-list (diary-entries-list)
          ;; looking for entries beginning with a time. If 
          ;; the entry begins with a time, add it to the
          ;; `appt-time-msg-list'.  Then sort `appt-time-msg-list'.
          (let ((entry-list (sort-diary-entries)) ; Sorts diary-entries-list.
                (rest-time-string ""))
            ;; Skip diary entries for dates before today.
            (while (and entry-list
                        (calendar-date-compare (car entry-list) today-cons))
              (setq entry-list (cdr entry-list)))
            ;; Parse the entries for today.
            (while (and entry-list
                        (calendar-date-equal today (caar entry-list)))
              (let ((time-string (cadar entry-list)))
                (while (string-match appt-time+msg-regexp time-string)
                  (let ((appt-time-string (substring time-string
                                                     (match-beginning 0)
                                                     (match-end 0))))
                    (if (< (match-end 0) (length time-string)) ; Multi-line msg
                        (setq rest-time-string (substring time-string 
                                                          (+ (match-end 0) 1)
                                                          nil))
                      (setq rest-time-string "")) ; No more lines in appt msg.
                    (let* ((appt-time (list (appt-convert-time 
                                             (substring time-string
                                                        (match-beginning 1)
                                                        (or (match-end 3) ; pm
                                                            (match-end 2))))))
                           (time+msg (cons appt-time appt-time-string)))
                      (setq time-string rest-time-string) ; Get next msg line.
                      (setq appt-time-msg-list
                            (nconc appt-time-msg-list (list time+msg)))))))
              (setq entry-list (cdr entry-list))))
          (appt-delete-past-appts)      ; Remove past appointments from list.
          (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))))
      (when view-appointments-initially ; Show, but don't tell.
        (let ((appt-audible nil)) (funcall appt-disp-window-function))))))
  
;;;###autoload
(defun save-reminders ()
  "Take into account any changes made to today's appointment reminders.

NOTE:
 1) Appointment reminders remain volatile (see `reminders-mode' doc).
    They are not saved between Emacs sessions.  They are for today only.

 2) This command does not modify (i.e. update) your diary.
    If you have modified a reminder that you had also previously added
    to your diary, be aware that the diary entry has not been updated."
  (interactive)
  (unless (live-buffer-name appt-buffer-name)
    (error "Cannot save reminders.  Not a live buffer: %s." appt-buffer-name))
  (when (y-or-n-p
         (format "Save changes to appointments as shown in buffer `%s'? "
                 appt-buffer-name))
    (setq appt-time-msg-list nil)       ; Clear the appointments list.
    (save-excursion
      (set-buffer appt-buffer-name)
      
      ;; Add reminders to `appt-time-msg-list'. Then sort it.
      (goto-char (point-min))
      (forward-line 3)
      (while (not (eolp))
        (if (re-search-forward
             appt-time+msg-regexp (save-excursion (end-of-line) (point)) t)
            (let* ((appt-time-string (buffer-substring (match-beginning 0)
                                                       (match-end 0))))
              (setq appt-time-msg-list
                    (nconc appt-time-msg-list
                           (list (cons (list (appt-convert-time
                                              appt-time-string))
                                       appt-time-string)))))
          (error "Bad TIME format on line %d." (current-line)))
        (forward-line 1)))
    (let ((removed-past-p (appt-remove-past-appts))) ; Remove past appts.
      (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))
      (show-reminders)
      (when removed-past-p (ding))
      (message
       (concat "Appointment changes saved.  "
               (and removed-past-p
                    "WARNING: There were past appointments that have been \
removed."))))))

(defun appt-remove-past-appts ()
  "Remove past appointments from `appt-time-msg-list', non-destructively.
Returns non-nil iff appointments were removed."
  (let ((cur-time (minutes-past-midnight))
        (orig-list appt-time-msg-list))
    (setq appt-time-msg-list
          (remove-if (function (lambda (ap) (< (caar ap) cur-time)))
                     appt-time-msg-list))
    (not (equal orig-list appt-time-msg-list))))

(defun appt-convert-time (time2conv)
  "Convert HH[:h]MM[am/pm] format to minutes from midnight."
  (string-match appt-time+msg-regexp time2conv)
  (multiple-value-bind (all1 all2 hr1 hr2 min1 min2 pm1 pm2) (match-data)
    (let ((hr 0)
          (min 0))
      (when hr1 (setq hr (string-to-number (substring time2conv hr1 hr2))))
      (when min1 (setq min (string-to-number (substring time2conv min1 min2))))
      (setq hr (mod hr 24))             ; 26:00 -> 2:00
      (when (and pm1 (< hr 12) (string= "pm" (substring time2conv pm1 pm2)))
        (incf hr 12))                   ; pm -> 24 hour time.
      (+ (* hr 60) min))))

;;;###autoload
(defun reminders-mode ()
  "Major mode for appointment reminders buffer.

The following commands are available in this mode to control today's
appointment reminders:
\\<reminders-mode-map>
\t\\[appt-add]\tAdd an appointment for today.
\t\\[appt-delete]\tDelete some of today's appointments.
\t\\[save-reminders]\tTake into account changes made here to appointments.
\t\\[clear-appointments]\tClear all added appointments.  Display the diary,
\t\tand reinitialize today's appointments from the diary.

\(Bindings in Reminders mode are generally the same as those in
Calendar mode, prefixed by `C-c'.)

Today's appointments were initialized from today's diary entries.
See the doc for `diary-file' and `calendar-mode' for diary info.
Appointment reminders were generated from 1-line diary entries that
take this form:   

\[ DATE ]  MESSAGE-AND-TIME

The DATE is optional.  If present, it must not be preceded on the line
by anything (including whitespace).  If it is not present, and if the
line begins with whitespace (TAB, SPC), then the preceding DATE in the
diary file is used.  The DATE's syntax is as for a normal diary entry.

Aside from the DATE, the entire diary entry line, MESSAGE-AND-TIME, is
used as the appointment reminder message.

The appointment TIME is searched for within the MESSAGE-AND-TIME.  If
there is more than one TIME expression on the entry line, it is the
*last* one that is considered to represent the appointment time.  The
TIME can be expressed in either 24 hour or am/pm notation (e.g. \"13h35\"
or \"1:35pm\").

As an example, here are six appointment diary entries:

02/23/95 18:00 Dinner                                               (1)
Thursday
  10:00 Meeting                                                     (2)
  Lunch: 11:45am                                                    (3)
  3:00pm Meeting                                                    (4)
  Pick up kids at 18:00                                             (5)
31 Oct:  20h30 - Halloween party.  Leave for home early: 18h00.     (6)

Note that the last entry (6) will produce a reminder for 6pm, not for
8:30pm, since \"18h00\" is the last TIME on the line.

Once today's appointments have been generated from the diary, if you
want to make any changes to them you can just edit the list here (in
Reminders mode), then type `\\[save-reminders]' to take the changes into \
account.
Edited reminders here should take the same form as in the diary (see
above):  TIME  MESSAGE.  (The date is unnecessary.)

As an alternative to free editing of appointments here, you can use
`\\[appt-delete]' and `\\[appt-add]' to remove and add them.

Unlike the entries in your `diary-file', appointment reminders are
*volatile*:  They will disappear if you exit Emacs, for instance.
And they are automatically removed once their time has past.

Non-volatile appointment reminders may be created by first making the
appropriate diary entries, then executing `\\[clear-appointments]' (or \
`diary', or
`calendar', or `list-diary-entries').  However, this execution will
regenerate the (volatile) appointments list, causing appointments that
you may have already added (via `\\[appt-add]' and/or `\\[save-reminders]') \
to be lost.

It is possible to cause `\\[appt-add]' to add an appointment to your diary,
in addition to creating it as a volatile reminder, by giving `\\[appt-add]'
a prefix argument.  If the prefix is negative, the new diary entry is
non-marking; otherwise, it is marking.  Diary entries are of course
not deleted automatically, once their time is up, and `\\[appt-delete]' has no
effect on them.

In order to further facilitate access to your diary, the following
commands are also available in Reminders mode:

\t\\[show-all-diary-entries]\tEdit your diary. Show all entries, for all dates.
\t\\[appt-insert-diary-entry]\tInsert a diary entry for today.
\t\\[appt-insert-weekly-diary-entry]\tInsert a weekly diary entry for today.
\t\\[appt-insert-monthly-diary-entry]\tInsert a monthly diary entry for today.
\t\\[appt-insert-yearly-diary-entry]\tInsert a yearly diary entry for today.
\t\\[appt-insert-anniversary-diary-entry]\tInsert an anniversary diary entry \
for today.
\t\\[appt-insert-cyclic-diary-entry]\tInsert a cyclic diary entry, starting \
today.
\t\\[clear-appointments]\tRecreate today's diary entries from `diary-file'.
\t\tWARNING: This also clears today's appointments
\t\t(see above).

Here are all of the Reminders mode key bindings:

\\{reminders-mode-map}"
  (interactive)
  (fundamental-mode)
  (use-local-map reminders-mode-map)
  (let ((today (calendar-current-date)))
    (setq displayed-month (car today))  ; Buffer local.
    (setq displayed-year (caddr today))) ; Buffer local.
  (setq major-mode 'reminders-mode)
  (setq mode-name "Reminders")
  (run-hooks 'reminders-mode-hook))


;; Differs from original GNU version: when `european-calendar-style',
;; this inserts `26 Jan 1995 :' instead of `26 Jan 1995'.  This
;; definition is coupled with that of `european-date-diary-pattern'
;; (which also has " :" at the end of a diary entry).
;;;###autoload
(defun appt-insert-diary-entry (arg)
  "Insert a diary entry for today. Prefix arg makes the entry nonmarking."
  (interactive "P")
  (make-diary-entry
   (concat (calendar-date-string (calendar-current-date) t t)
           (and european-calendar-style " :"))
   arg))

;;;###autoload
(defun appt-insert-weekly-diary-entry (arg)
  "Insert a weekly diary entry for today.
Prefix arg makes the entry nonmarking."
  (interactive "P")
  (make-diary-entry (calendar-day-name (calendar-current-date)) arg))

;;;###autoload
(defun appt-insert-monthly-diary-entry (arg)
  "Insert a monthly diary entry for today.
Prefix arg makes the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " * ")
            '("* " day))))
    (make-diary-entry (calendar-date-string (calendar-current-date) t) arg)))

;;;###autoload
(defun appt-insert-yearly-diary-entry (arg)
  "Insert an annual diary entry for today.
Prefix arg makes the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname)
            '(monthname " " day))))
    (make-diary-entry (calendar-date-string (calendar-current-date) t) arg)))

;;;###autoload
(defun appt-insert-anniversary-diary-entry (arg)
  "Insert an anniversary diary entry for today.
Prefix arg makes the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " month " " year)
            '(month " " day " " year))))
    (make-diary-entry
     (format "%s(diary-anniversary %s)"
             sexp-diary-entry-symbol
             (calendar-date-string (calendar-current-date) nil t))
     arg)))

;;;###autoload
(defun appt-insert-cyclic-diary-entry (arg)
  "Insert a cyclic diary entry starting today.
Prefix arg makes the entry nonmarking."
  (interactive "P")
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " month " " year)
            '(month " " day " " year))))
    (make-diary-entry
     (format "%s(diary-cyclic %d %s)"
             sexp-diary-entry-symbol
             (calendar-read "Repeat every how many days: "
                            '(lambda (x) (> x 0)))
             (calendar-date-string (calendar-current-date) nil t)) arg)))

;; Argument NEW-TIME is not used here, but is present for compatibility with
;; `appt-disp-window-function'.
;;;###autoload
(defun appt-remind (&optional min-to-app new-time appt-msg)
  "Reminds you of today's appointment reminders."
  (interactive)
  (show-reminders)
  (when (fboundp 'show-a-frame-on) (show-a-frame-on appt-buffer-name))
  (save-excursion (set-buffer appt-buffer-name) (reminders-mode))
  (when appt-msg                        ; Called with explicit msg arg.
    (when appt-audible (beep 1))
    (message (if (zerop min-to-app)
                 (concat "NOW:   " appt-msg)
               (concat min-to-app " minutes from now-----" appt-msg))))
  (unless appt-time-msg-list
    (when appt-audible (beep 1))
    (message "No appointments.")))
      
;; (make-local-hook 'temp-buffer-show-hook)
(remove-hook 'temp-buffer-show-hook 'help-mode-finish 'local-only)

(defun show-reminders ()
  "Display list of today's appointment reminders."
  (let* ((date-string (calendar-date-string (calendar-current-date)))
         (len (length date-string)))
    (with-output-to-temp-buffer appt-buffer-name
      (save-excursion (set-buffer appt-buffer-name) (setq buffer-undo-list nil))
      (princ (concat "\t\tAppointments on " date-string "\n\t\t"
                     (make-string (+ 16 len) ?-) "\n\n")) ; 16 is for "Appointm..."
      (mapcar (lambda (ap) (princ (concat (cdr ap) "\n"))) appt-time-msg-list)
      (unless appt-time-msg-list (princ "No appointments."))
      (princ
       (substitute-command-keys
        "\n\nYou can add, delete, or change appointment reminders here,\nthen \
`\\<reminders-mode-map>\\[save-reminders]' to save your changes.  For more \
info on mode: \\[describe-mode]")))))   ; Defined in `help.el'.

(defsubst show-reminders-reminder ()
  ;; Would be more correct if could eval `appt-disp-window-function'
  ;; and put result in the string, instead of hard-wiring-in `appt-remind'.
  (substitute-command-keys
   "To show appointments again: `\\<global-map>\\[appt-remind]', or, in \
calendar buffer, `\\<calendar-mode-map>\\[appt-remind]'."))

(defsubst appt-hide-reminders ()
  "Iconify reminder buffer, `appt-buffer-name'."
  (let ((reminders-window (get-buffer-window appt-buffer-name 'visible)))
    (when reminders-window
      (remove-window reminders-window)
      (message (show-reminders-reminder)))))

;;;###autoload
(defun appt (prefix)
  "Add an appointment, or delete one if prefix arg is non-nil."
  (interactive "P")
  (if prefix
      (appt-delete)
    (call-interactively 'appt-add)))

(defvar appt-timer (run-at-time t 60 'appt-check)
  "Timer used for diary appointment notifications (`appt-check').")

;;;;; DONE VIA `appt-timer' NOW, INSTEAD.
;;;;; (add-hook 'display-time-hook 'appt-check)

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'appt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appt.el ends here
