;;; calendar+.el --- Calendar, diary and appointments stuff.
;;
;; Filename: calendar+.el
;; Description: Calendar, diary and appointments stuff.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 13:11:46 1996
;; Version: 0
;; Last-Updated: Mon Jan  1 10:05:32 2018 (-0800)
;;           By: dradams
;;     Update #: 853
;; URL: https://www.emacswiki.org/emacs/download/calendar%2b.el
;; Keywords: calendar, mouse, local
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `cal-dst', `cal-julian', `cal-menu', `cal-persia', `calendar',
;;   `calendar+', `cl', `diary-lib', `easymenu', `faces',
;;   `lisp-float-type', `misc-fns', `solar'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions and corrections to `calendar.el', `cal-french.el',
;;    and `diary-lib.el'.  Calendar, diary, appointments stuff.
;;    See doc strings of functions `calendar-mode' and `calendar'.
;;
;; NOTE: This code is quite old, and is likely obsolete now.  You
;;       might find it useful in some way - or not. ;-)
;;
;; -------------------------------------------------------------------
;;
;;  Main new functions defined here:
;;
;;    `calendar-count-weekdays-region', `calendar-local-holiday-list',
;;    `calendar-mouse-3-menu', `calendar-mouse-drag-region',
;;    `mark-local-holidays', `monday-after-easter',
;;    `set-calendar-region-from-region', `show-calendar'.
;;
;;  New user options defined here:
;;
;;    `cal-mode-line-key-help', `calendar-local-holiday-marker',
;;    `calendar-region-marker', `weekend-face'.
;;
;;
;;  ***** NOTE: The following functions defined in `calendar.el' have
;;              been REDEFINED HERE:
;;
;;  `calendar-basic-setup' -
;;     1) `list-calendar-holidays' is now called by `calendar' on the
;;        3-month period starting with the current month, not with the
;;        previous month.  The rationale is that you want to be
;;        informed of future, not past, holidays.  This means that the
;;        holidays listed by `calendar', when
;;        `view-calendar-holidays-initially', are one month later than
;;        those listed by `list-calendar-holidays'.
;;     2) Fixed bug: ARG's value & sign was not being taken into
;;        account previously.  Now, distinguish between the case where
;;        ARG is explicitly numeric (in which case, use its value as
;;        is), and the C-u-only case (in which case, prompt for month
;;        and year).
;;  `calendar-count-days-region' - Marks days in region if visible.
;;  `calendar-mode' - New doc string.
;;  `generate-calendar-month' - Puts weekend days in `weekend-face'.
;;  `mark-visible-calendar-date' -
;;     Doesn't add a new overlay if one is already present with face
;;     MARK. (To allow for overlapping overlays.) Arg PRIORITY is new.
;;
;;
;;  ***** NOTE: The following function defined in `solar.el'
;;              has been REDEFINED HERE:
;;
;;  `solar-sunrise-sunset-string' - Avoid being seen as appt time.
;;
;;
;;  ***** NOTE: The following function defined in `diary-lib.el'
;;              has been REDEFINED HERE:
;;
;;  `insert-diary-entry' -
;;     When `european-calendar-style', this inserts `26 Jan 1995 :'
;;     instead of `26 Jan 1995'.  NOTE: This definition is coupled
;;     with that of `european-date-diary-pattern' (which also has " :"
;;     at the end of a diary entry).
;;
;;
;;  The following binding is made here for mode `calendar-mode'.
;;
;;    `='              `calendar-count-days-region'
;;
;;
;; This file redefines a few standard calendar functions.  It should
;; be loaded after loading any of the GNU files `calendar.el',
;; `cal-french.el', or `diary-lib.el'.  So, in your `~/.emacs' file,
;; do this:
;;        (eval-after-load "calendar" '(require 'calendar+))
;;        (eval-after-load "cal-french" '(require 'calendar+))
;;        (eval-after-load "diary-lib" '(require 'calendar+))
;;
;; Alternatively, you can put these autoloads in your `~/.emacs' file:
;;
;;    (autoload 'calendar "calendar+"
;;              "Display a 3-month calendar in another window." t)
;;    (autoload 'insert-diary-entry "calendar+"
;;              "Insert a diary entry for date indicated by point." t)
;;
;;
;; The companion file `cal-opts.el' sets a number of options regarding
;; the calendar, diary, etc.  Put (require 'cal-opts.el) in your
;; `~/.emacs' file if you like most of what is there, then modify what
;; you don't like.  Here are some things you might want to do in your
;; `~/.emacs' file, to counter individual settings from `cal-opts.el':
;;
;; (setq mark-holidays-in-calendar nil) ; Don't mark holidays.  Faster.
;; (setq mark-diary-entries-in-calendar nil) ; Don't mark diary entries.
;; (setq holidays-in-diary-buffer nil)  ; Don't list holidays in diary.
;; (setq general-holidays nil)          ; Get rid of U.S. holidays.
;; (setq christian-holidays nil)        ; Get rid of religious holidays.
;; (setq hebrew-holidays nil)           ;              "              "
;; (setq islamic-holidays nil)          ;              "              "
;; (setq calendar-latitude LAT)   ; Set LAT to your latitude.
;; (setq calendar-longitude LONG) ; Set LONG to your longitude.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/03 dadams
;;     Removed autoload cookies from defvars and non-interactive defuns.
;; 2010/03/29 dadams
;;     Use calendar-(update-mode-line|generate-window). Thx to Jean-Luc Rene.
;;
;; RCS Revision 1.5  2001/01/02 23:00:37  dadams
;; RCS 1. Optional require of solar.el via 3rd arg=t now.
;; RCS 2. solar-sunrise-sunset-string: require solar.el.
;; RCS
;; RCS Revision 1.3  2000/11/28 19:18:34  dadams
;; RCS Optional require's via 3rd arg=t now.
;; RCS
;; RCS Revision 1.2  2000/09/27 21:45:34  dadams
;; RCS Updated for Emacs 20.7:
;; RCS 1. Protected require's.
;; RCS 2. Autoload, don't require holidays.el.
;; RCS 3. Added exit-calendar to calendar-mouse-3-menu-items.
;; RCS 4. Added view-diary-entries to calendar-mode-map.
;; RCS 5. european-date-diary-pattern: added "backup", dayname \\s* -> \\W.
;; RCS 6. american-date-diary-pattern: dayname \\s* -> \\W.
;; RCS 7. defvar's -> defcustom's.
;; RCS 8. calendar-region-marker: Pale Goldenrod -> LightGray.
;; RCS 9. new-diary-file-doc: added diary-chinese-date etc.
;; RCS 10. calendar-basic-setup, show-calendar: no longer setup-calendar-frame.
;; RCS 11. Removed: setup-calendar-frame, diary-french-date.
;; RCS 12. calendar-basic-setup: read date if arg; increment-calendar-month.
;; RCS 13. Protected make-transient-mark-mode-buffer-local with fboundp.
;; RCS 14. generate-calendar-month: use calendar-day-name vs hard-coded.
;; RCS
;; RCS Revision 1.1  2000/09/13 20:06:13  dadams
;; RCS Initial revision
;; RCS
; Revision 1.9  1999/10/28  10:55:40  dadams
; calendar: Fixed previous fix when no arg given.
;
; Revision 1.8  1999/10/28  09:48:32  dadams
; Require strings when compile.
;
; Revision 1.7  1999/10/28  09:47:20  dadams
; calendar: Fixed bug when user entered empty month name (via C-u).
;
; Revision 1.6  1999/10/26  11:14:36  dadams
; 1. Use calendar-mouse-3-menu, not calendar-mouse-3-map:
; 2. Added: calendar-mouse-3-menu-items, calendar-mouse-3-menu.
; 3. Quote calendar-date-string sexp in calendar-mode-line-format.
;
; Revision 1.5  1999/10/25  11:22:49  dadams
; insert-diary-entry: Append " :", not ":", when european-calendar-style.
;
; Revision 1.4  1999/10/25  08:17:28  dadams
; 1. In calendar-mode-line-format:
;       describe-calendar-mode -> calendar-goto-info-node.
; 2. calendar: create buffer if it doesn't exist (e.g. no longer exists).
;
; Revision 1.3  1999/10/07  13:12:18  dadams
; 1. Require solar.el.
; 2. Added: setup-calendar-frame.  Moved calendar frame creation to it.
; 4. show-calendar: Create buffer.  If not in mode setup-calendar-frame.
; 5. calendar: If not in mode setup-calendar-frame.
;
; Revision 1.2  1999/10/07  09:44:39  dadams
; Added ;;;###autoload show-calendar
;
; Revision 1.1  1999/10/07  09:32:35  dadams
; Initial revision
;
; Revision 1.17  1996/08/02  11:53:21  dadams
; 1. Calendar frame: No longer set cursor-color and mouse-color.
; 2. No longer set calendar-today-face.
; 3. Set holiday-face (PaleTurquoise).
; 4. Change background of local-holiday-face to Pink.
;
; Revision 1.16  1996/07/01  13:41:26  dadams
; (trivial)
;
; Revision 1.15  1996/07/01  12:47:41  dadams
; Minor changes to Commentary.
;
; Revision 1.14  1996/06/14  12:48:47  dadams
; Updated file header Commentary to mention new fns and vars defined here.
;
; Revision 1.13  1996/06/12  12:26:19  dadams
; 1. New doc string for calendar-location-name.
; 2. Rewrote doc string for calendar-mode, and cleaned up others.
;
; Revision 1.12  1996/06/06  13:17:01  dadams
; Update of file dependency comments (e.g. "Autoloaded from...").
;
; Revision 1.11  1996/04/26  08:51:37  dadams
; Put escaped newlines on long-line strings.
;
; Revision 1.10  1996/04/12  09:54:35  dadams
; monday-after-easter: Fixed bug: Added missing `unless'.
;
; Revision 1.9  1996/04/05  14:04:58  dadams
; Improved Commentary:  List redefinitions.
;
; Revision 1.8  1996/03/19  08:22:21  dadams
; Set calendar frame width to 80.
;
; Revision 1.7  1996/03/18  13:22:56  dadams
; mark-local-holidays: defsubst -> defun.
;
; Revision 1.6  1996/03/14  11:53:18  dadams
; calendar-local-holiday-list: defun -> defsubst.
;
; Revision 1.5  1996/03/12  15:15:30  dadams
; Require holidays.el when compile.
;
; Revision 1.4  1996/03/12  15:07:23  dadams
; mark-local-holidays: defun -> defsubst (inline).
;
; Revision 1.3  1996/03/06  13:58:49  dadams
; Require frame-cmds.el for rename-frame when compile (else autoloaded).
;
; Revision 1.2  1996/03/06  08:20:09  dadams
; drew-util-19.el -> misc-fns.el.
;
; Revision 1.1  1996/03/05  12:32:45  dadams
; Initial revision
;;
;; Previous Change Log (as `drew-cal.el'):
;;
; Revision 1.29  1996/03/05  12:10:38  dadams
; 1. Copyright.  2. drew-cal-opts.el -> cal-opts.el.  3. Renamed this to
; calendar+.el.
;
; Revision 1.28  1996/02/27  10:18:03  dadams
; Put only calendar-buffer on special-display-buffer-names list.  Others
; (fancy-diary-buffer, holiday-buffer) rely on special-display-regexps.
;
; Revision 1.27  1996/02/12  09:19:43  dadams
; Updated header keywords (for finder).
;
; Revision 1.26  1996/02/06  10:51:30  dadams
; (trivial) Do face stuff only if set-face-foreground is bound.
;
; Revision 1.25  1996/01/08  13:42:56  dadams
; calendar-region-marker: Aquamarine -> Pale Goldenrod.
;
; Revision 1.24  1995/12/28  14:57:55  dadams
; Added ;;;###autoloads.
;
; Revision 1.23  1995/12/12  14:35:58  dadams
; Set fancy-diary-buffer here, after calendar.el has been loaded, since
; calendar.el clobbers it.  (It was set in drew-cal-opts.el, to no
; avail.)
;
; Revision 1.22  1995/11/22  15:12:53  dadams
; Moved diary-french-date here from drew-util-19.el.
;
; Revision 1.21  1995/11/16  14:44:25  dadams
; 1) Require diary-ins.el and calendar.el.  Don't require appt.el for
;    compiling.  Require drew-cal.el (after provide), to ensure loaded
;    before compiling.
; 2) File is no longer to be loaded via calendar-load-hook, but rather
;    via autoload.
; 3) Remove parameter and hook settings to new file drew-cal-opts.el,
;    to be loaded before this (thus before calendar.el etc.).
; 4) All calendar, diary, etc. buffers are special-display now.
; 5) Added calendar-mode from calendar.el, just to redefine doc string.
; 6) calendar: Make transient-mark-mode permanently buffer-local, and
;    turn it off, here, instead of doing it via initial-calendar-window-hook.
; 7) insert-diary-entry: a) No longer require diary-ins.el here.
;                        b) only add `:' if european-calendar-style.
; 8) calendar-mouse-drag-region: triple click corrected so don't lose
; region.
;
; Revision 1.20  1995/11/14  12:32:34  dadams
; 1) Added calendar-count-weekdays-region, & added it to calendar-mouse-3-map.
; 2) Bound `=' to calendar-count-days-region (same as `M-=').
; 3) calendar-count-days-region: a) Added arg. b) Removed unused args
;    point & mark.  c) Put message at end, outside of let. d) Don't
;    reposition cursor if no highlighting done.
; 4) Changed mouse-color.
;
; Revision 1.19  1995/11/10  16:46:55  dadams
; Added (require ...) to redefinitions of std fns to avoid original
; definitions being loaded after redefinitions
; (e.g. insert-diary-entry).
;
; Revision 1.18  1995/11/09  17:32:19  dadams
; 1) Remove Exit from MB-3 menu.  2) Use update-calendar-mode-line.
;
; Revision 1.17  1995/11/09  16:31:35  dadams
; Minor correction to calendar-mode-line-format.
;
; Revision 1.16  1995/11/09  15:18:31  dadams
; Major changes.
; 1) Turn off transient mark mode (and make it buffer local) via
;    initial-calendar-window-hook.  So require drew-util-19.el.
; 2) Moved here from appt.el: removal of Moon from menu bar.
; 3) Added calendar-count-days-region to calendar-mouse-3-map.
;    Bound calendar-mouse-drag-region to MB1, instead of mouse-drag-region.
; 4) Only add local-holidays to calendar-holidays if not already there.
; 5) Improved mode line.  Display time.  Added cal-mode-line-key-help.
; 6) Added calendar-count-days-region (replaces original): marks days in
;    region.  Added calendar-region-marker.
; 7) Added generate-calendar-month (replaces original): dims weekends.
;    Added weekend-face.
; 8) Added mark-visible-calendar-date (replaces original):
;    1) Added PRIORITY arg.  2) Doesn't add mark if already there.
; 9) Added calendar-mouse-drag-region.  Use instead of mouse-drag-region.
; 10) Added set-calendar-region-from-region.
;
; Revision 1.15  1995/10/24  12:51:51  dadams
; calendar-week-start-day is now 1 (Monday), not 0 (Sunday).
; Infinite appt-display-duration (t) by default now.
;
; Revision 1.14  1995/09/11  07:20:16  dadams
; 1) Require calendar when compile (for macros).
; 2) Removed autoload's (since use as calendar-load-hook).
;
; Revision 1.13  1995/09/07  15:07:51  dadams
; Added new redefn of insert-diary-entry that provides ":" after date.
;
; Revision 1.12  1995/09/04  13:44:01  dadams
; Changed header to GNU std.
;
; Revision 1.11  1995/09/04  07:32:27  dadams
; Updated doc of diary-file (new date formats).  Added new-diary-file-doc.
;
; Revision 1.10  1995/09/01  14:41:21  dadams
; 1) Redefined american-date-diary-pattern and european-date-diary-pattern.
; 2) Use european-calendar.
;
; Revision 1.9  1995/09/01  07:20:31  dadams
; 1) Use european date form and 24 hour time form.
; 2) Corrected latitude, longitude.
;
; Revision 1.8  1995/08/31  09:34:10  dadams
; Put "To create the calendar..." msg inside empty Calendar frame.
;
; Revision 1.7  1995/08/30  15:25:52  dadams
; 1) Added eval-when-compile (require appt).
; 2) Removed (require calendar), because now assume this is loaded via
;    calendar-load-hook.
; 3) Define local-holidays and (redefine) calendar-holidays here.
; 4) Force mode-line update.
; 5) Don't call calendar or appt-check here.
;
; Revision 1.6  1995/08/30  07:22:49  dadams
; 1) Added view-appointments-initially. 2)
; appt-issue-message->appt-checking-p.
;
; Revision 1.5  1995/08/24  13:13:27  dadams
; No scroll bar on Calendar frame.
;
; Revision 1.4  1995/08/22  13:57:24  dadams
; (trivial) Removed time from mode-line.  (It's not updated by
; display-time.)
;
; Revision 1.3  1995/08/22  12:46:06  dadams
; Added redefinition of calendar: 1) Initial holiday list is future.
;                                 2) Take numerical ARG into account.
;
; Revision 1.2  1995/08/22  06:39:06  dadams
; 1) Calendar window changes: cursor-color, mouse-color, mode-line,
;    calendar-today-face.
; 2) Added: monday-after-easter, calendar-local-holiday-marker,
;    mark-local-holidays, calendar-local-holiday-list.
; 3) Moved local-holidays assignment to ~/.emacs (commented out here).
; 4) today-[in]visible-calendar-hook are now mark-local-holidays.
;
; Revision 1.1  1995/08/18  09:42:12  dadams
; Initial revision
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

(require 'calendar)                     ; Ensure loaded first, because
(require 'diary-lib)                    ; some stuff is redefined here.
(require 'cl) ;; union, pop, unless, when, incf, decf, case, pushnew, caar

(require 'solar nil t) ;; (no error if not found): solar-sunrise-sunset
(require 'misc-fns nil t) ;; (no error if not found):
                          ;; make-transient-mark-mode-buffer-local

(autoload 'filter-visible-calendar-holidays "holidays")


;;;;;;;;;;;;;;;;;;;;;;

(provide 'calendar+)
(require 'calendar+)                    ; Ensure loaded before compile.

;;; Free variables here: DISPLAYED-YEAR, DISPLAYED-MONTH.

;;;;;;;;;;;;;;;;;;;;;;


;; It does no good to set this before `calendar.el' has been loaded,
;; because `calendar.el' does a defconst to it.
(setq fancy-diary-buffer "*Diary Entries*")


(defvar calendar-mouse-3-menu-items
  (list "Calendar"
        (list ""
              '("Show Diary" . show-all-diary-entries)
              '("Lunar Phases" . calendar-phases-of-moon)
              '("Unmark All" . calendar-unmark)
              '("Mark Holidays" . mark-calendar-holidays)
              '("List Holidays" . list-calendar-holidays)
              '("Mark Diary Entries" . mark-diary-entries)
              '("Scroll Backward" . scroll-calendar-right-three-months)
              '("Scroll Forward" . scroll-calendar-left-three-months)
              '("--")
              '("Count Region Days" . calendar-count-days-region)
              '("Count Region Weekdays" . calendar-count-weekdays-region)
              '("--")
              '("Exit calendar" . exit-calendar))))

;;;###autoload
(defun calendar-mouse-3-menu (event)
  "Pop up menu for Mouse-3 in the calendar window."
  (interactive "e")
  (let ((selection (x-popup-menu event calendar-mouse-3-menu-items)))
    (and selection (call-interactively selection))))


;; Keymaps for *Calendar* buffer.
(let ((map (current-local-map)))
  ;; `calendar-mode-map':
  (when (and (boundp 'calendar-mode-map) (keymapp calendar-mode-map))
    (define-key calendar-mode-map [down-mouse-3] 'calendar-mouse-3-menu)
    (define-key calendar-mode-map [C-down-mouse-3] 'calendar-mouse-3-menu)
    (define-key calendar-mode-map [mouse-3] 'ignore)

    (use-local-map calendar-mode-map)

    (local-unset-key [menu-bar moon])   ; Remove "Moon" entry from menu bar.
    (define-key calendar-mode-map [down-mouse-1] 'calendar-mouse-drag-region)
    (define-key calendar-mode-map "=" 'calendar-count-days-region)
    ;; Bug fix: this got left out / overwritten by "Other File".
    (define-key calendar-mode-map [menu-bar diary view-cursor]
      '("Cursor Date" . view-diary-entries))
    )
  (use-local-map map))


;;; Diary entry patterns.

;; Like original, but `26 Jan 1995 :' instead of `26 Jan 1995'.
;; This is necessary, in order for diary entry to allow TIME right after year.
;; (Also, this definition is more flexible concerning whitespace.)
;; NOTE: This definition is coupled with the definition of `insert-diary-entry'
;;       (which also puts " :" at the end of a diary entry).
;;        --- See `insert-diary-entry', below.
(setq european-date-diary-pattern
      '((day "/" month "\\s-*[^/0-9][ \t]*:?\\s-*") ; `26/1 :'
        (day "/" month "/" year "\\s-*[^0-9][ \t]*:?\\s-*") ; `26/1/95 :'
        (backup day "[ \t]*" monthname "[ \t]*:?\\s-*") ; `26 Jan :'
        (day "[ \t]*" monthname "[ \t]*" year "[ \t]*:?\\s-*") ; `26 Jan 1995 :'
        (dayname "\\W")))               ; `Fri'
;;;;;;;; (european-calendar)

;; More flexible than original, concerning whitespace.
(setq american-date-diary-pattern
      '((month "/" day "\\s-*[^/0-9]")  ; `1/26'
        (month "/" day "/" year "\\s-*[^0-9]") ; `1/26/95'
        (monthname "[ \t]*" day "\\s-*[^,0-9]") ; `Jan 26'
        (monthname "[ \t]*" day ",[ \t]*" year "\\s-*[^0-9]") ; `Jan 26, 1995'
        (dayname "\\W")))               ; `Fri'


(defvar weekend-face
  (and (fboundp 'set-face-background)
       (prog1 (make-face 'weekend-face)
         (set-face-background 'weekend-face "Gray")))
  "*Default face used in *Calendar* buffer to display weekend days.")

(defvar cal-mode-line-key-help
  "\\<calendar-mode-map>\\[calendar-goto-today]\\[calendar-other-month]g \
p\\[calendar-cursor-holidays] \\[view-diary-entries]i\
\\[show-all-diary-entries] \\[calendar-unmark]\\[mark-calendar-holidays]\
\\[mark-diary-entries] \\[calendar-phases-of-moon]\\[calendar-sunrise-sunset]"
  "*Help string to be put in *Calendar* buffer's mode-line." )

;;; *Calendar* window gets a special mode-line.
(setq calendar-mode-line-format
      (list
       (substitute-command-keys
        "<=\\<calendar-mode-map>\\[scroll-calendar-right-three-months]")
       (substitute-command-keys (concat "[" cal-mode-line-key-help "]"))
       (substitute-command-keys
        "(\\<calendar-mode-map>\\[calendar-goto-info-node] = help)")
       '(calendar-date-string (calendar-current-date) t)
       (substitute-command-keys
        "\\<calendar-mode-map>\\[scroll-calendar-left-three-months]=>")))
(if (fboundp 'calendar-update-mode-line)
    (calendar-update-mode-line)
  (update-calendar-mode-line))

;;;###autoload
(defcustom diary-entry-marker
  (if (not window-system)
      "+"
    (require 'faces)
    (make-face 'diary-face)
    (if (x-display-color-p)
        (set-face-foreground 'diary-face "Red")
      (copy-face 'bold 'diary-face))
    'diary-face)
  "*Used to mark dates that have diary entries.
Can be either a single-character string or a face."
  :type '(choice string face)
  :group 'diary)

;;;###autoload
(defcustom calendar-region-marker
  (if (not window-system)
      "X"
    (require 'faces)
    (make-face 'calendar-region-face)
    (if (x-display-color-p)
        (set-face-background 'calendar-region-face "LightGray")
      (copy-face 'bold 'calendar-region-face))
    'calendar-region-face)
  "*Used to mark days in calendar region.
Can be either a single-character string or a face."
  :type '(choice string face)
  :group 'calendar)

;;; Face for (non-local) holidays.
(when (fboundp 'set-face-foreground)
  (set-face-background 'holiday-face "PaleTurquoise"))

;; Used for local (e.g. legal) holidays.
;; (`calendar-holiday-marker' is used for all others.)
;;;###autoload
(defcustom calendar-local-holiday-marker
  (if (not window-system)
      "*"
    (require 'faces)
    (make-face 'local-holiday-face)
    (if (x-display-color-p)
        (set-face-background 'local-holiday-face "Pink")
      (set-face-background 'local-holiday-face "Black")
      (set-face-foreground 'local-holiday-face "White"))
    'local-holiday-face)
  "*Used to mark local holidays in the calendar.
Can be either a single-character string or a face."
  :type '(choice string face)
  :group 'holidays)

;;; Correct the doc string of `calendar-location-name':
;;; Flipped ordered pair: latitude should be first.
(put 'calendar-location-name 'variable-documentation
     "*Sexp that evals to name of `calendar-latitude', `calendar-longitude',
which is the calendar's geographical position, e.g. \"New York City\".
Default just gives the latitude, longitude pair, as in \"48.3N, 3.7E\".")

;;; Change doc string of `diary-file' to reflect new diary entry patterns.
(defconst new-diary-file-doc
  "*Name of the file in which your personal diary of dates is kept.

The diary file's entries are lines that begin with a date in any of
the following formats.  (This is the American date notation---see
below for European notation):

            MONTH/DAY
            MONTH/DAY/YEAR
            MONTHNAME DAY
            MONTHNAME DAY, YEAR           (Note the `,' in this form.)
            DAYNAME

The remainder of a diary entry line is the entry string proper for the
given date.

 - MONTH and DAY are one or two digit numbers.

 - YEAR is a number and may be written in full or abbreviated to the
   final two digits.  If the date does not explicitly contain a YEAR,
   it is generic and applies to any year.
 - DAYNAME entries apply to any date which is on that day of the week.
 - MONTHNAME and DAYNAME can be spelled in full, abbreviated to three
   characters (with or without a period), capitalized or not.
 - Any of DAY, MONTH, or MONTHNAME, YEAR can be `*' which matches any
   day, month, or year, respectively.

The European style (in which the day precedes the month) can be used
instead, if you execute `european-calendar' when in the calendar, or
set `european-calendar-style' to t in your ~/.emacs file.
The European forms are:

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR :          (Note the `:' in this form.)
            DAYNAME

To revert to the default American style from the European style,
execute `american-calendar' in the calendar.

A diary entry can be preceded by the character
`diary-nonmarking-symbol' (ordinarily `&') to make that entry
nonmarking--that is, it will not be marked on dates in the calendar
window but will appear in a diary window.

Multiline diary entries are made by indenting lines after the first
with either a TAB or one or more spaces.

Lines not in one the above formats are ignored.  Here are some sample
diary entries (in the default American style):

     12/22/1988 Twentieth wedding anniversary!!
     &1/1. Happy New Year!
     10/22 Ruth's birthday.
     21: Payday
     Tuesday--weekly meeting with grad students at 10am
              Supowit, Shen, Bitner, and Kapoor to attend.
     1/13/89 Friday the thirteenth!!
     &thu 4pm squash game with Lloyd.
     mar 16 Dad's birthday
     April 15, 1989 Income tax due.
     &* 15 time cards due.

If the first line of a diary entry consists only of the date or day
name, with no trailing blanks or punctuation, then that line is not
displayed in the diary window; only the continuation lines are shown.
For example, the following (single) diary entry will appear in the
diary window without the date line at the beginning:

     02/11/1989
      Bill Blattner visits Princeton today
      2pm Cognitive Studies Committee meeting
      2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'
      4:00pm Jamie Tappenden
      7:30pm Dinner at George and Ed's for Alan Ryan
      7:30-10:00pm dance at Stewart Country Day School

This can allow the diary window to look neater, but it can also cause
confusion when more than one day's entries are displayed.

Diary entries can be based on Lisp sexps.  For example, this diary
entry causes \"Vacation\" to appear from Nov 1 through Nov 10, 2002:

      %%(diary-block 11 1 2002 11 10 2002) Vacation

Other functions available are `diary-float', `diary-anniversary',
`diary-cyclic', `diary-day-of-year', `diary-iso-date',
`diary-french-date', `diary-hebrew-date', `diary-islamic-date',
`diary-mayan-date', `diary-chinese-date', `diary-coptic-date',
`diary-ethiopic-date', `diary-persian-date', `diary-yahrzeit',
`diary-sunrise-sunset', `diary-phases-of-moon', `diary-parasha',
`diary-omer', `diary-rosh-hodesh', and `diary-sabbath-candles'.
See the doc for the function `list-sexp-diary-entries' for more
details.

Diary entries based on the Hebrew and/or the Islamic calendar are also
possible, but because these are somewhat slow, they are ignored unless
you set the hooks `nongregorian-diary-listing-hook' and
`nongregorian-diary-marking-hook' appropriately.  See the doc for
these functions for details.

Diary files can contain directives to include the contents of other
files.  For details, see the documentation for the variable
`list-diary-entries-hook'.")
(put 'diary-file 'variable-documentation new-diary-file-doc)

;;;###autoload
(defun show-calendar ()
  "Show *Calendar* buffer, generating it if not already present.
If the calendar has already been generated, then, unlike the command
`calendar', this does not activate hooks that do things like display
your diary, appointments, holidays etc."
  (interactive)
  (get-buffer-create calendar-buffer)
  (if (save-excursion (set-buffer calendar-buffer)
                      (equal major-mode 'calendar-mode))
      (pop-to-buffer calendar-buffer)
    (calendar)))


;;;;; (pushnew
;;;;;  '("*Calendar*" (height . 9) (width . 80) (vertical-scroll-bars . right))
;;;;;  special-display-buffer-names :test 'equal)


(or (fboundp 'old-calendar-mode)
(fset 'old-calendar-mode (symbol-function 'calendar-mode)))

;; REPLACES ORIGINAL in `calendar.el':
;;   Nothing redefined here except the doc string.
(defun calendar-mode ()
  "Major mode for the \"*Calendar*\" buffer.
For more information, type \
\\<calendar-mode-map>\\[calendar-goto-info-node] in that buffer.

Whenever it makes sense, calendar commands take prefix arguments that
multiply their effect.  The digit keys (0-9) and the minus sign (-)
are bound to `digit-argument', so they need not be prefixed with `M-'.
For example, `2 \\[calendar-forward-week]' provides prefix argument 2 to \
command `\\[calendar-forward-week]'.


Mouse Menus
-----------

The mouse provides two popup menus in Calendar Mode (provided your
Emacs has a mouse):

    [mouse-2] provides a menu specific to the date under the mouse
    [mouse-3] provides a more general menu


Calendar Movement, Display
--------------------------

The calendar window can be refreshed at any time with `\\[redraw-calendar]'.
The commands for calendar movement are:

    `\\[scroll-calendar-right]'  scroll one month right \
`\\[scroll-calendar-left]'  scroll one month left
    `\\[scroll-calendar-right-three-months]'  scroll 3 months right    \
`\\[scroll-calendar-left-three-months]'  scroll 3 months left
    `\\[calendar-goto-today]'  display current month      \
`\\[calendar-other-month]'  display another month


Cursor Movement
---------------

Cursor movement in Calendar mode is a bit special.  It is as if the
only possible places for the cursor (point) and the mark were on
dates, and buffer positions were ordered chronologically rather than
from the top left of the buffer to the bottom right.  Thus, if the
cursor is at the end of a week and is then moved forward, it does not
move to the right, but rather to the next day.

The commands for cursor movement are:\\<calendar-mode-map>

    `\\[calendar-forward-day]'  one day forward         \
`\\[calendar-backward-day]'  one day backward
    `\\[calendar-forward-week]'  one week forward        \
`\\[calendar-backward-week]'  one week backward
    `\\[calendar-forward-month]'  one month forward       \
`\\[calendar-backward-month]'  one month backward
    `\\[calendar-forward-year]'  one year forward      \
`\\[calendar-backward-year]'  one year backward
    `\\[calendar-beginning-of-week]'  beginning of week       \
`\\[calendar-end-of-week]'  end of week
    `\\[calendar-beginning-of-month]'  beginning of month      \
`\\[calendar-end-of-month]'  end of month
    `\\[calendar-beginning-of-year]'  beginning of year       \
`\\[calendar-end-of-year]'  end of year

    `\\[calendar-goto-date]'  go to date

    `\\[calendar-goto-julian-date]'  go to Julian date       \
`\\[calendar-goto-astro-day-number]'  go to astro. (Julian) day
    `\\[calendar-goto-hebrew-date]'  go to Hebrew date       \
`\\[calendar-goto-islamic-date]'  go to Islamic date
    `\\[calendar-goto-iso-date]'  go to ISO date          \
`\\[calendar-goto-french-date]'  go to French Revol'y date

    `\\[calendar-goto-mayan-long-count-date]'  go to Mayan Long Count date
    `\\[calendar-next-haab-date]'  go to next occurrence of Mayan Haab date
    `\\[calendar-previous-haab-date]'  go to previous occurrence of Mayan \
Haab date
    `\\[calendar-next-tzolkin-date]'  go to next occurrence of Mayan Tzolkin \
date
    `\\[calendar-previous-tzolkin-date]'  go to previous occurrence of Mayan \
Tzolkin date
    `\\[calendar-next-calendar-round-date]'  go to next occurrence of Mayan \
Calendar Round date
    `\\[calendar-previous-calendar-round-date]'  go to previous occurrence \
of Mayan Calendar Round date

The \"one\" in some of these descriptions is the default.  A prefix
arg serves as a multiplier, as described above.  Thus, e.g. \
`2 \\[calendar-forward-year]'
moves the cursor forward two years.


The Region
----------

You can set the mark at a date in the calendar.  After moving the
cursor, a calendar region is defined between the cursor date and the
mark date.  You can then exchange the cursor and the mark positions.

    `\\[calendar-set-mark]'  mark date               \
`\\[calendar-exchange-point-and-mark]'  exchange point and mark

The region may also be defined as usual via the mouse (if your Emacs
has a mouse).  In addition, double clicking [mouse-1] on a date
selects that date as the region, and highlights it.  Triple clicking
\[mouse-1] on a date selects that date's week as the region, and
highlights it.

You can determine the number of days in the calendar region (including
the cursor date and the marked date) by:

    `\\[calendar-count-days-region]'  count days in calendar region

With a prefix argument (C-u), only weekdays (Mon-Fri) in the region
are counted.  If the entire region is visible, then \
`\\[calendar-count-days-region]' highlights it.


Holidays
--------

The following commands deal with holidays:

    `\\[calendar-cursor-holidays]'  give holidays for the date specified by \
the cursor
    `\\[mark-calendar-holidays]'  mark holidays in the calendar
    `\\[list-calendar-holidays]'  display holidays
    `\\[calendar-unmark]'  unmark all dates (not just holidays)

The command `\\[mark-calendar-holidays]' causes holidays to be \"marked\" \
\(visually highlighted).
`\\[mark-local-holidays]' does the same thing for local holidays.  The
variable `local-holidays' determines which dates are thus highlighted.

Set `view-calendar-holidays-initially' to a non-nil value to display
holidays for the current three month period on entry to the calendar.

Independently of the calendar, the command `\\[holidays]' displays the
holidays for the current, previous, and following month.


Solar & Lunar Information
-------------------------

To find the times of sunrise and sunset and the dates and times of
lunar phases:

    `\\[calendar-sunrise-sunset]'  show times of sunrise and sunset
    `\\[calendar-phases-of-moon]'  show times of quarters of the moon

The times given apply to location `calendar-location-name' at latitude
`calendar-latitude', and longitude `calendar-longitude'.  Set these
variables for your location.  The following variables are also
consulted, and you must set them if your system does not initialize
them properly:

    `calendar-time-zone',
    `calendar-daylight-time-offset',
    `calendar-standard-time-zone-name',
    `calendar-daylight-time-zone-name',
    `calendar-daylight-savings-starts',
    `calendar-daylight-savings-ends',
    `calendar-daylight-savings-starts-time',
    `calendar-daylight-savings-ends-time'.


Equivalent Dates
----------------

These commands express the cursor's date in other ways:

    `\\[calendar-print-day-of-year]'  show day number and number of days \
remaining in year
    `\\[calendar-print-iso-date]'  show equivalent date on the ISO \
commercial calendar
    `\\[calendar-print-julian-date]'  show equivalent date on the Julian \
calendar
    `\\[calendar-print-astro-day-number]'  show equivalent astronomical \
\(Julian) day number
    `\\[calendar-print-hebrew-date]'  show equivalent date on the Hebrew \
calendar
    `\\[calendar-print-islamic-date]'  show equivalent date on the Islamic \
calendar
    `\\[calendar-print-french-date]'  show equivalent date on the French \
Revolutionary calendar
    `\\[calendar-print-mayan-date]'  show equivalent date on the Mayan \
calendar

The day number in the year and the number of days remaining in the
year can be determined by:


Your Diary
----------

The following calendar mode commands control your diary:

    `\\[mark-diary-entries]'  mark diary entries         \
`\\[calendar-unmark]'  unmark all dates
    `\\[view-diary-entries]'  display diary entries      \
`\\[show-all-diary-entries]'  show all diary entries
    `\\[print-diary-entries]'  print diary entries

The command `\\[mark-diary-entries]' causes diary entries to be \"marked\", \
that is, visually
highlighted.  `\\[calendar-unmark]' removes all such calendar highlighting, \
not just
diary entry marks.

The command `\\[print-diary-entries]' prints the diary entries that
are shown by `\\[diary]'.

The command `\\[diary]' causes the diary entries for the cursor's date
to be displayed, independently of the calendar.  The number of days of
entries is governed by `number-of-diary-entries', but this is
overridden by a prefix argument.

Unless the variable `diary-display-hook' causes another sort of
display (see below), `\\[diary]' displays your diary file itself.  You
can then edit the diary entries there.  (The format of the entries is
described in the documentation for variable `diary-file'.)

\\<global-map>BE CAREFUL WHEN EDITING DIARY ENTRIES VIA `\\[diary]':
The buffer displayed by `\\[diary]' contains your *entire* diary file,
even though you can only see part of it.  This means, for instance,
that the command `\\[forward-char]' can put the cursor at what appears to be \
the
end of the line, but is in reality the middle of some concealed line.
\(Inserting additional lines or adding/ deleting characters in the
middle of a visible line will not cause problems.  Watch out for \
`\\[end-of-line]',
however--it may put you at the end of a concealed line far from where
the cursor appears to be.)

\\<calendar-mode-map>Because of this potential for confusion, if you do not use
`diary-display-hook', then before editing your diary it is best to
display the entire file with `\\[show-all-diary-entries]'.  (Be sure to save \
any changes you
make to the file.)

The following calendar mode commands assist in making diary entries:

    `\\[insert-diary-entry]'  insert a diary entry for the selected date
    `\\[insert-weekly-diary-entry]'  insert a diary entry for the selected \
day of the week
    `\\[insert-monthly-diary-entry]'  insert a diary entry for the selected \
day of the month
    `\\[insert-yearly-diary-entry]'  insert a diary entry for the selected \
day of the year
    `\\[insert-block-diary-entry]'  insert a diary entry for the days \
between point and mark
    `\\[insert-anniversary-diary-entry]'  insert an anniversary diary entry \
for the selected date
    `\\[insert-cyclic-diary-entry]'  insert a cyclic diary entry

There are corresponding commands to assist in making Hebrew or Islamic
date diary entries:

    `\\[insert-hebrew-diary-entry]'  insert a diary entry for the Hebrew \
date corresponding
                to the selected date
    `\\[insert-monthly-hebrew-diary-entry]'  insert a diary entry for the \
day of the Hebrew month
                corresponding to the selected day
    `\\[insert-yearly-hebrew-diary-entry]'  insert a diary entry for the day \
of the Hebrew year
                corresponding to the selected day
    `\\[insert-islamic-diary-entry]'  insert a diary entry for the Islamic \
date corresponding
                to the selected date
    `\\[insert-monthly-islamic-diary-entry]'  insert a diary entry for the \
day of the Islamic month
                corresponding to the selected day
    `\\[insert-yearly-islamic-diary-entry]'  insert a diary entry for the \
day of the Islamic year
                corresponding to the selected day

All of the diary entry commands make nonmarking (i.e. nonhighlighting)
entries when given a prefix argument.  With no prefix arg, the diary
entries are marking.

Set variable `view-diary-entries-initially' to a non-nil value to
display today's diary entries in another window when the calendar is
first displayed, if today's date is visible.  The variable
`number-of-diary-entries' controls the number of days of diary entries
to display initially or with the command `\\[diary]'.  E.g., the
default value 1 means display only today's diary entries.  The value 2
means display both today's and tomorrow's entries.

The value of `number-of-diary-entries' can also be a vector such as
\[0 2 2 2 2 4 1].  This value means: display no diary entries on
Sunday, display the entries for the current day and the following day
on Monday through Thursday, display Friday through Monday's entries on
Friday, and display only Saturday's entries on Saturday.

Set `mark-diary-entries-in-calendar' to a non-nil value to mark
\(highlight), in the calendar, all the dates that have diary entries.
The variable `diary-entry-marker' determines how to mark them.

The variable `diary-display-hook' is the list of functions called
after the diary buffer is prepared.  The default value simply displays
the diary file using selective-display to conceal irrelevant diary
entries.  An alternative function `fancy-diary-display' is provided
that, when used as the `diary-display-hook', causes a noneditable
buffer to be prepared with a neatly organized day-by-day listing of
relevant diary entries, together with any known holidays.  The
inclusion of the holidays slows this fancy display of the diary.
To speed it up, set the variable `holidays-in-diary-buffer' to nil.

The variable `print-diary-entries-hook' is the list of functions
called after a temporary buffer is prepared with the diary entries
currently visible in the diary buffer.  The default value of this hook
adds a heading (composed from the diary buffer's mode line), does the
printing with the command lpr-buffer, and kills the temporary buffer.
Other uses might include, for example, rearranging the lines into
order by day and time.


Appointment Reminding
---------------------

For information on making appointment reminders, and interfacing these
with your diary, see the documentation in file `appt.el' for functions
`appt-make-list', `appt-add', `appt-remind', `reminders-mode', and
variables `appt-checking-p' and `view-appointments-initially'.


Miscellaneous Calendar Variables and Hooks
------------------------------------------

The variable `calendar-load-hook', whose default value is nil, is a
list of functions to be called when the calendar is first loaded.

The variable `initial-calendar-window-hook', whose default value is
nil, is list of functions to be called when the calendar window is
first opened.  The functions invoked are called after the calendar
window is opened, but once opened is never called again.  Leaving the
calendar with the `q' command and reentering it will cause these
functions to be called again.

The variable `today-visible-calendar-hook', whose default value is
nil, is the list of functions called after the \"*Calendar*\" buffer
has been prepared with the calendar when today's date is visible in
the window.  This can be used, for example, to replace today's date
with asterisks.  The function `calendar-star-date' is included for
this purpose: (setq today-visible-calendar-hook 'calendar-star-date)
It could also be used to mark (highlight) today's date.  The function
`calendar-mark-today' is provided for this:
    (setq today-visible-calendar-hook 'calendar-mark-today)

The variable `today-invisible-calendar-hook', whose default value is
nil, is the list of functions called after the calendar buffer has
been prepared with the calendar when doday's date is not visible in
the window.


Calendar Mode Bindings
----------------------

\\{calendar-mode-map}---------------------------------------------------\
---------"
  (old-calendar-mode))


;; REPLACES ORIGINAL in `calendar.el':
;;
;; 1) `list-calendar-holidays' is now called by `calendar-basic-setup'
;;    on the 3-month period starting with the current month, not with
;;    the previous month.  The rationale is that you want to be
;;    informed of future, not past, holidays.  This means that the
;;    holidays listed by `calendar-basic-setup', when
;;    `view-calendar-holidays-initially', are one month later than
;;    those listed by `list-calendar-holidays'.
;;
;; 2) Fixed bug:  ARG's value & sign was not being taken into account
;;    previously.  Now, distinguish between the case where ARG is
;;    explicitly numeric (in which case, use its value as is), and the
;;    C-u-only case (in which case, prompt for month and year).
;;
;; Note: DISPLAYED-MONTH and DISPLAYED-YEAR are free variables here.
;;
;;;###autoload
(defun calendar-basic-setup (&optional arg)
  "Display a three-month calendar in another window.
The three months appear side by side, with the current month in the
middle surrounded by the previous and next months.  The cursor is put
on today's date (unless prefix ARG---see below).

Optional prefix argument ARG controls what 3-month period is used---
that is, what month takes the place of the current month.  Whenever a
different month is used in place of the current, the cursor is placed
on the first day of that month.

If called with an explicit numerical ARG (e.g. `\\[universal-argument] 2'), \
the calendar
is displayed ARG months in the future (ARG > 0) or in the past (< 0).
If called with just `\\[universal-argument]', without an \
explicit number (so that ARG is
a consp), then `calendar' prompts for the month (and year) to use in
place of the current month.

This function is suitable for execution in your `~/.emacs' file.
Appropriate setting of the variable `view-diary-entries-initially'
causes the diary entries for today's date to be displayed in another
window.  The value of the variable `number-of-diary-entries' controls
the number of days of diary entries displayed upon initial display of
the calendar.

Once in the calendar window, future or past months can be moved into
view.  Arbitrary months can be displayed, or the calendar can be
scrolled forward or backward.

The cursor can be moved forward or backward by a day, a week, a month,
or a year.  All such commands take prefix arguments which, when
negative, cause movement in the opposite direction.  The digit keys
\(0-9) and the minus sign (-) are automatically prefixes.  The window
is redisplayed as necessary to show the desired date.

Diary entries can be marked (i.e. highlighted) on the calendar, or
shown in another window.

Use `\\[describe-mode]' for details of key bindings in the calendar window.

The Gregorian calendar is assumed.

After loading calendar, the hooks in `calendar-load-hook' are run.
This is the place to add key bindings to the `calendar-mode-map'.

After preparing the calendar window initially, the hooks given by
`initial-calendar-window-hook' are run.

The hooks given by variable `today-visible-calendar-hook' are run
every time the calendar window gets scrolled, provided today's date is
visible in the window.  If it is not visible, the hooks given by the
variable `today-invisible-calendar-hook' are run instead.  Thus, for
example, setting `today-visible-calendar-hook' to `calendar-star-date'
will cause today's date to be replaced by asterisks whenever it is in
the window."
  (interactive "P")
  (require 'calendar)                   ; To avoid it being loaded later.
  (set-buffer (get-buffer-create calendar-buffer))
  (calendar-mode)
;;;  (setq calendar-window-configuration (current-window-configuration))
  (let* ((completion-ignore-case t)
         (pop-up-windows t)
         (split-height-threshold 1000)
         (date (if arg (calendar-read-date t) (calendar-current-date)))
         (just-ctl-u (consp arg))       ; Just Control-u.
         (month
          (cond (just-ctl-u
                 (cdr (assoc
                       (capitalize
                        (completing-read
                         "Month name: "
                         (mapcar (function list)
                                 (append calendar-month-name-array nil))
                         nil t))
                       (calendar-make-alist calendar-month-name-array))))
                (arg
                 (% (+ arg (extract-calendar-month date)) 12))
                (t
                 (extract-calendar-month date))))
         (year
          (cond (just-ctl-u
                 (calendar-read
                  "Year (>0): "
                  '(lambda (x) (> x 0))
                  (int-to-string (extract-calendar-year date))))
                (arg
                 (+ (/ (+ arg (extract-calendar-month date)) 12)
                    (extract-calendar-year date)))
                (t
                 (extract-calendar-year date)))))
    (unless month (setq month (extract-calendar-month date)))
    (while (< month 0)                  ; Avoid generate-calendar-window bug.
      (incf month 12) (decf year 1))
    (pop-to-buffer calendar-buffer)
    (increment-calendar-month month year (- calendar-offset))
    (if (fboundp 'calendar-generate-window)
        (calendar-generate-window month year)
      (generate-calendar-window month year))
    (when (and view-diary-entries-initially (calendar-date-is-visible-p date))
      (view-diary-entries
       (if (vectorp number-of-diary-entries)
           (aref number-of-diary-entries (calendar-day-of-week date))
         number-of-diary-entries)))
    (let* ((diary-buffer (get-file-buffer diary-file))
           (diary-window (and diary-buffer (get-buffer-window diary-buffer)))
           (split-height-threshold (if diary-window 2 1000)))
      (when view-calendar-holidays-initially
        (increment-calendar-month displayed-month displayed-year 1)
        (save-excursion (list-calendar-holidays))
        (increment-calendar-month displayed-month displayed-year -1))))
  ;; Make transient-mark-mode permanently local everywhere, and turn it off
  ;; in *Calendar* buffer.  This is not necessary, but it can reduce confusion.
  ;; The region is not really available anyway to user in *Calendar* buffer.
  (when (fboundp 'make-transient-mark-mode-buffer-local)
    (make-transient-mark-mode-buffer-local))
  ;; (setq transient-mark-mode nil) DON'T TURN IT OFF ANYMORE (pb in Windows)
  (run-hooks 'initial-calendar-window-hook))


;;; REPLACES ORIGINAL in `diary-lib.el':
;; When `european-calendar-style', this inserts `26 Jan 1995 :' instead of
;; `26 Jan 1995'.
;; NOTE: This definition is thus coupled with the definition of
;;       `european-date-diary-pattern' (which also has " :" at the end of a
;;       diary entry) --- See `european-date-diary-pattern', above.
;;;###autoload
(defun insert-diary-entry (arg)
  "Insert a diary entry for the date indicated by point.
Prefix ARG makes the entry nonmarking."
  (interactive "P")
  (make-diary-entry
   (concat (calendar-date-string (calendar-cursor-to-date t) t t)
           (and european-calendar-style " :"))
   arg))


;; REPLACES ORIGINAL in `calendar.el':
;;   Puts weekend days in `weekend-face'.
(defun generate-calendar-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted in the buffer starting at the line on which point
is currently located, but indented INDENT spaces.  The indentation is done
from the first character on the line and does not disturb the first INDENT
characters on the line."
  (require 'calendar)                   ; To avoid it being loaded later.
  (let* ((blank-days (mod (- (calendar-day-of-week (list month 1 year))
                             calendar-week-start-day)
                          7))           ; At start of month.
         (last (calendar-last-day-of-month month year)))
   (goto-char (point-min))
   (calendar-insert-indented
    (calendar-string-spread
     (list "" (format "%s %d" (calendar-month-name month) year) "") ?  20)
    indent t)
   (calendar-insert-indented "" indent);; Go to proper spot
   (calendar-for-loop i from 0 to 6 do
      (insert (calendar-day-name (mod (+ calendar-week-start-day i) 7)
                                 2 t))
      (insert " "))
   (calendar-insert-indented "" 0 t);; Force onto following line
   (calendar-insert-indented "" indent);; Go to proper spot
   ;; Add blank days before the first of the month
   (calendar-for-loop i from 1 to blank-days do (insert "   "))
   ;; Put in the days of the month
   (calendar-for-loop i from 1 to last do
      (insert (format "%2d " i))
      (put-text-property (- (point) (if (< i 10) 2 3)) (1- (point))
                         'mouse-face 'highlight)
      (when (memq (calendar-day-of-week (list month i year)) '(0 6)) ; Sun, Sat
        (put-text-property (- (point) 3) (1- (point))
                           'face weekend-face)) ; Shade weekend days.
      (and (zerop (mod (+ i blank-days) 7))
           (/= i last)
           (calendar-insert-indented "" 0 t)    ;; Force onto following line
           (calendar-insert-indented "" indent)))));; Go to proper spot


;; REPLACES ORIGINAL in `calendar.el':
;; Marks days in region (if all of region is visible).
;;;###autoload
(defun calendar-count-days-region (&optional weekdays-only-p)
  "Count days between point and mark (inclusive).  Weekdays if prefix.
If WEEKDAYS-ONLY-P arg is non-nil, only weekdays (Mon-Fri) are counted.
Mark the days in the calendar region, provided it is entirely visible.
Return the number of days in the calendar region, in any case."
  (interactive "P")
  (if weekdays-only-p
      (calendar-count-weekdays-region)
    (require 'calendar)                 ; To avoid it being loaded later.
    (let* ((point-date (calendar-cursor-to-date t))
           (mark-date (or (car calendar-mark-ring)
                          (error "No region in Calendar (no mark set)")))
           (max-date-pt-mark (if (calendar-date-compare (list point-date)
                                                        (list mark-date))
                                 mark-date
                               point-date))
           (cons-max-date (list max-date-pt-mark)) ; To remove from while loop.
           return-value)
      (save-excursion
        (let* ((num-days (- (calendar-absolute-from-gregorian point-date)
                            (calendar-absolute-from-gregorian mark-date)))
               (num-days (1+ (if (> num-days 0) num-days (- num-days)))))
          (setq return-value num-days)
          (when (calendar-date-is-visible-p point-date)
            (cond ((= 1 num-days)       ; Shortcut.
                   (redraw-calendar)    ; Remove previous region marking.
                   (mark-visible-calendar-date (calendar-cursor-to-date t)
                                               calendar-region-marker 100))
                  ((calendar-date-is-visible-p mark-date)
                   (message "Marking days in region...")
                   (redraw-calendar)    ; Remove previous region marking.
                   (unless (calendar-date-compare (list point-date)
                                                  (list mark-date))
                     (calendar-exchange-point-and-mark))
                   (while (> num-days 0)
                     (mark-visible-calendar-date (calendar-cursor-to-date t)
                                                 calendar-region-marker 100)
                     (when (calendar-date-compare
                            (list (calendar-cursor-to-date t)) cons-max-date)
                       (calendar-forward-day 1)) ; Don't advance beyond visible
                     (decf num-days)))))))
      (when (mark-marker) (set-marker (mark-marker) nil))
      (unless (calendar-date-equal (calendar-cursor-to-nearest-date)
                                   max-date-pt-mark)
        (if (calendar-date-equal mark-date max-date-pt-mark)
            (calendar-goto-date point-date)
          (calendar-goto-date mark-date)
          (calendar-exchange-point-and-mark)))
      (message "Region has %d day%s (including cursor and marked dates)."
               return-value (if (> return-value 1) "s" ""))
      return-value)))                   ; Return something useful.

;;;###autoload
(defun calendar-count-weekdays-region ()
  "Count the weekdays between the point and the mark (inclusive).
Mark the days in the calendar region, provided it is entirely visible.
Return the number of weekdays in the calendar region, in any case."
  (interactive)
  (require 'calendar)                   ; To avoid it being loaded later.
  (let ((point-date (calendar-cursor-to-date t))
        (mark-date (or (car calendar-mark-ring)
                       (error "No region in Calendar (no mark set)")))
        (num-weekdays 0)
        (mark-days-p nil)
        max-date-pt-mark
        cons-max-date+1                 ; Just to remove from while loop.
        temp-date
        return-value)
    (if (calendar-date-compare (list point-date) (list mark-date))
        (setq max-date-pt-mark mark-date)
      (setq max-date-pt-mark point-date)
      (calendar-exchange-point-and-mark))
    (setq cons-max-date+1
          (list (calendar-gregorian-from-absolute
                 (1+ (calendar-absolute-from-gregorian max-date-pt-mark)))))
    (when (setq mark-days-p (and (calendar-date-is-visible-p point-date)
                                 (calendar-date-is-visible-p mark-date)))
      (message "Marking days in region...")
      (redraw-calendar))                ; Remove previous region marking.
    (while (calendar-date-compare
            (list (setq temp-date (calendar-cursor-to-date t)))
            cons-max-date+1)
      (when (memq (calendar-day-of-week temp-date) '(1 2 3 4 5))
        (incf num-weekdays)
        (when mark-days-p
          (mark-visible-calendar-date temp-date
                                      calendar-region-marker 100)))
      (calendar-forward-day 1))
    (setq return-value num-weekdays)
    (when (mark-marker) (set-marker (mark-marker) nil))
    (if (calendar-date-equal mark-date max-date-pt-mark)
        (calendar-goto-date point-date)
      (calendar-goto-date mark-date)
      (calendar-exchange-point-and-mark))
    (message "Region has %d weekday%s (including cursor and marked dates)."
             return-value (if (> return-value 1) "s" ""))
    return-value))                      ; Return something useful.


;; REPLACES ORIGINAL in `calendar.el':
;;   Does not add a new overlay if one is already present with face MARK.
;;   This is to allow for overlapping overlays.  Arg PRIORITY is new too.
(defun mark-visible-calendar-date (date &optional mark priority)
  "Mark DATE in the calendar window with MARK, if not so marked already.
MARK is either a single-character string or a face.
MARK defaults to `diary-entry-marker'.
PRIORITY is an optional priority to give to MARK's face overlay."
  (require 'calendar)                   ; To avoid it being loaded later.
  (when (calendar-date-is-legal-p date)
    (save-excursion
      (set-buffer calendar-buffer)
      (calendar-cursor-to-visible-date date)
      (let ((mark (or mark diary-entry-marker)))
        (if (stringp mark)
            (let ((buffer-read-only nil))
              (forward-char 1)
              (delete-char 1)
              (insert mark)
              (forward-char -2))
          (unless (memq mark            ; Already present - Don't add again.
                        (mapcar (function (lambda (ovr)
                                            (overlay-get ovr 'face)))
                                (overlays-at (point))))
            (let ((ovrly (make-overlay (1-(point)) (1+ (point)))))
            (overlay-put ovrly 'face mark)
            (when priority (overlay-put ovrly 'priority priority)))))))))


;;;;; No longer needed; fixed in Emacs20.
;;;;; ;; REPLACES ORIGINAL in `cal-french.el':
;;;;; ;;   (calendar-cursor-to-date t) -> date.
;;;;; ;;   This is a *correction*, not an extension.
;;;;; ;;   NOTE: DATE is a free variable here.
;;;;; (defun diary-french-date ()
;;;;;   "French calendar equivalent of date diary entry."
;;;;;   (let ((f (calendar-french-date-string date)))
;;;;;     (if (string-equal f "") "Date is before the French Revolution." f)))


;; REPLACES ORIGINAL in `solar.el':
;;   Daylight is written "HH hr, MM min daylight",
;;   not "HH:MM hours daylight".
;;   This is so that when it is added to the diary this is not interpreted
;;   as an appointment time.
;;   NOTE: DATE is a free variable here.
(defun solar-sunrise-sunset-string (date)
  "String of *local* times of sunrise, sunset, and daylight on Gregorian DATE."
  (require 'solar)
  (let* ((l (solar-sunrise-sunset date)) ; In `solar.el'.
         (daylight (car (cdr (cdr l))))
         (len (length daylight)))
    (format
     "%s, %s at %s (%s hr, %s min daylight)"
     (if (car l)
         (concat "Sunrise " (apply 'solar-time-string (car l)))
       "No sunrise")
     (if (car (cdr l))
         (concat "sunset " (apply 'solar-time-string (car (cdr l))))
       "no sunset")
     (eval calendar-location-name)
     (if (= len 5)(substring daylight 0 2)(substring daylight 0 1))
     (if (= len 5)(substring daylight 3 5)(substring daylight 2 4)))))

;; Use this instead of `mouse-drag-region'.
;;;###autoload
(defun calendar-mouse-drag-region (event)
  "Like `mouse-drag-region', and marks days if dragged or multi-clicked.
Double click marks the day under pointer.  Triple click marks the week.
Dragging marks the days dragged over."
  (interactive "e")
  (let ((click-count (event-click-count event)))
    (mouse-drag-region event)
    (if (> click-count 1)
        (forward-char -1)
      (calendar-cursor-to-nearest-date))
    (case click-count
      (1 (when (and mark-active (mark) (/= (point) (mark)))
           (set-calendar-region-from-region))) ; Mark region if dragged.
      (2 (set-calendar-region-from-region) ; Double click: Mark single date.
         (mouse-set-point event))
      (3 (mouse-set-point event)          ; Triple click: Mark the week.
         (let ((beg-week (progn (calendar-beginning-of-week 1) (point))))
           (when (consp calendar-mark-ring) ; Remove mark made via double-click
             (pop calendar-mark-ring))  ; that occurred along way to triple.
           (calendar-set-mark nil)
           (calendar-end-of-week 1)
           (calendar-count-days-region)
           (calendar-exchange-point-and-mark))))))

;;;###autoload
(defun set-calendar-region-from-region ()
  "Set the calendar region to include the days in the region."
  (interactive)
  (calendar-cursor-to-nearest-date)
  (calendar-set-mark nil)
  (exchange-point-and-mark)
  (calendar-cursor-to-nearest-date)
  (calendar-count-days-region))

;;;###autoload
(defun mark-local-holidays ()
  "Mark local holidays in the calendar window."
  (interactive)
  (message "Marking local holidays...")
  (let ((holiday-list (calendar-local-holiday-list)))
    (while holiday-list
      (mark-visible-calendar-date
       (caar holiday-list) calendar-local-holiday-marker)
      (setq holiday-list (cdr holiday-list))))
  (message "Marking local holidays...done"))

(defsubst calendar-local-holiday-list ()
  "Form list of local holidays that occur on dates in calendar window.
The holidays are those in the list `local-holidays'."
  (let ((calendar-holidays local-holidays))
    (calendar-holiday-list)))


;; Adapted from holiday-easter-etc, in `holidays.el'.
;; Note: DISPLAYED-MONTH and DISPLAYED-YEAR are free variables here.
(defun monday-after-easter ()
  "Monday after Easter, as visible in calendar window."
  (unless (and (> displayed-month 5) (not all-christian-calendar-holidays))
    ;; Ash Wednesday, Good Friday, and Easter are visible.
    (let* ((century (1+ (/ displayed-year 100)))
           (shifted-epact;; Age of moon for April 5...
            (% (+ 14 (* 11 (% displayed-year 19));;     ...by Nicaean rule
                  (-;; ...corrected for the Gregorian century rule
                   (/ (* 3 century) 4))
                  (/;; ...corrected for Metonic cycle inaccuracy.
                   (+ 5 (* 8 century)) 25)
                  (* 30 century));;              Keeps value positive.
               30))
           (adjusted-epact;;  Adjust for 29.5 day month.
            (if (or (= shifted-epact 0)
                    (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
                (1+ shifted-epact)
              shifted-epact))
           (paschal-moon;; Day after the full moon on or after March 21.
            (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
               adjusted-epact))
           (abs-easter
            (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))
      (filter-visible-calendar-holidays
       (list (list (calendar-gregorian-from-absolute (1+ abs-easter))
                   "Lundi de Pâques -- Ferié"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calendar+.el ends here
