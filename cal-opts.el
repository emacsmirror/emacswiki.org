;;; cal-opts.el --- Set various calendar, diary etc. options.
;;
;; Filename: cal-opts.el
;; Description: Set various calendar, diary etc. options.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Wed Nov 15 12:47:13 1995
;; Version: 0
;; Last-Updated: Mon Jan  1 10:04:50 2018 (-0800)
;;           By: dradams
;;     Update #: 127
;; URL: https://www.emacswiki.org/emacs/download/cal-opts.el
;; Keywords: calendar, local
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `appt', `avoid', `cal-dst', `cal-julian', `cal-menu',
;;   `cal-persia', `calendar', `calendar+', `cl', `diary-lib',
;;   `easymenu', `faces', `frame-cmds', `frame-fns',
;;   `lisp-float-type', `misc-fns', `solar', `strings', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Set various calendar, diary etc. options.
;;
;;
;;  NOTE (9/2000) - I used this with Emacs 19.34.  I have not yet
;;                  tried to use this file with Emacs 20.x.  That will
;;                  probably require some tweaking, and perhaps some
;;                  of the code is no longer necessary/pertinent.
;;
;; Note: This code is quite old, and is likely obsolete now.  You
;;       might find it useful in some way to mine - or not. ;-)
;;
;; -------------------------------------------------------------------
;;
;;  To use, put this in your `~/.emacs': (require 'cal-opts.el)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; RCS $Log: cal-opts.el,v $
;; RCS Revision 1.7  2001/01/08 22:25:11  dadams
;; RCS Adapted file header for Emacs Lisp Archive.
;; RCS
;; RCS Revision 1.6  2001/01/03 17:30:33  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.5  2001/01/03 00:33:10  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.4  2001/01/02 23:20:31  dadams
;; RCS Protect appt-make-list and mark-local-holidays via fboundp.
;; RCS
;; RCS Revision 1.3  2000/11/28 19:17:06  dadams
;; RCS Optional require's via 3rd arg=t now.
;; RCS
;; RCS Revision 1.2  2000/09/27 21:35:35  dadams
;; RCS Updated for Emacs 20.7:
;; RCS 1. Commented out French holidays (local-holidays).
;; RCS 2. Changed latitude & longitude.
;; RCS 3. No view-diary-entries-initially or view-calendar-holidays-initially.
;; RCS
;; RCS Revision 1.1  2000/09/13 20:06:10  dadams
;; RCS Initial revision
;; RCS
; Revision 1.1  1999/10/07  09:41:17  dadams
; Initial revision
;
; Revision 1.4  1996/06/20  11:47:48  dadams
; (trivial)
;
; Revision 1.3  1996/06/06  13:06:46  dadams
; Update of file dependency comments (e.g. "Autoloaded from...").
;
; Revision 1.2  1996/03/06  08:18:48  dadams
; (trivial)
;
; Revision 1.1  1996/03/05  11:56:57  dadams
; Initial revision
;;
;; Previous Change Log (as `drew-cal-opts.el'):
;;
; Revision 1.5  1996/03/05  09:01:13  dadams
; 1. Copyright.
; 2. Renamed this to cal-opts.el.
;
; Revision 1.4  1996/02/12  09:19:11  dadams
; Updated header keywords (for finder).
;
; Revision 1.3  1996/01/18  08:10:24  dadams
; local-holidays: Added Pentecote, corrected Ascension for '96.
;
; Revision 1.2  1995/12/12  14:34:00  dadams
; No longer setq fancy-diary-buffer here, because calendar.el clobbers it.
;
; Revision 1.1  1995/11/16  14:48:47  dadams
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

(require 'appt nil t) ;; (no error if not found): appt-checking-p,
                      ;; appt-display-duration,
                      ;; appt-display-interval, appt-make-list,
                      ;; appt-message-warning-time,
(require 'calendar+ nil t) ;; (no error if not found): mark-local-holidays


;;;;;;;;;;;;;;;;;


;;;;; ;; Some of these shouldn't be hard-wired.
;;;;; (setq local-holidays                ; Should really be in `site-init.el'.
;;;;;       '((holiday-fixed 1 1 "Jour de l'An -- Ferié")
;;;;;         (monday-after-easter)
;;;;;         (holiday-fixed 5 1 "Fête du Travail (May Day!) -- Ferié")
;;;;;         (holiday-fixed 5 8 "Victoire, 1945 -- Ferié")
;;;;;         (holiday-float 5 4 3 "Ascension -- Ferié")
;;;;;         (holiday-float 5 7 4 "Pentecôte")
;;;;;         (holiday-fixed 7 14 "Fête Nationale -- Ferié")
;;;;;         (holiday-fixed 8 15 "Assomption -- Ferié")
;;;;;         (holiday-fixed 11 1 "Toussaint -- Ferié")
;;;;;         (holiday-fixed 11 11 "Armistice, 1918 -- Ferié")
;;;;;         (holiday-fixed 12 25 "Noël -- Ferié")))


;;; Solar stuff.

;; Latitude & Longitude
(setq calendar-latitude 37.7)           ; Pleasanton, California
(setq calendar-longitude -121.9)        ; Pleasanton, California
;; Aix-en-Provence, France: Lat.: 48.333, Long.: 3.667.

;;;;; (setq calendar-time-display-form  ; Use for European clock.
;;;;;       '(24-hours ":" minutes
;;;;;                  (if time-zone " (") time-zone (if time-zone ")")))

;;;;; (setq calendar-week-start-day 1)    ; Start week on Monday.

;;; Diary stuff.
;;;;; (setq view-diary-entries-initially t)
(setq mark-diary-entries-in-calendar t)
(setq number-of-diary-entries '[0 3 3 3 3 5 0])
;; It does no good setting this before `calendar.el' has been loaded,
;; because `calendar.el' does a `defconst' to it.
;; (setq fancy-diary-buffer "*Diary Entries*")

;;; Appointment stuff.  Defined in `appt.el'.
(setq appt-message-warning-time 15)
(setq appt-display-interval 5)
(setq appt-display-duration t)          ; Indefinite display.
(setq appt-checking-p t)
;; (setq view-appointments-initially t)

;;; Holiday stuff.
;;;;; (setq view-calendar-holidays-initially t)
(setq mark-holidays-in-calendar t)  ; Can set to nil to speed things up.
;; (setq holidays-in-diary-buffer t)   ; Can set to nil to speed things up.
(setq all-christian-calendar-holidays t) ; Alas, this is France.

;;; Hooks.
(when (fboundp 'appt-make-list)         ; Defined in `appt.el'.
  (add-hook 'diary-hook 'appt-make-list))
(add-hook 'diary-display-hook 'fancy-diary-display)
(when (fboundp 'mark-local-holidays)    ; Defined in `calendar+.el'
  (add-hook 'today-invisible-calendar-hook 'mark-local-holidays)
  (add-hook 'today-visible-calendar-hook 'mark-local-holidays))
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cal-opts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cal-opts.el ends here
