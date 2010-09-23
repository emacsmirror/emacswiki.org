;;; gnus-autocheck.el
;; Auto check for new incoming news and mails
;; Autocheck is binded to "vs" to start it and to "ve" to end it in Group mode
;;
;; Copyright (c) Olivier Sirven
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA
;;

(defvar gnus-autocheck-version-number "0.2" "Gnus autocheck version number")

(require 'gnus)

(defun gnus-autocheck-hourp(str)
  "Returns non-nil value if STR is a valid time using format HH:MM"

  (let (hour minute)

    (when (and
           (stringp str)
           (string-match "^\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)$" str))
      (setq hour (string-to-number (match-string 1 str)))
      (setq minute (string-to-number (match-string 2 str)))

      (and
       (>= hour 0)
       (< hour 24)
       (>= minute 0)
       (< minute 60)))))

;; custom
(defgroup gnus-autocheck nil
  "gnus-autocheck.el is an emacs package for interfacing with Gnus.
It makes then possible to define a time frame for gnus to automatically
check for new news and mails"
  :version gnus-autocheck-version-number
  :group 'gnus)

(defcustom gnus-autocheck-interval
  (* 60 5)
  "Interval in seconds between two checks"
  :type 'integer
  :group 'gnus-autocheck)

(defcustom gnus-autocheck-hook
  '()
  "Use this hook to customize what needs to be done before actually checking
for news and mails"
  :type 'hook
  :group 'gnus-autocheck)

(defcustom gnus-autocheck-active-days
  ()
  "List of days (from Sun to Sat) when the autocheck mode will be active.
For example you can definie it to be active on Saturday and Sunday.

Leave blank if you don't need it."
  :type '(repeat (choice (cons :tag "Sunday" 'Sun)
                         (cons :tag "Monday" 'Mon)
                         (cons :tag "Tuesday" 'Tue)
                         (cons :tag "Wednesday" 'Wed)
                         (cons :tag "Thursday" 'Thu)
                         (cons :tag "Friday" 'Fri)
                         (cons :tag "Saturday" 'Sat)))
  :group 'gnus-autocheck)

(defcustom gnus-autocheck-active-periods
  ()
  "List of hours periods when the autocheck mode will be active.
For example you can define it to be active during 00:00 to 08:00
and during 19:00 to 23:59. Time format is HH:MM and symbol is expected
to be a string.

Leave blank if you don't need it."
  :type '(repeat (list (restricted-sexp :tag "Start hour" :match-alternatives (gnus-autocheck-hourp))
                       (restricted-sexp :tag "Stop hour" :match-alternatives (gnus-autocheck-hourp))))
  :group 'gnus-autocheck)

(defvar gnus-autocheck-timer
  nil
  "Store the gnus autocheck timer refrence")

(defun gnus-autocheck-get-new-mails-news()
  "Get all new mails both by calling offlineimap and gnus-group-get-new-news
You can use gnus-autocheck-hook hook to customize what needs to be done before
checking for news"
  (run-hooks 'gnus-autocheck-hook)
  (gnus-group-get-new-news)
  t
)

(defun gnus-auto-checkmails()
  "Callback used auto check for new incoming emails during night hours"

  (let (
        (current-day (substring (current-time-string) 0 3))
        (current-hour (string-to-number (substring (current-time-string) 11 13)))
        (current-minute (string-to-number (substring (current-time-string) 14 16)))
        )
    (if (not (dolist (day-to-check gnus-autocheck-active-days)
               (if (string= day-to-check current-day)
                   (gnus-autocheck-get-new-mails-news)
                 )
               )
             )
        (dolist (period-to-check gnus-autocheck-active-periods)
          (let (
                (period-time-start-hour (string-to-number (substring (nth 0 period-to-check) 0 2)))
                (period-time-start-minute (string-to-number (substring (nth 0 period-to-check) 3)))
                (period-time-stop-hour (string-to-number (substring (nth 1 period-to-check) 0 2)))
                (period-time-stop-minute (string-to-number (substring (nth 1 period-to-check) 3)))
                )
            (if (and (>= current-hour period-time-start-hour)
                     (or (= current-hour period-time-start-hour) (>= current-minute period-time-start-minute))
                     (<= current-hour period-time-stop-hour)
                     (<= current-minute period-time-stop-minute))
                (gnus-autocheck-get-new-mails-news)
              )
            )
          )
      )
    )
)

(defun gnus-autocheck-start(&optional start-at-time)
  "Start auto check for news and mails"
  (interactive)

  (if (eq start-at-time nil)
      (setq start-at-time gnus-autocheck-interval))

  (if gnus-autocheck-timer
      (message "autocheck is already enabled")
    (setq gnus-autocheck-timer (run-at-time start-at-time gnus-autocheck-interval 'gnus-auto-checkmails))
    (message "start autocheck")
    )
)

(defun gnus-autocheck-stop()
  "Stop auto check for news and mails"
  (interactive)

  (if (not gnus-autocheck-timer)
      (message "autocheck is not enabled")
    (cancel-timer gnus-autocheck-timer)
    (setq gnus-autocheck-timer nil)
    (message "stop autocheck")
    )
)

;; bind autocheck to "vs" to start it and to "ve" to end it
(define-key gnus-group-mode-map (kbd "vs") '(lambda()
                                              (interactive)
                                              (gnus-autocheck-start 0)))
(define-key gnus-group-mode-map (kbd "ve") 'gnus-autocheck-stop)

(gnus-autocheck-start)
(add-hook 'gnus-exit-gnus-hook 'gnus-autocheck-stop)

(provide 'gnus-autocheck)
