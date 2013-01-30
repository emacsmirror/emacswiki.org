;;; time-ext.el --- more function for time/date

;; Time-stamp: <2013-01-31 06:48:11 rubikitch>

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/time-ext.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; More easy-to-use time/date functions. Using polymorphism,
;; TIME accepts (T1 T2 T3), (SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE) and
;; float time.

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x time-ext-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of time-ext.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "time-ext.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x time-ext-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put time-ext.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'time-ext)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET time-ext RET
;;


;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/time-ext.txt

;;; Code:

(eval-when-compile (require 'cl))
(defgroup time-ext nil
  "time-ext"
  :group 'emacs)

(defun time-as-float (time)
  "Return float time."
  (cond ((numberp time) time)
        ((<= (length time) 3) (float-time time))
        (t (float-time (apply 'encode-time time)))))
(defun time-as-list (time)
  "Return list time."
  (if (numberp time)
      (seconds-to-time time)
    time))

(defun day-of-week (time)
  "Return the day of week, an integer between 0 and 6,where 0 is Sunday."
  (nth 6 (decode-time (time-as-list time))))

(defun time-add-day (time ndays)
  "Return time after NDAYS."
  (time-as-list
   (+ (time-as-float time)
      (* ndays 24 3600))))

(defun next-nweek (weeknum &optional time)
  "Time of next week of WEEKNUM.
WEEKNUM is the numeric day of week from 0 (Sunday) to 6."
  (time-as-list
   (loop with tm = (time-as-float (or time (current-time)))
         do (incf tm (* 24 3600))
         until (eq weeknum (day-of-week tm))
         finally (return tm))))

(defun next-monday (&optional time)
  "Time of Next monday."
  (next-nweek 1 time))

;;;; Bug report
(defvar time-ext-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar time-ext-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of time-ext.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"time-ext.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun time-ext-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   time-ext-maintainer-mail-address
   "time-ext.el"
   nil
   nil nil
   time-ext-bug-report-salutation))


;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "time-as-float")
      (expect t
        (let ((ft (float-time)))
          (equal ft (time-as-float ft))))
      (expect t
        (let ((tm (current-time)))
          (equal (float-time tm) (time-as-float tm))))
      (expect t
        (let ((dec '(5 22 9 16 5 2010 0 nil 32400)))
          (equal (float-time (apply 'encode-time dec)) (time-as-float dec))))
      (desc "time-as-list")
      (expect t
        (let ((ft (float-time)))
          (equal (seconds-to-time ft) (time-as-list ft))))
      (expect t
        (let ((tm (current-time)))
          (equal tm (time-as-list tm))))
      (desc "day-of-week")
      (expect 0
        (day-of-week (encode-time 0 0 0 16 5 2010)))
      (expect 0
        (day-of-week (float-time (encode-time 0 0 0 16 5 2010))))
      (desc "time-add-day")
      (expect "2010/05/20"
        (format-time-string
         "%Y/%m/%d"
         (time-add-day (encode-time 0 0 0 17 5 2010) 3)))
      (expect "2010/05/20"
        (format-time-string
         "%Y/%m/%d"
         (time-add-day (float-time (encode-time 0 0 0 17 5 2010)) 3)))
      (desc "next-monday")
      (expect "2010/05/17"
        (format-time-string
         "%Y/%m/%d"
         (next-monday (encode-time 0 0 0 15 5 2010))))
      (expect "2010/05/17"
        (format-time-string
         "%Y/%m/%d"
         (next-monday (float-time (encode-time 0 0 0 15 5 2010)))))
      (expect "2010/05/24"
        (format-time-string
         "%Y/%m/%d"
         (next-monday (encode-time 0 0 0 17 5 2010))))
      )))


(provide 'time-ext)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "time-ext.el"))
;;; time-ext.el ends here
