;;;; time-ext.el --- more function for time/date
;; Time-stamp: <2010-05-16 09:34:43 rubikitch>

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
;; 

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
