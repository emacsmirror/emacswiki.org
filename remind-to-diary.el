;;; remind-to-diary.el --- convert remind `simple calendar' output to diary format

;; Copyright (C) 2010 by Robert P. Hamrick, all rights reserved.
;; Author: Robert P. Hamrick
;; Maintainer: Robert P. Hamrick
;; Created: 7 Feb 2010
;; Version: 0.1
;; Keywords: remind diary
;; URL:

;; This file is *NOT* part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the
;;
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:
;;
;; Simple package to convert remind's `simple calendar' output to Emacs diary
;; format and save the diary entries to a file.

;; Installation:
;;
;; Remind can be obtained from http://www.roaringpenguin.com/products/remind.
;;
;; To use this modest library, put this file in your Emacs-Lisp load path and
;; add the following to your startup file
;;
;;    (require 'remind-to-diary)

;; Description:
;;
;; Generate a diary file from remind with the interactive function
;;
;;    (remind-to-diary-run)
;;
;; This will save diary entries, one for each line of remind's simple calendar
;; output, to the file REMIND-TO-DIARY-DIARY-FILE (~/.diary-remind, by default).
;;
;; The following hook automates this process.
;;
;;    (add-hook 'diary-list-entries-hook 'remind-to-diary-run)
;;
;;
;; To configure the diary facility to use this file, use the following hook
;;
;;    (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
;;
;; and add the following line to your diary-file (by default ~/diary)
;;
;;    #include "~/.diary-remind"

;;; History:
;;
;; 0.1 - First version.

;;; Code:

(require 'cl)


;; user customizable variables

(defgroup remind-to-diary nil
  "Options for remind-conf-mode."
  :prefix "remind-to-diary-"
  :group 'applications)

(defcustom remind-to-diary-reminders-file
  (expand-file-name "~/.reminders")
  "Base reminders file or directory, by default `~/.reminders'.
See remind man page for details."
  :group 'remind-to-diary
  :type '(file))

(defcustom remind-to-diary-diary-file
  ;; (expand-file-name "diary-remind" rph/etc)
  (expand-file-name "~/.diary-remind")
  "File for diary."
  :group 'remind-to-diary
  :type '(file))

(defcustom remind-to-diary-remind-bin
  "remind"
  "Name or path to the remind binary."
  :group 'remind-to-diary
  :type '(string))

(defcustom remind-to-diary-include-advanced-reminders
  nil
  "Use remind's simple calendar option with the `a' modifier (e.g. -sa2)."
  :group 'remind-to-diary
  :type '(boolean))

(defcustom remind-to-diary-months-of-reminders
  3
  "Number of months to output in remind's simple calendar format, as specified with remind's -s option."
  :group 'remind-to-diary
  :type '(integer))

(defcustom remind-to-diary-use-24-hour-times
  t
  "If true, have remind output 24-hour times, otherwise have remind output 12-hour times."
  :group 'remind-to-diary
  :type '(boolean))


;; regex variables

(defconst remind-to-diary-year-regex
  "^[1-2][0-9][0-9][0-9]")

(defconst remind-to-diary-month-regex
  "[0-1][0-9]")

(defconst remind-to-diary-day-regex
  "[0-3][0-9]")

(defconst remind-to-diary-time-regex
  (if remind-to-diary-use-24-hour-times
      "[0-2][0-9]:[0-5][0-9]"
    "1?[0-9]:[0-5][0-9][ap]m"))

(defconst remind-to-diary-field-regex
  "[^ ]+"
  "Simple space-separated field.")

(defconst remind-to-diary-desc-regex
  "[^ ].*$"
  "The last field of remind's simple calendar output.")


;; define functions to replace remind output with diary formatted entries

(defmacro remind-to-diary-define-convert-reminder (fname regex fmt)
  "Define function FNAME to convert remind's simple calendar format to diary format by supplying REGEX and a replacement FMT."
  `(defun ,fname ()
     (let ((remind-re ,regex)
           (diary-fmt ,fmt))
       (save-excursion
         (if (re-search-forward remind-re (point-at-eol) t)
             (replace-match diary-fmt))))))

(remind-to-diary-define-convert-reminder
 remind-to-diary-convert-untimed-reminder
 (format "\\(%s\\)/\\(%s\\)/\\(%s\\) %s %s %s %s \\(%s\\)"
         remind-to-diary-year-regex
         remind-to-diary-month-regex
         remind-to-diary-day-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-desc-regex)
 "\\2/\\3/\\1 \\4")

(remind-to-diary-define-convert-reminder
 remind-to-diary-convert-timed-reminder
 (format "\\(%s\\)/\\(%s\\)/\\(%s\\) %s %s %s %s \\(%s\\) \\(%s\\)"
         remind-to-diary-year-regex
         remind-to-diary-month-regex
         remind-to-diary-day-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-time-regex
         remind-to-diary-desc-regex)
 "\\2/\\3/\\1 \\4 \\5")

(remind-to-diary-define-convert-reminder
 remind-to-diary-convert-timed-reminder-with-end-time
 (format "\\(%s\\)/\\(%s\\)/\\(%s\\) %s %s %s %s \\(%s\\)-\\(%s\\) \\(%s\\)"
         remind-to-diary-year-regex
         remind-to-diary-month-regex
         remind-to-diary-day-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-field-regex
         remind-to-diary-time-regex
         remind-to-diary-time-regex
         remind-to-diary-desc-regex)
 "\\2/\\3/\\1 \\4-\\5 \\6")


;; functions to run the conversion

;;;###autoload
(defun remind-to-diary-run (&optional arg)
  "Run remind process in temp buffer, convert each line to diary format, and save to REMIND-TO-DIARY-DIARY-FILE.

By default run only if there are new reminders, as determined by
REMIND-TO-DIARY-NEW-REMINDERS-P; but always run with prefix arg."
  (interactive "P")
  (if (and (not arg)
           (not (remind-to-diary-new-reminders-p)))
      (message "remind-to-diary-run: no new reminders")
    (with-temp-buffer
      (save-excursion
        (apply #'call-process remind-to-diary-remind-bin nil t nil
               (remind-to-diary-get-remind-args)))
      (remind-to-diary-convert-remind-simple-calendar-to-diary)
      (write-file remind-to-diary-diary-file))))

(defun remind-to-diary-new-reminders-p ()
  "Return t if REMIND-TO-DIARY-REMINDERS-FILE is newer than diary file (or doesn't exist)."
  ;; Note that this assumes that one either has all reminders in the .reminders
  ;; file, that .reminders is a directory that contains all the reminders files,
  ;; or that there is some mechanism for updating the timestamp of the
  ;; .reminders file.  In other words, it does not work if one has a .reminders
  ;; file that `includes' other files, and one updates one of the other files
  ;; without modifying the timestamp of .reminders.
  (if (and (file-exists-p remind-to-diary-diary-file)
           (file-newer-than-file-p remind-to-diary-diary-file
                                   remind-to-diary-reminders-file))
      nil
    t))

(defun remind-to-diary-get-remind-args ()
  "Return list of remind command-line arguments given user customizable variables.

The following options, or variants of them, are required:
   -q: don't run as a daemon
   -s: remind output in `simple calendar' format
   -b: 12-hour or 24-hour time format"
  (list "-q"
        (concat "-s"
                (if remind-to-diary-include-advanced-reminders "a" "")
                (int-to-string remind-to-diary-months-of-reminders))
        (if remind-to-diary-use-24-hour-times "-b1" "-b0")
        (shell-quote-argument remind-to-diary-reminders-file)))

(defun remind-to-diary-convert-remind-simple-calendar-to-diary ()
  "Process and save reminders to REMIND-DIARY-FILE in diary format."
  (let ((parsed-lines 0)
        (unparsed-lines 0))
    (while (not (eobp))
      (if (or (remind-to-diary-convert-timed-reminder-with-end-time)
              (remind-to-diary-convert-timed-reminder)
              (remind-to-diary-convert-untimed-reminder))
          (incf parsed-lines)
        (incf unparsed-lines))
        (forward-line 1))
      (if unparsed-lines
          (message "parsed %d remind lines, %d unparsed"
                   parsed-lines unparsed-lines))
      (message "parsed %d remind lines" parsed-lines)))

(provide 'remind-to-diary)

;;; remind-to-diary.el ends here
