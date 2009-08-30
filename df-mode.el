;;; df-mode.el --- Minor mode to show space left on devices in the mode line 

;; Copyright (C) 1999 by Association April
;; Copyright (C) 2004-2005 by Frederik Fouvry

;; Author: Benjamin Drieu <bdrieu@april.org>
;;         Frederik Fouvry <Frederik.Fouvry@coli.uni-saarland.de>
;; Keywords: unix, tools

;; This file is NOT part of GNU Emacs.
;; This is free software.

;; GNU Emacs as this program are free software; you can redistribute
;; them and/or modify them under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; They are both distributed in the hope that they will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;;  This is an extension to display disk usage in the mode line.  Disk
;;  space remaining is updated every `df-interval' seconds.
;; 
;;  If you work with a lot of users sharing the same partition, it
;;  sometimes happens that you have no place left to save your work,
;;  which can be extremely annoying, and might lead to loss of work.
;;  This package allows you to have disk space available and buffer
;;  size displayed in the mode line, so you know when you can save
;;  your file or when it is time to do some tidying up.
;;
;;  Comments and suggestions are welcome.
;;
;;  df is simple to use:
;;  - Make sure this file is in your load-path
;;  - Put in ~/.emacs:
;;      (autoload 'df-mode "df-mode" nil t)
;;      (df-mode 1)
;;    The minor mode can be toggled with M-x df-mode.
;;
;;  Version: 2004-11-05
;;  Note: this is a rewrite of df.el by Benjamin Drieu:
;;  - Added customization
;;  - Completely rewrote internals (the actual "engine" has been
;;    simplified, but the customisation enhanced)
;;  - The partitions that are checked are now buffer-local
;;  - Added the function that checks disk space before writing a
;;    buffer
;; 
;;  2005-05-09
;;  - Added test for the case that `buffer-file-name' is a
;;    non-existing file (e.g. because newly created)
;;  - Use define-minor-mode, and remove the code that was made
;;    redundant by this.  (df-mode var and function,
;;    df-mode-set-df-mode).  The trigger for the mode is now solely
;;    df-mode, and not df-timer anymore.  df-update now first checks
;;    df-mode.
;;  - Use new hook variable names in df-mode.
;;  - Use a cancel and initialisation function for the timer (to avoid
;;    having more than one timer, eg after starting from a desktop) 
;;  - Add check the df-interval > 0.  If not, the df check is not done
;;    anymore, but the mode is left on.

;;; Code:

(require 'timer)

(defgroup df nil
  "Disk space monitoring"
  :group 'environment
  :version "22.1")

(defvar df-timer nil
  "Variable containing the timer for `df-update'.")

(defsubst df-cancel-timer ()
  "Cancel `df-timer'."
  (setq df-timer (when df-timer (cancel-timer df-timer))))

(defsubst df-initialise-timer ()
  "Initialise `df-timer'."
  (df-cancel-timer)
  (when (> df-interval 0)
    (setq df-timer (run-at-time 0 df-interval 'df-update))))

(defcustom df-interval 60 
  "*The time \(in seconds) between updates of the disk usage
statistics in the mode line.  A value of 0 switches off the check
but leaves the mode on (i.e. it does not update the information
in the mode line)."
  :type 'integer
  :group 'df
  :set (lambda (sym val)
	 (set-default sym val)
	 (df-initialise-timer)))

(defcustom df-block-size 1000
  "*The size of the blocks \(in kilobytes) that
`directory-free-space-program' is asked to count.  It must be a
non-zero positive integer.  \"1k\" in the mode-line denotes one
such block.  Common values are 1000 and 1024 \(bytes) .  The
POSIX value of 512 can also be used, as can other values, but
they might be confusing (in the case of the POSIX value \"1k\"
denotes 512 bytes).

`df-block-size' is not to be confused with `df-unit', which is
the factor of difference between the unit prefixes."
  :type '(choice (const 1000)
		 (const 1024)
		 (const :tag "POSIX" 512)
		 (integer :tag "Another value"))
  :group 'df)

(defcustom df-unit 1000
  "*This is the difference in order of magnitude between the
subsequent elements in `df-unit-array': `df-unit'k is 1M, and so
on.  In SI, this is 1000, but elsewhere 1M \(for instance) is
commonly taken to be 1024k."
  :type '(choice (const :tag "SI" 1000)
		 (const :tag "Human-readable" 1024))
  :group 'df)

(defcustom df-output-regexp
  "^\\S-+\\s-+\\(?:[0-9]+\\s-+\\)\\{2\\}\\([0-9]+\\)\\s-+[0-9]\\{1,3\\}% .+$"
;; Linux, non POSIX:
;; "\\s-+\\(?:[0-9]+\\s-*\\)\\{2\\}\\([0-9]+\\)\\s-*[0-9]\\{1,3\\}% /"
  "*Regexp describing the output of
`directory-free-space-program'.  The amount of free space should
be in group 1.

See also `df-switches'."
  :type 'regexp
  :group 'df)

(defcustom df-switches '("-P")
  "*This is a list of strings containing command line switches
for `directory-free-space-program'.  If you add or remove
switches that change the format of the output from
`directory-free-space-program', you need to adapt
`df-output-regexp' as well.

In GNU df, \"-P\" makes `directory-free-space-program' print each
result on one line."
  :type '(repeat string)
  :group 'df)

(defcustom df-block-size-switch "-B"
  "*Command line switch to `directory-free-space-program' for
setting the block size.  The size \(see `df-block-size') will be
attached directly after the switch \(without spaces)."
  :type 'string
  :group 'df)


(defvar df-string ""
  "Variable containing the string to be inserted into the mode line.")
(make-variable-buffer-local 'df-string)
(defvar df-free-space nil
  "Variable containing the amount of free space on the current partition.")
(make-variable-buffer-local 'df-free-space)

(defvar df-buffer-weight 0)
(make-variable-buffer-local 'df-buffer-weight)

;; The smallest unit should be the first
(defconst df-unit-array ["k" "M" "G" "T" "P" "E" "Z" "Y"]
  "Array with unit prefixes.  Every next element should be
`df-unit' times bigger than the current one.")



(defun df-reduce-number (number)
  "Takes NUMBER \(assumed to be in the unit \"k\", the first
element of `df-unit-array'), and converts it to the largest unit
suitable \(i.e. where the value lies between 0 and `df-unit').
It returns a cons with the new value and its unit."
  (if (> number 0)
      (let* ((number (float number))
	     (exponent (floor (log number df-unit))))
	(cons (/ number (expt df-unit exponent))
	      (aref df-unit-array exponent)))
    (cons 0 (aref df-unit-array 0))))


(defun df-find-existing-path (path)
  (if (file-directory-p path)
      path
    (df-find-existing-path
     (file-name-directory (directory-file-name path)))))


(defun df-update ()
  "Updates the disk usage statistics \(if `df-interval' > 0).  It
is run every `df-interval' seconds."
  (interactive)
  (if (> df-interval 0)
      (when df-mode
	(setq df-buffer-weight (and (buffer-file-name)
				    (/ (buffer-size) df-unit)))
	;; Avoid trying to check and run in non-existing directories
	(let* ((default-directory (df-find-existing-path default-directory)))
	  (set-process-filter
	   (apply 'start-process directory-free-space-program nil 
		  directory-free-space-program
		  (cons (format "%s%d" df-block-size-switch df-block-size)
			(append df-switches (list default-directory))))
	   'df-filter)))
    ;; (setq df-string "")
    ))


;; For float values:
;; (format "%%.%df" (- (ceiling (log df-unit 10)) (ceiling (log (car res) 10))))
;; but trailing `0's should be removed
(defun df-filter (proc string)
  "Filter for the output from `directory-free-space-program'.
It sets the mode-line value." ;; from the process output
  (setq df-string
	(format " %s/%s"
		(if df-buffer-weight
		    (let ((res (df-reduce-number df-buffer-weight)))
		      (format "%d%s" (round (car res)) (cdr res)))
		  "-")
		(if (and (string-match df-output-regexp string)
			 (match-string 1 string))
		    (progn
		      (setq df-free-space
			    (string-to-number (match-string 1 string)))
		      (let ((res (df-reduce-number df-free-space)))
			(format "%d%s" (round (car res)) (cdr res))))
		  "-"))))


;; Is used in run-hooks-with-args-until-success, so it should always return
;; nil.
(defun df-check (&rest args)
  "Check whether there is still enough space to save the file.
If not, ask whether the file should be saved or not."
  (df-update)
  (when (and (numberp df-buffer-weight)
	     (numberp df-free-space)
	     (> df-buffer-weight df-free-space)
	     (not (yes-or-no-p 
		   "It seems there is not enough disk space.  Save anyway? ")))
    (error "Buffer %s not saved" (buffer-name)))
  nil)


;;;###autoload
(define-minor-mode df-mode
  "Toggle display of space that is left on the filesystem on
which the contents of the current buffer is stored.  This display
is updated every `df-interval' seconds.

With a numeric argument, enable this display if ARG is positive."
  :group 'df
  :global t
  :init-value nil
  :lighter df-string
  (if df-mode
      (progn
	(add-hook 'write-file-functions 'df-check)
	(add-hook 'find-file-hook 'df-update)
	(df-initialise-timer))
    (progn
      (remove-hook 'write-file-functions 'df-check)
      (remove-hook 'find-file-hook 'df-update)
      (df-cancel-timer)))
  df-mode)

(provide 'df-mode)

;;; df-mode.el ends here
