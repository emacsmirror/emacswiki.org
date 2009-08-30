;;; later-do.el --- execute lisp code ... later

;;; Copyright (C)  2004  Jorgen Schaefer <forcer@forcix.cx>

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;; Commentary

;; This file will execute lisp code "later on". This way it is
;; possible to work while elisp does some longer calculations, if you
;; can convert those calculations into a sequence of function calls.

;;; Code:

(defvar later-do-version "0.2 (2004-02-09)"
  "Version string of later-do.")

(defgroup later-do nil
  "*Running functions ... later!"
  :prefix "later-do-"
  :group 'development)

(defcustom later-do-interval 0.5
  "How many seconds to wait between running events."
  :group 'later-do
  :type 'number)

(defvar later-do-list nil
  "A list of functions to be called lateron.")

(defvar later-do-timer nil
  "The timer that later-do uses.")

(defun later-do (function &rest args)
  "Apply FUNCTION to ARGS later on. This is an unspecified amount of
time after this call, and definitely not while lisp is still
executing.
Code added using `later-do' is guaranteed to be executed in the
sequence it was added."
  (setq later-do-list (append later-do-list
                              (list (cons function args))))
  (unless later-do-timer
    (setq later-do-timer
          (run-with-timer later-do-interval nil 'later-do-timer))))

(defun later-do-timer ()
  "Run the next element in `later-do-list', or do nothing if it's
empty."
  (if (null later-do-list)
      (setq later-do-timer nil)
    (setq later-do-timer (run-with-timer later-do-interval
                                         nil
                                         'later-do-timer))
    (apply (caar later-do-list)
           (cdar later-do-list))
    (setq later-do-list (cdr later-do-list))))

(provide 'later-do)
;;; later-do.el ends here
