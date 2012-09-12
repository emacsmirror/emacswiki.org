;;; unicode-progress-reporter.el --- Progress-reporter with fancy characters
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/unicode-progress-reporter
;; URL: http://raw.github.com/rolandwalker/unicode-progress-reporter/master/unicode-progress-reporter.el
;; Version: 0.5.2
;; Last-Updated: 27 Aug 2012
;; EmacsWiki: UnicodeProgressReporter
;; Package-Requires: ((emacs "24.1.0") (ucs-utils "0.6.0") (persistent-soft "0.8.0") (pcache "0.2.3"))
;; Keywords: interface
;;
;; GPLv3 License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'unicode-progress-reporter)
;;
;;     (unicode-progress-reporter-setup)
;;
;;     ;; to see a demo
;;     (unicode-progress-reporter-test)
;;
;; Explanation
;;
;; This is a trivial modification to Emacs' built-in progress
;; reporter to display spinners using Unicode characters.
;;
;; To use unicode-progress-reporter, place the
;; unicode-progress-reporter.el library somewhere Emacs can
;; find it, and add the following to your ~/.emacs file:
;;
;;     (require 'unicode-progress-reporter)
;;     (unicode-progress-reporter-setup)
;;
;; See Also
;;
;;     M-x customize-group RET unicode-progress-reporter RET
;;
;;     (unicode-progress-reporter-test)
;;
;; Notes
;;
;;     redefines `progress-reporter-do-update'
;;
;;     alters private variable `progress-reporter--pulse-characters'
;;
;; Compatibility and Requirements
;;
;;     Requires GNU Emacs version 24.1 or above
;;
;;     Requires ucs-utils.el
;;
;; Bugs
;;
;; TODO
;;
;;; License
;;
;; This library is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Code:
;;

;;; requires

;; for let*
(eval-when-compile
  (defvar unicode-progress-reporter-type)
  (require 'cl))

(autoload 'ucs-utils-vector "ucs-utils" "Return a vector corresponding to SEQUENCE of UCS names or characters.")
(autoload 'ucs-utils-char   "ucs-utils" "Return the character corresponding to NAME, a UCS name.")

;;; variables

(defvar unicode-progress-reporter-pulse-characters
  '(("Horizontal Blocks"
     "Left One Eighth Block"
     "Left One Quarter Block"
     "Left Three Eighths Block"
     "Left Half Block"
     "Left Five Eighths Block"
     "Left Three Quarters Block"
     "Left Seven Eighths Block"
     "Full Block")
    ("Moons"
     "New Moon Symbol"
     "Waxing Crescent Moon Symbol"
     "First Quarter Moon Symbol"
     "Waxing Gibbous Moon Symbol"
     "Full Moon Symbol"
     "Waning Gibbous Moon Symbol"
     "Last Quarter Moon Symbol"
     "Waning Crescent Moon Symbol")
    ("Vertical Blocks"
     "Lower One Eighth Block"
     "Lower One Quarter Block"
     "Lower Three Eighths Block"
     "Lower Half Block"
     "Lower Five Eighths Block"
     "Lower Three Quarters Block"
     "Lower Seven Eighths Block"
     "Full Block")
    ("Vertical Counting Rods"
     "Counting Rod Tens Digit One"
     "Counting Rod Tens Digit Two"
     "Counting Rod Tens Digit Three"
     "Counting Rod Tens Digit Four"
     "Counting Rod Tens Digit Five")
    ("Clocks"
     "Clock Face Twelve Oclock"
     "Clock Face One Oclock"
     "Clock Face Two Oclock"
     "Clock Face Three Oclock"
     "Clock Face Four Oclock"
     "Clock Face Five Oclock"
     "Clock Face Six Oclock"
     "Clock Face Seven Oclock"
     "Clock Face Eight Oclock"
     "Clock Face Nine Oclock"
     "Clock Face Ten Oclock"
     "Clock Face Eleven Oclock")
    ("Ogham Letters"
     "Ogham Letter Muin"
     "Ogham Letter Gort"
     "Ogham Letter Ngeadal"
     "Ogham Letter Straif"
     "Ogham Letter Ruis")
    ("Horizontal Counting Rods"
     "Counting Rod Unit Digit One"
     "Counting Rod Unit Digit Two"
     "Counting Rod Unit Digit Three"
     "Counting Rod Unit Digit Four"
     "Counting Rod Unit Digit Five")
    ("Triangles"
     "Black Up-Pointing Triangle"
     "Black Right-Pointing Triangle"
     "Black Down-Pointing Triangle"
     "Black Left-Pointing Triangle")
    ("ASCII"
     ?-
     ?\\
     ?|
     ?/
     )))

;;; utility functions

(defun unicode-progress-reporter-redefine-spinner (symbol value)
  "Set `progress-reporter--pulse-characters'.

SYMBOL is a symbol to set to VALUE.

VALUE should be a key in `unicode-progress-reporter-pulse-characters'."
  (custom-set-default symbol value)
  (let ((cell (assoc-string unicode-progress-reporter-type
                            unicode-progress-reporter-pulse-characters)))
    (when (or (not cell)
              (not (ucs-utils-char (cadr cell) nil 'cdp)))
      (setq cell (assoc-string "ASCII"
                               unicode-progress-reporter-pulse-characters)))
    (setq progress-reporter--pulse-characters
          (apply 'vector
                 (mapcar 'string
                         (ucs-utils-vector (cdr cell)))))))

(defun unicode-progress-reporter-redefine-updater ()
  "Redefine `progress-reporter-do-update'."
  ;; a copy from subr.el from Emacs 24.1, with a single change
  ;; to allow varying numbers of reporter characters
  (defun progress-reporter-do-update (reporter value)
    (let* ((parameters   (cdr reporter))
           (update-time  (aref parameters 0))
           (min-value    (aref parameters 1))
           (max-value    (aref parameters 2))
           (text         (aref parameters 3))
           (current-time (float-time))
           (enough-time-passed
            ;; See if enough time has passed since the last update.
            (or (not update-time)
                (when (>= current-time update-time)
                  ;; Calculate time for the next update
                  (aset parameters 0 (+ update-time (aref parameters 5)))))))
      (cond ((and min-value max-value)
             ;; Numerical indicator
             (let* ((one-percent (/ (- max-value min-value) 100.0))
                    (percentage  (if (= max-value min-value)
                                     0
                                   (truncate (/ (- value min-value)
                                                one-percent)))))
               ;; Calculate NEXT-UPDATE-VALUE.  If we are not printing
               ;; message because not enough time has passed, use 1
               ;; instead of MIN-CHANGE.  This makes delays between echo
               ;; area updates closer to MIN-TIME.
               (setcar reporter
                       (min (+ min-value (* (+ percentage
                                               (if enough-time-passed
                                                   ;; MIN-CHANGE
                                                   (aref parameters 4)
                                                 1))
                                            one-percent))
                            max-value))
               (when (integerp value)
                 (setcar reporter (ceiling (car reporter))))
               ;; Only print message if enough time has passed
               (when enough-time-passed
                 (if (> percentage 0)
                     (message "%s%d%%" text percentage)
                   (message "%s" text)))))
            ;; Pulsing indicator
            (enough-time-passed
             (let ((index (mod (1+ (car reporter)) (length progress-reporter--pulse-characters)))
                   (message-log-max nil))
               (setcar reporter index)
               (message "%s %s"
                        text
                        (aref progress-reporter--pulse-characters
                              index))))))))


;;; customizable variables

;;;###autoload
(defgroup unicode-progress-reporter nil
  "Progress-reporter with fancy characters."
  :version "0.5.2"
  :link '(emacs-commentary-link "unicode-progress-reporter")
  :prefix "unicode-progress-reporter-"
  :group 'faces)

(defcustom unicode-progress-reporter-type "Horizontal Blocks"
  "Type of spinner characters to use for progress-reporter."
  :type `(choice ,@(mapcar #'(lambda (x)
                               (list 'const (car x)))
                           unicode-progress-reporter-pulse-characters))
  :initialize 'custom-initialize-default
  :set 'unicode-progress-reporter-redefine-spinner
  :group 'unicode-progress-reporter)

;;; debugging functions

(defun unicode-progress-reporter-test ()
  "Test unicode-progress-reporter."
  (let ((reporter (make-progress-reporter "Testing... ")))
    (dotimes (i 200)
      (progress-reporter-update reporter)
      (sit-for .1))
    (progress-reporter-done reporter)))

;;; interactive commands

;;;###autoload
(defun unicode-progress-reporter-setup ()
  "Set up unicode spinners for progress-reporter."
  (interactive)
  (unicode-progress-reporter-redefine-spinner 'unicode-progress-reporter-type unicode-progress-reporter-type)
  (unicode-progress-reporter-redefine-updater))

(provide 'unicode-progress-reporter)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; End:
;;
;; LocalWords: UnicodeProgressReporter utils Oclock Ogham
;;

;;; unicode-progress-reporter.el ends here
