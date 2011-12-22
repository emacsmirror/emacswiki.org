;;; sl.el --- sl for emacs lisp

;; Copyright (C) 2011  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: joke

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a emacs lisp version of sl program that exists in UNIX
;; environment.

;;; Config:

;; Please put this file into your load-path directory, And add following
;; line to your .emacs.
;;
;; (require 'sl)

;;; Usage:

;; You will simply use this program by M-x sl.

;;; References:

;; - sl (original)
;;   http://www.tkl.iis.u-tokyo.ac.jp/~toyoda/index_e.html
;;
;; - sl for javascript
;;   http://creazy.net/2008/02/sl_js.html

;;; Tested:

;; * Emacs versions
;;   - 23.3
;;   - 24.0.90

;;; Todo:

;; * More documents.
;;   - docstring, comment...
;;
;; * Add many various features of the scrolling.
;;   - scroll top to bottom.
;;   - scroll bottom to top.
;;   - scroll left to right.
;;
;; * Add animation feature.
;;   - show overlayed text image to the center of window and animation
;;     it.
;;
;; * Play stream whistle sound.
;;   # Is it possible to make only using emacs lisp funtions (do not use
;;   # external program)?

;;; ChangeLog:

;; * 0.0.1 (2011/12/22)
;;   * A initial version.

;;; Code:

;;; Constatns:

(defconst sl-version "0.0.1"
  "A sl.el version number string.")

(defconst sl-steam
  (list (concat "                      (@@) (  ) (@)  ( )  @@    ()    @     O     @     O      @" "\n"
		"                 (   )" "\n"
		"             (@@@@)" "\n"
		"          (    )" "\n"
		"\n"
		"        (@@@)" "\n")
	(concat "                      (  ) (@@) ( )  (@)  ()    @@    O     @     O     @      O" "\n"
		"                 (@@@)" "\n"
		"             (    )" "\n"
		"          (@@@@)" "\n"
		"\n"
		"        (   )" "\n")))

(defconst sl-body
  (concat
   "      ====        ________                ___________ "  "\n"
   "  _D _|  |_______/        \\__I_I_____===__|_________| " "\n"
   "   |(_)---  |   H\\________/ |   |        =|___ ___|      _________________         " "\n"
   "   /     |  |   H  |  |     |   |         ||_| |_||     _|                \\_____A  " "\n"
   "  |      |  |   H  |__--------------------| [___] |   =|                        |  " "\n"
   "  | ________|___H__/__|_____/[][]~\\_______|       |   -|                        |  " "\n"
   "  |/ |   |-----------I_____I [][] []  D   |=======|____|________________________|_ " "\n"))

(defconst sl-wheel
  (list (concat "__/ =| o |=-O=====O=====O=====O \\ ____Y___________|__|__________________________|_ " "\n"
		" |/-=|___|=    ||    ||    ||    |_____/~\\___/          |_D__D__D_|  |_D__D__D_|   " "\n"
		"  \\_/      \\__/  \\__/  \\__/  \\__/      \\_/               \\_/   \\_/    \\_/   \\_/    " "\n")
	(concat "__/ =| o |=-~~\\  /~~\\  /~~\\  /~~\\ ____Y___________|__|__________________________|_ " "\n"
		" |/-=|___|=O=====O=====O=====O   |_____/~\\___/          |_D__D__D_|  |_D__D__D_|   " "\n"
		"  \\_/      \\__/  \\__/  \\__/  \\__/      \\_/               \\_/   \\_/    \\_/   \\_/    " "\n")
	(concat "__/ =| o |=-~~\\  /~~\\  /~~\\  /~~\\ ____Y___________|__|__________________________|_ " "\n"
		" |/-=|___|=    ||    ||    ||    |_____/~\\___/          |_D__D__D_|  |_D__D__D_|   " "\n"
		"  \\_/      \\O=====O=====O=====O_/      \\_/               \\_/   \\_/    \\_/   \\_/    " "\n")
	(concat "__/ =| o |=-~~\\  /~~\\  /~~\\  /~~\\ ____Y___________|__|__________________________|_ " "\n"
		" |/-=|___|=    ||    ||    ||    |_____/~\\___/          |_D__D__D_|  |_D__D__D_|   " "\n"
		"  \\_/      \\_O=====O=====O=====O/      \\_/               \\_/   \\_/    \\_/   \\_/    " "\n")
	(concat "__/ =| o |=-~~\\  /~~\\  /~~\\  /~~\\ ____Y___________|__|__________________________|_ " "\n"
		" |/-=|___|=   O=====O=====O=====O|_____/~\\___/          |_D__D__D_|  |_D__D__D_|   " "\n"
		"  \\_/      \\__/  \\__/  \\__/  \\__/      \\_/               \\_/   \\_/    \\_/   \\_/    " "\n")
	(concat "__/ =| o |=-~O=====O=====O=====O\\ ____Y___________|__|__________________________|_ " "\n"
		" |/-=|___|=    ||    ||    ||    |_____/~\\___/          |_D__D__D_|  |_D__D__D_|   " "\n"
		"  \\_/      \\__/  \\__/  \\__/  \\__/      \\_/               \\_/   \\_/    \\_/   \\_/    " "\n")))

(defconst sl
  (let ((sl-strs))
    (dotimes (i (length sl-wheel))
      (setq sl-strs (cons (concat (if (> 3 i) (car sl-steam) (cadr sl-steam))
				  sl-body
				  (nth i sl-wheel))
			  sl-strs)))
    (nreverse sl-strs)))


;;; Customize group:

(defgroup sl nil
  "SL joke program."
  :tag "SL"
  :group 'sl)


;;; Variables:

(defcustom sl-face 'sl-face
  "*Face for sl."
  :type 'face
  :group 'sl)

(defcustom sl-place 'center
  "*Default displaying place of sl."
  :type 'symbol
  :group 'sl)

(defcustom sl-speed 100
  "*The speed of sl."
  :type 'number
  :group 'sl)


;;; Faces:

(defface sl-face
  '((((class color))  (:foreground "White" :background "Black" t))
    (t (:bold t)))
  "A face for sl.")


;;; Functions:

(defun sl-telop-make (telop-str &optional max-len)
  (let* ((telop (split-string telop-str "\n" t))
	 (max-len (if max-len max-len
		    (let ((len 0) str-len)
		      (dolist (i telop)
			(setq str-len (string-width i))
			(when (> str-len len)
			  (setq len str-len)))
		      len)))
	 str-len lis)
    (dolist (i telop)
      (setq str-len (string-width i))
      (setq lis
	    (cons (cond
		   ((= str-len max-len)
		    i)
		   ((> max-len str-len)
		    (concat i (make-string (- max-len str-len) ? ))))
		  lis)))
    (nreverse lis)))

(defun sl-telops-make (lis)
  (let* ((max-str-len (let ((max-len 0) len)
			(dolist (i lis)
			  (setq len (string-width (car (sl-telop-make i))))
			  (when (> len max-len)
			    (setq max-len len)))
			len))
	 telops)
    (dolist (i lis)
      (setq telops (cons (sl-telop-make i max-str-len) telops)))
    (nreverse telops)))

(defun sl-telop (str-list &optional face place speed not-inhibit-quit)
  (let* ((telops (sl-telops-make str-list))
	 (telop-height (length (car telops)))
	 (max-len (length (caar telops)))
	 (telop-frm (length telops))
	 (max-width (window-width))
	 (loops (+ max-width max-len))
	 (space 0)
	 (start (window-start))
	 (face (if face face sl-face))
	 (wait (/ 1.0 (if speed speed sl-speed)))
	 (place (if place place 'center))
	 (inhibit-quit (if not-inhibit-quit nil t))
	 (truncate-lines t)
	 (truncate-partial-width-windows t)
	 last-point telop telop-str overlay)
    (save-excursion
      (goto-char start)
      (cond
       ((eq place 'top)
	(forward-line telop-height))
       (t
	(forward-line (/ (- (window-height) telop-height) 2))
	(setq start (point))))
      (setq last-point (point))
      (dotimes (i loops)
	(setq telop (nth (% i telop-frm) telops)
	      overlay (make-overlay start last-point)
	      telop-str
	      (propertize
	       (concat
		(mapconcat
		 #'(lambda (x)
		     (let* (str left right)
		       (setq str
			     (cond
			      ((and (>= max-len i) (>= space max-width))
			       (substring x (- space max-width) max-width))
			      ((>= max-len i) (substring x 0 i))
			      ((>= space max-width)
			       (substring x (- space max-width)))
			      (t x))
			     space
			     (+ i (- (string-width str) (length str)))
			     left
			     (make-string
			      (cond
			       ((>= space max-width) 0)
			       ((>= max-len i) (- max-width space))
			       (t (- max-width space)))
			      ? )
			     right
			     (make-string
			      (cond
			       ((>= max-len i) 0)
			       ((>= space max-width)
				(- max-width (string-width str)))
			       (t (- i max-len)))
			      ? ))
		       (concat left str right)))
		 telop "\n")
		"\n")
	       'face face))
	(unwind-protect (progn
			  (overlay-put overlay 'after-string telop-str)
			  (overlay-put overlay 'invisible t)
			  (sleep-for wait)
			  (discard-input)
			  (redisplay))
	  (when (overlayp overlay)
	    (delete-overlay overlay)))))
    (discard-input)))

(defun sl (&optional face place speed not-inhibit-quit)
  (interactive)
  (sl-telop sl face place speed not-inhibit-quit))


(provide 'sl)

;;; sl.el ends here
