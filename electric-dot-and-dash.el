;;; electric-dot-and-dash.el --- Different way to enter brackets
;;
;; Copyright (C) 2006 Mathias Dahl
;;
;; Version: 0.1
;; Keywords: convenience
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This small hack makes it possible to enter left and right
;; parentheses by double tapping `.' or `-', respectively.  The keys
;; must be tapped in rapid succession, with a maximum delay taken from
;; the variable `electric-dot-and-dash-max-delay'.  When programming
;; Lisp, entering brackets is annoying since I have to use the
;; Shift-button and move my hand far up on the keyboard; I feel it
;; hampers the "flow" of typing.
;;
;; On my Swedish keyboard, these keys are just above the space key,
;; slightly to the left.
;;
;; The reason I wanted to try something like this was that it feels a
;; bit awkward having to press Shift+8 and Shift+9 to get at the
;; parentheses all the time when programming Lisp.
;;
;; It would be interesting to hear of similar approaches.
;;
;; I have only tried this for a few minutes and don't even know if I
;; like it (it might be totally useless), but I'll release it anyway
;; in case someone can find some use of it.
;;
;; Enjoy!


;;; History:
;; 

;;; Code:
(defvar electric-dot-and-dash-last-time nil
  "Keep track of the time we were last called.")

(defcustom electric-dot-and-dash-max-delay 0.5
  "Maximum delay between command invocations."
  :type 'float
  :group 'electric-dot-and-dash)

(defun electric-dot-and-dash (command)
  "Maybe behave in an electric way for COMMAND."
  (let ((time (current-time))
        (prev-cmd last-command))
    (if (and (eq prev-cmd command)
             (< (- (time-to-seconds time)
                   (time-to-seconds electric-dot-and-dash-last-time))
                electric-dot-and-dash-max-delay))
        (progn
          (delete-backward-char 1)
          (insert (cond ((eq command 'electric-dot-and-dash-dot) "(")
                        ((eq command 'electric-dot-and-dash-dash) ")"))))
      (insert (cond ((eq command 'electric-dot-and-dash-dot) ".")
                    ((eq command 'electric-dot-and-dash-dash) "-")))
      (setq electric-dot-and-dash-last-time (current-time)))))

(defun electric-dot-and-dash-dot ()
  "Insert a dot or parentheses.
If this command is called quickly two times in a row, a left
parentheses will be inserted instead of the two dots.  The delay
is controlled by `electric-dot-and-dash-max-delay'."
  (interactive)
  (electric-dot-and-dash 'electric-dot-and-dash-dot))

(defun electric-dot-and-dash-dash ()
  "Insert a dash or parentheses.
If this command is called quickly two times in a row, a right
parentheses will be inserted instead of the two dashes.  The
delay is controlled by `electric-dot-and-dash-max-delay'."
  (interactive)
  (electric-dot-and-dash 'electric-dot-and-dash-dash))

;; Try it in a buffer by binding `.' and `-'.
;; (local-set-key "." 'electric-dot-and-dash-dot)
;; (local-set-key "-" 'electric-dot-and-dash-dash)

(provide 'electric-dot-and-dash)

;;; electric-dot-and-dash.el ends here
