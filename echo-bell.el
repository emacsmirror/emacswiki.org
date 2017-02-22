;;; echo-bell.el --- Show visual bell at right in the echo area.    -*- coding:utf-8 -*-
;; 
;; Filename: echo-bell.el
;; Description: Show visual bell at right in the echo area.
;; Author: Miles Bader <miles@gnu.org>
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2009, Miles Bader, all rights reserved.
;; Copyright (C) 2009-2017, Drew Adams, all rights reserved.
;; Created: Fri Feb 27 20:32:14 2009 (-0800)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Tue Feb 21 16:35:30 2017 (-0800)
;;           By: dradams
;;     Update #: 125
;; URL: https://www.emacswiki.org/emacs/download/echo-bell.el
;; Keywords: echo area, bell, ding
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;  Show visual bell at right in the echo area.
;;
;;  Global minor mode `echo-bell-mode' replaces the audible bell
;;  with a visual indication in the echo area.
;;
;;  Options:
;;  * `echo-bell-string' is the text to show.
;;  * `echo-bell-delay' is the number of seconds to show it.
;;  * `echo-bell-background' is the background color to use.
;; 
;; Initial code was from http://www.emacswiki.org/emacs/MilesBader.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2015/10/07 dadams
;;     echo-bell-delay: Changed default value from 0.4 to 0.2.
;; 2015/03/01 dadams
;;     Added autoload cookie for echo-bell-mode.
;; 2015/02/28 dadams
;;     Added: echo-bell-mode, echo-bell-update.
;;     Added :type (using color widget) and :set to defcustoms (use echo-bell-update).
;;     Changed default values of defcustoms.
;;     echo-bell: Use echo-bell-update.
;; 2009/02/28 dadams
;;     Added: echo-bell-background.
;;     Changed default value of echo-bell-string from ding.
;;     Created from post at http://www.emacswiki.org/emacs/MilesBader.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar echo-bell-cached-string nil "Copy of last `echo-bell-string'.")
(defvar echo-bell-propertized-string nil "Display version of `echo-bell-string'.")

(defun echo-bell-update ()
  "Update strings used for `echo-bell-mode'."
  (setq echo-bell-propertized-string
        (propertize
         (concat (propertize
                  "x"
                  'display `(space :align-to (- right ,(string-width echo-bell-string))))
                 echo-bell-string)
         'face `(:background ,(if (boundp 'echo-bell-background) ; For first use only.
                                  echo-bell-background
                                  "Aquamarine"))))
  (setq echo-bell-cached-string  echo-bell-string))

(defcustom echo-bell-string "♪♪♪♪♪♪♪♪♪"
  "Message shown in the echo area by function `echo-bell'."
  :type  'string
  :set   (lambda (sym defs) (custom-set-default sym defs) (echo-bell-update))
  :group 'user)

(defcustom echo-bell-background "Aquamarine"
  "Background color for `echo-bell-string'."
  :type  'color
  :set   (lambda (sym defs) (custom-set-default sym defs) (echo-bell-update))
  :group 'user)

(defcustom echo-bell-delay 0.2
  "Number of seconds that `echo-bell' displays its message."
  :type 'number :group 'user)

(defun echo-bell ()
  "Show `echo-bell-string' in the echo area for `echo-bell-delay' sec.
It is shown at the far right of the echo area.
You can use this function as the value of `ring-bell-function'."
  (unless (equal echo-bell-string echo-bell-cached-string) (echo-bell-update))
  (message echo-bell-propertized-string) (sit-for echo-bell-delay) (message ""))

;;;###autoload
(define-minor-mode echo-bell-mode
    "Indicate bell visually in the echo area.
Turning the mode off resets `ring-bell-function' to nil (not to its
former value)."
  :global t
  (setq ring-bell-function  (and echo-bell-mode  'echo-bell))
  (when (interactive-p)
    (message "Visual bell in echo area is now %s." (if echo-bell-mode "ON" "OFF"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'echo-bell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; echo-bell.el ends here
