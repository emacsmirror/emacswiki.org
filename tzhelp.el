;;; tzhelp - very simple time zone tools

;;; Copyright (C) 2010 by Nic Ferrier <nic@ferrier.me.uk>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA

;;; Commentary:
;;;
;;; This was born from my need to have some simple tools for timezone
;;; handling. 
;;;
;;; See the help for each function.

(defun tzhelp-pst-to-gmt (hour)
  "Convert a specific hour from PST to GMT."
  (interactive "nHour: ")
  (let ((x (+ hour 8)))
    (message "the hour in GMT is: %d%s"
             (if (> x 24)
                 (- x 24)
               x)
             (if (> x 24) "AM" "PM")
             )))

;; End
