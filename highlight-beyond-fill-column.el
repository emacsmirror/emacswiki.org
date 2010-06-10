;;; highlight-beyond-fill-column.el --- font-lock-add-keywords aid for Emacs

;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

;; Author: Sandip Chitale (sandip.chitale@blazesoft.com)
;; Keywords: programming decipline convenience

;; Keywords:
;; Time-stamp: Aug 23 2001  8:56 PM Pacific Daylight Time
;; Version: 1.1

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary:

;; This defines a function that can be used by `font-lock-add-keywords' to find the columns
;; that are beyond `fill-column'.
;;
;; Installation:
;; Put the following in your .emacs
;;
;; (require 'highlight-beyond-fill-column)
;;
;; Example usage:
;;
;; Customize the `highlight-beyond-fill-column-in-modes' variable to
;; setup the list of modes in which to highlight-beyond-fill-column
;;
;; Customize the `highlight-beyond-fill-column-face' variable to
;; to setup the face used for highlight-beyond-fill-column
;;
;; Acknowledgement:
;;
;; This is based on initial code provided by Jim Janney (jjanney@xmission.com)
;;

;;; Code:
(defcustom highlight-beyond-fill-column-in-modes nil
  "The list of modes in which to highlight-beyond-fill-column."
  :group 'fill
  :type  '(repeat string)
  )

(defcustom highlight-beyond-fill-column-face 'underline
  "The face to use with highlight-beyond-fill-column."
  :group 'fill
  :type  'face
  )

(defun find-after-fill-column (limit)
  "A function that can be used by `font-lock-add-keywords' to find columns that are
beyond the `fill-column'."
  (let (
 ; remember the point
 (original-point (point))
 )
    ; if already past the fill column start on next line
    (if (> (current-column) fill-column)
 (forward-line 1)
      )
    (while (and (< (point) limit)                                     ; still within limit
                    (or (< (move-to-column fill-column) fill-column)  ; the line has less than `fill-column' columns
                        (= (point) (line-end-position))               ; end of line
   )
      )
      ; goto next line
      (forward-line 1)
      )

    (if (>= (point) limit)                                            ; beyond limit
 (progn
   (goto-char original-point)                                  ; restore point
   nil                                                         ; return nil
   )
      (set-match-data (list (point-marker)                            ; set match data
       (progn
         (end-of-line)
         (forward-char)                          ; this gives the highlight till the end of the window
         (point-marker)
         )
       )
        )
      t)                                                              ; return t indicating that the match data was set
    )
  )

(defun init-highlight-beyond-fill-column ()
  ""
  (let (
 (modelist highlight-beyond-fill-column-in-modes)
 mode
 )
    (while modelist
      (setq mode (intern (car modelist)))
      (if (and mode
        (functionp mode))
   (font-lock-add-keywords mode
      '(
        (find-after-fill-column 0 highlight-beyond-fill-column-face prepend)
        )
      )
 )
      (setq modelist (cdr modelist))
      )
    )
  )

(add-hook 'after-init-hook 'init-highlight-beyond-fill-column)

(provide 'highlight-beyond-fill-column)
