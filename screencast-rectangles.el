;;; screencast-rectangles.el
;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: esbenandreasen <esbenandreasen@gmail.com>(new)

;; Keywords: screencast

;; This file is not an official part of emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Code:

(require 'screencast-record)
(setq screencast-speech nil)
(defun screencast-rectangles (&optional arg)
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
         text
         "rectangles"
         1.1
         ()
         )
  )

(setq text
      '("This is the screencast for using rectangles in Emacs."n
        "Rectangles in Emacs are visually confusing when using a
visible region, the point to remember is that the area between
point and mark is rectangular."n
"You can write a lot of duplicate lines at the same time using rectangles, let's first create the region:"
(newline 4)
(set-mark (point))
(beginning-of-buffer)
(screencast-producer-show-region (point) (mark))
(string-rectangle (point) (mark) "<---->")
"Notice that the region is replaced with the text you type, let's use this to change the arrows:"
(forward-char 1)
(set-mark (point))
(next-line 3)
(forward-char 4)
(screencast-producer-show-region (mark) (point))
(string-rectangle (mark) (point) "====")
"Rectangles can be killed and yanked too:"
(forward-char 4)
(exchange-point-and-mark)
(screencast-producer-show-region (point) (mark))
(kill-rectangle (point) (mark))
"Yank twice, but be aware that a yank moves point:"
(yank-rectangle)
(previous-line 3)
(yank-rectangle)
"(This could have been done using copy-rectangle-to-register, which is the way to copy rectangles)"
b
"You can also replace a rectangle with whitespace:"
(set-mark (point))
(beginning-of-buffer)
(forward-char)
(screencast-producer-show-region (point) (mark))
(clear-rectangle (point) (mark))
"Or just delete it completely:"
(set-mark (point))
(next-line 3)
(forward-char 4)
(forward-char 4)
(screencast-producer-show-region (mark) (point))
(delete-rectangle (mark) (point))
"And finally 'reopen' it by filling a rectangle with whitespace (shifting other text):"
(progn
  (end-of-line)
  (insert "   ")
  )
(end-of-line)
(set-mark (point))
(beginning-of-buffer)
(forward-char)
(screencast-producer-show-region (point) (mark))
(open-rectangle (point) (mark))
)
)
