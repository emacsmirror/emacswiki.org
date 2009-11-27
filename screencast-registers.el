;;; screencast-registers.el

;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: esbenandreasen <esbenandreasen@gmail.com>

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
(defun screencast-registers (&optional arg)
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
         screencast-registers-text
         "registers"
         1.1
         ()
         )
  )
(setq screencast-registers-text
      '(
        "Hello, and welcome to the screencast for using the registers in Emacs." n
        "The registers can store all kinds of data in an intelligent way."n
        "Registers have single letter names, which you are prompted for on use."n
        "(let's put some text, to use as an example)"
        (progn (insert
                "List of stuff:
- funny chars: !@#$%^&*()-
- important location
-
-
-
"(save-buffer))
               (previous-line 4))

        b
        ;; positions
        "Registers can store positions, like a persistent mark:"
        (point-to-register 'point)
        "Let's move around"
        (next-line 1)
        (previous-line 3)
        "And jump back:"
        p p
        (jump-to-register 'point)
        (progn (search-backward "!"))
        b
        ;; bookmarks
        "Bookmarks are also recorded and restored like registers, but they are (of course) not forgotten when Emacs is closed."n
        "You just store the point like before, in a slightly different way: you get to name the bookmarks with longer names than just a single letter"n
        (bookmark-set "Screencast-register-bookmark" nil)
        "Restoring is also like point restoration, but now you get to type the name of the bookmark"n
        (previous-line 1)
        (bookmark-jump "Screencast-register-bookmark")
        "(please notice that the bookmarks by default aren't saved to a file every time you create a bookmark)"n
        (progn (bookmark-save))
        "You can also see all the stored bookmarks:"
        (bookmark-bmenu-list)
        (progn (switch-to-buffer "*Bookmark List*"))
        "This list is displayed in a special mode, which requires its own screencast."
        (progn (kill-buffer "*Bookmark List*"))
        b
        ;; text
        "Text can also be stored - just like in the kill ring, but now it isn't pushed back by other kills." n
        "Just mark some text, and put it in the register:"
        (set-mark-command nil)
        (end-of-line 1)
        (copy-to-register 'region (point) (mark))
        "(We can also append and prepend to an existing register)"n
        "Now we can easily insert the text any time we want it:"
        (insert-register 'region)
        b
        ;; rectangles
        (progn (beginning-of-line 1))
        "A special case of saving text, is when it is in a rectangle."n
        "This is handled the same way as before:"
        (set-mark-command nil)
        (next-line 3)
        (forward-char 1)
        (copy-rectangle-to-register 'rectangle (mark) (point))
        (end-of-buffer)
        "Insertion is the same as before, the storing is just a little different:"
        (insert-register 'rectangle)
        b                           
        ;; numbers
        "Numbers can be stored as numbers (I'll store 42):"
        (number-to-register 42 'number)
        "This allows you to increase it at will:"
        (increment-register 1 'number)
        "Insertion is once again done like normal text:"
        (insert-register 'number)
        b
        ;; windows                  
        "You can also save Window configurations!"
        (window-configuration-to-register 'windows)
        "Let's mess up the windows:"
        (split-window-vertically)
        (split-window-horizontally)
        (split-window-vertically)
        "And restore it in the same way as we did when restoring point:"
        (jump-to-register 'windows)
        (progn
          (other-window 1)
          (switch-to-buffer (get-buffer screencast-message-buffer-name))
          (goto-char (point-max))
          )
        "(This is also possible for frame configurations.)"n
        b
        "That's it for now."
        )
      )
