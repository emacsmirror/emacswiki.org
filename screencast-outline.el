;;; screencast-outline.el

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

(require 'screencast)
(defconst screencast-outline-mode-navigation-text
  '(
    "Welcome to the second part of the screencast for outline-mode."n
    "Now I'll show you how to navigate the trees."
    n p
    "Let's use the same text and mode as before." n
    (progn (insert screencast-outline-mode-example-text)
           (outline-mode)
           (goto-char (point-min)))
    "You can move to the next and previous heading - simply up and down in the document."n
    "Let's go three headings forward."
    (outline-next-visible-heading 1)
    (outline-next-visible-heading 1)
    (outline-next-visible-heading 1)
    p
    "And one back."
    (outline-previous-visible-heading 1)
    b
    "Notice that the cursor now is \"deep\" down in a subtree."n
    "We can go directly to the parent heading of that subtree. Skipping any headings on the same or lower levels."
    (outline-up-heading 1)
    b
    "The last thing we can do is navigate between headings on the same level."
    (outline-forward-same-level 1)
    "And back"
    (outline-backward-same-level 1)
    b
    "All of these commands use the numeric prefix argument, so if you want to move a long distance, consider using it."
    ))
(defconst screencast-outline-mode-hiding-text
  '(
    "This is the screencast for the outline-mode."n
    "outline-mode is a mode for viewing only selected parts of a buffer at a time."n
    "To use the mode type some text:"
    (insert screencast-outline-mode-example-text)
    (progn (goto-char (point-min)))
    p p
   
    "And now activate the mode, for some colors (requires font-lock-mode)"
    (outline-mode)
    b
    "The *'s marks headings of different levels, and thus forms a tree structure." n 
    "Each heading can have leaf content of text."
    n
    "We can hide these trees, and their content" n 
    "Let's hide the content of the  first heading:"
    (hide-entry)
    "We can also unhide it:"
    (show-entry)
    n p
    "For a quick overview, we can hide everything"
    (hide-body)
    "And unhide"
    (show-all)
    b
    "Now we'll use the tree structure and hide the subtree of the first heading."
    (hide-subtree)
    "Notice how the other heading at the same level are untouched."
    p n
    "We'd like to get an overview of the content of the tree, and we make the next level of headings visible."
    (show-children)
    "This is not enough, and we decide to display all headings in the current tree - but we still don't show the content of the headings!"
    (show-branches)
    b
    "This could also have been achieved in another way. Let's go back to when the whole subtree was visible."
    p
    (progn (show-subtree))
    p
    " Ok." p " Let's do it the other way:"
    (hide-leaves)
    "This displays all headings, but still no content."
    b
    "Finally, we'd like to show just this subtree, and nothing else."
    n
    "Let's hide everything again."
    p
    (progn (hide-body))
    p
    " Ok." p
    (show-subtree)
    b
    "This concludes the first part of the screencast. The next part will show how to navigate around the trees efficiently." 
    )
  )
(defconst screencast-outline-mode-example-text
  "* Food\nThis is the body,\nwhich says something about the topic of food.

** Delicious Food\nThis is the body of the second-level header.

** Distasteful Food\nThis could have\na body too, with\nseveral lines.

*** Dormitory Food
None at the moment
* Shelter
Another first-level topic with its header line.
** Stuff
\(secret)")


(defun screencast-outline-mode-hiding (&optional arg)
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
         screencast-outline-mode-hiding-text
         "outline-hiding-screencast" 
         1
         ()
         )
  )

(defun screencast-outline-mode-navigation (&optional arg)
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
         screencast-outline-mode-navigation-text
         "outline-navigation-screencast"
         1
         ()
         )
  )
