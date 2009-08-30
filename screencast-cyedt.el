;;; screencast-cyedt.el

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

(setq screencast-pause-command-length 0.5) ; for speeding the _many_ commands up
'(require screencast-record)

(defun screencast-cyedt ()
  (interactive)
  (apply
   ;; the standard if with a prefix-argument doesn't work because orgtbl-* uses the
   ;;   prefix-argument even though it has been read!  

                         
               ;   'screencast-record ;; uncomment to record
   'screencast ;; comment to record

   screencast-cyedt-text
   "cyedt"
   1
   ()
   )
  )

(defconst screencast-cyedt-text
  '(
    "Hello this is an improvement of the video:"n
    "\"Emacs Power: Can your editor do THIS!\""n
    "This screencast shows that Emacs have no need for youtube annotations"n
    b
    (progn
      (html-mode)
      (flyspell-mode)
      (auto-fill-mode)
      (orgtbl-mode)
      (insert
       "<!DOCTYPE html PUBLIC \"-//W3C/DTD XHTML 1.1//EN\"
http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd>
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
<head>
<title>Books to buy</title>
</head>
<body>
<h1>Books to buy</h1>
<!--

-->
</body>
</html>"
       )
      (indent-region (point-min)(point-max))
      (previous-line 3)
      )
    p p
    "Ok, now we have the same text as in the video. Let's start:"n
    (i "|Title|Price")
    "Automatic table adjustment:"
    (orgtbl-hijacker-command-102 t)
    "The line:"
    (backward-char 1)
    (i "-")
    (orgtbl-hijacker-command-102 t)
    (i "The Art of Computer Programming")
    (orgtbl-hijacker-command-102 t)
    (i "151.99")
    (orgtbl-hijacker-command-102 t)
    (i "C++ Programming Langaga")
    (progn (flyspell-buffer))
    "Spell-checking:"
    (flyspell-auto-correct-word)
    (orgtbl-hijacker-command-102 t)
    (i "55.43")
    (orgtbl-hijacker-command-102 t)
    (i "C&nbsp;P")
    "Auto completion:"
    (dabbrev-expand 1)
    (i " L")
    "Auto completion (again):"
    (dabbrev-expand 1)
    (orgtbl-hijacker-command-102 t)
    (i "48.30")
    (orgtbl-hijacker-command-102 t)
    (i "Introduction to Algorithms")
    (orgtbl-hijacker-command-102 t)
    (i "108.60")
    (orgtbl-hijacker-command-102 t)
    "The line:"
    (backward-char 1)
    (i "-")
    (orgtbl-hijacker-command-102 t)
    (i "Total")
    (orgtbl-hijacker-command-102 t)
    b
    "Sum the column, using the spreadsheet:"
    (i ":=vsum(@I..@II)")
    (orgtbl-ctrl-c-ctrl-c t)
    b
    "Prepare for export:" n
    (progn (previous-line 8)
           (newline))
    (i "#+ORGTBL: SEND books orgtbl-to-html")
    "Prepare for import:" n
    (progn (previous-line 2)
           (end-of-line)
           (newline))
    (i 
     "<!-- BEGIN RECEIVE ORGTBL books -->")
    (kill-whole-line 1)
    (yank 1)
    (yank 1)
    "(search for begin)"
    (search-backward "begin")
    (kill-word 1)
    (i "END")
    "Now export the table:"
    (next-line 1)
    (next-line 1)
    (orgtbl-ctrl-c-ctrl-c t)
    b
    "Now we add more data to the table:"
    (forward-word 1)
    (forward-word 1)
    "Insert columns:"
    (orgtbl-hijacker-command-4 t)
    (orgtbl-hijacker-command-4 t)
    "Rearrange columns:"
    (orgtbl-hijacker-command-102 t)
    (orgtbl-hijacker-command-102 t)
    (orgtbl-hijacker-command-2 nil)
    (orgtbl-hijacker-command-2 nil)
    "Headers:"
    (orgtbl-hijacker-command-102 t)
    (i "Count")
    (orgtbl-hijacker-command-102 t)
    (i "Total")
    "Data:"
    (orgtbl-hijacker-command-102 t)
    (orgtbl-hijacker-command-102 t)
    (orgtbl-hijacker-command-102 t)
    (i "1")
    (next-line 1)
    (i "2")
    (next-line 1)
    (i "2")
    (next-line 1)
    (i "1")
    "Calculations:"
    (orgtbl-hijacker-command-102 t)
    (i ":=$2*$3")
    (orgtbl-ctrl-c-ctrl-c t)
    "Alter the formulas:"n
    "(Search for @)"
    (search-forward "@")
    (delete-char 1)
    (backward-delete-char 1)
    (forward-word 1)
    (forward-word 1)
    (forward-word 1)
    (forward-word 1)
    (backward-delete-char 1)
    (i "4")
    "Calculation and export:"
    (orgtbl-ctrl-c-ctrl-c t)
    (previous-line 1)
    (forward-word 1)
    (backward-kill-word 1)
    (kill-word 1)
    (orgtbl-ctrl-c-ctrl-c t)
    p
    p
    b
    "A final note on the export:"n
    "(search for html)"
    (search-backward "html")
    (kill-word 1)
    (i "latex")
    (orgtbl-ctrl-c-ctrl-c t)
    p
    p
    b
    "Let's keep the html for the moment:"
    (undo 1)
    "Open the buffer in Emacs own browser W3M:"
    (progn
      (find-file (expand-file-name "~/tmp/cyedt.html"))
      (delete-region (point-min) (point-max))
      (insert-buffer (get-buffer "cyedt"))
      (save-buffer))
    (w3m (expand-file-name "~/tmp/cyedt.html"))
    "That's it! Hope you are even more tempted to use orgtbl-mode now."
    (progn
      (kill-buffer "*w3m*")
      (kill-buffer "cyedt.html")
      )
    )
  )

(defmacro i (s)
  (list
   'progn
   (pop-to-buffer "cyedt")
   (screencast-insert-with-delay
    s)
   (pop-to-buffer "*Screencast Messages*")
   ))   
