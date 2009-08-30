;;; fortune-mode.el --- Interface to write fortunes quickly
 
;; Copyright (C) 1999 by Association April, 2000 by Michael Shulman
 
;; Emacs Lisp Archive Entry
;; Filename: fortune.el
;; Author: Benjamin Drieu <drieu@alpha12.bocal.cs.univ-paris8.fr>
;; Maintainer: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Keywords: unix, games
;; Version: 1.4
 
;;{{{ GPL
 
;; This file is NOT part of GNU Emacs.
 
;; This is free software; you can
;; redistribute them and/or modify them under the terms of the GNU
;; General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.
 
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;;}}}
 
;;; Commentary:
 
;; This file defines a major mode and associated functions for editing
;; fortune files, i.e. files containing quotations separated by `%'
;; characters.  It can be used in one of two ways.  The first is as a
;; major mode when editing such files directly.  Since such files do
;; not have a consistent extension, you must use file local variables
;; or `auto-mode-alist' to load it automatically, for example:
;;
;; (add-to-list 'auto-mode-alist
;;              '("\\`/usr/share/games/fortunes/" . fortune-mode))
 
;; The second way to use it is via the `fortune' command, which opens
;; a buffer in which you can enter one or more fortunes and then issue
;; a command which will append them to the fortune file of your
;; choice.
 
;; This simple hack allows you to write fortunes into bases.  This is
;; very simple, and the only goal was my amusement in emacs-lisp
;; programming, and having fun with friends citations.
 
;;; Revision 1.2  1999/07/14 20:49:52  drieu
;;; - Fix minors bugs
;;; - Document more
;;; - Add $HOME in fortune-directory (much cleaner)
;;;
;;; Revision 1.3  2000/01/09 Michael Shulman
;;; - Merged with fortune.el by MAS
;;; - Added fortune-newline
;;; - Miscellaneous fixes, improvements
 
;;; Code:
 
;;{{{ Major Mode
 
(define-derived-mode fortune-mode text-mode "Fortune"
  "Major mode for editing fortunes.
The following key bindings are available:
\\<fortune-mode-map>
\\[fortune-cite-author] - Insert author citation strings
\\[fortune-newline]     - Insert newline and citation/separators
\\[fortune-take-action] - Run strfile or append to a file.
"
  (set (make-local-variable 'paragraph-start) "[\n\f]\\|%")
  (set (make-local-variable 'paragraph-separate) "[\f]*$\\|%$"))
 
(define-key fortune-mode-map "\C-c\C-a" 'fortune-cite-author)
(define-key fortune-mode-map "\C-j" 'fortune-newline)
(define-key fortune-mode-map "\C-c\C-c" 'fortune-take-action)
 
(defun fortune-take-action ()
  "Take appropriate action on the current fortune-mode buffer.
If it is visiting a file, run strfile on that file.  Otherwise, append
it to a fortune file and run strfile on that."
  (interactive)
  (if (buffer-file-name)
      (fortune-strfile-file)
    (fortune-write)))
 
;;}}}
;;{{{ Add Fortunes to Files
 
(defvar fortune-buffer "*fortune*")
 
(defvar fortune-directory (concat (getenv "HOME") "/.fortune/")
  "*User directory where fortunes are stored.")
 
(defun fortune-save-fortune ()
  "Append the fortune buffer to the end of a fortune file."
  (beginning-of-buffer)
  (let ((filename (expand-file-name
                   (read-file-name "Write to base: "
                                   fortune-directory))))
    (cond
     ((file-exists-p filename)
      (unless (looking-at "%")
        (insert "%\n"))
      (append-to-file (point-min) (point-max) filename)
      t)
     ((y-or-n-p (format "%s doesn't exist.  Create it? " filename))
      (append-to-file (point-min) (point-max) filename)))
    (when (y-or-n-p (format "Run strfile on %s? " filename))
      (fortune-strfile-file filename))))
 
(defun fortune-write ()
  "Write the fortune to a base file and delete its window."
  (interactive)
  (when (fortune-save-fortune)
    (unless (buffer-file-name)
      (set-buffer-modified-p nil)
      (delete-auto-save-file-if-necessary t))
    (set-window-configuration fortune-saved-window-config)))
 
(defvar fortune-saved-window-config nil
  "Saved window config. before creating a fortune buffer and window.")
 
(defun fortune ()
  "Open a buffer in a new window to add fortunes.\\<fortune-mode-map>
When finished, press \\[fortune-take-action] to append to a file.  The
buffer is in `fortune-mode', which see."
  (interactive)
  (setq fortune-saved-window-config (current-window-configuration))
  (pop-to-buffer fortune-buffer)
  (erase-buffer)
  (fortune-mode))
 
;;}}}
;;{{{ Run Strfile
 
(defun fortune-strfile-file (&optional file)
  "Run strfile on FILE or the current file."
  (interactive)
  (if (equal (call-process "strfile"
                           nil nil nil
                           (or file (buffer-file-name)))
             0)
      (message "Strfile completed successfully.")
    (message "Strfile returned an error!")))
 
;;}}}
;;{{{ Cite Authors
 
(defvar fortune-cite-string "        -- "
  "*String to insert when citing an author")
(defvar fortune-citation-regexp "[ \t]*-+[ \t]*"
  "Regular expression matching citation strings")
 
(defun fortune-cite-author (arg)
  "Insert the \"cite author\" string at beginning of current line.
If such a string is already there, do nothing unless ARG is greater
than 1, in which case delete it and add a new \(standardized) one.
With negative ARG, remove any citation string on this line."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (cond ((= arg 1)
           (unless (looking-at fortune-citation-regexp)
             (insert fortune-cite-string)))
          ((> arg 1)
           (when (looking-at fortune-citation-regexp)
             (replace-match ""))
           (insert fortune-cite-string))
          (t
           (when (looking-at fortune-citation-regexp)
             (replace-match ""))))))
 
(defun fortune-newline ()
  "Insert a newline and an appropriate string.
If current line has a citation string, begin a new fortune.
Otherwise, add a citation string."
  (interactive)
  (end-of-line)
  (cond
   ((string= fortune-cite-string
             (buffer-substring
              (save-excursion (beginning-of-line) (point))
              (save-excursion (end-of-line) (point))))
    (delete-region
     (save-excursion (beginning-of-line) (point))
     (save-excursion (end-of-line) (point)))
    (insert "%\n"))
   ((save-excursion
      (beginning-of-line)
      (looking-at fortune-citation-regexp))
    (insert "\n%\n"))
   ((save-excursion
      (beginning-of-line)
      (looking-at "$"))
    (insert fortune-cite-string))
   (t
    (insert "\n" fortune-cite-string))))
 
;;}}}
 
(provide 'fortune-mode)
 
;;; fortune-mode.el ends here
