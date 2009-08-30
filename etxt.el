;;; etxt.el -- major mode for reading etexts

;; Version: 0.3
;; Time-stamp: <2007-01-22 14:02:28 yargo>

;; This file is not part of Emacs

;; Author: Lee Bigelow <ligelowbee@gmail.com>
;; Copyright (C) 2006, 2007 by Lee Bigelow

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. 

;;; Commentary

;; SET UP
;; Place in your lisp path and add this to your .emacs file:
;;     (require 'etxt)
;; Then restart emacs and set your ebook directory up:
;;    M-x customize-group RET etxt RET

;; Now you have a nice major mode for viewing etexts.

;; Once an 'etxt-directory' is set (defaults to ~/ebooks), files
;; visited in that directory will start in etxt-mode.

;; The mode can be started with 'M-x etxt' and a nice info screen
;; with the handy keybindings will be shown.  The speedbar is a nice
;; way to browse through your books.

;; This mode will automatically extract the text file from a zip
;; archive in your ebook directory.  This makes using the zipped
;; etexts from Project Gutenberg much nicer.  

;; Also, when quitting an etext you will be asked if you would like to
;; save a bookmark.  The bookmark will automatically be jumped to when
;; you next visit the file.

(require 'view)
(require 'bookmark)
(require 'speedbar)

(defvar etxt-intro-buffer nil)
(defcustom etxt-directory "~/ebooks"
  "Directory where etext files are to be found."
  :type 'string
  :group 'etxt
)

(defvar etxt-mode-map (make-sparse-keymap))
(set-keymap-parent etxt-mode-map view-mode-map)
(define-key etxt-mode-map "W" 'browse-gutenberg)
(define-key etxt-mode-map "q" 'etxt-quit)
(define-key etxt-mode-map "b" 'bookmark-bmenu-list)
(define-key etxt-mode-map "a" 'bookmark-set)
(define-key etxt-mode-map "l" 'speedbar)
(define-key etxt-mode-map "D" 'etxt-mode-disable)

(defun browse-gutenberg ()
  "Browse the gutenberg web site"
  (interactive)
  (browse-url "http://www.gutenberg.org/catalog"))

(defun etxt ()
  "Start a session for reading EText files."
  (interactive)
  (switch-to-buffer "Etxt Intro")
  (setq etxt-intro-buffer (current-buffer))
  (cd etxt-directory)
  (insert "Welcome to Etxt Mode

The following keys are helpful:

'l' \t to have a speedbar list the files in your etxt-directory
'b' \t to see your previously set bookmarks
'W' \t to visit the www.gutenberg.org online catalog
'q' \t to quit and save a bookmark
'a' \t to set a bookmark without quiting
SPC \t to page forward
DEL \t to page backward
'h' \t for more help on this mode")
    (etxt-mode))

(defun etxt-bufferp (buffer)
  (if (stringp (buffer-file-name buffer))
      (string-match 
       (file-name-as-directory (expand-file-name etxt-directory))  
       (file-name-directory (buffer-file-name buffer)))
    nil))

(defun etxt-quit ()
  "Quit the etxt reading mode"
  (interactive)
  (if (string-equal major-mode "etxt-mode")
      (let ((etxt-buffer (current-buffer)))
	(unless (equal etxt-buffer etxt-intro-buffer)
	  (let ((bookmark-ans (read-string 
			   "Set a bookmark before quitting (Y/n)? ")))
	    (when (string-match "^[yY]?$" bookmark-ans)
	      (bookmark-set))))
	(kill-buffer etxt-buffer))
    (princ "etxt-mode not set")))

(defun etxt-mode-disable ()
  "Return frame to normal-mode without kill anything."
  (interactive)
  (if (string-equal major-mode "etxt-mode")
      (progn
	(toggle-read-only -1)
	(normal-mode))
    (princ "etxt-mode not set")))
 
(define-derived-mode etxt-mode view-mode "Etxt" "E-Text mode.

This mode sets up emacs for easy reading of etext files.
\\{etxt-mode-map}"
  (view-mode -1)
  (toggle-read-only 1))
  
(defun etxt-after-find-file()
  (when (etxt-bufferp (current-buffer))
    (let ( (etxt-buffer (current-buffer))
	   (bpos (bookmark-get-position (bookmark-buffer-name))) )
      (when (string-match "\\.zip$" (buffer-file-name))
        (erase-buffer)
        (archive-zip-extract (buffer-file-name) "*.txt")
        (set-buffer-modified-p nil)
        (beginning-of-buffer))
      (etxt-mode)
      (when (buffer-live-p etxt-intro-buffer)
	(kill-buffer etxt-intro-buffer))
      (when bpos (goto-char bpos)))))

(add-hook 'find-file-hooks 'etxt-after-find-file)
(provide 'etxt)
