;;; dired-single.el -- reuse the current dired buffer to visit another directory
;; @(#) $Id: dired-single.el,v 1.6 2001/01/11 02:56:01 root Exp $

;; This file is not part of Emacs

;; Copyright (C) 2000-2001 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         August 17, 2000
;; Latest Version:  http://www.northbound-train.com/emacs.html

;; COPYRIGHT NOTICE

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
;;; **************************************************************************

;;; Description:
;;
;;  This package provides a way to reuse the current dired buffer to visit
;;  another directory (rather than creating a new buffer for the new directory).
;;  Optionally, it allows the user to specify a name that all such buffers will
;;  have, regardless of the directory they point to.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add the following to your
;;  ~/.emacs startup file
;;
;;     (require 'dired-single)
;;
;;  See below for key-binding suggestions.

;;; Usage:
;;
;;  M-x `joc-dired-single-buffer'
;;     Visits the selected directory in the current buffer, replacing the
;;     current contents with the contents of the new directory.  This doesn't
;;     prevent you from having more than one dired buffer.  The main difference
;;     is that a given dired buffer will not spawn off a new buffer every time
;;     a new directory is visited.
;;
;;     If the variable joc-dired-use-magic-buffer is non-nil, and the current
;;     buffer's name is the same as that specified by the variable
;;     joc-dired-magic-buffer-name, then the new directory's buffer will retain
;;     that same name (i.e. not only will dired only use a single buffer, but
;;     its name will not change every time a new directory is entered).
;;
;;     See below for key-binding recommendations.
;;
;;  M-x `joc-dired-single-buffer-mouse'
;;     Essentially this is the same as joc-dired-single-buffer, except that the
;;     action is initiated by a mouse-click instead of a keystroke.
;;
;;     See below for key-binding recommendations.
;;
;;  M-x `joc-dired-magic-buffer'
;;     Switch to an existing buffer whose name is the value of
;;     joc-dired-magic-buffer-name. If no such buffer exists, launch dired in a
;;     new buffer and rename that buffer to the value of
;;     joc-dired-magic-buffer-name.  If the current buffer is the magic buffer,
;;     it will prompt for a new directory to visit.
;;
;;     See below for key-binding recommendations.
;;
;;  M-x `joc-dired-toggle-buffer-name'
;;     Toggle between the `magic' buffer name and the `real' dired buffer
;;     name.  Will also seek to uniquify the `real' buffer name.
;;
;;     See below for key-binding recommendations.

;;; Recomended Keybindings:
;;
;;  To use the single-buffer feature most effectively, I recommend adding the
;;  following code to your .emacs file.  Basically, it remaps the [Return] key
;;  to call the joc-dired-single-buffer function instead of its normal function
;;  (dired-advertised-find-file).  Also, it maps the caret ("^") key
;;  to go up one directory, using the joc-dired-single-buffer command instead of
;;  the normal one (dired-up-directory), which has the same effect as hitting
;;  [Return] on the parent directory line ("..")).  Finally, it maps a
;;  button-one click to the joc-dired-single-buffer-mouse function, which does
;;  some mouse selection stuff, and then calls into the main
;;  joc-dired-single-buffer function.
;;
;;  NOTE: This should only be done for the dired-mode-map (NOT globally!).
;;
;;  The following code will work whether or not dired has been loaded already.
;;
;;      (defun my-dired-init ()
;;        "Bunch of stuff to run for dired, either immediately or when it's
;;         loaded."
;;        ;; <add other stuff here>
;;        (define-key dired-mode-map [return] 'joc-dired-single-buffer)
;;        (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
;;        (define-key dired-mode-map "^"
;;              (function
;;               (lambda nil (interactive) (joc-dired-single-buffer "..")))))
;;
;;      ;; if dired's already loaded, then the keymap will be bound
;;      (if (boundp 'dired-mode-map)
;;              ;; we're good to go; just add our bindings
;;              (my-dired-init)
;;        ;; it's not loaded yet, so add our bindings to the load-hook
;;        (add-hook 'dired-load-hook 'my-dired-init))
;;
;;  To use the magic-buffer function, you first need to start dired in a buffer
;;  whose name is the value of joc-dired-magic-buffer-name.  Once the buffer
;;  has this name, it will keep it unless explicitly renamed.  Use the function
;;  joc-dired-magic-buffer to start this initial dired magic buffer (or you can
;;  simply rename an existing dired buffer to the magic name).  I bind this
;;  function globally, so that I can always get back to my magic buffer from
;;  anywhere.  Likewise, I bind another key to bring the magic buffer up in the
;;  current default-directory, allowing me to move around fairly easily.  Here's
;;  what I have in my .emacs file:
;;
;;      (global-set-key [(f5)] 'joc-dired-magic-buffer)
;;      (global-set-key [(control f5)] (function
;;              (lambda nil (interactive)
;;              (joc-dired-magic-buffer default-directory))))
;;      (global-set-key [(shift f5)] (function
;;              (lambda nil (interactive)
;;              (message "Current directory is: %s" default-directory))))
;;      (global-set-key [(meta f5)] 'joc-dired-toggle-buffer-name)
;;
;;  Of course, these are only suggestions.

;;; Customization:
;;
;;  M-x `joc-dired-single-customize' to customize all package options.
;;
;;  The following variables can be customized:
;;
;;  o `joc-dired-use-magic-buffer'
;;        Boolean used to determine if the joc-dired-single functions should
;;        look for and retain a specific buffer name.  The buffer name to look
;;        for is specified with joc-dired-magic-buffer-name.
;;
;;  o `joc-dired-magic-buffer-name'
;;        Name of buffer to use if joc-dired-use-magic-buffer is true.  Once a
;;        dired buffer has this name, it will always keep this name (unless it's
;;        explicitly renamed by you).

;;; To Do:
;;
;;  o Nothing, at the moment.

;;; Credits:
;;
;;  The single buffer code is loosely based on code posted to the NT-Emacs
;;  mailing list by Steve Kemp & Rob Davenport.  The magic buffer name code
;;  is all mine.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of dired-single was developed and tested with NTEmacs 20.5.1
;;  and 2.7 under Windows NT 4.0 SP6 and Emacs 20.7.1 under Linux (RH7).
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:

;;; **************************************************************************
;;; ***** customization routines
;;; **************************************************************************
(defgroup joc-dired-single nil
  "joc-dired-single package customization"
  :group 'tools)

;; ---------------------------------------------------------------------------
(defun joc-dired-single-customize ()
  "Customization of the group joc-dired-single."
  (interactive)
  (customize-group "joc-dired-single"))

;; ---------------------------------------------------------------------------
(defcustom joc-dired-use-magic-buffer t
  "Boolean used to determine if the joc-dired-single functions should
   look for and retain a specific buffer name.  The buffer name to look
   for is specified with joc-dired-magic-buffer-name."
  :group 'joc-dired-single
  :type 'boolean)

;; ---------------------------------------------------------------------------
(defcustom joc-dired-magic-buffer-name "*dired*"
  "Name of buffer to use if joc-dired-use-magic-buffer is true.  Once a
   dired buffer has this name, it will always keep this name (unless it's
   explicitly renamed by you)."
  :group 'joc-dired-single
  :type 'string)

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst joc-dired-single-version
  "$Revision: 1.6 $"
  "joc-dired-single version number.")

;; ---------------------------------------------------------------------------
(defun joc-dired-single-version-number ()
  "Returns joc-dired-single version number."
  (string-match "[0123456789.]+" joc-dired-single-version)
  (match-string 0 joc-dired-single-version))

;; ---------------------------------------------------------------------------
(defun joc-dired-single-display-version ()
  "Displays joc-dired-single version."
  (interactive)
  (message "joc-dired-single version <%s>." (joc-dired-single-version-number)))

;;; **************************************************************************
;;; ***** interactive functions
;;; **************************************************************************
(defun joc-dired-single-buffer (&optional default-dirname)
  "Visits the selected directory in the current buffer, replacing the
   current contents with the contents of the new directory.  This doesn't
   prevent you from having more than one dired buffer.  The main difference
   is that a given dired buffer will not spawn off a new buffer every time
   a new directory is visited.

   If the variable joc-dired-use-magic-buffer is non-nil, and the current
   buffer's name is the same as that specified by the variable
   joc-dired-magic-buffer-name, then the new directory's buffer will retain
   that same name (i.e. not only will dired only use a single buffer, but
   its name will not change every time a new directory is entered)."
  (interactive)
  ;; use arg passed in or find name of current line
  (let ((name (or default-dirname (dired-get-filename))))
        (save-excursion
          (save-match-data
                ;; See if the selection is a directory or not.
                (end-of-line)
                (let ((eol (point)))
                  (beginning-of-line)
                  ;; assume directory if arg passed in
                  (if (or default-dirname (re-search-forward "^  d" eol t))
                          ;; save current buffer's name
                          (let ((current-buffer-name (buffer-name)))
                                ;; go ahead and read in the directory
                                (find-alternate-file name)
                                ;; if the saved buffer's name was the magic name, rename this buffer
                                (if (and joc-dired-use-magic-buffer
                                                 (string= current-buffer-name joc-dired-magic-buffer-name))
                                        (rename-buffer joc-dired-magic-buffer-name)))
                        ;; it's just a file
                        (find-file name)))))))

;;;; ------------------------------------------------------------------------
(defun joc-dired-single-buffer-mouse (click)
  "Essentially this is the same as joc-dired-single-buffer, except that the
   action is initiated by a mouse-click instead of a keystroke."
  (interactive "e")
  (let* ( (start (event-start click))
                  (window (car start))
                  (pos (car (cdr start))) )
        (select-window window)
        (goto-char pos))
  (joc-dired-single-buffer))

;;;; ------------------------------------------------------------------------
(defun joc-dired-magic-buffer (&optional default-dirname)
  "Switch to an existing buffer whose name is the value of
   joc-dired-magic-buffer-name. If no such buffer exists, launch dired in a
   new buffer and rename that buffer to the value of
   joc-dired-magic-buffer-name.  If the current buffer is the magic buffer,
   it will prompt for a new directory to visit."
  (interactive)
  ;; do we not have one or are we already in it?
  (let ((magic-dired-buffer (get-buffer joc-dired-magic-buffer-name)))
        (if (or (eq magic-dired-buffer nil)
                        (eq magic-dired-buffer (current-buffer)))
                ;; nothing to switch to
                ;; get directory name to start in
                (let ((dirname (or default-dirname
                                                   (read-file-name (format "Dired %s(directory): " "")
                                                                                   nil default-directory t))))

                  ;; make sure it's really a directory
                  (if (not (file-directory-p dirname))
                          (error "Error: <%s> is not a directory" dirname))

                  ;; do we need a new buffer?
                  (if (eq magic-dired-buffer nil)
                          ;; find the file in new buffer, current window
                          (find-file dirname)
                        ;; just find in place of current buffer
                        (find-alternate-file dirname))
                  ;; rename the buffer, where ever we found it
                  (rename-buffer joc-dired-magic-buffer-name))
          ;; we're not there (we have one already), so simply switch to it
          (switch-to-buffer magic-dired-buffer)
          ;; if called with a default, try it again
          (if default-dirname
                  (joc-dired-magic-buffer default-dirname)))))

;;;; ------------------------------------------------------------------------
(defun joc-dired-toggle-buffer-name ()
  "Toggle between the `magic' buffer name and the `real' dired buffer
   name.  Will also seek to uniquify the `real' buffer name."
  (interactive)

  ;; make sure it's a dired buffer
  (if (not (string= major-mode "dired-mode"))
          (error "Error: not a dired buffer"))

  ;; do we have magic name currently?
  (if (string= (buffer-name) joc-dired-magic-buffer-name)
          (rename-buffer
           (abbreviate-file-name
                (expand-file-name (directory-file-name default-directory))) t)

        ;; make sure the buffer doesn't currently exist
        (let ((existing-buffer (get-buffer joc-dired-magic-buffer-name)))
          (if existing-buffer
                  (kill-buffer existing-buffer))
          (rename-buffer joc-dired-magic-buffer-name))))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'dired-single)
;; dired-single.el ends here

