;;; toggle-case.el --- toggles case at point

;; This file is not part of Emacs

;; Copyright (C) 2001 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         January 03, 2001
;; Latest Version:  http://www.northbound-train.com/emacs.html
;; @(#) $Id: joc-toggle-case.el,v 1.2 2003-11-27 01:55:45 psg Exp $

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

;;; Commentary:
;; 
;; Description:
;;
;;  This packages provides a sophisticated (over-engineered?) set of
;;  functions to toggle the case of the character under point, with
;;  which you can emulate vi's ~ function, which I found useful and
;;  miss.  Basically, the vi command (and my version of it) toggles
;;  the case of the current character and then advances to the next
;;  character, allowing successive invocations to progress down the
;;  line.

;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add the following to your
;;  ~/.emacs startup file
;;
;;     (require 'joc-toggle-case)
;;
;;  See below for key-binding suggestions.

;; Usage:
;;
;;  M-x `joc-toggle-case'
;;     Toggles the case of the character under point.  If called with
;;     a prefix argument, it toggles that many characters (see
;;     joc-toggle-case-stop-at-eol).  If the prefix is negative, the
;;     case of the character before point is toggled, and if called
;;     with a prefix argument, N characters before point will have
;;     their case toggled (see also joc-toggle-case-backwards).
;;
;;  M-x `joc-toggle-case-backwards'
;;     Convenience function to toggle case of character preceeding
;;     point.  This is the same as calling joc-toggle-case with a
;;     negative prefix (and is in fact implemented that way).
;;
;;  M-x `joc-toggle-case-by-word'
;;     Similar to joc-toggle-case except that the count (supplied by
;;     the prefix argument) is of the number of words, not letters, to
;;     be toggled.  It will start from point and move to the end of
;;     the first word at a minimum, and then take whole words from
;;     there.  If called with a negative prefix, then from point to
;;     beginning of current word will have their case toggled, going
;;     backwards for N words (see also
;;     joc-toggle-case-by-word-backwards).  Note that the
;;     joc-toggle-case-stop-at-eol setting will be honored.
;;
;;  M-x `joc-toggle-case-by-word-backwards'
;;     Convenience function to toggle case by word, backwards.  This
;;     is the same as calling joc-toggle-case-by-word with a
;;     negative prefix (and is in fact implemented that way).
;;
;;  M-x `joc-toggle-case-by-word-backwards'
;;     Toggles the case of all characters in the current region.

;; Customization:
;;
;;  M-x `joc-toggle-case-customize' to customize all package options.
;;
;;  The following variables can be customized:
;;
;;  o `joc-toggle-case-stop-at-eol'
;;        Boolean used to determine whether or not the toggle
;;        advancement stops at the end of a line.  Set to `t' it will
;;        stop at the end of the line, set to `nil' it will not (it
;;        will continue on to the next line).  If direction of toggle
;;        is reversed, the semantics of this are reveresed as well
;;        (i.e. does it stop at the beginning of the line).

;; Keybinding examples:
;;
;;  This is what I have -- use it or not as you like.
;;
;;       (global-set-key [(control \`)] 'joc-toggle-case)
;;       (global-set-key [(control ~)] 'joc-toggle-case-backwards)
;;
;;       (global-set-key [(control meta \`)] 'joc-toggle-case-by-word)
;;       (global-set-key [(control meta ~)] 'joc-toggle-case-by-word-backwards)
;;
;;       (define-key joc-F3-keymap [(\`)] 'joc-toggle-case-by-region)
;;
;;   I have a special F3 keymap which this last one is bound to.
;;   Email me if you'd like more details.

;;; To Do:
;;
;;  o Nothing, at the moment.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of joc-toggle-case was developed and tested with NTEmacs
;;  2.7 under Windows NT 4.0 SP6 and Emacs 20.7.1 under Linux (RH7).
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************

;;; History:
;; 
;;  2003-11-23 Peter S Galbraith <psg@debian.org>
;;   This version, distributed in the Debian package `emacs-goodies-el',
;;   was renamed from toggle-case.el to joc-toggle-case.el.  The prefix
;;   was also added in the file where appropriate.

;;; Code:

;;; **************************************************************************
;;; ***** customization routines
;;; **************************************************************************
(defgroup joc-toggle-case nil
  "joc-toggle-case package customization"
  :group 'tools)

;; ---------------------------------------------------------------------------
(defun joc-toggle-case-customize ()
  "Customization of the group `joc-toggle-case'."
  (interactive)
  (customize-group "joc-toggle-case"))

;; ---------------------------------------------------------------------------
(defcustom joc-toggle-case-stop-at-eol nil
  "Boolean used to determine whether or not the toggle
advancement stops at the end of a line.  Set to `t' it will
stop at the end of the line, set to `nil' it will not (it
will continue on to the next line).  If direction of toggle
is reversed, the semantics of this are reveresed as well
\(i.e. does it stop at the beginning of the line)."
  :group 'joc-toggle-case
  :type 'boolean)

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst joc-toggle-case-version
  "$Revision: 1.2 $"
  "joc-toggle-case version number.")

;; ---------------------------------------------------------------------------
(defun joc-toggle-case-version-number ()
  "Return `joc-toggle-case' version number."
  (string-match "[0123456789.]+" joc-toggle-case-version)
  (match-string 0 joc-toggle-case-version))

;; ---------------------------------------------------------------------------
(defun joc-toggle-case-display-version ()
  "Displays `joc-toggle-case' version."
  (interactive)
  (message "joc-toggle-case version <%s>." (joc-toggle-case-version-number)))

;;; **************************************************************************
;;; ***** interactive functions
;;; **************************************************************************
;;;###autoload
(defun joc-toggle-case (prefix)
  "Toggle the case of the character under point.
If called with a PREFIX argument, it toggles that many
characters (see joc-toggle-case-stop-at-eol).  If the prefix is
negative, the case of the character before point is toggled, and
if called with a prefix argument, N characters before point will
have their case toggled (see also joc-toggle-case-backwards)."

  (interactive "*p")

  ;; loop thru N times
  (let ((forward-flag (> prefix 0))
		(count (abs prefix))
		(lcv 0))
	(while (< lcv count)
	  (joc-internal-toggle-case forward-flag)
	  (setq lcv (1+ lcv))

	  ;; make sure we're not at [be]ol
	  (if (and joc-toggle-case-stop-at-eol
			   (or (and forward-flag (eolp))
				   (and (not forward-flag) (bolp))))
		  ;; set it high to exit
		  (setq lcv count))

	  ;; make sure we're not at the [be]ob
	  (if (or (bobp) (eobp))
		  ;; set it high to exit
		  (setq lcv count)))))

;; ---------------------------------------------------------------------------
;;;###autoload
(defun joc-toggle-case-backwards (prefix)
  "Convenience function to toggle case of character preceeding point.
This is the same as calling joc-toggle-case with a negative
prefix (and is in fact implemented that way)."
  (interactive "*p")
  (joc-toggle-case (- prefix)))

;; ---------------------------------------------------------------------------
;;;###autoload
(defun joc-toggle-case-by-word (prefix)
  "Similar to joc-toggle-case except that the count (supplied by
the prefix argument) is of the number of words, not letters, to
be toggled.  It will start from point and move to the end of
the first word at a minimum, and then take whole words from
there.  If called with a negative prefix, then from point to
beginning of current word will have their case toggled, going
backwards for N words (see also
joc-toggle-case-by-word-backwards).  Note that the
joc-toggle-case-stop-at-eol setting will be honored."

  (interactive "*p")

  ;; just look n words out, leave it to the lower level
  ;; functions to determine if a boundary's been reached
  (let ((start (point)) (end))
	(save-excursion
	  ;; this leaves us at the end (or beginning) of the word
	  (forward-word prefix)
	  (setq end (point)))
	(joc-toggle-case (- end start))))

;; ---------------------------------------------------------------------------
;;;###autoload
(defun joc-toggle-case-by-word-backwards (prefix)
  "Convenience function to toggle case by word, backwards.
This is the same as calling joc-toggle-case-by-word with a
negative prefix (and is in fact implemented that way)."
  (interactive "*p")
  (joc-toggle-case-by-word (- prefix)))

;; ---------------------------------------------------------------------------
;;;###autoload
(defun joc-toggle-case-by-region (start end)
  "Toggles the case of all characters in the current region."
  (interactive "*r")
  (save-excursion
	(let ((deactivate-mark nil))
	  (goto-char start)
	  (joc-toggle-case (- end start))
	  (forward-char 2))))

;;; **************************************************************************
;;; ***** non-interactive functions
;;; **************************************************************************
(defun joc-internal-toggle-case (forward-flag)
  "Internal workhorse for joc-toggle-case functions."

  (let ((backward-flag (not forward-flag)))
	;; if we're to stop at [be]ol and we're already there, check that first
	(if (and joc-toggle-case-stop-at-eol
			 (or (and backward-flag (bolp))
				 (and forward-flag (eolp))))
		;; note an error
		(ding)

	  ;; backup first if going backward, as we always delete forward
	  (if backward-flag
		  (backward-char))

	  ;; actually delete and replace the character
	  (let ((c (following-char)))
		(if (eq c (upcase c))
			(insert-char (downcase c) 1 t)
		  (insert-char (upcase c) 1 t))
		(delete-char 1 nil)

		;; again, backup if we're backing up
		(if backward-flag
			(backward-char))

		;; point is where it's supposed to be unless at [be]ol
		;; maybe move point to next position

		;; stop && backwards && BOL
		(if (and joc-toggle-case-stop-at-eol
				 backward-flag
				 (bolp))
			;; warn the user
			(ding)
		  ;; stop && forwards && EOL
		  (if (and joc-toggle-case-stop-at-eol
				   forward-flag
				   (eolp))
			  ;; warn the user
			  (ding)
			;; no-stop && backwards && BOL
			(if (and backward-flag (bolp))
				(backward-char 1)
			  ;; no-stop && forwards && EOL
			  (if (and forward-flag (eolp))
				  (forward-char 1)))))
		))))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'joc-toggle-case)

;;; joc-toggle-case.el ends here
