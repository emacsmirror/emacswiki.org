;;; space-chord.el --- key chord with Space
;; $Id: space-chord.el,v 1.3 2008/11/05 03:38:22 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/space-chord.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A thumb is a finger of the strongest! Let's utilize a thumb.
;; This package defines key-chord starting with Space.
;; This package depends on key-chord.el:
;;  http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el

;;; Usage:

;; Bind Space-f to `find-file' in global-map
;;   (space-chord-define-global "f" 'find-file)
;;      or
;;   (space-chord-define-global ?f 'find-file)

;; Bind Space-c to `compile' in c-mode-map
;;   (space-chord-define c-mode-map "c" 'compile)
;;      or
;;   (space-chord-define c-mode-map ?c 'compile)

;;; History:

;; $Log: space-chord.el,v $
;; Revision 1.3  2008/11/05 03:38:22  rubikitch
;; Set `space-chord-delay' to 0.08 by default
;;
;; Revision 1.2  2008/11/05 03:34:37  rubikitch
;; commentary, docstring
;;
;; Revision 1.1  2008/11/05 02:28:18  rubikitch
;; Initial revision
;;

;;; Code:

(defvar space-chord-version "$Id: space-chord.el,v 1.3 2008/11/05 03:38:22 rubikitch Exp $")
(require 'key-chord)
(defvar space-chord-delay 0.08
  "Max time delay between two key press to be considered a key chord.
`key-chord-two-keys-delay' for space-chord.")

(defun space-chord-define-global (key command)
  "Define a key-chord of KEY with space starting a COMMAND.
KEY is a character or a 1-length string.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed."
  (interactive "sSet space chord globally (1 key): \nCSet chord \"%s\" to command: ")
  (space-chord-define (current-global-map) key command))

(defun space-chord-define (keymap key command)
  "Define in KEYMAP, a key-chord of KEY with space starting a COMMAND.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed."
  (define-key keymap (vector 'key-chord ? (if (stringp key) (aref key 0) key)) command))

(defadvice key-chord-input-method (around space-chord activate)
  "Set `key-chord-two-keys-delay' to `space-chord-delay' when starting a key-chord with Space."
  (if (eq (ad-get-arg 0)  ? )
      (let ((key-chord-two-keys-delay space-chord-delay))
        ad-do-it)
    ad-do-it))
;; (progn (ad-disable-advice 'key-chord-input-method 'around 'space-chord) (ad-update 'key-chord-input-method)) 
;; (define-key (current-global-map) (vector 'key-chord ?  ) nil)
;; (define-key (current-global-map) (vector 'key-chord ?f ) nil)
;; (space-chord-define-global "f" 'find-file)
;; (space-chord-define-global ?f 'view-file)
;; (space-chord-define-global "f" nil)

(provide 'space-chord)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "space-chord.el")
;;; space-chord.el ends here
