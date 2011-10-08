;;; ucs-cmds.el --- Macro to create commands that insert Unicode chars.
;; 
;; Filename: ucs-cmds.el
;; Description: Macro to create commands that insert Unicode chars.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2011, Drew Adams, all rights reserved.
;; Created: Tue Oct  4 07:32:20 2011 (-0700)
;; Version: 23.0
;; Last-Updated: Fri Oct  7 23:00:30 2011 (-0700)
;;           By: dradams
;;     Update #: 84
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ucs-cmds.el
;; Keywords: unicode, characters, encoding, commands, ucs-names
;; Compatibility: GNU Emacs 23.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  Macro `ucs-make-commands' creates simple commands that insert
;;  Unicode characters.
;;
;;  You know about vanilla command `ucs-insert' (`C-x 8 RET'), which
;;  lets you type input to complete against a Unicode character name
;;  and then inserts that character.
;;
;;  There are *many* completion candidates, so it can be a bit slow.
;;  Sometimes you might want a command that just inserts one given
;;  Unicode character.  You can use macro `ucs-make-commands' to
;;  create one or more such simple character-insertion commands.
;;
;;  Obviously, you can bind the commands to keys, effectively adding
;;  Unicode characters to your keyboard.  You can use a prefix
;;  argument with any of the commands to insert multiple copies of the
;;  given character.
;;
;;  Macro `ucs-make-commands' takes a regexp as argument, which is
;;  matched against all Unicode character names (in `ucs-names').  An
;;  insertion command is created for each of the characters whose name
;;  matches.  The commands created have the same names as the
;;  characters they insert, except that `SPC' characters in the
;;  character names are replaced by hyphens (`-'), and the command
;;  names are lowercase, not uppercase like the character names.
;;
;;
;;  Icicles Can Help
;;  ----------------
;;
;;  Both the commands created using macro `ucs-make-commands' and the
;;  more general command `ucs-insert' are enhanced if you use
;;  `Icicles' (http://www.emacswiki.org/cgi-bin/wiki/icicles.el).
;;
;;  For both, you can use more powerful completion with `Icicles',
;;  including regexp, substring (a subset of regexp), and various
;;  kinds of fuzzy matching.
;;
;;  But more importantly, you can use progressive completion, to match
;;  parts of a candidate name in any order.  And you can "chip away at
;;  the non-elephant", removing whole sets of candidates that match
;;  patterns that you are *not* interested in.
;;
;;  With `Icicles', `ucs-insert' (but not the commands defined using
;;  `ucs-make-commands') has the additional advantage that it displays
;;  the characters themselves next to their names, in `*Completions*':
;;  WYSIWYG.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2011/10/04 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;;###autoload
(defmacro ucs-make-commands (regexp)
  "Create commands to insert Unicode characters whose names match REGEXP.
Letter case is ignored for matching.
The set of character names used is taken from `ucs-names'.  There are
*many* such characters, so consider using a tighter regexp to limit
the number of commands created."
  (dolist (name.code  (ucs-names))
    (when (let ((case-fold-search  t))
            (string-match (upcase regexp) (car name.code)))
      (eval `(defun ,(intern
                      (downcase
                       (replace-regexp-in-string " " "-" (car name.code) nil t)))
                 (arg)
               ,(concat "Insert Unicode character `" (car name.code)
                        (format "'.\nThis character has code point %d"
                                (cdr name.code)) ".")
               (interactive "*p")
               (insert (make-string arg ,(cdr name.code))))))))

;; Some tests you might want to try:
;;
;; You need a Unicode font.  One of these might help:
;; (set-frame-font "DejaVu Sans Mono-10")
;; (set-frame-font "DejaVu Sans 10")
;; (set-frame-font "Arial Unicode MS")
;;
;; Sample command creations:
;; (ucs-make-commands "^math")
;; (ucs-make-commands "latin")
;; (ucs-make-commands "arabic")
;; (ucs-make-commands "^cjk")
;; (ucs-make-commands "^box drawings ")
;; (ucs-make-commands "^greek [a-z]+ letter")
;; (ucs-make-commands "\\(^hangul\\|^circled hangul\\|^parenthesized hangul\\)")

;;;;;;;;;;;;;;;

(provide 'ucs-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ucs-cmds.el ends here
