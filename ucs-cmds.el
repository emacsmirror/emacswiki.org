;;; ucs-cmds.el --- Macro to create commands that insert Unicode chars.
;;
;; Filename: ucs-cmds.el
;; Description: Macro to create commands that insert Unicode chars.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2011-2012, Drew Adams, all rights reserved.
;; Created: Tue Oct  4 07:32:20 2011 (-0700)
;; Version: 23.0
;; Last-Updated: Fri Jun  1 15:59:11 2012 (-0700)
;;           By: dradams
;;     Update #: 151
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
;;  This library defines a macro, `ucsc-make-commands', and a command
;;  `ucsc-insert' that can define simple commands that insert Unicode
;;  characters.
;;
;;  Command `ucsc-insert' is a replacement for vanilla command
;;  `ucs-insert', which is bound by default to `C-x 8 RET' and which
;;  lets you type input to complete against a Unicode character name
;;  and then inserts that character.
;;
;;  Its behavior and code are identical to those of `ucs-insert',
;;  except for what happens when you use a negative prefix argument:
;;
;;  1. It acts as if the prefix-arg value was positive.  So a value of
;;     -3 inserts three copies of the character, just as 3 does.
;;
;;  2. In addition to inserting the character, it defines a command
;;     that you can use thereafter to insert that character.  You can
;;     use a prefix argument with that command to insert multiple
;;     copies of the given character.  The command name is the same as
;;     the character name, except it is lowercase and any `SPC'
;;     characters in the character name are replaced by hyphens (`-').
;;
;;  This gives you a command that is tailor-made to insert a given
;;  Unicode character.  You can then bind the command to a key
;;  sequence, effectively adding Unicode characters to your keyboard.
;;
;;  Whenever `ucs-insert' does anything (it does nothing for a
;;  negative prefix arg), `ucsc-insert' does the same thing.  Because
;;  of this, I recommend that you bind `ucsc-insert' to `C-x 8 RET' as
;;  a replacement for `ucs-insert'.  Put this in your init file:
;;
;;    (define-key global-map [remap ucs-insert] 'ucsc-insert)
;;
;;  If you only need a few such commands for inserting particular
;;  Unicode characters, then using `ucsc-insert' to define them is
;;  sufficiently convenient.  But it, like `ucs-insert', can be a bit
;;  slow if you use completion, because there are many, *MANY*
;;  completion candidates.
;;
;;  As an alternative, you can use macro `ucsc-make-commands' to
;;  quickly create a whole set of such commands for characters whose
;;  names are similar.
;;
;;  You provide a regexp as the macro argument.  It is matched against
;;  all Unicode character names (in `ucs-names').  An insertion
;;  command is created for each of the characters whose name matches.
;;
;;  For example, here are some tests to try.  You need a Unicode font.
;;  One of these fonts might help:
;;
;;   (set-frame-font "DejaVu Sans Mono-10")
;;   (set-frame-font "DejaVu Sans 10")
;;   (set-frame-font "Arial Unicode MS")
;;
;;  Sample command creations:
;;
;;   (ucsc-make-commands "^math") ; Math symbols
;;   (ucsc-make-commands "latin") ; Latin alphabet characters
;;   (ucsc-make-commands "arabic")
;;   (ucsc-make-commands "^cjk")  ; Chinese, Japanese, Korean characters
;;   (ucsc-make-commands "^box drawings ")
;;   (ucsc-make-commands "^greek [a-z]+ letter") ; Greek characters
;;   (ucsc-make-commands "\\(^hangul\\|^circled hangul\\|^parenthesized hangul\\)")
;;
;;
;;
;;  Icicles Can Help
;;  ----------------
;;
;;  The commands created using macro `ucsc-make-commands' or the more
;;  general command `ucsc-insert' (or `ucs-insert') are enhanced if
;;  you use `Icicles'
;;  (http://www.emacswiki.org/cgi-bin/wiki/icicles.el).
;;
;;  For both, you can use more powerful completion with `Icicles',
;;  including regexp, substring (a subset of regexp), and various
;;  kinds of fuzzy matching.
;;
;;  More importantly, you can use progressive completion, to match
;;  parts of a candidate name in any order.  And you can "chip away at
;;  the non-elephant", removing whole sets of candidates that match
;;  patterns that you are *not* interested in.
;;
;;  With `Icicles', `ucsc-insert' (but not the individual insertion
;;  commands defined using it or `ucsc-make-commands') has the
;;  additional advantage that it displays the characters themselves
;;  next to their names, in `*Completions*'.  So you see immediately
;;  what the names represent: WYSIWYG.
;;
;;
;;  Macros defined here:
;;
;;    `ucsc-make-commands.'.
;;
;;  Commands defined here:
;;
;;    `ucsc-insert'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/06/01 dadams
;;     Added: ucsc-insert.
;;     Renamed ucs-make-commands to ucsc-make-commands.
;; 2012/01/14 dadams
;;     ucs-make-commands: Improve doc string.
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
(defmacro ucsc-make-commands (regexp)
  "Create commands to insert Unicode characters whose names match REGEXP.
Letter case is ignored for matching.

The set of char names used is taken from `ucs-names'.  There are
*many* such chars, so consider using a tighter regexp to limit the
number of commands created.

The commands created have the same names as the chars they insert,
except that `SPC' chars in the character names are replaced by
hyphens (`-'), and the command names are lowercase."
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


;; Same as `ucs-insert', except:
;;
;; 1) A negative prefix arg has the same COUNT effect as a positive one.
;;
;; 2) A negative prefix arg also creates a command to insert the
;;    character.  The command name is the same as the character name,
;;    except it is lower case and uses hyphens instead of spaces.
;;
;; 3) Optional arg MSGP is added, and a confirmation message is shown.
;;
(defun ucsc-insert (character &optional count inherit msgp)
  "Insert COUNT copies of CHARACTER of the given Unicode code point.
Interactively, prompts for a Unicode character name or a hex number
using `read-char-by-name'.

You can type a few of the first letters of the Unicode name of
CHARACTER, and use completion.  If you type a substring of the Unicode
name preceded by an asterisk `*' and use completion, it will show all
the characters whose names include that substring, not necessarily at
the beginning of the name.

Also accepts as input CHARACTER a hexadecimal number of Unicode code
point or a number in hash notation, e.g. #o21430 for octal, #x2318 for
hex, or #10r8984 for decimal.

Optional third arg INHERIT (non-nil when called interactively), says
to inherit text properties from adjoining text, if those properties
are sticky.

If COUNT is negative:

1. Insert (- COUNT) copies (so -3 acts the same as 3).

2. Define a command that inserts CHARACTER having the same name as the
character itself, but lowercase and with any spaces replaced by
hyphens.  For example, if the character is named `GREEK CAPITAL LETTER
DELTA', then the command, which inserts one or more such chars, is
named `greek-capital-letter-delta'.

You can then bind the created command to a convenient key.

Non-nil MSGP (prefix arg, if interactive) means echo confirmation of a
command creation."
  (interactive
   (list (read-char-by-name "Unicode (name or hex): ")
	 (prefix-numeric-value current-prefix-arg)
	 t
         t))
  (let ((create-cmd-p  (< count 0)))
    (setq count  (abs count))
    (ucs-insert character count inherit)
    (when create-cmd-p
      (let* ((char-name  (car (rassq character (ucs-names))))
             (cmd-name   (downcase
                          (replace-regexp-in-string " " "-" char-name nil t))))
        (unless char-name
          (error "No such Unicode character: `%s'" character)) ; Impossible?
        (eval `(defun ,(intern cmd-name) (arg)
                 ,(concat "Insert Unicode character `" char-name
                          (format "'.\nThis character has code point %d"
                                  character) ".")
                 (interactive "*p")
                 (insert (make-string arg ,character))))
        (when msgp (message "Created command `%s'"
                            (downcase (replace-regexp-in-string
                                       " " "-" char-name nil t))))))))

;;;;;;;;;;;;;;;

(provide 'ucs-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ucs-cmds.el ends here
