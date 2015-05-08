;;; ucs-cmds.el --- Macro to create commands that insert Unicode chars.
;;
;; Filename: ucs-cmds.el
;; Description: Macro to create commands that insert Unicode chars.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2015, Drew Adams, all rights reserved.
;; Created: Tue Oct  4 07:32:20 2011 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Fri May  8 15:44:48 2015 (-0700)
;;           By: dradams
;;     Update #: 264
;; URL: http://www.emacswiki.org/ucs-cmds.el
;; Doc URL: http://www.emacswiki.org/UnicodeEncoding
;; Keywords: unicode, characters, encoding, commands, ucs-names
;; Compatibility: GNU Emacs: 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This library defines a macro, `ucsc-make-commands', and two
;;  commands:
;;
;;  * `ucsc-define-char-insert-cmd' - Define a command to insert a
;;                                    Unicode character.
;;
;;  * `ucsc-insert' - Insert a Unicode character and possibly define a
;;                    command to insert it.
;;
;;  You can use `ucsc-make-commands' or `ucsc-define-char-insert-cmd'
;;  in Emacs-Lisp code (such as in your init file) to define
;;  character-inserting commands.  In all cases:
;;
;;  * The names of the character-inserting commands created are the
;;    same as the char names, except that they are lowercase and any
;;    `SPC' chars in the character name are replaced by hyphens (`-').
;;
;;  * You can use a numeric prefix argument with the command, to
;;    insert multiple copies of the given character.
;;
;;  The commands are tailor-made to insert a given Unicode character.
;;  You can bind such a command to a key sequence, effectively adding
;;  Unicode characters to your keyboard.
;;
;;  Command `ucsc-insert' is a replacement for vanilla command
;;  `insert-char' (called `ucs-insert' prior to Emacs 24), which Emacs
;;  binds by default to `C-x 8 RET' and which lets you type input to
;;  complete against a Unicode character name and then inserts that
;;  character.
;;
;;  The behavior and code of `ucsc-insert' are identical to those of
;;  `insert-char' (`ucs-insert') except for what happens when you use
;;  a negative prefix argument:
;;
;;  1. It acts as if the prefix-arg value was positive.  So a value of
;;     -3 inserts three copies of the character, just as 3 does.
;;
;;  2. In addition to inserting the character, it uses
;;     `ucsc-define-char-insert-cmd' to define a command that you can
;;     use thereafter to insert that character.
;;
;;  Whenever `insert-char' does anything (it does nothing for a
;;  negative prefix arg), `ucsc-insert' does the same thing.  Because
;;  of this, I recommend that you bind `ucsc-insert' to `C-x 8 RET' as
;;  a replacement for `insert-char'.  Put this in your init file:
;;
;;    (define-key global-map [remap insert-char] 'ucsc-insert)
;;
;;  If you need only a few such commands for inserting particular
;;  Unicode characters, and you do not want to look up their code
;;  points, then using `ucsc-insert' or `ucsc-define-char-insert-cmd'
;;  to define them interactively is sufficiently convenient.  But
;;  these commands, like `insert-char', can be a bit slow if you use
;;  completion, because there are many, *MANY* completion candidates.
;;
;;  You can use macro `ucsc-make-commands' to quickly create a whole
;;  set of such commands for characters whose names are similar.
;;
;;  You provide a regexp as the argument to `ucsc-make-commands'.  It
;;  is matched against all Unicode character names (in `ucs-names').
;;  An insertion command is created for each of the characters whose
;;  name matches.
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
;;  Use of the commands created using `ucsc-define-char-insert-cmd',
;;  `ucsc-make-commands', and `ucsc-insert' is enhanced by `Icicles'
;;  (http://www.emacswiki.org/icicles.el).
;;
;;  When you enter the command name or code point interactively, you
;;  can take advantage of the more powerful completion of `Icicles',
;;  including regexp, substring (a subset of regexp), and various
;;  kinds of fuzzy matching.
;;
;;  More importantly, you can use progressive completion, to match
;;  parts of a candidate name in any order.  And you can "chip away at
;;  the non-elephant", removing whole sets of candidates that match
;;  patterns that you are *not* interested in.
;;
;;  With `Icicles', commands, such as `ucsc-define-char-insert-cmd'
;;  and `ucsc-insert', which use function `read-char-by-name' have the
;;  additional advantage that the characters themselves are displayed
;;  next to their names in buffer `*Completions*'.  So you see
;;  immediately what the names represent: WYSIWYG.
;;
;;
;;  Macros defined here:
;;
;;    `ucsc-make-commands'.
;;
;;  Commands defined here:
;;
;;    `ucsc-define-char-insert-cmd', `ucsc-insert'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/01/11 dadams
;;     Added: ucsc-define-char-insert-cmd.  Use it in ucsc-make-commands and ucsc-insert.
;;     ucsc-make-commands, ucsc-insert: Ensure non-nil ARG for non-interactive use.
;;                                      Mention hex and octal in doc string.
;;     Added autoload cookie for ucsc-insert.
;; 2012/12/15 dadams
;;     ucsc-insert: Raise error if CHARACTER is not characterp.
;; 2012/10/06 dadams
;;     ucsc-insert: Provided missing CHARACTER arg to insert-char.
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
(defun ucsc-define-char-insert-cmd (character &optional msgp)
  "Define a command that inserts CHARACTER.
You are prompted for the CHARACTER name or code point, just as for `insert-char'.
The command symbol is returned.

The command has the same name as the character itself, but lowercase
and with any spaces replaced by hyphens.

For example, if the character is named `GREEK CAPITAL LETTER DELTA',
then the command, which inserts one or more such chars, is named
`greek-capital-letter-delta'.

Non-interactively:
 CHARACTER is a character - it must satisfy `characterp'.
 MSGP non-nil means echo the name of the created command."
  (interactive (list (read-char-by-name "Unicode (name or hex): ") t))
  (let* ((char-name  (car (rassq character (ucs-names))))
         (cmd-name   (and char-name
                          (downcase (replace-regexp-in-string " " "-" char-name nil t))))
         (cmd        (and cmd-name  (intern cmd-name))))
    (unless char-name (error "No such Unicode character: `%s'" character)) ; Impossible?
    (eval `(defun ,cmd (arg)
             ,(format "Insert Unicode character `%s'.
This char has code point %d decimal \(%X hex, %o octal)."
                      char-name character character character)
             (interactive "*p")
             (unless arg (setq arg  1))
             (insert (make-string arg ,character))))
    (when msgp (message "Created command `%s'"
                        (downcase (replace-regexp-in-string " " "-" char-name nil t))))
    cmd))

;; Same as `insert-char' (aka `ucs-insert'), except:
;;
;; 1) A negative prefix arg has the same COUNT effect as a positive one.
;;
;; 2) A negative prefix arg also creates a command to insert the
;;    character.  The command name is the same as the character name,
;;    except it is lower case and uses hyphens instead of spaces.
;;
;; 3) Optional arg MSGP is added, and a confirmation message is shown.
;;
;; 4) Better error message if input CHARACTER is not a Unicode character.
;;
;;;###autoload
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

Interactively, or with non-nil MSGP arg, echo confirmation of the
command creation."
  (interactive
   (list (read-char-by-name "Unicode (name or hex): ")
	 (prefix-numeric-value current-prefix-arg)
	 t
         t))
  (unless (characterp character) ; Protect `insert-char' from low-level err.
    (error "No such Unicode character: `%s'" character))
  (let ((create-cmd-p  (< count 0)))
    (setq count  (abs count))
    (if (commandp 'insert-char) ; Emacs 24.  Handle the renaming this way.
        (insert-char character count inherit)
      (ucs-insert character count inherit))
    (when create-cmd-p (ucsc-define-char-insert-cmd character msgp))))

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
    (when (let ((case-fold-search  t)) (string-match (upcase regexp) (car name.code)))
      (ucsc-define-char-insert-cmd (cdr name.code)))))

;;;;;;;;;;;;;;;

(provide 'ucs-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ucs-cmds.el ends here
