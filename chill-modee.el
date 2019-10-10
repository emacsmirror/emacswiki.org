;;; chill-modee.el --- major mode for editing CHILL code.

;; Copyright (C) 1993-1998 Telecomunicacoes Brasileiras SA (TELEBRAS)

;; Authors: (1993) Gustavo Chaves <gustavo@cpqd.com.br>
;;          (1996) Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;; Maintainer: Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;; Created: Aug 7, 1993
;; Revised: Mar 14, 1999
;; Version: 3.1
;; Keywords: chill languages

;; This file is NOT (yet?) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; INTRODUCTION
;; ------------
;;
;; chill-mode facilitates editing of CHILL code by automatically expanding
;; syntax constructs and handling the indentation of lines of code.  Besides a
;; few ideas of our own we tried to stay as close as possible to the philosophy
;; of cc-mode.  Please, refer to the GNU Emacs manual, chapter 21 for more
;; information on "Editing Programs."
;;
;; chill-mode is based on CCITT Z.200/92 recommendation (COM X-R 34-E).
;;
;; chill-mode was tested on GNU Emacs 19.33.1, 19.34.1 and 20.3.1, there is no
;; warranty that will run properly on older version or on other Emacs variants
;; (XEmacs, or any other).
;;
;;
;; INSTALLATION
;; ------------
;;
;; To use chill-mode, add the following to your ~/.emacs file:
;;
;; (autoload 'chill-mode "chill-mode" "CHILL Editing Mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.chl$" . chill-mode) ; CHILL program source
;;		   ("\\.spc$" . chill-mode)) ; CHILL specification source
;;		 auto-mode-alist))
;;
;; You may want to customize certain chill-mode variables (e.g. for controlling
;; the indentation).  The best place to do this is in the mode hook variable
;; called `chill-mode-hooks'.  See the Emacs manual (chapter 21) and the
;; chill-mode's documentation for more information.
;;
;; It is recommended to compile chill-mode, because it is used a lot of macros
;; on sentence indentation, forward sentence and backward sentence.
;;
;;
;; SYNTAX STRUCTURE EXPANSION
;; --------------------------
;;
;; The syntax structure expansion is made when user type an upper case keyword
;; (for example: IF) followed by a punctuation (space, tab, newline, =, <,
;; etc.), for example:
;;
;;    1) type "IF "
;;
;;    2) the following expansion is made:
;; 	    IF  THEN
;; 	    FI;
;; 	 cursor is positioned between strings "IF " and " THEN"
;;
;; If user type "if ", no expansion occurs; lower case keyword may be expanded
;; through `chill-upper-keyword' (C-return) with cursor on the keyword or just
;; at right of the keyword.
;;
;; It is also possible to type all in lower case, then select the proper region
;; and turn all keyword to upper case through `chill-upper-region' (M-return).
;;
;; See also documentation for `chill-upper' (M-C-return).
;;
;;
;; INDENTATION
;; -----------
;;
;; Line indentation is made through `chill-indent-line' (TAB or newline).
;;
;; See also documentation for `chill-indent' (C-tab).
;;
;; Sentence indentation is made through `chill-sentence-indent-parser' (M-C-q),
;; this command indents a complete CHILL action at once, that is, if sentence
;; indentation is activated in the beginning of an IF action the indentation
;; proceeds until find the corresponding FI.
;;
;; The following variables control indentation behavior:
;;
;;    `chill-indent-comment'
;;       Non-nil value indicates to indent /*...*/ comment.
;;    `chill-tab-always-indent'
;;       Controls the operation of the TAB key.
;;       Accept the following values:
;;       t          hitting TAB always just indents the current line.
;;       nil        hitting TAB indents the current line if point is at the
;;                  left margin or in the line's indentation, otherwise it
;;                  insert a real TAB character.
;;       any other  TAB is inserted only within literals -- defined as
;;                  comments and strings --, but line is always reindented.
;;       Note that indentation of lines containing /*...*/ comments is also
;;       controlled by the `chill-indent-comment' variable.
;;    `chill-group-level'
;;   	 Indentation of CHILL actions within group delimited
;;       by (...) and [...].
;;    `chill-indent-level'
;;   	 Indentation of CHILL actions within surrounding block.
;;    `chill-else-level'
;;   	 Indentation of the ELSE/ELSIF lines within the surrounding IF
;;       (or CASE, if it is ELSE line).
;;    `chill-label-offset'
;;   	 Extra indentation given to a line which preceding line is a label.
;;    `chill-alternative-offset'
;;   	 Extra indentation given to a line which is an alternative of CASE,
;;       DELAY CASE, RECEIVE CASE, or ON structures.
;;    `chill-continued-statement-offset'
;;   	 Extra indentation given to a line that does not begin a new action,
;;       like the lines beginning with WHILE, FOR, or WITH.
;;
;; The default values are:
;;
;;    chill-indent-comment		t
;;    chill-tab-always-indent		'indent
;;    chill-group-level			1
;;    chill-indent-level		3
;;    chill-else-level			0
;;    chill-label-offset		2
;;    chill-alternative-offset		0
;;    chill-continued-statement-offset	2
;;
;; See documentation for `chill-set-style' and `chill-add-style'.
;;
;;
;; LINE COMMENT
;; ------------
;;
;; Line comment is defined on CHILL language since CCITT Z.200/1989, but some
;; CHILL implementation may not define it.
;;
;; The variable `chill-line-comment' specifies which string is the line comment
;; delimiter (see documentation on the code).  The default line comment is "--",
;; like Ada.
;;
;; See below for filling comment paragraph.
;;
;;
;; QUICK HELP FEATURE
;; ------------------
;;
;; When editing a text that contains some action which syntax is not so easy, or
;; a built in procedure which has a lot of parameters, it displays a window that
;; has a tip of syntax action or built in procedure parameters.
;;
;; The variable `chill-quick-help' specifies if quick help feature is on
;; (non-`nil' value) or off (`nil' value).  The default is on (non-`nil').
;;
;; See documentation for `chill-quick-help-alist'.
;;
;;
;; OTHER COMMANDS
;; --------------
;;
;; Index (C-c C-x)
;;    Create an index menu on menubar using imenu.
;;
;; Fill Comment Paragraph (C-c C-j)
;;    Fills the current comment paragraph.
;;    Only for line comments (like --...end of line).
;;
;; Justify Comment Paragraph (M-q)
;;    The same as Fill Comment Paragraph with justification.
;;
;; Postfix Comment Paragraph (M-Q)
;;    The same as Justify Comment Paragraph with appending of a postfix.
;;
;; Hungry Delete Minor Mode (C-c C-v C-d)
;;    The same as on cc-mode.
;;    When this minor mode is on, the delete key eliminate at once all preceding
;;    spaces, tabs and newlines.
;;
;; Send Bug Report (C-c C-v C-b)
;;    Send a bug report via email to chill-mode's maintainer.
;;
;; End of Reach (M-C-e)
;;    Go to the end of current reach, that is, to the end of current procedure,
;;    process, module, or region.
;;
;; Beginning of Reach (M-C-a)
;;    Go to the beginning of current reach, that is, to the beginning of current
;;    procedure, process, module, or region.
;;
;; Forward Sentence (M-e)
;;    Go to the end of current command, for example, if it is at beginning of
;;    IF command, goes to corresponding "FI;".
;;
;; Backward Sentence (M-a)
;;    Go to the beginning of current command, for example, if it is at end of IF
;;    command ("FI;"), goes to corresponding "IF".
;;
;; Forward Sibling (M-E)
;;    It is like forward sentence, but stops at intermediate parts of IF, CASE,
;;    DELAY CASE, RECEIVE CASE, AFTER, AT, CYCLE, ON handler, MODULE, PROC,
;;    PROCESS, and REGION, that is, stops at TIMEOUT, ELSE and ELSIF branchs,
;;    and nested MODULE, PROC, PROCESS or REGION.
;;
;; Backward Sibling (M-A)
;;    It is like backward sentence, but stops at intermediate parts of IF, CASE,
;;    DELAY CASE, RECEIVE CASE, AFTER, AT, CYCLE, ON handler, MODULE, PROC,
;;    PROCESS, and REGION, that is, stops at TIMEOUT, ELSE and ELSIF branchs,
;;    and nested MODULE, PROC, PROCESS or REGION.
;;
;; Mark Block (M-C-h)
;;    Go to the beginning of current block, that is, do a backward sentence.
;;    Do a mark of end of current block.
;;
;; Show All Comments (C-c C-h C-s)
;;    Show all comment /*...*/
;;
;; Hide All Comments (C-c C-h C-h)
;;    Hide all comment /*...*/
;;
;; Comment Region (C-c C-h C-r)
;;    Comment all region.
;;    It is used the following mapping:
;;    - /*{{...}}*/ is placed at region border
;;    - /*...*/ comment is replaced by {{...}}
;;    - /*{{...}}*/ comment inside region is replaced by {{...}}
;;    - {{...}} remains intact
;;
;; Uncomment Region (C-c C-h C-u)
;;    Uncomment all region.
;;    It is used the following mapping:
;;    - /*{{...}}*/ comment delimiters are eliminated
;;    - {{...}} is replaced by /*{{...}}*/, if have nested {{...}};
;;      otherwise, is replaced by /*...*/
;;    - nested {{...}} remains intact
;;
;;
;; CREDITS
;; -------
;;
;; Some features were adapted from other packages.
;; Many thanks to all people who made the following packages:
;;
;;    * cc-mode.el   A lot of features/ideas:
;;                     + TAB command,
;;                     + hungry delete/auto insert minor mode,
;;                     + style indentation setting
;;    * ada-mode.el  Fill comment paragraph (--...$)
;;    * hideif.el    Show/Hide comments (/*...*/)
;;
;; Many thanks to Gustavo Chaves <gustavo@cpqd.com.br> who made the very first
;; chill-mode.
;;
;;
;;
;; TO DO
;; -----
;;
;;    * full support Auto Insert Minor Mode from cc-mode.
;;    * better treatment for preprocessor pragmas.
;;    * support for XEmacs.
;;
;;
;; BUGS
;; ----
;;
;; To submit bug reports, type "C-c C-v C-b".
;;
;;
;;
;; Feel free to send any comments, compliments, or complaints to the maintainer.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Options:

(defvar chill-indent-comment             t
  "*Non-nil value indicates to indent /*...*/ comment.")

(defvar chill-group-level                1
  "*Indentation of CHILL actions within group delimited by (...) and [...].")

(defvar chill-indent-level               3
  "*Indentation of CHILL statements within surrounding block.")

(defvar chill-else-level                 0
  "*Indentation of the ELSE/ELSIF lines within the surrounding IF
(or CASE, if it is ELSE line).")

(defvar chill-label-offset               2
  "*Extra indent for lines which preceding line is a label.")

(defvar chill-alternative-offset         0
  "*Extra indentation given to a line which is an alternative of CASE,
DELAY CASE, RECEIVE CASE, or ON structures.")

(defvar chill-continued-statement-offset 2
  "*Extra indentation given to a line that does not begin a new action, like
the lines beginning with WHILE, FOR, or WITH.")

(defvar chill-expand-templates           t
  "*If non-nil allows the expansion of CHILL action templates.")

(defvar chill-spec-file-extension "\\.spc$"
  "*Regular expression for specification file extension.")

(defvar chill-line-comment "--"
  "*String designating the beginning of a line comment.
Set this variable to nil, if local CHILL environment does not support
line comment.")


(defvar chill-quick-help t
  "*Non-nil turn on quick help.

See `chill-quick-on', `chill-quick-off', `chill-quick-help-buffer' and
`chill-quick-help-region'.")


;; variable adapted from hideif.el

(defvar chill-hide-read-only nil
  "*Set to non-nil if you want buffer to be read-only while hiding comments.")


;; variables adapted from cc-mode.el

(defvar chill-delete-function 'backward-delete-char-untabify
  "*Function called by `chill-electric-delete' when deleting characters.")

(defvar chill-tab-always-indent 'indent
  "*Controls the operation of the TAB key.
Accept the following values:
t          hitting TAB always just indents the current line.
nil        hitting TAB indents the current line if point is at the left
	   margin or in the line's indentation, otherwise it insert a
	   real TAB character.
any other  TAB is inserted only within literals -- defined as comments
	   and strings --, but line is always reindented.
Note that indentation of lines containing /*...*/ comments is also
controlled by the `chill-indent-comment' variable.")

(defvar chill-file-style nil
  "*Variable interface for setting style via File Local Variables.
In a file's Local Variable section, you can set this variable to a
string suitable for `chill-set-style'.  When the file is visited, chill-mode
will set the style of the file to this value automatically.")
(make-variable-buffer-local 'chill-file-style)


;; variables adapted from ada-mode.el

(defvar chill-fill-comment-prefix (concat chill-line-comment " ")
  "*This is inserted in the first columns when filling a comment paragraph.")

(defvar chill-fill-comment-postfix (concat " " chill-line-comment)
  "*This is inserted at the end of each line when filling a comment paragraph
with `chill-fill-comment-paragraph-postfix'.")


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables, Constants and Bindings:


(defvar chill-line-comment-regexp (and chill-line-comment
				       (regexp-quote chill-line-comment))
  "Regexp for the beginning of a line comment, or nil.")


(defvar chill-spec-file-p nil
  "Non-nil when current buffer name has specification file extension.")
(make-variable-buffer-local 'chill-spec-file-p)


(defvar chill-mode-syntax-table nil
  "Syntax table used while in CHILL mode.")

(if chill-mode-syntax-table
    ()
  (setq chill-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?!  "_" 	  chill-mode-syntax-table)
  (modify-syntax-entry ?#  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?$  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?%  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?&  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?\' "."    chill-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" chill-mode-syntax-table)
  (modify-syntax-entry ?+  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?-  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?/  ". 14" chill-mode-syntax-table)
  (modify-syntax-entry ?:  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?<  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?=  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?>  "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?\\ "." 	  chill-mode-syntax-table)
  (modify-syntax-entry ?_  "w" 	  chill-mode-syntax-table))


(defvar chill-mode-map nil
  "Keymap used in CHILL mode.")

(if chill-mode-map
    ()
  (setq chill-mode-map (make-sparse-keymap))
  (define-key chill-mode-map "\177"       'chill-electric-delete)
  (define-key chill-mode-map ":"          'chill-electric-terminator)
  (define-key chill-mode-map "\n"         'chill-newline-and-indent)
  (define-key chill-mode-map " "          'chill-try-expand-char)
  (define-key chill-mode-map "\t"         'chill-indent-command)
  (define-key chill-mode-map "\C-m"       'chill-try-expand-char)
  (define-key chill-mode-map "!"          'chill-try-expand-char)
  (define-key chill-mode-map "\""         'chill-try-expand-char)
  (define-key chill-mode-map "#"          'chill-try-expand-char)
  (define-key chill-mode-map "'"          'chill-try-expand-char)
  (define-key chill-mode-map "("          'chill-try-expand-char)
  (define-key chill-mode-map ")"          'chill-try-expand-char)
  (define-key chill-mode-map "*"          'chill-try-expand-char)
  (define-key chill-mode-map "+"          'chill-try-expand-char)
  (define-key chill-mode-map ","          'chill-try-expand-char)
  (define-key chill-mode-map "-"          'chill-try-expand-char)
  (define-key chill-mode-map "."          'chill-try-expand-char)
  (define-key chill-mode-map "/"          'chill-try-expand-char)
  (define-key chill-mode-map ";"          'chill-try-expand-char)
  (define-key chill-mode-map "<"          'chill-try-expand-char)
  (define-key chill-mode-map "="          'chill-try-expand-char)
  (define-key chill-mode-map ">"          'chill-try-expand-char)
  (define-key chill-mode-map "["          'chill-try-expand-char)
  (define-key chill-mode-map "]"          'chill-try-expand-char)

  (define-key chill-mode-map [C-tab]      'chill-indent)

  (define-key chill-mode-map "\M-\C-z"    'chill-sentence-indent)

  (define-key chill-mode-map [C-return]   'chill-upper-keyword)
  (define-key chill-mode-map [M-return]   'chill-upper-region)
  (define-key chill-mode-map [M-C-return] 'chill-upper)
  (define-key chill-mode-map "\M-\C-q"    'chill-sentence-indent-parser)
  (define-key chill-mode-map "\M-e"       'chill-sentence-forward)
  (define-key chill-mode-map "\M-a"       'chill-sentence-backward)
  (define-key chill-mode-map "\M-E"       'chill-sibling-forward)
  (define-key chill-mode-map "\M-A"       'chill-sibling-backward)
  (define-key chill-mode-map "\M-\C-e"    'chill-end-of-reach)
  (define-key chill-mode-map "\M-\C-a"    'chill-beginning-of-reach)
  (define-key chill-mode-map "\M-q"       'chill-fill-comment-paragraph-justify)
  (define-key chill-mode-map "\M-Q"       'chill-fill-comment-paragraph-postfix)

  (define-key chill-mode-map "\M-\C-h"    'chill-mark-block)

  (define-key chill-mode-map "\C-c\C-z"   'chill-mode-version)
  (define-key chill-mode-map "\C-c\C-x"   'chill-imenu-menubar)
  (define-key chill-mode-map "\C-c\C-j"   'chill-fill-comment-paragraph)
  (define-key chill-mode-map "\C-c\C-l"   'chill-indent-region)

  (define-key chill-mode-map "\C-c\C-v\C-a" 'chill-toggle-auto-state)
  (define-key chill-mode-map "\C-c\C-v\C-b" 'chill-submit-bug-report)
  (define-key chill-mode-map "\C-c\C-v\C-c" 'comment-region)
  (define-key chill-mode-map "\C-c\C-v\C-d" 'chill-toggle-hungry-state)
  (define-key chill-mode-map "\C-c\C-v\C-t" 'chill-toggle-auto-hungry-state)

  (define-key chill-mode-map "\C-c\C-g"     'chill-insert-directive)
  (define-key chill-mode-map "\C-c\C-h\C-t" 'chill-hide-toggle-read-only)
  (define-key chill-mode-map "\C-c\C-h\C-s" 'chill-show-all-comments)
  (define-key chill-mode-map "\C-c\C-h\C-h" 'chill-hide-all-comments)
  (define-key chill-mode-map "\C-c\C-h\C-r" 'chill-comment-region)
  (define-key chill-mode-map "\C-c\C-h\C-u" 'chill-uncomment-region)
  (define-key chill-mode-map "\C-c\C-h\C-l" 'chill-insert-line-comment)
  (define-key chill-mode-map "\C-c\C-h\C-c" 'chill-insert-comment)

  (define-key chill-mode-map "\C-c\C-a\C-f" 'chill-insert-after)
  (define-key chill-mode-map "\C-c\C-a\C-t" 'chill-insert-at)
  (define-key chill-mode-map "\C-c\C-b"     'chill-insert-begin)
  (define-key chill-mode-map "\C-c\C-c\C-a" 'chill-insert-case)
  (define-key chill-mode-map "\C-c\C-c\C-d" 'chill-insert-code)
  (define-key chill-mode-map "\C-c\C-c\C-o" 'chill-insert-context)
  (define-key chill-mode-map "\C-c\C-c\C-y" 'chill-insert-cycle)
  (define-key chill-mode-map "\C-c\C-d\C-e" 'chill-insert-delay)
  (define-key chill-mode-map "\C-c\C-d\C-c" 'chill-insert-delay-case)
  (define-key chill-mode-map "\C-c\C-d\C-o" 'chill-insert-do)
  (define-key chill-mode-map "\C-c\C-f"     'chill-insert-do-for)
  (define-key chill-mode-map "\C-c\C-w\C-h" 'chill-insert-do-while)
  (define-key chill-mode-map "\C-c\C-w\C-i" 'chill-insert-do-with)
  (define-key chill-mode-map "\C-c\C-e\C-l" 'chill-insert-else)
  (define-key chill-mode-map "\C-c\C-e\C-i" 'chill-insert-elsif)
  (define-key chill-mode-map "\C-c\C-e\C-x" 'chill-insert-exceptions)
  (define-key chill-mode-map "\C-c\C-i"     'chill-insert-if)
  (define-key chill-mode-map "\C-c\C-n"     'chill-insert-in_line)
  (define-key chill-mode-map "\C-c\C-m"     'chill-insert-module)
  (define-key chill-mode-map "\C-c\C-o"     'chill-insert-on)
  (define-key chill-mode-map "\C-c\C-p\C-c" 'chill-insert-proc)
  (define-key chill-mode-map "\C-c\C-p\C-s" 'chill-insert-process)
  (define-key chill-mode-map "\C-c\C-r\C-c" 'chill-insert-receive)
  (define-key chill-mode-map "\C-c\C-r\C-g" 'chill-insert-region)
  (define-key chill-mode-map "\C-c\C-r\C-t" 'chill-insert-returns)
  (define-key chill-mode-map "\C-c\C-s"     'chill-insert-struct)

  ;; define menu `CHILL'

  (require 'easymenu)

  (easy-menu-define
   chill-mode-menu chill-mode-map "Menu keymap for CHILL mode."
   '("CHILL"
     ("Comment"
      ["Insert Comment"            chill-insert-comment      t]
      ["Insert Line Comment"       chill-insert-line-comment chill-line-comment]
      "--"
      ["Comment Region"            chill-comment-region      mark-active]
      ["Uncomment Region"          chill-uncomment-region    mark-active]
      "--"
      ["Fill Comment Paragraph"    chill-fill-comment-paragraph         t]
      ["Justify Comment Paragraph" chill-fill-comment-paragraph-justify t]
      ["Postfix Comment Paragraph" chill-fill-comment-paragraph-postfix t]
      "--"
      ["Hide Comments"             chill-hide-all-comments              t]
      ["Show Comments"             chill-show-all-comments              t]
      ["Hide Toggle Read Only"     chill-hide-toggle-read-only          t]
      )
     ("Sentence"
      ["Indent Sentence"        chill-sentence-indent-parser t]
      "--"
      ["Forward Sentence"       chill-sentence-forward       t]
      ["Backward Sentence"      chill-sentence-backward      t]
      "--"
      ["Forward Sibling"        chill-sibling-forward        t]
      ["Backward Sibling"       chill-sibling-backward       t]
      "--"
      ["End of Reach"           chill-end-of-reach           t]
      ["Beginning of Reach"     chill-beginning-of-reach     t]
      "--"
      ["Mark Block"             chill-mark-block             t]
      )
     ("Utils"
      ["Insert Directive"       chill-insert-directive    t]
      ["Indent Line"            chill-indent-line         t]
      ["Expand Keyword"         chill-upper-keyword       t]
      "--"
      ["Indent Region"          chill-indent-region       mark-active]
      ["Expand Region"          chill-upper-region        mark-active]
      "--"
      ["Index"                  chill-imenu-menubar       t]
      ["Toggle Read Only"       vc-toggle-read-only       t]
      ["Toggle Hungry Delete"   chill-toggle-hungry-state t]
      ["Toggle Auto Insert"     chill-toggle-auto-state   t]
      ["Toggle Hungry and Auto" chill-toggle-auto-hungry-state t]
      ["Toggle Quick Help"      chill-toggle-quick-help   t]
      "--"
      ["Send Bug Report"        chill-submit-bug-report   t]
      "--"
      ["Show Version"           chill-mode-version        t]
      )
     "--"
     ["AFTER"        chill-insert-after      (not chill-spec-file-p)]
     ["AT"           chill-insert-at         (not chill-spec-file-p)]
     ["BEGIN"        chill-insert-begin      (not chill-spec-file-p)]
     ["CASE"         chill-insert-case       t]
     ["CODE"         chill-insert-code       (not chill-spec-file-p)]
     ["CONTEXT"      chill-insert-context    t]
     ["CYCLE"        chill-insert-cycle      (not chill-spec-file-p)]
     ["DELAY CASE"   chill-insert-delay-case (not chill-spec-file-p)]
     ["DELAY"        chill-insert-delay      (not chill-spec-file-p)]
     ["DO FOR"       chill-insert-do-for     (not chill-spec-file-p)]
     ["DO WHILE"     chill-insert-do-while   (not chill-spec-file-p)]
     ["DO WITH"      chill-insert-do-with    (not chill-spec-file-p)]
     ["DO"           chill-insert-do         (not chill-spec-file-p)]
     ["ELSE"         chill-insert-else       (not chill-spec-file-p)]
     ["ELSIF"        chill-insert-elsif      (not chill-spec-file-p)]
     ["EXCEPTIONS"   chill-insert-exceptions t]
     ["IF"           chill-insert-if         (not chill-spec-file-p)]
     ["IN_LINE"      chill-insert-in_line    (not chill-spec-file-p)]
     ["MODULE"       chill-insert-module     t]
     ["ON"           chill-insert-on         (not chill-spec-file-p)]
     ["PROCESS"      chill-insert-process    t]
     ["PROC"         chill-insert-proc       t]
     ["RECEIVE"      chill-insert-receive    (not chill-spec-file-p)]
     ["REGION"       chill-insert-region     t]
     ["RETURNS"      chill-insert-returns    t]
     ["STRUCT"       chill-insert-struct     t]
     )))


;; The CHILL reserved names, predefined names and exception names are
;; put in the association list below.  Some entries below are commented
;; because it is not implemented on our environment.
;; Feel free to delete the comments in your on initialization file.

(defconst chill-special-alist
  '(
    ("ABS"            . chill-expand-argument)
    ("ABSTIME"        . chill-expand-argument)
    ("ACCESS"         . chill-expand-template)
    ("AFTER"          . chill-expand-timing)
    ("ALL"            . chill-expand-template)
    ("ALLOCATE"       . chill-expand-argument)
    ("ALLOCATEFAIL"   . chill-expand-template)
    ("AND"            . chill-expand-template)
    ("ANDIF"          . chill-expand-template)
    ("ARCCOS"         . chill-expand-argument)
    ("ARCSIN"         . chill-expand-argument)
    ("ARCTAN"         . chill-expand-argument)
    ("ARRAY"          . chill-expand-argument)
    ("ASSERT"         . chill-expand-action)
    ("ASSERTFAIL"     . chill-expand-template)
    ("ASSOCIATE"      . chill-expand-argument-action)
    ("ASSOCIATEFAIL"  . chill-expand-template)
    ("ASSOCIATION"    . chill-expand-template)
    ("AT"             . chill-expand-timing)
    ("BEGIN"          . chill-expand-begin)
    ("BIN"            . chill-expand-argument)
    ("BODY"           . chill-expand-template)
    ("BOOL"           . chill-expand-template)
    ("BOOLS"          . chill-expand-argument)
    ("BUFFER"         . chill-expand-template)
    ("BY"             . chill-expand-normal)
    ("CARD"           . chill-expand-argument)
    ("CARDINAL"       . chill-expand-template) ; INT mode variation
    ("CASE"           . chill-expand-case)
    ("CAUSE"          . chill-expand-action)
    ("CHAR"           . chill-expand-template)
    ("CHARS"          . chill-expand-argument)
    ("CONNECT"        . chill-expand-argument-action)
    ("CONNECTFAIL"    . chill-expand-template)
    ("CONTEXT"        . chill-expand-context)
    ("CONTINUE"       . chill-expand-action)
    ("COS"            . chill-expand-argument)
    ("CREATE"         . chill-expand-argument-action)
    ("CREATEFAIL"     . chill-expand-template)
    ("CYCLE"          . chill-expand-timing)
    ("DAYS"           . chill-expand-argument)
    ("DCL"            . chill-expand-template)
    ("DELAY"          . chill-expand-delay)
    ("DELAYFAIL"      . chill-expand-template)
    ("DELETE"         . chill-expand-argument-action)
    ("DELETEFAIL"     . chill-expand-template)
    ("DISCONNECT"     . chill-expand-argument-action)
    ("DISSOCIATE"     . chill-expand-argument-action)
    ("DO"             . chill-expand-do)
    ("DOWN"           . chill-expand-normal)
    ("DURATION"       . chill-expand-template)
    ("DYNAMIC"        . chill-expand-template)
    ("ELSE"           . chill-expand-else)
    ("ELSIF"          . chill-expand-elsif)
    ("EMPTY"          . chill-expand-template)
    ("END"            . chill-expand-template-and-indent)
    ("EOLN"           . chill-expand-argument)
    ("ESAC"           . chill-expand-template-and-indent)
    ("EVENT"          . chill-expand-template)
    ("EVER"           . chill-expand-normal)
    ("EXCEPTIONS"     . chill-expand-returns)
    ("EXISTING"       . chill-expand-argument)
    ("EXIT"           . chill-expand-action)
    ("EXP"            . chill-expand-argument)
    ("EXPIRED"        . chill-expand-argument)
    ("FALSE"          . chill-expand-template)
    ("FI"             . chill-expand-template-and-indent)
    ("FIRST"          . chill-expand-template)
    ("FLOAT"          . chill-expand-template)
    ("FOR"            . chill-expand-template-do)
    ("FORBID"         . chill-expand-template)
    ("GENERAL"        . chill-expand-template)
    ("GETASSOCIATION" . chill-expand-argument)
    ("GETSTACK"       . chill-expand-argument)
    ("GETTEXTACCESS"  . chill-expand-argument)
    ("GETTEXTINDEX"   . chill-expand-argument)
    ("GETTEXTRECORD"  . chill-expand-argument)
    ("GETUSAGE"       . chill-expand-argument)
;;; ("GOTO"           . chill-expand-action)   ; it is not implemented
    ("GRANT"          . chill-expand-template)
    ("HOURS"          . chill-expand-argument)
    ("IF"             . chill-expand-if)
    ("IN"             . chill-expand-template)
    ("INDEXABLE"      . chill-expand-argument)
    ("INIT"           . chill-expand-template)
    ("INLINE"         . chill-expand-template)
    ("INOUT"          . chill-expand-template)
    ("INSTANCE"       . chill-expand-template)
    ("INT"            . chill-expand-template)
    ("INTTIME"        . chill-expand-argument)
    ("ISASSOCIATED"   . chill-expand-argument)
    ("LAST"           . chill-expand-template)
    ("LENGTH"         . chill-expand-argument)
    ("LN"             . chill-expand-argument)
    ("LOC"            . chill-expand-template)
    ("LOG"            . chill-expand-argument)
    ("LONG_CARDINAL"  . chill-expand-template) ; INT mode variation
    ("LONG_FLOAT"     . chill-expand-template) ; FLOAT mode variation
    ("LONG_INT"       . chill-expand-template) ; INT mode variation
    ("LOWER"          . chill-expand-argument)
    ("MAX"            . chill-expand-argument)
    ("MILLISECS"      . chill-expand-argument)
    ("MIN"            . chill-expand-argument)
    ("MINUTES"        . chill-expand-argument)
    ("MOD"            . chill-expand-template)
    ("MODIFY"         . chill-expand-argument-action)
    ("MODIFYFAIL"     . chill-expand-template)
    ("MODULE"         . chill-expand-modulion)
    ("NEWMODE"        . chill-expand-template)
    ("NONREF"         . chill-expand-template)
    ("NOPACK"         . chill-expand-template)
    ("NOT"            . chill-expand-template)
    ("NOTASSOCIATED"  . chill-expand-template)
    ("NOTCONNECTED"   . chill-expand-template)
    ("NULL"           . chill-expand-template)
    ("NUM"            . chill-expand-argument)
    ("OD"             . chill-expand-template-and-indent)
    ("OF"             . chill-expand-template)
    ("ON"             . chill-expand-on)
    ("OR"             . chill-expand-template)
    ("ORIF"           . chill-expand-template)
    ("OUT"            . chill-expand-template)
    ("OUTOFFILE"      . chill-expand-argument)
    ("OVERFLOW"       . chill-expand-template)
    ("PACK"           . chill-expand-template)
    ("PERVASIVE"      . chill-expand-template) ; old version - obsolete name
    ("POS"            . chill-expand-argument)
    ("POWERSET"       . chill-expand-template)
    ("PRED"           . chill-expand-argument)
    ("PREFIXED"       . chill-expand-template)
    ("PRIORITY"       . chill-expand-template)
    ("PROC"           . chill-expand-proc)
    ("PROCESS"        . chill-expand-proc)
    ("PTR"            . chill-expand-template)
    ("RANGE"          . chill-expand-argument)
    ("RANGEFAIL"      . chill-expand-template)
    ("READ"           . chill-expand-template)
    ("READABLE"       . chill-expand-argument)
    ("READFAIL"       . chill-expand-template)
    ("READONLY"       . chill-expand-template)
    ("READRECORD"     . chill-expand-argument-action)
    ("READTEXT"       . chill-expand-argument-action)
    ("READWRITE"      . chill-expand-template)
    ("RECEIVE"        . chill-expand-receive)
    ("REF"            . chill-expand-template)
    ("REGION"         . chill-expand-modulion)
    ("REM"            . chill-expand-template)
    ("REMOTE"         . chill-expand-action)
    ("RESULT"         . chill-expand-action)
    ("RETURN"         . chill-expand-action)
    ("RETURNS"        . chill-expand-returns)
    ("ROW"            . chill-expand-template)
    ("SAME"           . chill-expand-template)
    ("SECS"           . chill-expand-argument)
    ("SEIZE"          . chill-expand-template)
    ("SEND"           . chill-expand-action)
    ("SENDFAIL"       . chill-expand-template)
    ("SEQUENCIBLE"    . chill-expand-argument)
    ("SET"            . chill-expand-argument)
    ("SETTEXTACCESS"  . chill-expand-argument-action)
    ("SETTEXTINDEX"   . chill-expand-argument-action)
    ("SETTEXTRECORD"  . chill-expand-argument-action)
    ("SHORT_CARDINAL" . chill-expand-template) ; INT mode variation
    ("SHORT_FLOAT"    . chill-expand-template) ; FLOAT mode variation
    ("SHORT_INT"      . chill-expand-template) ; INT mode variation
    ("SIGNAL"         . chill-expand-template)
    ("SIMPLE"         . chill-expand-template)
    ("SIN"            . chill-expand-argument)
    ("SIZE"           . chill-expand-argument)
    ("SPACEFAIL"      . chill-expand-template)
    ("SPEC"           . chill-expand-template)
    ("SQRT"           . chill-expand-argument)
    ("START"          . chill-expand-template)
    ("STATIC"         . chill-expand-template)
    ("STEP"           . chill-expand-argument)
    ("STOP"           . chill-expand-action)
    ("STRUCT"         . chill-expand-struct)
    ("SUCC"           . chill-expand-argument)
    ("SYN"            . chill-expand-template)
    ("SYNMODE"        . chill-expand-template)
    ("TAGFAIL"        . chill-expand-template)
    ("TAN"            . chill-expand-argument)
    ("TERMINATE"      . chill-expand-argument-action)
    ("TEXT"           . chill-expand-argument)
    ("TEXTFAIL"       . chill-expand-template)
    ("THEN"           . chill-expand-template-and-indent)
    ("THIS"           . chill-expand-template)
    ("TIME"           . chill-expand-template)
    ("TIMEOUT"        . chill-expand-template)
    ("TIMERFAIL"      . chill-expand-template)
    ("TO"             . chill-expand-template)
    ("TRUE"           . chill-expand-template)
    ("UNDERFLOW"      . chill-expand-template)
    ("UP"             . chill-expand-template)
    ("UPPER"          . chill-expand-argument)
    ("USAGE"          . chill-expand-template)
    ("VARIABLE"       . chill-expand-argument)
    ("VARYING"        . chill-expand-template)
    ("WAIT"           . chill-expand-argument)
    ("WHERE"          . chill-expand-template)
    ("WHILE"          . chill-expand-template-do)
    ("WITH"           . chill-expand-template-do)
    ("WRITEABLE"      . chill-expand-argument)
    ("WRITEFAIL"      . chill-expand-template)
    ("WRITEONLY"      . chill-expand-template)
    ("WRITERECORD"    . chill-expand-argument-action)
    ("WRITETEXT"      . chill-expand-argument-action)
    ("XOR"            . chill-expand-template)
    )
  "Association list for reserved names in CHILL.

Contains all special simple name strings defined in appendix C (Z200),
that is, reserved simple name strings, predefined simple name strings
and exception names.

Contains also the following mode variations:
   CARDINAL, SHORT_CARDINAL, LONG_CARDINAL
   SHORT_INT, LONG_INT
   SHORT_FLOAT, LONG_FLOAT

Each element has the following form:

   (KEYWORD . EXPANSION)

Where KEYWORD is a string and EXPANSION is a function used to keyword
expansion, this function is called with two arguments that is the beginning
and end of region.")



(defvar chill-implementation-alist
  '(
    ("ADDRESS"        . chill-expand-directive)
    ("CODE"           . chill-expand-code)
    ("DISPATCH"       . chill-expand-argument-action)
    ("EXTRA"          . chill-expand-directive)
    ("GETF"           . chill-expand-argument-action)
    ("GETFLN"         . chill-expand-argument-action)
    ("INSTANCES"      . chill-expand-directive)
    ("INTCALL"        . chill-expand-directive)
    ("INTERRUPT"      . chill-expand-directive)
    ("IN_LINE"        . chill-expand-code)
    ("PID"            . chill-expand-directive)
    ("PIDN"           . chill-expand-directive)
    ("PROGID"         . chill-expand-directive)
    ("PROGIDN"        . chill-expand-directive)
    ("PUTF"           . chill-expand-argument-action)
    ("PUTFLN"         . chill-expand-argument-action)
    ("SID"            . chill-expand-directive)
    ("STACKSIZE"      . chill-expand-directive)
    ("TTY"            . chill-expand-normal)
    ("VERSION"        . chill-expand-directive)
    )
  "*Association list for implementation defined names.

Change these associations if your local CHILL compiler has other names.
These names are basically directive names, procedure names and
device names.

See `chill-special-alist' for documentation.")


;; Regular expressions used by indentation (line and sentence)
;; and sentence movement

(defconst chill-label-regexp
  "[a-zA-Z][a-zA-Z0-9_]*\\s-*\\(<>[^<\n]*<>\\s-*\\)*:[^=]")

(defconst chill-relevant-regexp
  (concat "[()]\\|:[^=]\\|\\[\\|\\]\\|\\<\\("
	  "AFTER\\|ASSERT\\|AT\\|BEGIN\\|"
	  "CASE\\|CAUSE\\|CONTEXT\\|CONTINUE\\|CYCLE\\|"
	  "DCL\\|DELAY\\|DO\\|ELSE\\|ELSIF\\|END\\|ESAC\\|EXIT\\|FI\\|FOR\\|"
	  "GRANT\\|IF\\|MODULE\\|NEWMODE\\|OD\\|ON\\|PROC\\|PROCESS\\|"
	  "RECEIVE\\|REGION\\|REMOTE\\|RESULT\\|RETURN\\|"
	  "SEIZE\\|SEND\\|SIGNAL\\|START\\|STOP\\|SYN\\|SYNMODE\\|TIMEOUT"
	  "\\)\\>"))

(defconst chill-sentence-regexp
  (concat ";\\|\\<\\("
	  "AFTER\\|AT\\|BEGIN\\|CASE\\|CONTEXT\\|CYCLE\\|DCL\\|DO\\|"
	  "ELSE\\|ELSIF\\|END\\|ESAC\\|FI\\|FOR\\|GRANT\\|IF\\|"
	  "MODULE\\|NEWMODE\\|OF\\|OD\\|ON\\|PROC\\|PROCESS\\|REGION\\|"
	  "SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\|THEN\\|TIMEOUT"
	  "\\)\\>"))

(defconst chill-reach-regexp "\\<\\(MODULE\\|PROC\\|PROCESS\\|REGION\\)\\>")


;; variables adapted from hideif.el

(defvar chill-comment-hiding nil
  "Non-nil when text may be hidden.")
(make-variable-buffer-local 'chill-comment-hiding)

(defvar chill-outside-read-only nil
  "Buffer read only state when before hide comment.")
(make-variable-buffer-local 'chill-outside-read-only)


;; variables adapted from cc-mode.el

(defvar chill-style-alist
  '(("z200"
     (chill-group-level                . 1)
     (chill-indent-level               . 2)
     (chill-else-level                 . 2)
     (chill-label-offset               . 1)
     (chill-alternative-offset         . 2)
     (chill-continued-statement-offset . 1))
    ("wide"
     (chill-indent-level               . 4))
    ("narrow"
     (chill-indent-level               . 1)))
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any chill-mode variable (but may be any variable that
affect the use o chill-mode like `fill-column'), and VALUE is the intended
value for that variable when using the selected style.

Note that all styles inherit from the `chill-mode' style, which is
computed at the time the mode is loaded.")

;; minor mode variables (adapted from cc-mode.el)

(defvar chill-hungry-delete-key nil
  "Internal state of hungry delete key feature.")
(make-variable-buffer-local 'chill-hungry-delete-key)

(defvar chill-auto-newline nil
  "Internal state of auto newline feature.")
(make-variable-buffer-local 'chill-auto-newline)

(defvar chill-auto-hungry-string nil
  "Internal auto-newline/hungry-delete designation string for mode line.")
(make-variable-buffer-local 'chill-auto-hungry-string)


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for imenu


;; get imenu variables and functions when byte-compiling
(eval-when-compile (require 'imenu))


(defconst chill-imenu-generic-expression
  (let ((label-regexp
	 "^\\s-*\\([A-Za-z0-9_]+\\)\\s-*\\(<>[^<]*<>\\s-*\\)*:\\s-*"))
    (list
     (list nil         (concat label-regexp "\\(SPEC\\s-+\\)?MODULE\\>") 1)
     (list ".REGION."  (concat label-regexp "\\(SPEC\\s-+\\)?REGION\\>") 1)
     (list ".PROCESS." (concat label-regexp "PROCESS\\>") 1)
     (list ".PROC."    (concat label-regexp "PROC\\>") 1)))
  "Imenu generic expression for CHILL mode.  See `imenu-generic-expression'.")


(defun chill-imenu-menubar ()
  (interactive)
  (or (lookup-key (current-local-map) [menu-bar index])
      (progn
	(imenu-add-to-menubar "CHILL-index")
	(force-mode-line-update t))))


(defun chill-imenu-create-index ()
  "Create index alist for CHILL buffers."
  (let ((index-alist (imenu--generic-function imenu-generic-expression)))
    (if imenu-sort-function
	(chill-imenu-sort-index index-alist)
      index-alist)))


(defun chill-imenu-sort-index (index-alist)
  "Sort index alist for CHILL buffers.
Variable `imenu-sort-function' MUST be non-nil."
  (let ((elist index-alist)
	element)
    (while elist
      (setq element (car elist)
	    elist   (cdr elist))
      (if (and element (listp (cdr element)))
	  (setcdr element (chill-imenu-sort-index (cdr element))))))
  (sort index-alist imenu-sort-function))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode Command:


(defconst chill-mode-version "3.1")
(defconst chill-mode-date    "Sun 14 Mar 1999")

(defun chill-mode-version (&optional here)
  (interactive "P")
  (let ((version (concat "CHILL-mode version " chill-mode-version
			 " of " chill-mode-date)))
    (and here (insert-and-inherit version))
    (message version)
    version))


;;;###autoload
(defun chill-mode ()
  "Major mode for editing CHILL programs.
To submit a problem report, enter `\\[chill-submit-bug-report]' from a
chill-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case and send the message.

To see what version of chill-mode you are running, enter \
`\\[chill-mode-version]'.

There are two kind of comments:
 * begin-end comment: delimited with /* ... */
 * line comment     : delimited with -- ... end-of-line
		      (see variable `chill-line-comment')

Paragraphs are delimited by blank lines only.

Convert tabs to spaces as it deletes char back.

Variables controlling indentation style:

 `chill-group-level'
    Indentation of CHILL actions within group delimited by (...) and [...].

 `chill-indent-level'
    Indentation of CHILL actions within surrounding block.

 `chill-else-level'
    Indentation of the ELSE/ELSIF lines within the surrounding IF
    (or CASE, if it is ELSE line).

 `chill-label-offset'
    Extra indentation given to a line which preceding line is a label.

 `chill-alternative-offset'
    Extra indentation given to a line which is an alternative of CASE,
    DELAY CASE, RECEIVE CASE, or ON structures.

 `chill-continued-statement-offset'
    Extra indentation given to a line that does not begin a new action,
    like the lines beginning with WHILE, FOR, or WITH.

 `chill-tab-always-indent'
    Controls the operation of the TAB key.
    Accept the following values:
    t          hitting TAB always just indents the current line.
    nil        hitting TAB indents the current line if point is at the left
	       margin or in the line's indentation, otherwise it insert a
	       real TAB character.
    any other  TAB is inserted only within literals -- defined as comments
	       and strings --, but line is always reindented.
    Note that indentation of lines containing /*...*/ comments is also
    controlled by the `chill-indent-comment' variable.

Settings for authors' and Z.200 indentation styles are:

				     AUTHORS  Z200
  `chill-group-level'			1	1
  `chill-indent-level'			3	2
  `chill-else-level'			0	2
  `chill-label-offset'			2	1
  `chill-alternative-offset'		0	2
  `chill-continued-statement-offset'	2	1

Default value of variable `chill-tab-always-indent' is 'indent.

Set `chill-expand-templates' to nil to disallow the expansion of CHILL action
templates.

Set `chill-indent-comment' to nil to disallow the indentation of /*...*/
comment.

Turning on CHILL mode calls the value of the variable `chill-mode-hooks'
with no args, if that value is non-nil.

There is a hungry delete minor mode feature that when it is enabled (as
evidenced by the `/h', `/ah', `/qh' or `/qah' on the modeline after the
mode name) the delete key gobbles all preceding whitespace in one fell swoop.

\\{chill-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map chill-mode-map)
  (set-syntax-table chill-mode-syntax-table)
  (setq major-mode 'chill-mode
	mode-name "CHILL")

  (set (make-local-variable 'paragraph-start)
       (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate)
       paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix)
       t)
  (set (make-local-variable 'fill-paragraph-function)
       'chill-fill-comment-paragraph)

  (set (make-local-variable 'indent-line-function)
       'chill-indent-line)
  (set (make-local-variable 'require-final-newline)
       t)

  (set (make-local-variable 'comment-start)
       "/*")
  (set (make-local-variable 'comment-end)
       "*/")
  (set (make-local-variable 'comment-start-skip)
       "/\\*\\s-*")
  (set (make-local-variable 'comment-column)
       0)
  (set (make-local-variable 'comment-indent-function)
       'chill-comment-indent)
  (set (make-local-variable 'parse-sexp-ignore-comments)
       t)

  (set (make-local-variable 'imenu-create-index-function)
       'chill-imenu-create-index)
  (set (make-local-variable 'imenu-generic-expression)
       chill-imenu-generic-expression)
  (set (make-local-variable 'imenu-sort-function)
       'imenu--sort-by-name)

  (set (make-local-variable 'font-lock-defaults)
       (list 'chill-font-lock-keywords nil nil nil 'chill-sibling-backward
	     (cons 'font-lock-comment-start-regexp comment-start-skip)
	     '(font-lock-mark-block-function . chill-mark-block)))

  (setq chill-spec-file-p
	(and (string-match chill-spec-file-extension
			   buffer-file-name)
	     t))

  ;; put auto-hungry designators onto minor-mode-alist, but only once
  (or (assq 'chill-auto-hungry-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(chill-auto-hungry-string chill-auto-hungry-string)
		  minor-mode-alist)))

  ;; initialize faces for Font Lock
  (chill-font-lock-color)

  ;; local hooks
  (make-local-hook 'pre-command-hook)
  (add-hook 'pre-command-hook 'chill-quick-off nil t)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'chill-post-command nil t)
  (make-local-hook 'find-file-hooks)
  (add-hook 'find-file-hooks 'chill-quick-off nil t)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'chill-quick-off nil t)
  ;;(make-local-hook 'mouse-leave-buffer-hook)
  ;;(add-hook 'mouse-leave-buffer-hook 'chill-quick-off nil t)

  ;; (chill-load-options)
  (run-hooks 'chill-mode-hooks)
  (chill-quick-help-buffer)
  (chill-post-command)
  (chill-update-modeline))


(defun chill-mark-block ()
  "Go to the beginning of current block, that is, do a backward sentence.
Do a mark of end of current block."
  (interactive)
  (push-mark (point))
  (push-mark (save-excursion
	       (chill-sibling-forward)
	       (point)))
  (chill-sibling-backward))


 
;; this is used by `indent-for-comment' to decide how much to indent a comment
;; in CHILL code based on its context.
(defun chill-comment-indent ()
  (let ((ccol (current-column)))
    (if (looking-at "/\\*")
	ccol				; existing comment stays there.
      (save-excursion
	(skip-chars-backward " \t\f")
	(if (= (current-column) 0)
	    ccol
	  (max (1+ (current-column))	; else indent at comment column
	       comment-column))))))	;  except leave at least one space.


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for font-lock


(defconst chill-font-lock-keywords
  (append
   '(
     ;; Pragmas
     ("^#.*"
      (0 chill-pragma-face keep t))
     ;; Literal Integers Base
     ("[^']\\<\\([BDHObdho]'\\)" (1 font-lock-builtin-face))
     ;; Literal Characteres
     ("'\\(\\^\\([@-_]\\|([^)\n]+)\\)\\|.\\)'" (0 font-lock-string-face nil t))
     ;; Labels
     ("^\\s-*\\([A-Za-z0-9_]+\\)\\s-*\\(<>[^<]*<>\\s-*\\)*:[^=]"
      (1 font-lock-variable-name-face)
      (2 chill-directive-face nil t))
     ;; Directives
     ("<>[^<]*<>" (0 chill-directive-face append))
     ;; --Main Block Keywords--
     ;; BEGIN BODY END EXCEPTIONS MODULE ON PROC PROCESS REGION RETURNS
     ("\\<\\(B\\(EGIN\\|ODY\\)\\|EXCEPTIONS\\|MODULE\\|ON\\|PROC\\(ESS\\)?\
\\|RE\\(GION\\|TURNS\\)\\)\\>" . font-lock-function-name-face)
     ("\\<\\(END\\)\\>\\(\\s-+[a-zA-Z0-9_]+\\)?"
      (1 font-lock-function-name-face)
      (2 font-lock-variable-name-face nil t))
     ;; --Predefined & Builtins - implementation defined--
     ;; CODE DISPATCH GETF GETFLN IN_LINE PUTF PUTFLN TTY
     ("\\<\\(CODE\\|DISPATCH\\|GETF\\(LN\\)?\\|IN_LINE\
\\|PUTF\\(LN\\)?\\|TTY\\)\\>" . chill-implement-face)
     ;; --Exceptions--
     ;; ALLOCATEFAIL ASSERTFAIL ASSOCIATEFAIL CONNECTFAIL CREATEFAIL
     ;; DELAYFAIL    DELETEFAIL EMPTY         MODIFYFAIL  NOTASSOCIATED
     ;; NOTCONNECTED OVERFLOW   RANGEFAIL     READFAIL    SENDFAIL
     ;; SPACEFAIL    TAGFAIL    TEXTFAIL      TIMERFAIL   UNDERFLOW
     ;; WRITEFAIL
     ("\\<\\(\\(A\\(LLOCATE\\|SS\\(ERT\\|OCIATE\\)\\)\
\\|C\\(ONNECT\\|REATE\\)\\|DEL\\(AY\\|ETE\\)\\|MODIFY\
\\|R\\(ANGE\\|EAD\\)\\|S\\(END\\|PACE\\)\\|T\\(AG\\|EXT\\|IMER\\)\
\\|WRITE\\)FAIL\\|EMPTY\\|NOT\\(ASSOCIA\\|CONNEC\\)TED\
\\|\\(OV\\|UND\\)ERFLOW\\)\\>" . chill-exception-face)
     ;; --Predefined & Builtins--
     ;; ABS           ABSTIME        ALLOCATE       ASSOCIATE     ASSOCIATION
     ;; BOOL          CARD           CARDINAL       CHAR          CONNECT
     ;; CREATE        DAYS           DELETE         DISCONNECT    DISSOCIATE
     ;; DURATION      EOLN           EXISTING       EXPIRED       FALSE
     ;; FIRST         FLOAT          GETASSOCIATION GETSTACK      GETTEXTACCESS
     ;; GETTEXTINDEX  GETTEXTRECORD  GETUSAGE       HOURS         INDEXABLE
     ;; INSTANCE      INT            INTTIME        ISASSOCIATED  LAST
     ;; LENGTH        LONG_CARDINAL  LONG_FLOAT     LONG_INT      LOWER
     ;; MAX           MILLISECS      MIN            MINUTES       MODIFY
     ;; NULL          NUM            OUTOFFILE      PRED          PTR
     ;; READABLE      READONLY       READRECORD     READTEXT      READWRITE
     ;; SAME          SECS           SEQUENCIBLE    SETTEXTACCESS SETTEXTINDEX
     ;; SETTEXTRECORD SHORT_CARDINAL SHORT_FLOAT    SHORT_INT     SIZE
     ;; SUCC          TERMINATE      TIME           TRUE          UPPER
     ;; USAGE         VARIABLE       WAIT           WHERE         WRITEABLE
     ;; WRITEONLY     WRITERECORD    WRITETEXT
     ("\\<\\(A\\(BS\\(TIME\\)?\\|LLOCATE\\|SSOCIAT\\(E\\|ION\\)\\)\
\\|BOOL\\|C\\(ARD\\(INAL\\)?\\|HAR\\|ONNECT\\|REATE\\)\
\\|D\\(AYS\\|ELETE\\|IS\\(CONNECT\\|SOCIATE\\)\\|URATION\\)\
\\|E\\(OLN\\|X\\(ISTING\\|PIRED\\)\\)\\|F\\(ALSE\\|IRST\\|LOAT\\)\
\\|GET\\(ASSOCIATION\\|STACK\\|TEXT\\(ACCESS\\|INDEX\\|RECORD\\)\\|USAGE\\)\
\\|HOURS\\|I\\(N\\(DEXABLE\\|STANCE\\|T\\(TIME\\)?\\)\\|SASSOCIATED\\)\
\\|L\\(AST\\|ENGTH\\|O\\(NG_\\(CARDINAL\\|FLOAT\\|INT\\)\\|WER\\)\\)\
\\|M\\(AX\\|I\\(LLISECS\\|N\\(UTES\\)?\\)\\|ODIFY\\)\
\\|NU\\(LL\\|M\\)\\|OUTOFFILE\\|P\\(RED\\|TR\\)\
\\|READ\\(ABLE\\|ONLY\\|RECORD\\|TEXT\\|WRITE\\)\
\\|S\\(AME\\|E\\(CS\\|QUENCIBLE\\|TTEXT\\(ACCESS\\|INDEX\\|RECORD\\)\\)\
\\|HORT_\\(CARDINAL\\|FLOAT\\|INT\\)\\|IZE\\|UCC\\)\
\\|T\\(ERMINATE\\|IME\\|RUE\\)\\|U\\(PPER\\|SAGE\\)\\|VARIABLE\
\\|W\\(AIT\\|HERE\\|RITE\\(ABLE\\|ONLY\\|RECORD\\|TEXT\\)\\)\\)\\>" .
font-lock-builtin-face)
     ;; --Predefined & Builtins--
     ;; ARCCOS ARCSIN ARCTAN COS EXP LN LOG SIN SQRT TAN
     ("\\<\\(ARC\\(COS\\|SIN\\|TAN\\)\\|COS\\|EXP\\|L\\(N\\|OG\\)\
\\|S\\(IN\\|QRT\\)\\|TAN\\)\\>" .
font-lock-builtin-face)
     ;; --Type, Signal And Constant Declarations--
     ;; DCL NEWMODE SIGNAL SYN SYNMODE
     ("\\<\\(DCL\\|NEWMODE\\|S\\(IGNAL\\|YN\\(MODE\\)?\\)\\)\\>" .
      font-lock-type-face)
     ;; --Keywords--
     ;; ACCESS   AFTER   ALL      AND      ANDIF    ARRAY    ASSERT   AT
     ;; BIN      BOOLS   BUFFER   BY       CASE     CAUSE    CHARS    CONTEXT
     ;; CONTINUE CYCLE   DELAY    DO       DOWN     DYNAMIC  ELSE     ELSIF
     ;; ESAC     EVENT   EVER     EXIT     FI       FOR      FORBID   GENERAL
     ;; GOTO     GRANT   IF       INIT     INLINE   INOUT    LOC      MOD
     ;; NONREF   NOPACK  NOT      OD       OF       OR       ORIF     OUT
     ;; PACK     POS     POWERSET PREFIXED PRIORITY RANGE    READ     RECEIVE
     ;; REF      REM     REMOTE   RESULT   RETURN   ROW      SEIZE    SEND
     ;; SET      SIMPLE  SPEC     START    STATIC   STEP     STOP     STRUCT
     ;; TEXT     THEN    THIS     TIMEOUT  TO       UP       VARYING  WHILE
     ;; WITH     XOR
     ("\\<\\(A\\(CCESS\\|FTER\\|LL\\|ND\\(IF\\)?\\|RRAY\\|SSERT\\|T\\)\
\\|B\\(IN\\|OOLS\\|UFFER\\|Y\\)\
\\|C\\(AU?SE\\|HARS\\|ONT\\(EXT\\|INUE\\)\\|YCLE\\)\
\\|D\\(ELAY\\|O\\(WN\\)?\\|YNAMIC\\)\
\\|E\\(LS\\(E\\|IF\\)\\|VE\\(NT\\|R\\)\\|XIT\\)\
\\|FOR\\(BID\\)?\\|G\\(ENERAL\\|OTO\\|RANT\\)\
\\|I\\(F\\|N\\(IT\\|LINE\\|OUT\\)?\\)\\|LOC\\|MOD\
\\|NO\\(NREF\\|PACK\\|T\\)\\|O\\(F\\|R\\(IF\\)?\\|UT\\)\
\\|P\\(ACK\\|O\\(S\\|WERSET\\)\\|R\\(EFIXED\\|IORITY\\)\\)\
\\|R\\(ANGE\\|E\\(AD\\|CEIVE\\|F\\|M\\(OTE\\)?\\|SULT\\|TURN\\)\\|OW\\)\
\\|S\\(E\\(IZE\\|ND\\|T\\)\\|IMPLE\\|PEC\
\\|T\\(A\\(RT\\|TIC\\)\\|[EO]P\\|RUCT\\)\\)\
\\|T\\(EXT\\|H\\(EN\\|IS\\)\\|IMEOUT\\|O\\)\
\\|UP\\|VARYING\\|W\\(HILE\\|ITH\\)\\|XOR\\)\\>" . font-lock-keyword-face)
     ("\\<\\(ESAC\\|FI\\|OD\\)\\>\\([ \t]+[a-zA-Z0-9_]+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
     ;; Miscellaneous
     ("\\([#!]\\|:=\\|\\(\\[\\|\\]\\)+\\)" (0 font-lock-keyword-face append))
     ;; Obsolete
     ("\\<PERVASIVE\\>" . chill-obsolete-face)
     )
   ;; Line Comment
   (and chill-line-comment
	(list (cons (concat chill-line-comment-regexp ".*$")
		    '((0 font-lock-comment-face keep)))))
   )
  "Highlighting for CHILL mode.")


(defconst chill-font-lock-faces
  '(
    ;; 0 - new monochrome faces
    (
     (chill-pragma-face    nil     nil     nil t   nil)
     (chill-directive-face nil     nil     nil t   t  )
     (chill-implement-face nil     nil     nil nil t  )
     (chill-exception-face nil     nil     nil t   nil)
     (chill-obsolete-face  nil     nil     t   t   t  )
     )
    ;; 1 - new light gray scale faces
    (
     (chill-pragma-face    nil nil         nil t   nil)
     (chill-directive-face nil nil         nil t   t  )
     (chill-implement-face nil nil         nil nil t  )
     (chill-exception-face nil "LightGray" nil t   nil)
     (chill-obsolete-face  nil nil         t   t   t  )
     )
    ;; 2 - new dark gray scale faces
    (
     (chill-pragma-face    nil nil         nil t   nil)
     (chill-directive-face nil nil         nil t   t  )
     (chill-implement-face nil nil         nil nil t  )
     (chill-exception-face nil "Gray50"    nil t   nil)
     (chill-obsolete-face  nil nil         t   t   t  )
     )
    ;; 3 - new light faces
    (
     (chill-pragma-face    "SteelBlue"        nil nil t   nil)
     (chill-directive-face "DarkGoldenrod"    nil nil t   nil)
     (chill-implement-face "ForestGreen"      nil nil nil nil)
     (chill-exception-face "Grey40"           nil nil t   nil)
     (chill-obsolete-face  "White"    "Firebrick" t   t   t  )
     )
    ;; 4 - new dark faces
    (
     (chill-pragma-face    "MediumAquamarine" nil nil t   nil)
     (chill-directive-face "LightGoldenrod"   nil nil t   nil)
     (chill-implement-face "Green"            nil nil nil nil)
     (chill-exception-face "Orange"           nil nil t   nil)
     (chill-obsolete-face  "Ivory"     "Moccasin" t   t   t  )
     ))
  "Faces used by chill-mode.

This list has the following form:

   (MONO LIGHT-GRAY DARK-GRAY LIGHT DARK)

MONO is a list of faces to monochromatic screen.
LIGHT-GRAY is a list of faces to gray scale screen with ligth background.
DARK-GRAY is a list of faces to gray scale screen with dark background.
LIGHT is a list of faces to color screen with light background.
DARK is a list of faces to color screen with dark background.

All list of faces have elements like `font-lock-face-attributes'.")


(defun chill-font-lock-color ()
  "Initialize chill-mode faces for Font Lock."
  (interactive)
  (or (and font-lock-display-type font-lock-background-mode)
      (font-lock-make-faces))
  (or
   ;; or already initialized
   (assq 'chill-exception-face font-lock-face-attributes)
   ;; or initialize now!
   (mapcar
    '(lambda (face-attributes)
       (font-lock-make-face face-attributes)
       (let ((maplist (assq (car face-attributes) font-lock-face-attributes)))
	 (if maplist
	     (setcdr maplist (cdr face-attributes))
	   (setq font-lock-face-attributes (cons face-attributes
						 font-lock-face-attributes)))))
    (nth (cond
	  ((memq font-lock-display-type '(mono monochrome))
	   0)				; new monochrome faces
	  ((memq font-lock-display-type
		 '(grayscale greyscale grayshade greyshade))
	   (if (eq font-lock-background-mode 'light)
	       1			; new light gray scale faces
	     2))			; new dark gray scale faces
	  ((eq font-lock-background-mode 'light)
	   3)				; new light faces
	  (t
	   4))				; new dark faces
	 chill-font-lock-faces))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill Comment Paragraph (from ada-mode.el)
;;
;; fill comment lines (--...$) (see `chill-line-comment')


(defun chill-fill-comment-paragraph-justify ()
  "Fills current comment paragraph and justifies each line as well."
  (interactive)
  (chill-fill-comment-paragraph t))


(defun chill-fill-comment-paragraph-postfix ()
  "Fills current comment paragraph and justifies each line as well.
Prompts for a postfix to be appended to each line."
  (interactive)
  (chill-fill-comment-paragraph t t))


(defun chill-fill-comment-paragraph (&optional justify postfix)
  "Fills the current comment paragraph.
If JUSTIFY is non-nil, each line is justified as well.
If POSTFIX and JUSTIFY are  non-nil, `chill-fill-comment-postfix'
is appended to each filled and justified line."
  (interactive "P")
  (and chill-line-comment
       (let ((line-comment
	      (concat chill-line-comment-regexp ".*$"))
	     (empty-line-comment
	      (concat chill-line-comment-regexp "[ \t]*$"))
	     (leading-line-comment
	      (concat "^[ \t]*" chill-line-comment-regexp "[ \t]*"))
	     (opos (point-marker))
	     (chill-fill-comment-old-postfix "")
	     begin end end-2 indent fill-prefix)

	 ;; check if inside comment
	 (or (eq (chill-in-literal) 'ada)
	     (error "not inside line comment"))

	 ;; prompt for postfix if wanted
	 (and justify postfix
	      (setq chill-fill-comment-postfix
		    (read-from-minibuffer
		     "enter new postfix string: "
		     chill-fill-comment-postfix)))

	 ;; prompt for old postfix to remove if necessary
	 (and justify postfix
	      (setq chill-fill-comment-old-postfix
		    (read-from-minibuffer
		     "enter already existing postfix string: "
		     chill-fill-comment-postfix)))

	 ;; find limits of paragraph
	 (message "Filling comment paragraph...")
	 (save-excursion
	   (back-to-indentation)
	   ;; find end of paragraph
	   (while (and (looking-at line-comment)
		       (not (looking-at empty-line-comment)))
	     (forward-line 1)
	     (back-to-indentation))
	   (beginning-of-line)
	   (setq end (point-marker))
	   (goto-char opos)
	   ;; find begin of paragraph
	   (back-to-indentation)
	   (while (and (looking-at line-comment)
		       (not (looking-at empty-line-comment)))
	     (forward-line -1)
	     (back-to-indentation))
	   (forward-line 1)
	   ;; get indentation to calculate width for filling
	   (chill-indent-line)
	   (back-to-indentation)
	   (setq indent (current-column)
		 begin (point-marker)))

	 ;; delete old postfix if necessary
	 (and justify postfix
	      (save-excursion
		(goto-char begin)
		(let ((old-postfix
		       (concat chill-fill-comment-old-postfix "\n")))
		  (while (re-search-forward old-postfix end t)
		    (replace-match "\n")))))

	 ;; delete leading whitespace and uncomment
	 (save-excursion
	   (goto-char begin)
	   (beginning-of-line)
	   (while (re-search-forward leading-line-comment end t)
	     (replace-match "")))

	 ;; calculate fill width
	 (setq fill-column (- fill-column indent
			      (length chill-fill-comment-prefix)
			      (if postfix
				  (length chill-fill-comment-postfix)
				0)))
	 ;; fill paragraph
	 (fill-region begin (1- end) justify)
	 (setq fill-column (+ fill-column indent
			      (length chill-fill-comment-prefix)
			      (if postfix
				  (length chill-fill-comment-postfix)
				0)))
	 ;; find end of second last line
	 (save-excursion
	   (goto-char end)
	   (forward-line -2)
	   (end-of-line)
	   (setq end-2 (point-marker)))

	 ;; re-comment and re-indent region
	 (save-excursion
	   (goto-char begin)
	   (indent-to indent)
	   (insert chill-fill-comment-prefix)
	   (let ((prefix (concat "\n" chill-fill-comment-prefix)))
	     (while (re-search-forward "\n" (1- end-2) t)
	       (replace-match prefix)
	       (beginning-of-line)
	       (indent-to indent))))

	 ;; append postfix if wanted
	 (and justify postfix
	      chill-fill-comment-postfix
	      (let ((postfix (concat chill-fill-comment-postfix "\n")))
		;; append postfix up to there
		(save-excursion
		  (goto-char begin)
		  (while (re-search-forward "\n" (1- end-2) t)
		    (replace-match postfix))

		  ;; fill last line and append postfix
		  (end-of-line)
		  (insert-char ?\  (- fill-column
				      (current-column)
				      (length chill-fill-comment-postfix)))
		  (insert chill-fill-comment-postfix))))

	 ;; delete the extra line that gets inserted somehow(??)
	 (save-excursion
	   (goto-char (1- end))
	   (end-of-line)
	   (delete-char 1))

	 (message "Filling comment paragraph...done")
	 (goto-char opos)
	 t)))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions adapted from cc-mode.el


(defun chill-update-modeline ()
  ;; set the chill-auto-hungry-string for the correct
  ;; designation on the modeline
  (setq chill-auto-hungry-string
	(and (or chill-quick-help chill-auto-newline chill-hungry-delete-key)
	     (concat "/"
		     (and chill-quick-help "q")
		     (and chill-auto-newline "a")
		     (and chill-hungry-delete-key "h"))))
  ;; updates all modelines
  (force-mode-line-update t))


(defun chill-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg.
  ;; If arg is nil or zero, toggle the state.
  ;; If arg is negative, turn the state off, and
  ;; if arg is positive, turn the state on.
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))


;; Auto Newline is not fully implemented.
;; Now, only indent the current line if it is typed `:' or `;'.
;; (See chill-electric-terminator).
(defun chill-toggle-auto-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG, if supplied turns on auto-newline when positive,
turns it off when negative, and just toggles it when zero.

When the auto-newline feature is enabled (as evidenced by the `/a', `/qa',
`/ah' or `/qah' on the modeline after the mode name) newlines are
automatically inserted after special places like semi-colon,
colon (end of case label), */ (end of begin-end comment), BEGIN,
ELSE (not on a case label), IN (end of AFTER, AT, or CYCLE), MODULE, OF,
REGION, TIMEOUT, and THEN."
  (interactive "P")
  (setq chill-auto-newline
	(chill-calculate-state arg chill-auto-newline))
  (chill-update-modeline))


(defun chill-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG, if supplied turns on hungry-delete when positive,
turns it off when negative, and just toggles it when zero.

When the hungry-delete-key feature is enabled (as evidenced by the
`/h', `/qh', `/ah' or `/qah' on the modeline after the mode name) the delete
key gobbles all preceding whitespace in one fell swoop."
  (interactive "P")
  (setq chill-hungry-delete-key
	(chill-calculate-state arg chill-hungry-delete-key))
  (chill-update-modeline))


(defun chill-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key features.
Optional numeric ARG, if supplied turns on auto-newline and
hungry-delete when positive, turns them off when negative, and just
toggles them when zero.

See `chill-toggle-auto-state' and `chill-toggle-hungry-state' for details."
  (interactive "P")
  (setq chill-auto-newline
	(chill-calculate-state arg chill-auto-newline))
  (setq chill-hungry-delete-key
	(chill-calculate-state arg chill-hungry-delete-key))
  (chill-update-modeline))


(defun chill-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `chill-hungry-delete-key' is non-nil, as evidenced by the `/h', `/qh',
`/ah' or `/qah' string on the mode line, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `chill-hungry-delete-key' is
nil, or point is inside a literal then the function in the variable
`chill-delete-function' is called."
  (interactive "P")
  (if (or (not chill-hungry-delete-key)
	  arg
	  (chill-in-literal))
      (funcall chill-delete-function (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall chill-delete-function 1)))))


;; commands to indent lines, regions, and expressions
(defun chill-indent-command (&optional whole-exp)
  "Indent current line as CHILL code, or in some cases insert a tab character.

If `chill-tab-always-indent' is:

t,                   always just indent the current line.

nil,                 indent the current line only if point is at the left
		     margin or in the line's indentation;
		     otherwise insert a tab.

other than nil or t, then tab is inserted only within literals (comments and
		     strings) and inside preprocessor directives, but line is
		     always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as CHILL
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (chill-indent-line))
	    beg end)
	(save-excursion
	  (if (eq chill-tab-always-indent t)
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end (- shift-amt) "#")))
    ;; No arg supplied, use chill-tab-always-indent to determine
    ;; behavior
    (cond
     ;; CASE 1: indent when at column zero or in lines indentation,
     ;; otherwise insert a tab
     ((not chill-tab-always-indent)
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))
	  (insert-tab)
	(chill-indent-line)))
     ;; CASE 2: just indent the line
     ((eq chill-tab-always-indent t)
      (chill-indent-line))
     ;; CASE 3: if in a literal, insert a tab, but always indent the
     ;; line
     (t
      (if (memq (chill-in-literal nil 'NOBORDER) '(ada c char string))
	  (insert-tab))
      (chill-indent-line)))))


(defun chill-postprocess-file-styles ()
  "Function that post processes relevant file local variables.
Currently, this function simply applies any style and offset settings
found in the file's Local Variable list.  Applies any style setting
found in `chill-file-style'."
  (and chill-file-style
       (chill-set-style chill-file-style)))


;; Add the postprocessing function to `hack-local-variables-hook'.  As
;; of 28-Aug-1995, XEmacs 19.12 and Emacs 19.29 support this.
(and (fboundp 'add-hook)
     (add-hook 'hack-local-variables-hook 'chill-postprocess-file-styles))


;;;###autoload
(defun chill-set-style-value (stylevars)
  "Given a style's variable alist, institute the style.
STYLEVARS is an association list describing the style and
must be of the form:

  ((VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `chill-style-alist' for the semantics of
VARIABLE and VALUE.

Use `chill-set-style' to set an existing style indentation.
Use `chill-add-style' to create a new style indentation, or
to modify an existing one."
  (interactive (list (eval-minibuffer "Style description: ")))
  (mapcar
   '(lambda (conscell) (set (car conscell) (cdr conscell)))
   stylevars))


;;;###autoload
(defun chill-set-style (stylename)
  "Set chill-mode variables to use one of several different indentation styles.
STYLENAME is a string representing the desired style from the list of
styles described in the variable `chill-style-alist'.  See that variable
for details of setting up styles."
  (interactive (list (let ((completion-ignore-case t))
		       (completing-read
			(format "Which %s indentation style? "
				mode-name)
			chill-style-alist nil t))))
  (let ((vars    (or (cdr (or (assoc stylename chill-style-alist)
			      (assoc (downcase stylename) chill-style-alist)
			      ;; backwards compatibility
			      (assoc (upcase stylename) chill-style-alist)))
		     (error "Invalid indentation style `%s'" stylename)))
	(default (or (cdr (assoc "chill-mode" chill-style-alist))
		     (error "No `chill-mode' style found!"))))
    ;; first reset the style to `chill-mode' to give every style a
    ;; common base. Then institute the new style.
    (chill-set-style-value default)
    (or (string= stylename "chill-mode")
	(chill-set-style-value vars))))


(defun chill-add-style (style descrip &optional set-p)
  "Adds a style to `chill-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ((VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `chill-style-alist' for the semantics of VARIABLE and
VALUE.  This function also sets the current style to STYLE using
`chill-set-style' if the optional SET-P flag is non-nil."
  (interactive
   (let ((stylename   (completing-read "Style to add: " chill-style-alist))
	 (description (eval-minibuffer "Style description: ")))
     (list stylename description
	   (y-or-n-p "Set the style too? "))))
  (setq style (downcase style))
  (let ((s (assoc style chill-style-alist)))
    (if s
	(setcdr s (copy-alist descrip))	; replace
      (setq chill-style-alist (cons (cons style descrip) chill-style-alist))))
  (and set-p (chill-set-style style)))


;; Dynamically append the default value of most variables. This is
;; crucial because future chill-set-style calls will always reset the
;; variables first to the `chill-mode' style before instituting the new
;; style.  Only do this once!
(or (assoc "chill-mode" chill-style-alist)
    (chill-add-style "chill-mode"
		     (mapcar '(lambda (var) (cons var (symbol-value var)))
			     '(chill-group-level
			       chill-indent-level
			       chill-else-level
			       chill-label-offset
			       chill-alternative-offset
			       chill-continued-statement-offset))
		     'SET-DEFAULT-STYLE-NOW))


;; get `reporter-submit-bug-report' when byte-compiling
(eval-when-compile (require 'reporter))


(defconst chill-mode-help-address "viniciusjl@ig.com.br"
  "Address for chill-mode bug reports.")


(defconst chill-mode-maintainer "Vinicius"
  "Maintainer first name for chill-mode bug reports.")


(defun chill-submit-bug-report ()
  "Submit via mail a bug report on chill-mode."
  (interactive)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t))
    (and
     (if (y-or-n-p "Do you want to submit a report on chill-mode? ")
	 t (message "") nil)
     (require 'reporter)
     (reporter-submit-bug-report
      chill-mode-help-address
      (concat "chill-mode " chill-mode-version
	      " as included in " emacs-version)
      '(chill-indent-comment
	chill-group-level
	chill-indent-level
	chill-else-level
	chill-label-offset
	chill-alternative-offset
	chill-continued-statement-offset
	chill-expand-templates
	chill-spec-file-extension
	chill-hide-read-only
	chill-tab-always-indent
	chill-delete-function
	chill-fill-comment-prefix
	chill-fill-comment-postfix
	chill-spec-file-p
	chill-quick-help
	chill-hungry-delete-key
	chill-auto-newline
	fill-column)
      'newline
      nil
      (concat "Hi " chill-mode-maintainer ",")))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick Help


(defvar chill-quick-help-alist
  (let ((alist
	 '(("<>"           "<>"            chill-quick-help-directives
	    "\
		       | Procedure Directives
 Process Directives    |    INTCALL = interrrupt, service
   PID = integer       |    INTERRUPT
   PIDN = string       |    STACKSIZE = integer
   PRIORITY = integer  |
   PROGID = integer    | Variable Directives
   PROGIDN = string    |    ADDRESS = base, offset
   STACKSIZE = integer |    EXTRA
   VERSION = string    |
		       | Signal Directives
		       |    SID = integer")
	   ("ASSOCIATE"    ";\\|\\<ON\\>"  chill-quick-help-associate
	    "\
ASSOCIATE (association_location { , location_or_value }* );

exceptions: ASSOCIATEFAIL")
	   ("CAUSE"         ";"            chill-quick-help-cause
	    "\
ALLOCATEFAIL ASSERTFAIL ASSOCIATEFAIL CONNECTFAIL CREATEFAIL
DELAYFAIL    DELETEFAIL EMPTY         MODIFYFAIL  NOTASSOCIATED
NOTCONNECTED OVERFLOW   RANGEFAIL     READFAIL    SENDFAIL
SPACEFAIL    TAGFAIL    TEXTFAIL      TIMERFAIL   UNDERFLOW
WRITEFAIL")
	   ("CONNECT"      ";\\|\\<ON\\>"  chill-quick-help-connect
	    "\
CONNECT (access_or_text_location, association_location,
	 { READONLY | WRITEONLY | READWRITE }
	 [ , { FIRST | SAME | LAST } [ , index_expression ] ]);

exceptions: CONNECTFAIL, EMPTY, NOTASSOCIATED, RANGEFAIL")
	   ("EXCEPTIONS"    ")"            chill-quick-help-exceptions
	    "\
ALLOCATEFAIL ASSERTFAIL ASSOCIATEFAIL CONNECTFAIL CREATEFAIL
DELAYFAIL    DELETEFAIL EMPTY         MODIFYFAIL  NOTASSOCIATED
NOTCONNECTED OVERFLOW   RANGEFAIL     READFAIL    SENDFAIL
SPACEFAIL    TAGFAIL    TEXTFAIL      TIMERFAIL   UNDERFLOW
WRITEFAIL")
	   ("READTEXT"     ";\\|\\<ON\\>"  chill-quick-help-readtext
	    "\
READTEXT (text_location_or_char_string_expression [ , index_expression ]
	  format_string_expression
	  { , discrete_or_floating_point_or_string_location }* );

exceptions: TEXTFAIL")
	   ("READRECORD"   ";\\|\\<ON\\>"  chill-quick-help-readrecord
	    "\
READRECORD (access_location [ , index_expression ] [ , store_location ]);

exceptions: NOTCONNECTED, RANGEFAIL, READFAIL, TAGFAIL")
	   ("WRITETEXT"    ";\\|\\<ON\\>"  chill-quick-help-writetext
	    "\
WRITETEXT (text_location_or_char_string_location [ , index_expression ]
	   format_string_expression
	   { , discrete_or_floating_point_or_string_expression }* );

exceptions: TEXTFAIL")
	   ("WRITERECORD"  ";\\|\\<ON\\>"  chill-quick-help-writerecord
	    "\
WRITERECORD (access_location [ , index_expression ] , write_expression);

exceptions: NOTCONNECTED, RANGEFAIL, TAGFAIL, WRITEFAIL"))))
    (mapcar '(lambda (elist)
	       (set (nth 2 elist) (nth 3 elist)))
	    alist)
    alist)
  "Alist for quick help.
The elements have the following form:

   (START END VARIABLE HELP)

START is a string in the beginning of a quick help region.
END is a string in the end of a quick help region.
VARIABLE is a symbol variable that will contain HELP.
HELP is a quick help string message to be displayed.

See `chill-quick-on', `chill-quick-help-buffer' and
`chill-quick-help-region'.")


(defconst chill-quick-buffer-name "*CHILL Quick Help*"
  "Used in `chill-quick-on' and `chill-quick-off'.")


(defvar chill-last-property nil
  "Used in `chill-post-command'.")
(make-variable-buffer-local 'chill-last-property)


(defun chill-post-command ()
  (and (boundp 'chill-last-property)
       chill-quick-help
       (let ((this-property (get-text-property (point) 'chill-quick)))
	 (cond ((and (eq chill-last-property this-property)
		     (get-buffer chill-quick-buffer-name))
		)
	       (this-property
		(chill-quick-on))
	       (t
		(chill-quick-off)))
	 (setq chill-last-property this-property))))


(defun chill-toggle-quick-help (arg)
  "Toggle quick help feature.
Optional numeric ARG, if supplied turns on quick help when positive,
turns it off when negative, and just toggles it when zero.

When the quick help feature is enabled (as evidenced by the `/q',
`/qa', `/qh' or `/qah' on the modeline after the mode name) a help
window is automatically showed when point is on a quick help region."
  (interactive "P")
  (setq chill-quick-help (chill-calculate-state arg chill-quick-help))
  (chill-update-modeline))


;; keep buffer modified state
(defmacro chill-quick-save-modified (&rest body)
  `(let ((modified (buffer-modified-p)))
     ,@body
     (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil))))


;; remove quick help property from a region
(defun chill-quick-remove-property (start end)
  (remove-text-properties start end
			  '(
			    chill-quick nil
			    mouse-face  secondary-selection)))


;; add quick help property in a region.
(defun chill-quick-add-property (start end help)
  (add-text-properties start end
		       (list
			'chill-quick help
			'mouse-face  'secondary-selection)))


;; Turn on quick help.
(defun chill-quick-on ()
  (save-excursion
    (let* ((pos (point))
	   (quick-symbol (get-text-property pos 'chill-quick)))
      (if quick-symbol
	  ;; display quick help
	  (let ((buffer (get-buffer-create chill-quick-buffer-name)))
	    (set-buffer buffer)
	    (delete-windows-on buffer)
	    (erase-buffer)
	    (insert (symbol-value quick-symbol))
	    (set-buffer-modified-p nil)
	    (let* ((lines (count-lines (point-min) (point-max)))
		   (size (- (window-height)
			    (max window-min-height
				 (if (progn
				       (goto-char (point-max))
				       (zerop (current-column)))
				     (1+ lines)
				   lines)))))
	      (goto-char (point-min))
	      (if (>= (window-height) (+ window-min-height window-min-height))
		  (set-window-buffer (split-window nil size) buffer)
		(kill-buffer buffer)
		(message "Window height too small for CHILL quick help")
		(ding))))
	;; remove quick help property
	(chill-quick-off)
	(chill-quick-save-modified
	 (chill-quick-remove-property
	  (if (and (> pos (point-min))
		   (get-text-property (1- pos) 'chill-quick))
	      (previous-single-property-change pos 'chill-quick nil (point-min))
	    pos)
	  (if (and (< pos (point-max))
		   (get-text-property (1+ pos) 'chill-quick))
	      (next-single-property-change pos 'chill-quick nil (point-max))
	    pos)))))))


;; Turn off quick help.
(defun chill-quick-off ()
  (let ((buffer (get-buffer chill-quick-buffer-name)))
    (and buffer
	 (save-excursion
	   (delete-windows-on buffer)
	   (kill-buffer buffer))))
  nil)


;; Put quick help property in a valid region.
(defun chill-quick-help-region (start end)
  (save-match-data
    (let (match help case-fold-search)
      (and chill-quick-help
	   ;; check valid quick help start region
	   (save-excursion
	     (goto-char start)
	     (setq match (assoc (cond ((looking-at "<>") "<>")
				      ((/= (char-syntax (preceding-char)) ?w)
				       (chill-current-word))
				      (t nil))
				chill-quick-help-alist)
		   help  (nth 2 match)
		   match (nth 1 match)))
	   ;; check valid quick help end region
	   (save-excursion
	     (goto-char end)
	     (and (re-search-backward match start t)
		  (= (match-end 0) end)))
	   ;; add quick help property
	   (chill-quick-save-modified
	    (chill-quick-add-property start (match-beginning 0) help))))))


;; Start quick help property in buffer.
(defun chill-quick-help-buffer (&optional start end)
  (save-excursion
    (message "Quick Help...")
    (or start (setq start (point-min)))
    (chill-quick-save-modified
     (mapcar
      '(lambda (elist)
	 (let* ((name      (nth 0 elist))
		(match-end (nth 1 elist))
		(help      (nth 2 elist))
		(match-start (if (= (char-syntax (aref name 0)) ?w)
				 (concat "\\<" name "\\>")
			       name))
		begin found case-fold-search)
	   (goto-char start)
	   (while (re-search-forward match-start end t)
	     (setq begin (match-beginning 0))
	     (or (chill-in-literal 'END)
		 (progn
		   (while (and (re-search-forward match-end end t)
			       (setq found (match-beginning 0))
			       (chill-in-literal 'END)))
		   (and found
			(chill-quick-add-property begin found help)))))))
      chill-quick-help-alist))
    (message "Quick Help...done")))


(defun chill-quick-beginning-of-word ()
  (save-excursion
    (skip-syntax-backward "w_")
    (point)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Macros: (macros must be defined before use)


(defmacro chill-search-forward (sregexp smerror shere)
  `(or (re-search-forward ,sregexp nil 'MOVE)
       (progn
	 (goto-char ,shere)
	 (error "No matching %s" ,smerror))))


(defmacro chill-search-backward (bob-p sregexp smerror shere)
  (if bob-p
      `(re-search-backward ,sregexp nil 'MOVE)
    `(or (re-search-backward ,sregexp nil 'MOVE)
	 (progn
	   (goto-char ,shere)
	   (error "No matching %s" ,smerror)))))


(defmacro chill-literal-number-p (&optional back)
  (if back
      `(save-excursion
	 (backward-char ,back)
	 (looking-at "\\<[BDHObdho]'"))
    `(looking-at "\\<[BDHObdho]'")))


(defmacro chill-cond-literal-forward (save &rest cond-alternatives)
  (if save
      `(progn
	 (goto-char (match-beginning 0))
	 (cond
	  ((save-match-data
	     (chill-in-literal 'END))	; skip comments, string, or char
	   t)
	  ,@cond-alternatives))
    `(progn
       (goto-char (match-beginning 0))
       (cond
	((chill-in-literal 'END)	; skip comments, string, or char
	 t)
	,@cond-alternatives))))


(defmacro chill-cond-literal-backward (&rest cond-alternatives)
  `(cond
    ((chill-in-literal 'BEGIN)		; skip comments, string, or char
     t)
    ,@cond-alternatives))


(defmacro chill-bobp ()
  `(save-excursion
     (beginning-of-line)
     (bobp)))


(defmacro chill-next-scope (regexp merror &rest cond-alternatives)
  `(let ((here (point)))
     (while (and (chill-search-forward ,regexp ,merror here)
		 (chill-cond-literal-forward t ,@cond-alternatives)))))


(defmacro chill-previous-scope (bob-p regexp merror &rest cond-alternatives)
  (if bob-p
      `(let ((here (point)))
	 (while (and (not (chill-bobp))
		     (chill-search-backward t ,regexp ,merror here)
		     (chill-cond-literal-backward ,@cond-alternatives))))
    `(let ((here (point)))
       (while (and (chill-search-backward nil ,regexp ,merror here)
		   (chill-cond-literal-backward ,@cond-alternatives))))))


(defmacro chill-if-has-label-then-adjust-indentation-to (indent)
  `(progn
     (setq ,indent (current-indentation))
     (chill-previous-scope
      t
      ";\\|:[^=]\\|\
\\<\\(AFTER\\|AT\\|BEGIN\\|CONTEXT\\|CYCLE\\|\
DO\\|IF\\|MODULE\\|PROC\\|PROCESS\\|REGION\\)\\>"
      ";, :, or begin block"
      (t nil))
     (and (= (following-char) ?:)
	  (eq (chill-decide-label-mark (point)) 'label)
	  (setq ,indent (current-indentation)))))


(defmacro chill-DO-action-p ()
  `(progn
     (chill-skip-spaces-backward)
     (skip-syntax-backward "w")
     (looking-at "\\<DO\\>")))


(defmacro chill-empty-line-p (&optional begin)
  (if begin
      `(save-excursion
	 (beginning-of-line)
	 (looking-at "^[ \t\f]*$"))
    `(looking-at "^[ \t\f]*$")))


(defmacro chill-outer-state (outer)
  `(aref ,outer 0))


(defmacro chill-outer-indent (outer)
  `(aref ,outer 1))


(defmacro chill-outer-extra (outer)
  `(aref ,outer 2))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line Indentation


(defun chill-indent ()
  "Indent current region or line as CHILL code.
If transient-mark-mode is on and mark is active, indent region;
otherwise, indent line."
  (interactive)
  (call-interactively (if (and transient-mark-mode mark-active)
			  'chill-indent-region
			'chill-indent-line)))


(defun chill-indent-region (start end)
  "Indent current region as CHILL code."
  (interactive "*r")
  (save-excursion
    (let ((the-end (chill-adjust-region-markers start end)))
      (skip-chars-forward " \f\t\n" the-end)
      (while (< (point) the-end)
	(chill-indent-line 'NOERROR)
	(forward-line 1)
	(skip-chars-forward " \f\t\n" the-end)))))


(defun chill-indent-line (&optional noerror)
  "Indent current line as CHILL code.
Return the amount the indentation changed by."
  (interactive)
  (if (chill-empty-line-p t)
      (progn
	(delete-horizontal-space)
	0)
    (let* ((relative (- (point-max) (point)))
	   (actual-state (chill-actual-scope-state noerror))
	   (indent (or (chill-calculate-special-indent actual-state)
		       (chill-calculate-proper-indent
			actual-state
			(chill-outer-scope-state actual-state noerror))))
	   (amount (- indent (current-indentation))))
      (or (zerop amount)
	  (progn
	    (beginning-of-line)
	    (delete-horizontal-space)
	    (indent-to indent)))
      (chill-indent-line-comment)
      ;; if initial point was within line indentation
      ;;  then position after the indentation
      ;;  else stay at same point in text
      (if (> (- (point-max) relative) (point))
	  (goto-char (- (point-max) relative)))
      amount)))


(defun chill-indent-line-comment ()
  ;; adjust line comment column on current line,
  ;; based on line comment on previous line.
  (save-excursion
    (let (col)
      (beginning-of-line)
      ;; if current line has line comment and...
      (and (chill-line-comment-p)
	   ;; ...previous line has line comment
	   (save-excursion
	     (forward-line -1)
	     (and (chill-line-comment-p)
		  (setq col (current-column))))
	   ;; then
	   ;;  indent line comment on current line with column of
	   ;;  previous line comment
	   (or (= (current-column) col)
	       (progn
		 (delete-horizontal-space)
		 (indent-to col)))))))


(defun chill-line-comment-p (&optional limit)
  ;; return t if current line has a line comment
  (and chill-line-comment
       (let ((matching (concat chill-line-comment-regexp "\\|/\\*\\|[\"']")))
	 (or limit
	     (setq limit (save-excursion (end-of-line) (point))))
	 (while (and (re-search-forward matching limit 'MOVE)
		     (cond
		      ((= (preceding-char) ?\")
		       (search-forward "\"" limit 'MOVE))
		      ((= (preceding-char) ?\')
		       (or (chill-literal-number-p 2)
			   (search-forward "\'" limit 'MOVE)))
		      (t
		       (goto-char (match-beginning 0))
		       (and (looking-at "/\\*")
			    (chill-end-of-sequence "/\\*" "*/" 2 limit))))))
	 (looking-at chill-line-comment-regexp))))


(defun chill-calculate-special-indent (actual-state)
  "Return appropriate indentation for current line as CHILL code,
if current line is in a comment or is beginning of a comment.
Return an integer (the column to indent to) or nil."
  (cond
   ;; 'begin-line-comment
   ;;    'begin-block     ci/cc
   ;;    'case-label      ci/cc
   ;;    'close-group     ci/cc
   ;;    'declaration     ci/cc
   ;;    'declaration-end ci/cc
   ;;    'else-if         ci/cc
   ;;    'end-block       ci/cc
   ;;    'label           ci/cc
   ;;    'open-group      ci/cc
   ;;    'relevant        ci/cc
   ;;    'timeout         ci/cc
   ((eq actual-state 'begin-line-comment)
    ;; if previous line has line comment
    (or (save-excursion
	  (forward-line -1)
	  (and (chill-line-comment-p)
	       ;; then (current-column)
	       ;; where previous line comment begins
	       (current-column)))
	;; else (current-indentation)
	(current-indentation)))
   ;; 'c-comment 'ada-comment
   ;;    'begin-block     ci
   ;;    'case-label      ci
   ;;    'close-group     ci
   ;;    'declaration     ci
   ;;    'declaration-end ci
   ;;    'else-if         ci
   ;;    'end-block       ci
   ;;    'label           ci
   ;;    'open-group      ci
   ;;    'relevant        ci
   ;;    'timeout         ci
   ((memq actual-state '(c-comment ada-comment))
    (current-indentation))
   ;; otherwise, nil
   (t
    nil)))


(defun chill-calculate-proper-indent (actual-state ostate)
  "Return appropriate indentation based on ACTUAL-STATE (the current line
state) and OSTATE (outer scope state that immediatly contains the current
line).
Return an integer: the column to indent to."
  (let ((outer-state  (chill-outer-state  ostate))
	(indent       (chill-outer-indent ostate))
	(outer-extra  (chill-outer-extra  ostate)))
    (cond
     ;; Oh, oh! beginning or end of buffer
     ((or (eq outer-state  'begin-buffer)
	  (eq actual-state 'end-buffer))
      indent)
     ;; 'proc-ret
     ;;    'begin-block     extra-indentation
     ;;    'case-label      extra-indentation
     ;;    'close-group     extra-indentation
     ;;    'declaration     extra-indentation
     ;;    'declaration-end extra-indentation
     ;;    'else-if         extra-indentation
     ;;    'end-block       extra-indentation
     ;;    'label           extra-indentation
     ;;    'open-group      extra-indentation
     ;;    'relevant        extra-indentation
     ;;    'timeout         extra-indentation
     ((eq actual-state 'proc-ret)
      outer-extra)
     ;; 'begin-comment
     ;;    'begin-block     current-indentation
     ;;    'case-label      current-indentation
     ;;    'close-group     current-indentation
     ;;    'declaration     current-indentation
     ;;    'declaration-end current-indentation
     ;;    'else-if         current-indentation
     ;;    'end-block       current-indentation
     ;;    'label           current-indentation
     ;;    'open-group      current-indentation
     ;;    'relevant        current-indentation
     ;;    'timeout         current-indentation
     ((eq actual-state 'begin-comment)
      (save-excursion
	(back-to-indentation)
	(chill-indent-comment
	 (+ indent
	    (cond
	     ((eq outer-state 'open-group)
	      chill-group-level)
	     ((eq outer-state 'label)
	      chill-label-offset)
	     ((memq outer-state '(begin-block case-label else-if timeout))
	      chill-indent-level)
	     (t
	      0)))))
      (current-indentation))
     (t
      (+ indent
	 (cond
	  ;; 'continuation
	  ;;    'begin-block     chill-continued-statement-offset
	  ;;    'case-label      chill-continued-statement-offset
	  ;;    'close-group     chill-continued-statement-offset
	  ;;    'declaration     chill-continued-statement-offset
	  ;;    'declaration-end chill-continued-statement-offset
	  ;;    'else-if         chill-continued-statement-offset
	  ;;    'end-block       chill-continued-statement-offset
	  ;;    'label           chill-continued-statement-offset
	  ;;    'open-group      chill-continued-statement-offset
	  ;;    'relevant        chill-continued-statement-offset
	  ;;    'timeout         chill-continued-statement-offset
	  ((eq actual-state 'continuation)
	   chill-continued-statement-offset)
	  ;; 'double-continuation
	  ;;    'begin-block     2 chill-continued-statement-offset
	  ;;    'case-label      2 chill-continued-statement-offset
	  ;;    'close-group     2 chill-continued-statement-offset
	  ;;    'declaration     2 chill-continued-statement-offset
	  ;;    'declaration-end 2 chill-continued-statement-offset
	  ;;    'else-if         2 chill-continued-statement-offset
	  ;;    'end-block       2 chill-continued-statement-offset
	  ;;    'label           2 chill-continued-statement-offset
	  ;;    'open-group      2 chill-continued-statement-offset
	  ;;    'relevant        2 chill-continued-statement-offset
	  ;;    'timeout         2 chill-continued-statement-offset
	  ((eq actual-state 'double-continuation)
	   (+ chill-continued-statement-offset
	      chill-continued-statement-offset))
	  ;; 'close-group
	  ;;    'begin-block     - chill-group-level
	  ;;    'case-label      - chill-group-level
	  ;;    'close-group     - chill-group-level
	  ;;    'declaration     - chill-group-level
	  ;;    'declaration-end - chill-group-level
	  ;;    'else-if         - chill-group-level
	  ;;    'end-block       - chill-group-level
	  ;;    'label           - chill-group-level
	  ;;    'open-group      0
	  ;;    'relevant        - chill-group-level
	  ;;    'timeout         - chill-group-level
	  ((eq actual-state 'close-group)
	   (if (eq outer-state 'open-group)
	       0
	     (- chill-group-level)))
	  ;; 'end-block 'timeout
	  ;;    'begin-block     0
	  ;;    'case-label      0
	  ;;    'close-group     0
	  ;;    'declaration     0
	  ;;    'declaration-end - chill-indent-level
	  ;;    'else-if         0
	  ;;    'end-block       - chill-indent-level
	  ;;    'label           0
	  ;;    'open-group      0
	  ;;    'relevant        - chill-indent-level
	  ;;    'timeout         0
	  ((memq actual-state '(end-block timeout))
	   (if (memq outer-state '(declaration-end end-block relevant))
	       (- chill-indent-level)
	     0))
	  ;; 'case-label
	  ;;    'begin-block     chill-alternative-offset
	  ;;    'case-label      0
	  ;;    'close-group     chill-alternative-offset
	  ;;    'declaration     chill-alternative-offset
	  ;;    'declaration-end chill-alternative-offset
	  ;;    'else-if         chill-alternative-offset
	  ;;    'end-block       - chill-indent-level
	  ;;    'label           chill-alternative-offset
	  ;;    'open-group      chill-alternative-offset
	  ;;    'relevant        - chill-indent-level
	  ;;    'timeout         chill-alternative-offset
	  ((eq actual-state 'case-label)
	   (cond
	    ((eq outer-state 'case-label)
	     0)
	    ((memq outer-state '(end-block relevant))
	     (- chill-indent-level))
	    (t
	     chill-alternative-offset)))
	  ;; 'else-if
	  ;;    'begin-block     (if 'case chill-alternative-offset
	  ;;                               chill-else-level)
	  ;;    'case-label      0
	  ;;    'close-group     chill-else-level
	  ;;    'declaration     chill-else-level
	  ;;    'declaration-end chill-else-level
	  ;;    'else-if         0
	  ;;    'end-block       - chill-indent-level
	  ;;    'label           chill-else-level
	  ;;    'open-group      chill-else-level
	  ;;    'relevant        - chill-indent-level
	  ;;    'timeout         chill-else-level
	  ((eq actual-state 'else-if)
	   (cond
	    ((memq outer-state '(case-label else-if))
	     0)
	    ((eq outer-extra 'case)
	     chill-alternative-offset)
	    ((memq outer-state '(end-block relevant))
	     (- chill-indent-level))
	    (t
	     chill-else-level)))
	  ;; 'proc-ess 'declaration 'begin-block
	  ;; 'open-group  'relevant
	  (t
	   (+ (if (chill-continue-p outer-state)
		  chill-continued-statement-offset
		0)
	      (cond
	       ;; 'declaration
	       ;;    'begin-block     chill-indent-level (continue-p)
	       ;;    'case-label      0                  (continue-p)
	       ;;    'close-group     0                  (continue-p)
	       ;;    'declaration     0                  (continue-p)
	       ;;    'declaration-end 0                  (continue-p)
	       ;;    'else-if         0                  (continue-p)
	       ;;    'end-block       0                  (continue-p)
	       ;;    'label           0                  (continue-p)
	       ;;    'open-group      0                  (continue-p)
	       ;;    'relevant        0                  (continue-p)
	       ;;    'timeout         0                  (continue-p)
	       ((eq actual-state 'declaration)
		(if (eq outer-state 'begin-block)
		    chill-indent-level
		  0))
	       ;; 'proc-ess
	       ;;    'begin-block     chill-label-offset (continue-p)
	       ;;    'case-label      chill-label-offset (continue-p)
	       ;;    'close-group     chill-label-offset (continue-p)
	       ;;    'declaration     chill-label-offset (continue-p)
	       ;;    'declaration-end chill-label-offset (continue-p)
	       ;;    'else-if         chill-label-offset (continue-p)
	       ;;    'end-block       chill-label-offset (continue-p)
	       ;;    'label           chill-label-offset (continue-p)
	       ;;    'open-group      chill-label-offset (continue-p)
	       ;;    'relevant        chill-label-offset (continue-p)
	       ;;    'timeout         chill-label-offset (continue-p)
	       ((eq actual-state 'proc-ess)
		chill-label-offset)
	       ;; 'begin-block 'open-group 'relevant
	       ;;    'begin-block     chill-indent-level (continue-p)
	       ;;    'case-label      chill-indent-level (continue-p)
	       ;;    'close-group     0                  (continue-p)
	       ;;    'declaration     0                  (continue-p)
	       ;;    'declaration-end 0                  (continue-p)
	       ;;    'else-if         chill-indent-level (continue-p)
	       ;;    'end-block       0                  (continue-p)
	       ;;    'label           chill-label-offset (continue-p)
	       ;;    'open-group      chill-group-level  (continue-p)
	       ;;    'relevant        0                  (continue-p)
	       ;;    'timeout         chill-indent-level (continue-p)
;;;	       (t
;;;		(cond
	       ((eq outer-state 'open-group)
		chill-group-level)
	       ((eq outer-state 'label)
		chill-label-offset)
	       ((memq outer-state '(begin-block case-label else-if timeout))
		chill-indent-level)
	       (t
		0)
;;;		 ))
	       )))))))))		; end of chill-calculate-proper-indent


(defun chill-continue-p (outer-state)
  ;; return t if current line is a continuation from previous.
  (save-excursion
    ;; skip to previous valid word or punctuation
    (beginning-of-line)
    (chill-skip-spaces-backward)
    ;; it is a continuation if it is NOT a...
    (not
     (cond
      ;; ...beginning of file
      ((bobp)
       t)
      ((= (char-syntax (preceding-char)) ?w)
       (skip-syntax-backward "w")
       (cond
	;; ...if previous word is the end of a CONTEXT...FOR structure
	((looking-at "\\<FOR\\>")
	 (eq outer-state 'end-block))
	;; ...if previous word is a member pertination operator and
	;;    is inside a group or block
	((looking-at "\\<IN\\>")
	 (memq outer-state '(open-group begin-block)))
	;; ...if previous word is a logical pertination operator and
	;;    is inside a group
	((looking-at "\\<\\(AND\\|ANDIF\\|OR\\|ORIF\\|XOR\\)\\>")
	 (eq outer-state 'open-group))
	;; ...if previous word is a begin block, begin declaration or
	;;    words ELSE, OF, THEN or TIMEOUT
	(t
	 (looking-at
	  "\\<\\(ELSE\\|OF\\|THEN\\|TIMEOUT\\|BEGIN\\|CONTEXT\\|\
MODULE\\|ON\\|REGION\\|DCL\\|GRANT\\|NEWMODE\\|\
SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\)\\>"))))
      ;; ...if previous punctuation denotes a label or case label
      ((= (preceding-char) ?:)
       (backward-char 1)
       (memq (chill-decide-label-mark (point)) '(label case-label)))
      ;; ...if previous punctuation is `,', `[', `(', or `;'
      (t
       (memq (preceding-char) '(?, ?\[ ?\( ?\;)))))))


(defun chill-indent-comment (indent)
  ;; it MUST be at beginning of /*...*/ comment
  (let ((start (point))
	(amount (- indent (current-indentation)))
	begin)
    (prog1
	(chill-end-of-sequence "/\\*[^{]" "*/" 2)
      ;; if it is to indent comment and...
      (and chill-indent-comment
	   ;; ...the amount to indent is not zero and
	   (not (zerop amount))
	   ;; ...this is not a commented region and
	   ;; ...there is only blanks between
	   ;; the beginning of comment and the beginning of line
	   (save-excursion
	     (goto-char start)
	     (if (looking-at "[ \t\f]*/\\*{{") ; (see `chill-comment-region')
		 nil
	       (beginning-of-line)
	       (setq begin (point))
	       (skip-chars-forward " \t\f" start)
	       (= (point) start)))
	   ;; then indent comment region text
	   (chill-indent-rigidly begin (point) amount t)))))


(defsubst chill-limit-backward ()
  (- (point) 4096))


(defsubst chill-limit-forward ()
  (+ (point) 4096))


(defun chill-in-declaration (&optional move-p)
  (let ((original (point))
	case-fold-search)
    (while (and (re-search-backward "DCL\\|;\\|:[^=]" (chill-limit-backward) t)
		(chill-in-literal 'BEGIN)))
    (prog1
	(looking-at "DCL")
      (or move-p (goto-char original)))))


(defun chill-actual-scope-state (&optional noerror)
  "Return symbol state of current line as CHILL code."
  (let (case-fold-search)
    (save-excursion
      (back-to-indentation)
      (cond
       ;; --end of buffer--
       ((eobp)
	'end-buffer)
       ;; --beginning of comment--
       ((looking-at "/\\*")
	'begin-comment)
       ;; --beginning of line comment--
       ((and chill-line-comment
	     (looking-at chill-line-comment-regexp))
	'begin-line-comment)
       (t
	(let ((literal (chill-in-literal)))
	  (cond
	   ;; --inside comment--
	   ((eq literal 'c)
	    'c-comment)
	   ;; --inside line comment or prepocessor line--
	   ((memq literal '(ada pound))
	    'ada-comment)
	   ;; --inside literal--
	   (literal
	    'relevant)
	   ;; --case-label open-group--
	   ((looking-at "(")
	    (if (eq (chill-decide-parenthesis) 'case-label)
		'case-label
	      (chill-previous-scope
	       nil
	       "[;()]\\|\\[\\|\\]\\|\
\\<\\(OF\\|THEN\\|FOR\\|WHILE\\|WITH\\|ELSIF\\|AFTER\\|AT\\|BEGIN\\|\
CASE\\|CONTEXT\\|CYCLE\\|IF\\|MODULE\\|ON\\|REGION\\)\\>"
	       "begin block or group"
	       ((= (following-char) ?\))
		(chill-backward-match-paren nil nil noerror))
	       ((= (following-char) ?\])
		(chill-backward-match-brack nil nil noerror))
	       (t
		nil))
	      (cond
	       ((looking-at
		 "\\<\\(AFTER\\|AT\\|CASE\\|CYCLE\\|ELSIF\\|IF\\)\\>")
		'continuation)
	       ((looking-at "\\<\\(FOR\\|WHILE\\|WITH\\)\\>")
		'double-continuation)
	       (t
		'open-group))))
	   ;; --open bracket--
	   ((looking-at "\\[")
	    'open-group)
	   ;; --close parenthesis and bracket--
	   ((looking-at ")\\|\\]")
	    'close-group)
	   ;; --PROC PROCESS--
	   ((looking-at "\\<\\(PROC\\|PROCESS\\)\\>")
	    'proc-ess)
	   ;; --AFTER AT BEGIN CASE CONTEXT CYCLE DO IF MODULE ON REGION--
	   ((looking-at "\\<\\(AFTER\\|AT\\|BEGIN\\|CASE\\|CONTEXT\\|CYCLE\\|\
DO\\|IF\\|MODULE\\|ON\\|REGION\\)\\>")
	    'begin-block)
	   ;; --END ESAC FI OD--
	   ((looking-at "\\<\\(END\\|ESAC\\|FI\\|OD\\)\\>")
	    'end-block)
	   ;; --DCL GRANT NEWMODE SEIZE SIGNAL SYN SYNMODE--
	   ((looking-at "\\<\\(DCL\\|GRANT\\|NEWMODE\\|\
SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\)\\>")
	    'declaration)
	   ;; --EXCEPTIONS RETURNS--
	   ((looking-at "\\<\\(EXCEPTIONS\\|RETURNS\\)\\>")
	    'proc-ret)
	   ;; --ELSE ELSIF--
	   ((looking-at "\\<\\(ELSE\\|ELSIF\\)\\>")
	    'else-if)
	   ;; --TIMEOUT--
	   ((looking-at "\\<TIMEOUT\\>")
	    'timeout)
	   ;; --WHILE--
	   ((looking-at "\\<WHILE\\>")
	    (if (chill-DO-action-p)
		'continuation
	      'double-continuation))
	   ;; --FOR--
	   ((looking-at "\\<FOR\\>")
	    (if (chill-DO-action-p)
		'continuation
	      'end-block))
	   ;; --SET WITH--
	   ((looking-at "\\<\\(SET\\|WITH\\)\\>")
	    'continuation)
	   ;; --SEND--
	   ((looking-at "\\<SEND\\>")
	    'relevant)

	   ;; --it is double-continuation if it is inside...
	   ;; ... DO FOR|WHILE|WITH action header--
	   ((save-excursion
	      (let ((here (point)))
		(and (re-search-backward "\\<DO\\>\\|;" nil t)
		     (/= (following-char) ?\;)
		     (progn
		       (forward-char 2)
		       (chill-skip-spaces-forward)
		       (looking-at "\\<\\(FOR\\|WHILE\\|WITH\\)\\>"))
		     (progn
		       (while (and (search-forward ";" here t)
				   (chill-in-literal 'END)))
		       (/= (preceding-char) ?\;)))))
	    'double-continuation)

	   ;; --it is continuation if it is inside...
	   ;; ... OF|THEN|TO structures--
	   ((save-excursion
	      (let ((here (point))
		    find-p begin)
		(while (and (setq find-p
				  (re-search-forward
				   "[;()]\\|\\[\\|\\]\\|\
\\<\\(CASE\\|DO\\|ELSIF\\|IF\\|OF\\|SEND\\|STRUCT\\|THEN\\|TO\\)\\>"
				   nil t))
			    (goto-char (setq begin (match-beginning 0)))
			    (cond
			     ((chill-in-literal 'END)
			      )
			     ((looking-at "(")
			      (chill-forward-match-paren nil noerror))
			     ((looking-at "\\[")
			      (chill-forward-match "\\[\\|\\]" "\\]" "]"
						   nil noerror))
			     (t
			      nil))))
		(and find-p
		     (goto-char begin)
		     (looking-at "\\<\\(OF\\|THEN\\|TO\\)\\>"))))
	    'continuation)

	   ;; --it is continuation if it is inside...
	   ;; ... AFTER|AT|CYCLE action header--
	   ((let ((here (point))
		  find-p)
	      (while (and (setq find-p
				(re-search-backward
				 "[();]\\|\\[\\|\\]\\|\
\\<\\(AFTER\\|AT\\|CYCLE\\|IN\\)\\>"
				 nil t))
			  (cond
			   ((chill-in-literal 'BEGIN)
			    )
			   ((looking-at ")")
			    (chill-backward-match-paren nil noerror))
			   ((looking-at "\\]")
			    (chill-backward-match-brack nil noerror))
			   (t
			    nil))))
	      (and find-p
		   (looking-at "\\<\\(AFTER\\|AT\\|CYCLE\\)\\>")
		   (progn
		     (while (and (re-search-forward "\\<IN\\>"
						    here 'MOVE)
				 (chill-in-literal 'END)))
		     (= (point) here))))
	    'continuation)

	   ;; otherwise (anything else, including label)
	   ;; --relevant--
	   (t
	    'relevant)
	   )))))))			; end of chill-actual-scope-state


;; use `chill-outer-state' to get STATE from outer state vector
;; use `chill-outer-indent' to get INDENT from outer state vector
;; use `chill-outer-extra' to get EXTRA from outer state vector

(defun chill-outer-scope-state (actual-state noerror)
  "Return state of outer scope that immediatly contains the current line.
Return a state vector:

   [STATE INDENT EXTRA]

Where STATE is the outer state symbol,
INDENT is the indentation,
EXTRA is the complement symbol for STATE."
  (let ((here (save-excursion
		(end-of-line)
		(let ((eol (point)))
		  (beginning-of-line)
		  (and (search-forward ";" eol 'MOVE)
		       (backward-char 1))
		  (point))))
	case-fold-search
	state indent)
    (save-excursion
      (if (eq actual-state 'end-block)
	  (progn
	    (back-to-indentation)
	    (cond
	     ;; --END--
	     ;; terminated by "\\<\\(AFTER\\|AT\\|BEGIN\\|CYCLE\\|
	     ;; MODULE\\|ON\\|PROC\\|PROCESS\\|REGION\\)\\>"
	     ((looking-at "\\<END\\>")
	      (chill-backward-match-end))
	     ;; --ESAC--
	     ;; terminated by "\\<CASE\\>"
	     ((looking-at "\\<ESAC\\>")
	      (chill-backward-match-esac nil nil noerror)
	      (chill-backward-case))
	     ;; --OD--
	     ;; terminated by "\\<DO\\>"
	     ((looking-at "\\<OD\\>")
	      (chill-backward-match-od nil nil noerror))
	     ;; --FI--
	     ;; terminated by "\\<IF\\>"
	     (t
	      (chill-backward-match-fi nil nil noerror)))
	    (vector 'begin-block (current-indentation) nil))
	;; skip current line
	(beginning-of-line)
	(while (and
		;; skip to previous relevant word...
		(setq indent (re-search-backward chill-relevant-regexp
						 nil 'MOVE))
		(cond
		 ;; ...which is not in a string, char or comment, and...
		 ((chill-in-literal 'BEGIN)
		  )
		 ;; ...skip previous `]' to previous matching `['
		 ((looking-at "\\]")
		  (chill-backward-match-brack nil 'MOVE t))
		 ;; ...skip previous `)' to previous matching `('
		 ((looking-at ")")
		  (chill-backward-match-paren nil 'MOVE t))
		 ;; ...decide if previous word is a valid label,
		 ;;    slice or case label
		 ((looking-at ":[^=]")
		  (null (setq state (chill-decide-label-mark here))))
		 ;; ...skip previous CASE expression
		 ((looking-at "\\<ESAC\\>")
		  (chill-backward-match-esac nil nil noerror)
		  (chill-backward-case))
		 ;; ...skip previous IF expression
		 ((looking-at "\\<FI\\>")
		  (chill-backward-match-fi nil nil noerror))
		 ;; otherwise, get out `while'
		 (t
		  nil)))))
      (chill-outer-scope-state2 here state indent noerror))))


(defun chill-outer-scope-state2 (here state indent noerror)
  (let (extra)
    (cond
     ;; --reaches beginning of buffer without find any relevant word--
     ((and (bobp) (not indent))
      (setq state 'begin-buffer
	    indent (current-indentation)))
     ;; --previous word is a valid label, slice or case label--
     (state
      (setq indent (if (eq state 'open-group)
		       (current-column)
		     (current-indentation))))
     ;; --decide if it is a CONTEXT...FOR or DO FOR...OD structure--
     ((looking-at "\\<FOR\\>")
      (setq indent (current-indentation))
      (if (chill-DO-action-p)
	  (setq state  'begin-block
		indent (current-indentation)
		extra  'do)
	(setq state 'end-block)))
     ;; --DCL GRANT NEWMODE SEIZE SIGNAL SYN SYNMODE--
     ;; (see chill-calculate-proper-indent)
     ((looking-at
       "\\<\\(DCL\\|GRANT\\|NEWMODE\\|SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\)\\>")
      (chill-outer-scope-declaration here 'state 'indent))
     ;; --PROC--
     ((looking-at "\\<PROC\\>")
      (if (chill-in-declaration 'MOVE)
	  (chill-outer-scope-declaration here 'state 'indent)
	(setq extra (current-column))
	(if chill-spec-file-p
	    (progn
	      (setq state 'relevant)
	      (chill-if-has-label-then-adjust-indentation-to indent))
	  (setq state  'begin-block
		indent (current-indentation)))))
     ;; --PROCESS--
     ((looking-at "\\<PROCESS\\>")
      (setq extra (current-column))
      (if chill-spec-file-p
	  (progn
	    (setq state 'relevant)
	    (chill-if-has-label-then-adjust-indentation-to indent))
	(setq state  'begin-block
	      indent (current-indentation))))
     ;; --CASE--
     ((looking-at "\\<CASE\\>")
      (setq state  'begin-block
	    indent (current-indentation)
	    extra 'case))
     ;; --AFTER AT BEGIN CONTEXT CYCLE DO IF MODULE ON REGION--
     ((looking-at "\\<\\(AFTER\\|AT\\|BEGIN\\|CONTEXT\\|CYCLE\\|\
DO\\|IF\\|MODULE\\|ON\\|REGION\\)\\>")
      (setq state  'begin-block
	    indent (current-indentation)))
     ;; --END--
     ;; terminated by "\\<\\(AFTER\\|AT\\|BEGIN\\|CYCLE\\|
     ;; MODULE\\|ON\\|PROC\\|PROCESS\\|REGION\\)\\>"
     ((looking-at "\\<END\\>")
      (chill-backward-match-end)
      (setq state  'end-block
	    indent (current-indentation))
      (if (not (looking-at "\\<ON\\>"))
	  (let ((begin (point)))
	    ;; skip SPEC word, if exists
	    (and chill-spec-file-p
		 (progn
		   (forward-word -1)
		   (not (looking-at "\\<SPEC\\>")))
		 (goto-char begin))
	    (chill-if-has-label-then-adjust-indentation-to indent))
	;; ON treatment
	(chill-previous-scope
	 nil
	 ":[^=]\\|;\\|\
\\<\\(ELSE\\|OF\\|TIMEOUT\\|THEN\\|END\\|ESAC\\|FI\\|OD\\|AFTER\\|AT\\|\
BEGIN\\|CONTEXT\\|CYCLE\\|DO\\|IF\\|MODULE\\|PROC\\|PROCESS\\|REGION\\)\\>"
	 "`;', `:', or begin block"
	 ;;
	 ;; label
	 ((= (following-char) ?:)
	  (or (not (eq (chill-decide-label-mark here) 'label))
	      (progn
		(setq indent (current-indentation))
		nil)))
	 ;; inside a block (on border block)
	 ((looking-at "<\\(ELSE\\|OF\\|TIMEOUT\\|THEN\\)\\>")
	  (goto-char (match-end 0))
	  (chill-skip-spaces-forward)
	  (setq indent (current-indentation))
	  nil)
	 ;; terminated by "\\<\\(CASE\\|DO\\|IF\\|END\\|AFTER\\|AT\\|
	 ;; BEGIN\\|CYCLE\\|MODULE\\|ON\\|PROC\\|PROCESS\\|REGION\\)\\>"
	 ((cond
	   ((looking-at "\\<ESAC\\>")
	    (chill-backward-match-esac nil nil noerror)
	    (chill-backward-case)
	    t)
	   ((looking-at "\\<OD\\>")
	    (chill-backward-match-od nil nil noerror))
	   ((looking-at "\\<FI\\>")
	    (chill-backward-match-fi nil nil noerror))
	   ((looking-at "\\<END\\>")
	    (chill-backward-match-end nil nil noerror))
	   (t
	    nil))
	  (chill-if-has-label-then-adjust-indentation-to indent)
	  nil)
	 ;; otherwise
	 (t
	  (if (= (following-char) ?\;)
	      (forward-char 1)
	    (skip-syntax-forward "w"))
	  (chill-skip-spaces-forward)
	  (setq indent (current-indentation))
	  nil))))
     ;; --ESAC--
     ;; terminated by "\\<CASE\\>"
     ((looking-at "\\<ESAC\\>")
      (chill-backward-match-esac nil nil noerror)
      (chill-backward-case)
      (setq state  'end-block)
      (chill-if-has-label-then-adjust-indentation-to indent))
     ;; --OD--
     ;; terminated by "\\<DO\\>"
     ((looking-at "\\<OD\\>")
      (chill-backward-match-od nil nil noerror)
      (setq state  'end-block)
      (chill-if-has-label-then-adjust-indentation-to indent))
     ;; --FI--
     ;; terminated by "\\<IF\\>"
     ((looking-at "\\<FI\\>")
      (chill-backward-match-fi nil nil noerror)
      (setq state  'end-block)
      (chill-if-has-label-then-adjust-indentation-to indent))
     ;; --open-group `(' `['--
     ((looking-at "(\\|\\[")
      (setq state  'open-group
	    indent (current-column)))
     ;; --close-group `)' `]'--
     ((looking-at ")\\|\\]")
      (setq state  'close-group
	    indent (current-indentation)))
     ;; --ELSE ELSIF--
     ((looking-at "\\<ELSE\\|ELSIF\\>")
      (setq state  'else-if
	    indent (current-indentation)))
     ;; --TIMEOUT--
     ((looking-at "\\<TIMEOUT\\>")
      (setq state  'timeout
	    indent (current-indentation)))
     ;; otherwise, --relevant--
     (t
      (setq state  'relevant
	    indent (current-indentation))))
    (vector state indent extra)))	; end of chill-outer-scope-state2


(defun chill-outer-scope-declaration (limit state-sym indent-sym)
  (goto-char (match-end 0))
  (let ((end (chill-forward-word-or-eol))
	(ci  (current-indentation))
	(cc  (current-column)))
    (while (and (search-forward ";" limit 'MOVE)
		(chill-in-literal 'END)))
    (if (= (preceding-char) ?\;)
	(progn
	  (set state-sym  'declaration-end)
	  (set indent-sym ci))
      (set state-sym  'declaration)
      (set indent-sym (if end
			  (+ ci chill-indent-level)
			cc)))))


(defun chill-decide-label-mark (here)
  ;; Decide if previous word is a valid label, slice or case label.
  ;; A valid label does not have a `;' between the label position
  ;; and HERE position.  In other words, there isn't an action
  ;; between the label and HERE position.
  ;;
  ;; Return:
  ;; 'open-group : label mark designate a slice
  ;;               stay at `(' position
  ;; 'case-label : label mark designate a CASE or ON alternative
  ;;               stay at `(' position
  ;; 'label      : label mark designate a label
  ;;               stay at `:' position
  ;; nil         : label mark has no meaning on current context
  ;;               stay at `:' position
  ;;
  ;; CASES:
  ;; (...):  ==> case label ---> current-indentation
  ;;             Must find ;, CASE or ON, because:
  ;;               1- ((A+B)*(C+D):     --> slice
  ;;               2- CASE ... (A),(B): --> case label
  ;;               3- ON ... (B):       --> case label
  ;;               4- ; (A),(B):        --> case label
  ;; (...:   ==> slice      ---> open-group
  ;; [...:   ==> slice      ---> open-group
  ;; name:   ==> label      ---> current-indentation
  (let ((label-line (point)))
    (chill-skip-spaces-backward)
    (if (= (preceding-char) ?\))
	;; if `:' is preceded by `)', decide if is slice or case label
	(let (state-case open-point)
	  ;; search backward "; ( ) CASE ON"
	  (while (and (re-search-backward "[();]\\|\\<\\(CASE\\|ON\\)\\>"
					  nil 'MOVE)
		      (chill-cond-literal-backward
		       ;; ;     ==> may be case label
		       ((looking-at ";")
			(forward-char 1)
			(chill-skip-spaces-forward)
			(setq state-case
			      (if (and open-point (= (point) open-point))
				  'case-label
				'open-group))
			nil)
		       ;; )     ==> closing (
		       ((looking-at ")")
			(if (chill-backward-match-paren
			     (chill-limit-backward) t t)
			    (setq open-point (point))
			  (setq state-case 'open-group)
			  nil))
		       ;; (     ==> slice!
		       ((looking-at "(")
			(setq state-case 'open-group)
			nil)
		       ;; CASE  ==> case label!
		       ;; ON    ==> case label!
		       (t
			(setq state-case 'case-label)
			nil))))
	  state-case)
      ;; if `:' is not preceded by `)', decide if is slice or label
      (let ((limit (chill-limit-backward)))
	;; search backward "( ) ;"
	(while (and (re-search-backward "[();]\\|\\[\\|\\]" limit t)
		    (chill-cond-literal-backward
		     ((= (following-char) ?\))
		      (chill-backward-match-paren limit t t))
		     ((= (following-char) ?\])
		      (chill-backward-match-brack limit t t))
		     (t
		      nil))))
	(if (memq (following-char) '(?\( ?\[))
	    ;; ( ==> slice!
	    'open-group
	  ;; if there is no `;' between current position and `:'
	  ;;   then is a label!
	  ;;   else nil (has no meaning in current context)
	  (goto-char label-line)
	  (save-excursion
	    (while (and (search-forward ";" here 'MOVE)
			(chill-in-literal 'END)))
	    (and (/= (preceding-char) ?\;)
		 'label)))))))


(defun chill-decide-parenthesis ()
  ;; Decide if it is a case label or not.
  ;; case label syntax is (...){,(...)}*:
  ;;
  ;; Return:
  ;; 'case-label : open parenthesis is a beginning of a case label
  ;; 'open-group : open parenthesis is not a case label
  (save-excursion
    (let ((limit (chill-limit-forward)))
      (if (not (chill-forward-match-paren limit t))
	  'open-group
	(while (and (progn
		      (chill-skip-spaces-forward limit)
		      (looking-at ","))
		    (progn
		      (forward-char 1)
		      (chill-skip-spaces-forward limit)
		      (looking-at "("))
		    (chill-forward-match-paren limit t)))
	(if (looking-at ":[^=]")
	    'case-label
	  'open-group)))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beginning/End of Reach


(defun chill-end-of-reach (&optional noerror)
  "Go to the end of current reach, that is, to the end of current
procedure, process, module, or region.
Return point at beginning of current reach, or nil if there is no more
end of reach.
If optional argument NOERROR is non-nil, no error message is echoed."
  (interactive)
  (let ((here (point))
	end)
    (and (looking-at chill-reach-regexp)
	 (forward-char 1))
    (and (setq end (chill-beginning-of-reach 'NOERROR))
	 (eq here end)
	 (setq end (chill-beginning-of-reach 'NOERROR)))
    (if end
	(prog1
	    (point)
	  (goto-char end))
      (goto-char here)
      (or noerror (error "No more end of reach"))
      nil)))


(defun chill-beginning-of-reach (&optional noerror)
  "Go to the beginning of current reach, that is, to the beginning of current
procedure, process, module, or region.
Return point at end of current reach, or nil if there is no more beginning of
reach.
If optional argument NOERROR is non-nil, no error message is echoed."
  (interactive)
  (let ((here (point))
	end findp)
    (or (looking-at chill-reach-regexp)
	(skip-syntax-forward "w"))
    (while (progn
	     (while (and (setq findp
			       (re-search-backward chill-reach-regexp nil t))
			 (chill-in-literal 'BEGIN)))
	     (and findp
		  (< (setq end (save-excursion
				 (chill-sentence-forward)
				 (point)))
		     here))))
    (if findp
	end
      (goto-char here)
      (or noerror (error "No more beginning of reach"))
      nil)))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sentence Indentation
;;
;; This command indents all lines between current position and end of
;; current sentence using chill-indent-line.


(defun chill-sentence-indent (&optional no-message-p move-p)
  "Indent current sentence.

If optional argument NO-MESSAGE-P is non-nil, no message that indicates
begin/end of sentence indentation is showed.

If optional argument MOVE-P is non-nil, go to the end of sentence when
finishes indentation.

Use of chill-indent-line on a large region is SLOWER than using
`chill-sentence-indent-parser'."
  (interactive "*")
  (or no-message-p (message "Sentence indentation..."))
  (let ((begin (point))
	end-of-sentence case-fold-search)
    (save-excursion
      (chill-sentence-forward)
      (setq end-of-sentence (point))
      (skip-chars-backward " \f\t")
      (or (bolp) (forward-line 1))
      (chill-indent-region begin (point)))
    (and move-p (goto-char end-of-sentence)))
  (or no-message-p (message "Sentence indentation...done")))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sentence Indentation Parser


(defvar chill-bolp nil)
(defvar chill-comment-column nil)


(defvar chill-stack-parser (make-vector 200 0)
  "Stack for `chill-sentence-indent-parser'.")
(defvar chill-top-of-stack 0)


(defmacro chill-stack-init ()
  `(setq chill-top-of-stack 0))


(defmacro chill-stack-not-empty-p ()
  `(> chill-top-of-stack 0))


(defmacro chill-push (value)
  `(progn
     (aset chill-stack-parser chill-top-of-stack ,value)
     (setq chill-top-of-stack (1+ chill-top-of-stack))))


(defmacro chill-stack-map (get-value do-action &optional variable map)
  (if variable
      (if map
	  `(setq ,variable (,map (,get-value)))
	`(setq ,variable (,get-value)))
    (if map
	`(,map (,get-value))
      `(,do-action))))


(defmacro chill-pop-basic ()
  `(setq chill-top-of-stack (1- chill-top-of-stack)))


(defmacro chill-pop-value-basic ()
  `(aref chill-stack-parser (chill-pop-basic)))


(defmacro chill-pop-value (&optional variable map)
  `(chill-stack-map chill-pop-value-basic chill-pop-basic ,variable ,map))


(defmacro chill-pop (&optional variable map)
  `(and (chill-stack-not-empty-p)
	(chill-pop-value ,variable ,map)))


(defmacro chill-top-of-stack-basic ()
  `(aref chill-stack-parser (1- chill-top-of-stack)))


(defmacro chill-top-of-stack (&optional variable map)
  `(and (chill-stack-not-empty-p)
	(chill-stack-map chill-top-of-stack-basic chill-top-of-stack-basic
			 ,variable ,map)))


;; These macros are for indentation stack management
;; in `chill-sentence-indent-parser'.
;; The following variables MUST exist:
;; continue-p   non-nil means it can set continuation
;; stack        stack indentation
;; indent       current indentation value

;; Continuation indentation was stacked using negation, but
;; indentation zero is a problem.  So, it was adopted the
;; bias mapping below.

;;; push/pop bias mapping
(defmacro chill-push-bias (value)
  `(+ ,value 100000))


(defmacro chill-pop-bias (value)
  `(- ,value 100000))


(defmacro chill-bias-p (value)
  `(>= ,value 100000))


;;; push/pop general indentation
(defmacro chill-push-indent (&optional plevel pindent)
  (if pindent
      (if plevel
	  `(progn
	     (chill-push indent)
	     (setq continue-p nil
		   indent (+ ,pindent ,plevel)))
	`(progn
	   (chill-push indent)
	   (setq continue-p nil
		 indent (+ ,pindent chill-indent-level))))
    (if plevel
	`(progn
	   (chill-push indent)
	   (setq continue-p nil
		 indent (+ indent ,plevel)))
      `(progn
	 (chill-push indent)
	 (setq continue-p nil
	       indent (+ indent chill-indent-level))))))


(defmacro chill-pop-indent ()
  `(progn
     (chill-pop indent)
     (chill-top-of-stack continue-p chill-bias-p)))


;;; push/pop label- or continuation- indentation
(defmacro chill-push-negative-aux (aindent alevel)
  `(progn
     (chill-push (chill-push-bias indent))
     (setq indent (+ ,aindent ,alevel))))


(defmacro chill-push-negative (nlevel &optional nindent nlevel2)
  (if nindent
      (if nlevel2
	  `(chill-push-negative-aux ,nindent (+ ,nlevel ,nlevel2))
	`(chill-push-negative-aux ,nindent ,nlevel))
    (if nlevel2
	`(chill-push-negative-aux indent (+ ,nlevel ,nlevel2))
      `(chill-push-negative-aux indent ,nlevel))))


(defmacro chill-pop-negative ()
  `(while (chill-top-of-stack nil chill-bias-p)
     (chill-pop-value indent chill-pop-bias)))


;;; push/pop continuation indentation
(defmacro chill-force-push-continue-indent (&optional cindent clevel)
  `(progn
     (setq continue-p t)
     (chill-push-negative chill-continued-statement-offset ,cindent ,clevel)))


(defmacro chill-push-continue-indent (&optional cindent clevel)
  `(or continue-p
       (chill-force-push-continue-indent ,cindent ,clevel)))


(defmacro chill-pop-continue-indent ()
  `(progn
     (setq continue-p nil)
     (chill-pop-negative)))


;;; push/pop label indentation
(defmacro chill-push-label-indent (llevel)
  `(progn
     (skip-chars-forward " \t\f")
     (and (eolp)
	  (chill-push-negative ,llevel))))


(defmacro chill-pop-label-indent ()
  `(and (chill-stack-not-empty-p)
	(progn
	  (chill-pop-continue-indent)
	  (and declaration-p
	       (progn
		 (setq declaration-p nil)
		 (chill-pop-value indent))))))


;;; goto end of matching and indent
(defmacro chill-indent-at-end (&optional eindent)
  (if eindent
      `(progn
	 (goto-char (match-end 0))
	 (chill-indent-to ,eindent))
    `(progn
       (goto-char (match-end 0))
       (chill-indent-to indent))))


;;; error messages
(defmacro chill-error-unbalanced ()
  `(error "Unbalanced parentheses or brackets at %d" (point)))


(defmacro chill-error-mixing ()
  `(error "Mixing parentheses and brackets at %d" (point)))


;;; open parenthesis or brackets indentation
(defmacro chill-indent-open (olevel)
  `(progn
     (forward-char 1)
     (setq ,olevel (1+ ,olevel))
     (chill-indent-to indent)
     (chill-push-indent chill-group-level (- (current-column) 1))))


;;; close parenthesis or brackets indentation
(defmacro chill-indent-close (clevel)
  `(progn
     (and (zerop ,clevel)
	  (chill-error-unbalanced))
     (forward-char 1)
     (setq ,clevel (1- ,clevel))
     (chill-pop-continue-indent)
     (chill-indent-to (- indent chill-group-level))
     (chill-pop-indent)))


;;; top of stack
(defmacro chill-tos-p ()
  `(or (chill-top-of-stack) indent))


(defmacro chill-proper-indent-to (column)
  `(progn
     (delete-horizontal-space)
     (indent-to ,column)))


(defmacro chill-proper-indent-to-region (start end column)
  `(progn
     (goto-char ,start)
     (while (< (point) ,end)
       (chill-proper-indent-to ,column)
       (forward-line 1))))


(defmacro chill-indent-comment-and-directive (indent)
  `(while (progn
	    (chill-skip-blanks)
	    (cond

	     ;; indent begin-end comment
	     ((looking-at "/\\*")
	      (setq chill-bolp nil)
	      (chill-indent-comment ,indent))

	     ;; indent line comment
	     ((looking-at "--")
	      (if chill-comment-column
		  (or (= (current-column) chill-comment-column)
		      (chill-proper-indent-to chill-comment-column))
		(setq chill-comment-column (current-column)))
	      (forward-line 1)
	      (setq chill-bolp t))

	     ;; indent directive
	     ((looking-at "<>")
	      (chill-indent-to ,indent)
	      (forward-char 2)
	      (let ((column-directive (current-column)))
		(if (search-forward "<>" (save-excursion (end-of-line) (point))
				    'MOVE)
		    (chill-skip-blanks t)
		  (let ((start (progn (forward-char 1) (point))))
		    (setq chill-comment-column nil)
		    (search-forward "<>" nil 'MOVE)
		    (save-excursion
		      (forward-line 1)
		      (chill-proper-indent-to-region
		       start (point) column-directive)))))
	      t)

	     ;; skip preprocessing line
	     ((looking-at "^#")
	      (end-of-line)
	      t)

	     ;; otherwise, get out of here
	     (t
	      nil)))))


;;; sentence indentation (parsing sentence)
(defun chill-sentence-indent-parser (&optional no-message-p move-p)
  (interactive "*")
  (or no-message-p (message "Sentence indentation..."))
  (let ((level-case 0) (level-begin 0)
	(level-paren 0) (level-brack 0) (level-case-label 0)
	level-on level-paren-brack level-state
	indent indent-proc continue-regexp end-of-sentence
	declaration-p continue-p context-p timing-p continuation-p
	case-fold-search)
    (save-excursion
      ;; initial adjustments
      (chill-stack-init)
      (skip-chars-forward " \f\t\n")
      (chill-indent-line)
      (back-to-indentation)
      ;; if inside comment, skip it.
      (and (chill-in-literal 'END)
	   (chill-skip-blanks))
      ;; some initializations
      (setq indent (current-column)
	    chill-bolp (= indent (current-indentation))
	    chill-comment-column nil)
      ;; here begins all fun
      (while (progn
	       (setq end-of-sentence nil)
	       (chill-indent-comment-and-directive indent)
	       (cond

		;; --beginning of declaration--
		;; DCL GRANT NEWMODE SEIZE SIGNAL SYN SYNMODE
		((looking-at "\\<\\(DCL\\|GRANT\\|NEWMODE\\|\
SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\)\\>")
		 (chill-indent-at-end)
		 (setq declaration-p t)
		 (chill-pop-continue-indent)
		 (if (chill-forward-word-or-eol)
		     (chill-push-indent chill-indent-level)
		   (chill-push-indent 0 (current-column))))

		;; --IN--
		((looking-at "\\<IN\\>")
		 (cond
		  ;; IN (matching AFTER, AT or CYCLE)
		  ((and timing-p (zerop level-paren) (zerop level-brack))
		   (setq timing-p nil))
		  ;; expression delimiters
		  ((or (> level-paren 0) (> level-brack 0))
		   (chill-push-continue-indent))
		  ;; otherwise
		  )
		 (chill-indent-at-end)
		 (chill-pop-continue-indent))

		;; --expression delimiters--
		;; comma
		((looking-at ",")
		 (chill-push-continue-indent)
		 (chill-indent-at-end)
		 (chill-pop-continue-indent))

		;; --expression delimiters--
		;; AND ANDIF OR ORIF XOR
		((looking-at "\\<\\(AND\\|ANDIF\\|OR\\|ORIF\\|XOR\\)\\>")
		 (chill-push-continue-indent)
		 (chill-indent-at-end)
		 (and (or (> level-paren 0) (> level-brack 0))
		      (chill-pop-continue-indent)))

		;; --label--
		((looking-at chill-label-regexp)
		 (goto-char (1- (match-end 0)))
		 (chill-indent-to indent)
		 (chill-push-label-indent chill-label-offset))

		;; --end of action|declaration--
		;; (this is END-OF-SENTENCE)
		((looking-at ";")
		 (and (not (and (zerop level-paren) (zerop level-brack)))
		      (chill-error-unbalanced))
		 (forward-char 1)
		 (setq end-of-sentence (point))
		 (chill-indent-to indent)
		 (chill-pop-label-indent))

		;; --beginning of procedure|process reach--
		((looking-at "\\<\\(PROC\\|PROCESS\\)\\>")
		 (chill-indent-to indent)
		 (setq indent-proc (current-column))
		 (skip-syntax-forward "w")
		 (if chill-spec-file-p
		     (chill-push-continue-indent)
		   (setq level-begin (1+ level-begin))
		   (chill-push-indent)))

		;; --procedure declaration part--
		;; EXCEPTIONS RETURNS
		((looking-at "\\<\\(EXCEPTIONS\\|RETURNS\\)\\>")
		 (chill-indent-at-end indent-proc))

		;; Continuation cases:
		;; -------------------
		;;
		;; 1) if|elsif...then
		;; 2) after|at|cycle...in
		;; 3) case...of
		;; 4) case...of...;
		;; 5) delay|receive case priority|set...;
		;; 6) do for|while|with...;

		;; --DO action & continuation cases--
		;; + 5) delay|receive case priority|set...;
		;; + 6) do for|while|with...;
		((setq continue-regexp
		       (or
			;; DO
			(and
			 (looking-at "\\<DO\\>")
			 ;; FOR WHILE WITH
			 "\\<\\(FOR\\|WHILE\\|WITH\\)\\>")
			;; DELAY CASE
			;; RECEIVE CASE
			(and
			 (looking-at "\\<\\(DELAY\\|RECEIVE\\)[ \t]+CASE\\>")
			 (setq level-case (1+ level-case))
			 ;; PRIORITY SET
			 "\\<\\(PRIORITY\\|SET\\)\\>")))
		 (chill-indent-at-end)
		 (let ((old indent))
		   (chill-push-indent)
		   (save-excursion
		     (chill-skip-blanks)
		     (and (setq continuation-p (looking-at continue-regexp))
			  (chill-push-continue-indent old)))))

		;; --CASE action & IF action & AFTER|AT|CYCLE action &
		;;   continuation cases--
		;; + 1) if...then
		;; + 2) after|at|cycle...in
		;; + 3) case...of
		;; + 4) case...of...;
		((or
		  ;; IF
		  (looking-at "\\<IF\\>")
		  ;; CASE
		  (and (looking-at "\\<CASE\\>")
		       (setq level-case (1+ level-case)))
		  ;; AFTER AT CYCLE
		  (and (looking-at "\\<\\(AFTER\\|AT\\|CYCLE\\)\\>")
		       (setq timing-p t
			     level-begin (1+ level-begin))))
		 (chill-indent-at-end)
		 (let ((old indent))
		   (chill-push-indent)
		   (chill-push-continue-indent old)))

		;; --CONTEXT reach & ON handler &
		;;   beginning of reach--
		((or
		  ;; BEGIN MODULE REGION
		  (looking-at "\\<\\(BEGIN\\|MODULE\\|REGION\\)\\>")
		  ;; ON
		  (and (looking-at "\\<ON\\>")
		       (setq level-on (cons level-begin level-on)))
		  ;; CONTEXT
		  (and (looking-at "\\<CONTEXT\\>")
		       (setq context-p t)))
		 (or context-p
		     (setq level-begin (1+ level-begin)))
		 (chill-indent-at-end)
		 (chill-push-indent))

		;; --end of CASE|DO|IF action & end of reach &
		;;   end of ON handler & end of CONTEXT reach--
		((or
		  ;; FI OD
		  (looking-at "\\<\\(FI\\|OD\\)\\>")
		  ;; ESAC
		  (and (looking-at "\\<ESAC\\>")
		       (setq level-case (1- level-case)))
		  ;; END
		  (and (looking-at "\\<END\\>")
		       (progn
			 (setq level-begin (1- level-begin))
			 ;; END (matching ON)
			 (and level-on
			      (= (car level-on) level-begin)
			      (setq level-on (cdr level-on)))
			 t))
		  ;; FOR (matching CONTEXT)
		  (and context-p (looking-at "\\<FOR\\>")))
		 (chill-pop-continue-indent)
		 (chill-pop-indent)
		 (chill-indent-at-end)
		 (or context-p
		     (chill-push-continue-indent))
		 (setq context-p nil))

		;; --FOR|WHILE|WITH|PRIORITY|SET--
		;; FOR WHILE WITH (matching DO)
		;; PRIORITY SET (matching DELAY CASE or RECEIVE CASE)
		((and continuation-p
		      (looking-at "\\<\\(FOR\\|WHILE\\|WITH\\|\
PRIORITY\\|SET\\)\\>"))
		 (setq continuation-p nil)
		 (chill-indent-at-end)
		 (chill-force-push-continue-indent))

		;; --OF|THEN--
		((looking-at "\\<\\(OF\\|THEN\\)\\>")
		 (setq timing-p nil)
		 (chill-indent-at-end)
		 (chill-pop-continue-indent))

		;; --ELSE--
		((looking-at "\\<ELSE\\>")
		 (chill-indent-at-end (+ (chill-tos-p) chill-else-level)))

		;; --ELSIF & continuation cases--
		;; + 1) elsif...then
		((looking-at "\\<ELSIF\\>")
		 (let ((old (chill-tos-p)))
		   (chill-indent-at-end (+ old chill-else-level))
		   (chill-push-continue-indent old chill-else-level)))

		;; --open parenthesis--
		((looking-at "(")
		 ;; if it is inside a CASE action or ON handler and
		 (and (or (> level-case 0) level-on)
		      ;; it is not treating a case label and
		      (not (eq level-state 'case-label))
		      ;; current open parenthesis is a case label beginning
		      (eq (chill-decide-parenthesis) 'case-label)
		      ;; then treat case label
		      (let ((case-indent (chill-tos-p)))
			(setq level-state 'case-label
			      level-case-label level-paren)
			(chill-push-indent chill-alternative-offset
					   case-indent)))
		 ;; in any case, treat all inside parenthesis as a group
		 (chill-indent-open level-paren))

		;; --close parenthesis--
		((looking-at ")")
		 (chill-indent-close level-paren))

		;; --open bracket--
		((looking-at "\\[")
		 (setq level-paren-brack level-paren)
		 (chill-indent-open level-brack))

		;; --close bracket--
		((looking-at "\\]")
		 (or (eq level-paren-brack level-paren)
		     (chill-error-mixing))
		 (chill-indent-close level-brack))

		;; --colon--
		((looking-at ":")
		 (forward-char 1)
		 (cond
		  ;; end of case label
		  ((and (eq level-state 'case-label)
			(= level-case-label level-paren))
		   (setq level-state nil)
		   (chill-indent-to indent)
		   (chill-pop-indent))
		  ;; assignment sign & label|slice mark
		  (t
		   (chill-push-continue-indent)
		   (chill-indent-to indent))))

		;; --SPEC--
		((looking-at "\\<SPEC\\>")
		 (chill-indent-at-end))

		;; --TIMEOUT--
		((looking-at "\\<TIMEOUT\\>")
		 (chill-indent-at-end (chill-tos-p)))

		;; --otherwise--
		(t
		 (chill-indent-to indent)
		 ;; skip sequence of non-blank characters
		 (while (cond
			 ;; begin char
			 ((eq (following-char) ?\')
			  (chill-end-of-sequence "'" "'" 1)
			  t)
			 ;; begin string
			 ((eq (following-char) ?\")
			  (chill-end-of-sequence "\"" "\"" 1)
			  t)
			 ;; not a begin comment (--, /*)
			 ((looking-at "-[^-]\\|/+[^*]")
			  (goto-char (1- (match-end 0))))
			 ;; begin literal number in other base
			 ((chill-literal-number-p)
			  (forward-char 2)
			  t)
			 ;; sequence of words and operators
			 (t
			  (> (skip-chars-forward "!*+.0-9<=>A-Z_a-z") 0))))
		 (chill-push-continue-indent)))
	       ;; loop while stack is not empty or is not end of sentence
	       (or (not end-of-sentence) (chill-stack-not-empty-p)))
	;; skip blanks
	(chill-skip-blanks t)))
    ;; here stack is empty and is end of sentence
    (and move-p (goto-char end-of-sentence)))
  (or no-message-p (message "Sentence indentation...done")))


(defmacro chill-eobp ()
  `(and (eobp)
	(error "Something is wrong: End Of Buffer")))


(defun chill-skip-blanks (&optional comment-p)
  (let ((eolp-n 0))
    (while (progn
	     (skip-chars-forward " \t\f")
	     (and (eolp)
		  (progn
		    (chill-eobp)
		    (forward-char 1)
		    (setq eolp-n (1+ eolp-n)
			  chill-bolp t)))))
    (and (or (and comment-p chill-bolp)
	     (> eolp-n 1))
	 (setq chill-comment-column nil))
    t))


(defun chill-indent-to (column)
  (and (integerp column)
       (progn
	 (or (not chill-bolp)
	     (zerop (- (current-indentation) column))
	     (let ((here-relative (- (point-max) (point))))
	       (beginning-of-line)
	       (chill-proper-indent-to column)
	       (goto-char (- (point-max) here-relative))))
	 (setq chill-bolp nil))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forward Sentence


(defun chill-sibling-forward ()
"Go to the end of current sentence, and stops at following points:

structures                    | stop points
------------------------------+---------------------------------------------
CASE                          | ELSE, ESAC
IF                            | ELSIF, ELSE, FI
AFTER, AT, CYCLE              | TIMEOUT, END
ON                            | ELSE, END
MODULE, PROC, PROCESS, REGION | END, or nested MODULE, PROC, PROCESS, REGION"
  (interactive)
  (chill-sentence-forward t))


(defun chill-sentence-forward (&optional sibling-p)
  "Go to the end of current sentence.

If optional argument SIBLING-P is non-nil, consider intermediate points
(see `chill-sibling-forward')."
  (interactive)
  (let (initial-point end case-fold-search)
    (and (= (char-syntax (preceding-char)) ?w)
	 (= (char-syntax (following-char)) ?w)
	 (skip-syntax-backward "w"))
    (setq initial-point (point))
    (while (and (re-search-forward chill-sentence-regexp nil 'MOVE)
		(setq end (match-end 0))
		(chill-cond-literal-forward
		 nil
		 ;; FOR special case
		 ((looking-at "\\<FOR\\>")
		  (prog1
		      (chill-DO-action-p)
		    (goto-char end)))
		 ;; otherwise
		 (t
		  nil))))
    (chill-sentence-forward2 sibling-p initial-point end)))


(defun chill-sentence-forward2 (sibling-p initial-point end)
  (cond
   ;; --CONTEXT--
   ;; terminated by "\\<FOR\\>"
   ((looking-at "\\<CONTEXT\\>")
    (chill-forward-scope "\\<FOR\\>" "FOR"))
   ;; --DO--
   ;; terminated by "\\<OD\\>"
   ((looking-at "\\<DO\\>")
    (chill-forward-match "\\<\\(DO\\|OD\\)\\>" "\\<OD\\>" "OD")
    (chill-forward-semicolon))
   ;; --CASE--
   ;; terminated by "\\<ESAC\\>"
   ((looking-at "\\<CASE\\>")
    (if sibling-p
	(chill-forward-match-else "\\<ESAC\\>" "ESAC")
      (chill-forward-match-esac)
      (chill-forward-semicolon)))
   ;; --ELSE--
   ;; terminated by "\\<\\(END\\|ESAC\\|FI\\)\\>"
   ((looking-at "\\<ELSE\\>")
    (chill-forward-match-sibling "\\<\\(ESAC\\|CASE\\|IF\\|FI\\)\\>"
				 "END, ESAC, or FI")
    (chill-forward-semicolon))
   ;; --ELSIF--
   ;; terminated by "\\<\\(ELSIF\\|ELSE\\|FI\\)\\>"
   ((looking-at "\\<ELSIF\\>")
    (chill-forward-match-else "\\<ELSIF\\>" "ELSIF, ELSE, or FI"))
   ;; --IF--
   ;; terminated by "\\<FI\\>"
   ((looking-at "\\<IF\\>")
    (if sibling-p
	(chill-forward-match-else "\\<ELSIF\\>" "ELSIF, ELSE, or FI")
      (chill-forward-match-fi)
      (chill-forward-semicolon)))
   ;; --ON--
   ;; terminated by "\\<END\\>"
   ((looking-at "\\<ON\\>")
    (cond
     ((/= (point) initial-point)
      nil)
     (sibling-p
      (chill-forward-match-sibling "\\<\\(ELSE\\|CASE\\|IF\\|TIMEOUT\\)\\>"
				   "END or ELSE")
      (chill-forward-end-semicolon))
     (t
      (chill-forward-match-end)
      (chill-forward-semicolon))))
   ;; --AFTER AT CYCLE--
   ;; terminated by "\\<END\\>"
   ((looking-at "\\<\\(AFTER\\|AT\\|CYCLE\\)\\>")
    (if sibling-p
	(progn
	  (chill-forward-match-sibling "\\<TIMEOUT\\>" "END or TIMEOUT")
	  (chill-forward-end-semicolon))
      (chill-forward-match-end)
      (chill-forward-semicolon)))
   ;; --PROC PROCESS--
   ;; terminated by "\\<END\\>"
   ((looking-at "\\<\\(PROC\\|PROCESS\\)\\>")
    (cond
     (chill-spec-file-p
      (chill-forward-scope ";" "`;'"))
     (sibling-p
      (chill-forward-reach-sibling))
     (t
      (chill-forward-match-end)
      (chill-forward-semicolon))))
   ;; --MODULE REGION--
   ;; terminated by "\\<END\\>"
   ((looking-at "\\<\\(MODULE\\|REGION\\)\\>")
    (if sibling-p
	(chill-forward-reach-sibling)
      (chill-forward-match-end)
      (chill-forward-semicolon)))
   ;; --BEGIN TIMEOUT--
   ;; terminated by "\\<END\\>"
   ((looking-at "\\<\\(BEGIN\\|TIMEOUT\\)\\>")
    (chill-forward-match-end)
    (chill-forward-semicolon))
   ;; --DCL END ESAC FI GRANT NEWMODE OD SEIZE SIGNAL SYN SYNMODE--
   ;; terminated by ";"
   ((looking-at "\\<\\(DCL\\|END\\|ESAC\\|FI\\|GRANT\\|NEWMODE\\|OD\\|\
SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\)\\>")
    (chill-forward-scope ";" "`;'"))
   ;; --OF THEN--
   ((looking-at "\\<\\(OF\\|THEN\\)\\>")
    (goto-char end)
    (chill-skip-spaces-forward))
   ;; otherwise
   (t
    (goto-char end))))


(defun chill-forward-reach-sibling ()
  (goto-char (match-end 0))
  (chill-next-scope
   (concat "\\<\\(END\\|AFTER\\|AT\\|BEGIN\\|CYCLE\\|ON\\)\\>\\|"
	   chill-reach-regexp)
   "No reach point found"
   ((looking-at "\\<\\(AFTER\\|AT\\|BEGIN\\|CYCLE\\|ON\\)\\>")
    (chill-forward-match-end))
   (t
    nil))
  (chill-forward-end-semicolon))


(defun chill-forward-match-else (regexp merror)
  (goto-char (match-end 0))
  (chill-next-scope
   (concat "\\<\\(CASE\\|ELSE\\|FI\\|IF\\)\\>\\|"
	   regexp)
   merror
   ((looking-at "\\<CASE\\>")
    (chill-forward-match-esac))
   ((looking-at "\\<IF\\>")
    (chill-forward-match-fi))
   (t
    nil))
  (and (looking-at "\\<\\(ESAC\\|FI\\)\\>")
       (chill-forward-semicolon)))


(defun chill-forward-match-sibling (regexp merror)
  (goto-char (match-end 0))
  (chill-next-scope
   (concat "\\<\\(END\\|AFTER\\|AT\\|BEGIN\\|CYCLE\\|MODULE\\|ON\\)\\>\\|"
	   regexp)
   merror
   ((looking-at "\\<\\(AFTER\\|AT\\|BEGIN\\|CYCLE\\|MODULE\\|ON\\)\\>")
    (chill-forward-match-end))
   ((looking-at "\\<CASE\\>")
    (chill-forward-match-esac))
   ((looking-at "\\<IF\\>")
    (chill-forward-match-fi))
   (t
    nil)))


(defun chill-forward-match-fi ()
  (chill-forward-match "\\<\\(FI\\|IF\\)\\>" "\\<FI\\>" "FI"))


(defun chill-forward-match-esac ()
  (chill-forward-match "\\<\\(CASE\\|ESAC\\)\\>" "\\<ESAC\\>" "ESAC"))


(defun chill-forward-match-end ()
  (chill-forward-match
   (if chill-spec-file-p
       "\\<\\(END\\|MODULE\\|REGION\\)\\>"
     "\\<\\(END\\|AFTER\\|AT\\|BEGIN\\|CYCLE\\|MODULE\\|ON\\|\
PROC\\|PROCESS\\|REGION\\)\\>")
   "\\<END\\>" "END"))


(defun chill-forward-match-paren (&optional bound noerror nomatch)
  (chill-forward-match "[()]" ")" ")" bound noerror nomatch))


(defun chill-forward-match (match-regexp end-regexp merror
					 &optional bound noerror nomatch)
  (let ((level 1)
	(here (if nomatch (point) (match-end 0)))
	end)
    (goto-char here)
    (while (and (> level 0)
		(re-search-forward match-regexp bound t))
      (setq end (point))
      (chill-cond-literal-forward
       nil
       (t
	(setq level (+ level (if (looking-at end-regexp) -1 1)))
	(goto-char end))))
    (or (zerop level)
	(and (not noerror)
	     (progn
	       (goto-char here)
	       (error "No matching %s" merror))))))


(defun chill-forward-end-semicolon ()
  (and (looking-at "\\<END\\>")
       (chill-forward-semicolon)))


(defun chill-forward-semicolon ()
  (skip-syntax-forward "w")
  (let ((here (point)))
    (chill-forward-scope ";\\|\\<ON\\>" "`;' or ON")
    (and (/= (preceding-char) ?\;)
	 (goto-char here))))


(defun chill-forward-scope (regexp merror)
  (chill-next-scope regexp merror
		    (t
		     (goto-char (match-end 0))
		     nil)))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backward Sentence


(defun chill-sibling-backward ()
"Go to the beginning of current sentence, and stops at following points:

structures                    | stop points
------------------------------+---------------------------------------------
CASE                          | ELSE, ESAC
IF                            | ELSIF, ELSE, FI
AFTER, AT, CYCLE              | TIMEOUT, END
ON                            | ELSE, END
MODULE, PROC, PROCESS, REGION | END, or nested MODULE, PROC, PROCESS, REGION"
  (interactive)
  (chill-sentence-backward t))


(defun chill-sentence-backward (&optional sibling-p)
  "Go to the beginning of current sentence.

If optional argument SIBLING-P is non-nil, consider intermediate points
(see `chill-sibling-backward')."
  (interactive)
  (let ((start (point))
	special-point start-word-p case-fold-search)
    ;; if it is in middle of a word or at beginning of words below
    ;;   then skip to the end of word
    (if (or (setq start-word-p
		  (looking-at "\\<\\(ELSIF\\|ELSE\\|END\\|ESAC\\|FI\\|FOR\\|\
OD\\|TIMEOUT\\)\\>"))
	    (and (= (char-syntax (preceding-char)) ?w)
		 (= (char-syntax (following-char)) ?w)))
	(skip-syntax-forward "w"))
    ;; skip backward to a relevant point
    ;; that it is not a comment or literal
    (while (and (re-search-backward  chill-sentence-regexp nil 'MOVE)
		(chill-cond-literal-backward
		 ;; OF|THEN special case
		 ((looking-at "\\<\\(OF\\|THEN\\)\\>")
		  (save-excursion
		    (goto-char (match-end 0))
		    (chill-skip-spaces-forward)
		    (or (= (point) start)
			(progn
			  (setq special-point (point))
			  nil))))
		 ;; otherwise
		 (t
		  nil))))
    ;; --semicolon--
    ;; started by DCL\\|GRANT\\|NEWMODE\\|
    ;; SEIZE\\|SIGNAL\\|SYN\\|SYNMODE
    (if (= (following-char) ?\;)
	(let (end)
	  (while (and (re-search-backward
		       ";\\|:[^=]\\|\\<\\(DO\\|ELSE\\|END\\|ESAC\\|FI\\|\
OD\\|OF\\|THEN\\|TIMEOUT\\|DCL\\|GRANT\\|NEWMODE\\|\
SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\|AFTER\\|AT\\|BEGIN\\|\
CONTEXT\\|CYCLE\\|MODULE\\|REGION\\)\\>"
		       nil 'MOVE)
		      (setq end (match-end 0))
		      (chill-in-literal 'BEGIN)))
	  ;; if it is neither an end of action
	  ;; nor a declaration beginning
	  (and (not (looking-at "\\<\\(END\\|ESAC\\|FI\\|OD\\|DCL\\|GRANT\\|\
NEWMODE\\|SEIZE\\|SIGNAL\\|SYN\\|SYNMODE\\)\\>"))
	       ;; then skip to next valid word or punctuation
	       (progn
		 (and end (goto-char end))
		 (chill-skip-spaces-forward)))))
    (chill-sentence-backward2 sibling-p start special-point start-word-p)))


(defun chill-sentence-backward2 (sibling-p start special-point start-word-p)
  (cond
   ;; --OF|THEN special case--
   (special-point
    (goto-char special-point))
   ;; --END--
   ;; started by "\\<\\(AFTER\\|AT\\|BEGIN\\|
   ;; CYCLE\\|MODULE\\|ON\\|PROC\\|PROCESS\\|REGION\\)\\>"
   ((looking-at "\\<END\\>")
    (if (or chill-spec-file-p (not sibling-p))
	(chill-backward-match-end)
      (chill-previous-scope
       nil
       "\\<\\(END\\|AFTER\\|AT\\|BEGIN\\|CYCLE\\|ELSE\\|ESAC\\|FI\\|\
MODULE\\|ON\\|PROC\\|PROCESS\\|REGION\\|TIMEOUT\\)\\>"
       "AFTER, AT, BEGIN, CYCLE, MODULE, ON, PROC, PROCESS, or REGION"
       ((looking-at "\\<FI\\>")
	(chill-backward-match-fi))
       ((looking-at "\\<ESAC\\>")
	(chill-backward-match-esac))
       ((looking-at "\\<END\\>")
	(chill-backward-match-end)
	(not (and sibling-p (looking-at chill-reach-regexp))))
       (t
	nil))))
   ;; --TIMEOUT--
   ;; started by "\\<\\(AFTER\\|AT\\|CYCLE\\)\\>"
   ((and start-word-p (looking-at "\\<TIMEOUT\\>"))
    (let ((here (point)))
      (chill-backward-match-end)
      (and (not (looking-at "\\<\\(AFTER\\|AT\\|CYCLE\\)\\>"))
	   (progn
	     (goto-char here)
	     (error "No matching AFTER, AT, or CYCLE")))))
   ;; --ESAC--
   ;; started by "\\<CASE\\>"
   ((looking-at "\\<ESAC\\>")
    (if sibling-p
	(chill-backward-else "\\<\\(CASE\\|ELSE\\)\\>"
			     "CASE or ELSE")
      (chill-backward-match-esac))
    (and (looking-at "\\<CASE\\>")
	 (chill-backward-case)))
   ;; --OD--
   ;; started by "\\<DO\\>"
   ((looking-at "\\<OD\\>")
    (chill-backward-match-od))
   ;; --FI--
   ;; started by "\\<IF\\>"
   ((looking-at "\\<FI\\>")
    (if sibling-p
	(chill-backward-else "\\<\\(ELSE\\|ELSIF\\|IF\\)\\>"
			     "ELSE, ELSIF, or IF")
      (chill-backward-match-fi)))
   ;; --ELSIF--
   ;; started by "\\<\\(ELSIF\\|IF\\)\\>"
   ((and start-word-p (looking-at "\\<ELSIF\\>"))
    (chill-backward-else "\\<\\(ELSIF\\|IF\\)\\>"
			 "ELSIF, or IF"))
   ;; --ELSE--
   ;; started by "\\<\\(CASE\\|ELSIF\\|IF\\)\\>"
   ((and start-word-p (looking-at "\\<ELSE\\>"))
    (chill-backward-else "\\<\\(CASE\\|ELSIF\\|IF\\|ON\\)\\>"
			 "CASE, ELSIF, IF, or ON"))
   ;; --FOR--
   ;; started by "\\<\\(CONTEXT\\|DO\\)\\>"
   ((looking-at "\\<FOR\\>")
    (chill-previous-scope
     nil
     "\\<\\(CONTEXT\\|DO\\)\\>"
     "CONTEXT or DO"
     (t nil)))
   ;; --CASE--
   ;; started by "\\<\\(RECEIVE\\|DELAY\\)?\\>"
   ((looking-at "\\<CASE\\>")
    (chill-backward-case))))


(defun chill-backward-else (regexp merror)
  (chill-previous-scope
   nil
   (concat "\\<\\(ESAC\\|FI\\)\\>\\|"
	   regexp)
   merror
   ((looking-at "\\<ESAC\\>")
    (chill-backward-match-esac))
   ((looking-at "\\<FI\\>")
    (chill-backward-match-fi))
   ((looking-at "\\<CASE\\>")
    (chill-backward-case))
   (t
    nil)))


(defun chill-backward-match-od (&optional bound movep noerror)
  (chill-backward-match "\\<\\(DO\\|OD\\)\\>" "\\<OD\\>" "DO"
			bound movep noerror))


(defun chill-backward-match-esac (&optional bound movep noerror)
  (chill-backward-match "\\<\\(CASE\\|ESAC\\)\\>" "\\<ESAC\\>" "CASE"
			bound movep noerror))


(defun chill-backward-match-fi (&optional bound movep noerror)
  (chill-backward-match "\\<\\(FI\\|IF\\)\\>" "\\<FI\\>" "IF"
			bound movep noerror))


(defun chill-backward-match-end (&optional bound movep noerror)
  (chill-backward-match
   (if chill-spec-file-p
       "\\<\\(END\\|MODULE\\|REGION\\)\\>"
     "\\<\\(END\\|AFTER\\|AT\\|BEGIN\\|CYCLE\\|MODULE\\|ON\\|\
PROC\\|PROCESS\\|REGION\\)\\>")
   "\\<END\\>"
   "AFTER, AT, BEGIN, CYCLE, MODULE, ON, PROC, PROCESS, or REGION"
   bound movep noerror))


(defun chill-backward-match-paren (&optional bound movep noerror)
  (chill-backward-match "[()]" ")" "(" bound movep noerror))


(defun chill-backward-match-brack (&optional bound movep noerror)
  (chill-backward-match "\\[\\|\\]" "\\]" "[" bound movep noerror))


(defun chill-backward-match (match-regexp end-regexp merror
					  &optional bound movep noerror)
  (let ((level 1)
	(here (point))
	(action (or movep 'MOVE)))
    (while (and (> level 0)
		(re-search-backward match-regexp bound action))
      (chill-cond-literal-backward
       (t
	(setq level (+ level (if (looking-at end-regexp) 1 -1))))))
    (or (zerop level)
	(and (not noerror)
	     (progn
	       (goto-char here)
	       (error "No matching %s" merror))))))


(defun chill-backward-case ()
  (let* ((here (point))
	 (end (progn
		(skip-syntax-forward "w")
		(point))))
    (if (not (and (re-search-backward
		   "\\<\\(RECEIVE\\|DELAY\\)[ \f\t\n]+CASE\\>"
		   (chill-limit-backward) t)
		  (= (match-end 0) end)))
	(goto-char here))
    nil))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defun chill-adjust-region-markers (start end)
  "Adjust the START and END of region."
  (let (tmp)
    (if (< end start)
	(setq tmp   start
	      start end
	      end   tmp))
    (goto-char end)
    (and (not (bolp))
	 (progn
	   (forward-line 1)
	   (setq end (point))))
    (goto-char start)
    (beginning-of-line)
    (if (markerp end)
	end
      (copy-marker end))))


(defun chill-forward-word-or-eol ()
  "Skip spaces, comments and directives until next word or end of line.
Return t if reaches end of line."
  (chill-skip-spaces-forward (save-excursion (end-of-line) (point)))
  (eolp))


(defun chill-forward-word (&optional limit)
  "Move cursor to next word, or to position LIMIT.
Skip spaces, comments, string, char literal and punctuation.
Return t if point is in a word."
  (while (progn
	   (chill-skip-blanks-forward limit) ; skip spaces and comments
	   (cond
	    ((= (following-char) ?\")	; skip string
	     (forward-char 1)
	     (search-forward "\"" limit 'MOVE))
	    ((= (following-char) ?\')	; skip char literal
	     (forward-char 2)
	     (search-forward "'" limit 'MOVE))
	    ((memq (char-syntax (following-char)) '(?. ?\( ?\))) ; skip punctuation
	     (skip-syntax-forward ".()" limit)
	     t)
	    (t
	     nil))))
  (= (char-syntax (following-char)) ?w))


(defun chill-skip-spaces-forward (&optional limit)
  "Skip forward spaces, comments and directives, until
reaches position LIMIT, or any word or punctuation."
  (while (progn
	   (chill-skip-blanks-forward limit) ; skip spaces and comments
	   (and (looking-at "<>")	; skip directive
		(progn
		  (forward-char 2)
		  (search-forward "<>" limit 'MOVE))))))


(defun chill-skip-spaces-backward ()
  "Skip backward spaces, comments and directives."
  (while (progn
	   (chill-skip-blanks-backward) ; skip spaces and comments
	   (and (= (preceding-char) ?>)
		(= (char-after (- (point) 2)) ?<) ; skip directive
		(progn
		  (backward-char 2)
		  (search-backward "<>" nil 'MOVE))))))


(defun chill-skip-blanks-forward (&optional limit)
  "Skip forward spaces and comments, until
reaches position LIMIT, or any word or punctuation."
  (while (progn
	   (skip-chars-forward " \f\t\n" limit) ; skip spaces
	   (cond
	    ((and chill-line-comment	;skip line comment
		  (looking-at chill-line-comment-regexp))
	     (end-of-line)
	     t)
	    ((looking-at "/\\*")	; skip comment
	     (chill-end-of-sequence "/\\*" "*/" 2 limit))
	    (t
	     nil)))))


(defun chill-skip-blanks-backward ()
  "Skip backward spaces and comments."
  (while (progn
	   (skip-chars-backward " \f\t\n") ; skip spaces
	   (cond
	    ;; skip preprocessor line
	    ((save-excursion
	       (beginning-of-line)
	       (= (following-char) ?#))
	     (beginning-of-line)
	     t)
	    ;; skip comment
	    ((and (= (preceding-char) ?/)
		  (= (char-after (- (point) 2)) ?*))
	     (chill-beginning-of-comment nil 2))
	    ;; skip line comment
	    (chill-line-comment
	     (let ((here (point))
		   (matching (concat chill-line-comment-regexp
				     "\\|/\\*")))
	       (beginning-of-line)
	       (and (re-search-forward matching here 'MOVE)
		    (progn
		      (goto-char (match-beginning 0))
		      ;; or line comment was skipped and
		      ;;    continue skipping
		      (or (looking-at chill-line-comment-regexp)
			  ;; or return to proper position and
			  ;;    terminate skipping
			  (progn
			    (goto-char here)
			    nil))))))
	    ;; nothing to skip
	    (t
	     nil)))))


(defun chill-beginning-of-comment (&optional limit back)
  (and back (backward-char back))
  (let (begin)
    (while (and (re-search-backward "/\\*\\|\\*/" limit 'MOVE)
		(looking-at "/\\*"))
      (setq begin (point)))
    (and begin (goto-char begin))))


 
(defun chill-get-label ()
  "Get label and return it, if exists;
otherwise, return nil."
  (save-excursion
    (and (progn				; if is preceded by `:'
	   (chill-skip-spaces-backward)
	   (= (preceding-char) ?:))
	 (progn				; and before `:' there is no `)'
	   (backward-char 1)
	   (chill-skip-spaces-backward)
	   (/= (preceding-char) ?\)))
	 (buffer-substring		; then return the label
	  (progn (forward-word -1) (point))
	  (progn (forward-word 1) (point))))))


(defun chill-indent-rigidly-with-increment (start end col &optional toend)
  (chill-indent-rigidly
   start end
   (- (+ col chill-indent-level) (current-indentation))
   toend))


(defun chill-indent-rigidly (start end col &optional toend)
  (and col start end
       (progn
	 (or (markerp end)
	     (setq end (copy-marker end)))
	 (indent-rigidly start end col)
	 (if toend
	     (progn
	       (goto-char end)
	       (if (bolp)
		   (backward-char 1)
		 (end-of-line)))))))


(defun chill-insert-semicolon ()
  (if (looking-at "[ \t\f\n]*;")
      (goto-char (match-end 0))
    (insert ";")))


(defun chill-expand-block (end-word &optional start end nosemicolon)
  "Expand the block initiated by the word right before point.
Point is not moved after expansion.
END-WORD is used as the block's closing word.
If there is a label in front of the block, it'll be repeated at the end.
If the block has already been expanded, it is not re-expanded.  A block
is considered expanded if the first word after the current line is
identical to END-WORD and is indented by the same amount.
Return the column the block is indented to.  If the block is already
expanded, return nil.

If START and END are both non-nil, START and END denote a beginning and
end (respectively) of a region. The region is indented by chill-indent-level,
and END-WORD is inserted at end of region.

If NOSEMICOLON is non-nil, the semicolon at end of block is not inserted."
  (prog1
      (save-excursion
	(forward-word -1)
	(chill-indent-line 'NOERROR)
	;; get column and label (if exists)
	(let ((col (current-column))
	      (label (chill-get-label)))
	  ;; if label is on the same line then get current indentation
	  (or (= (point)
		 (progn
		   (beginning-of-line)
		   (chill-skip-spaces-forward)
		   (point)))
	      (setq col (current-indentation)))
	  ;; do this expansion only if it is a new one
	  (and (save-excursion
		 (forward-line 1)
		 (skip-chars-forward " \f\t\n")
		 (not (and (= (current-column) col)
			   (looking-at end-word))))
	       (progn
		 (chill-indent-rigidly-with-increment start end col t)
		 (end-of-line)
		 (newline)
		 (indent-to col)
		 (insert end-word)
		 (and label (insert " " label))
		 (or nosemicolon (chill-insert-semicolon))
		 col))))
    (and (= (following-char) ?\;)
	 (delete-char 1))))


(defun chill-in-literal (&optional movep noborder)
  "Determine if point is in a CHILL `literal'.
Return:
'c      if in a C style comment         (syntax: /*...*/),
'ada    if in an Ada style line comment (syntax: --...$),
'string if in a string literal          (syntax: \"...\"),
'char   if in a char literal            (syntax: '.'),
'pound  if in a preprocessor line       (syntax: ^#...$),
nil     if not in a literal at all.

Optional argument MOVEP has the following values:
nil        keep current position;
'END       move to the end of literal;
otherwise  move to the beginning of literal.

Optional argument NOBORDER when non-nil, do not check
if point is at literal border.

If variable `chill-line-comment' is nil, never is returned the
value 'ada.

See variable `chill-line-comment' to check if current
CHILL environment support line comment (as Ada does)."
  (let ((limit (point))
	state begin)
    (or
     ;; or current line is contained in a /*...*/ comment
     (and (save-excursion
	    (setq begin (chill-beginning-of-comment)))
	  (< begin (save-excursion (beginning-of-line) (point)))
	  (setq state 'c))
     ;; or current line must be seen closer
     (save-excursion
       (setq begin (chill-in-literal-line nil limit)
	     state (car begin)
	     begin (cdr begin))))
    ;; or current position is inside a CHILL literal
    (or state
	noborder
	;; or current position may be on a CHILL literal border
	(setq state
	      (cond
	       ((looking-at "/\\*")
		'c)
	       ((= (following-char) ?\")
		'string)
	       ((= (following-char) ?\')
		(and (not (chill-literal-number-p 1))
		     'char))
	       ((and (= (following-char) ?*)
		     (= (preceding-char) ?/))
		(and movep (backward-char 1))
		'c)
	       ((= (following-char) ?#)
		'pound)
	       (t
		;; check if it is on a line comment border
		;; if it is not, return nil
		(and chill-line-comment
		     ;; or it may be in the very beginning of line comment
		     (or (looking-at chill-line-comment-regexp)
			 ;; or it may be inside line comment delimiter
			 ;; CAUTION:
			 ;; Do not try to use something like:
			 ;; (looking-at (concat "\\(.*\\)" delimiter))
			 ;; this will match the longest possible sequence,
			 ;; lets see an example:
			 ;; ^ is the point position
			 ;; -- is the line comment delimiter
			 ;; 1. Initial state                    : "-^-  --"
			 ;; 2. After repositioning              : "^--  --"
			 ;; 3. After looking-at and executing a
			 ;;    (goto-char (match-end 1))        : "--  ^--"
			 (save-excursion
			   (let ((size (1- (length chill-line-comment))))
			     (and
			      (> (point) size)
			      (progn
				(backward-char size)
				(search-forward chill-line-comment
						(+ (point) size size) t))))))
		     ;; ok! it is a line comment border
		     (progn
		       (and movep (goto-char (match-beginning 0)))
		       'ada))))
	      begin (point)))
    ;; if it is inside a (or on a border of) literal and it is to move
    (and state movep
	 ;; then move to beginning of literal
	 (goto-char begin)
	 ;; but if it is to move to the end
	 (eq movep 'END)
	 ;; then lets move to the end of literal
	 (cond
	  ((eq state 'char)
	   (chill-end-of-sequence "'" "'" 1))
	  ((eq state 'string)
	   (chill-end-of-sequence "\"" "\"" 1))
	  ((eq state 'c)
	   (chill-end-of-sequence "/\\*" "*/" 2))
	  (t
	   (end-of-line))))
    state))


(defun chill-end-of-sequence (begin-regexp end-string forward-step
					   &optional limit)
  "Go to the end of a sequence started by BEGIN-REGEXP and terminated by
END-STRING.

FORWARD-STEP indicates the number of characters to forward.

Optional argument LIMIT is the search boundary position."
  (let (findp)
    (while (progn
	     (forward-char forward-step)
	     (and (setq findp (search-forward end-string limit 'MOVE))
		  (looking-at begin-regexp))))
    findp))


(defun chill-in-literal-line (regexp &optional limit-pos)
  "Search forward REGEXP from beginning of line until end of line or
LIMIT-POS (if non-nil).
If LIMIT-POS is non-nil, it is supposed that it is a position before
end of current line.
If REGEXP is nil, it is checked if position LIMIT-POS is inside a
string, char or comment.
If REGEXP is non-nil, it searchs forward until REGEXP is found and
it is not inside a comment, string or char.

It is returned:

t   if REGEXP was found and it is not inside a comment, string or char,
nil if REGEXP was not found and it is not inside a comment, string or char,
(STATE . BEGIN)
    if LIMIT-POS is inside a comment, string or char.
    Where, BEGIN is the position at beginning of comment, string or char,
    STATE is one of following values:
    'c      C style comment   (syntax: /*...*/)
    'ada    Ada style comment (syntax: --...$)
    'string string            (syntax: \"...\")
    'char   char              (syntax: '.').
    'pound  preprocessor line (syntax: ^#...$).

See `chill-in-literal' and `chill-line-comment' for docummentation on
line comment."
  (let ((matching (concat "[\"']\\|/\\*"
			  (and chill-line-comment "\\|")
			  chill-line-comment-regexp
			  regexp))
	(limit (or limit-pos (save-excursion (end-of-line) (point))))
	begin state)
    (beginning-of-line)
    (if (= (following-char) ?#)
	;; preprocessor
	(setq state 'pound
	      begin (point))
      (while (and (null state)
		  (re-search-forward matching limit 'MOVE))
	(setq begin (match-beginning 0))
	(cond
	 ;; string
	 ((= (preceding-char) ?\")
	  (or (search-forward "\"" limit 'MOVE)
	      (setq state 'string)))
	 ;; char
	 ((= (preceding-char) ?\')
	  (or (chill-literal-number-p 2)
	      (search-forward "'" limit 'MOVE)
	      (setq state 'char)))
	 (t
	  (goto-char begin)
	  (cond
	   ;; begin-end comment
	   ((looking-at "/\\*")
	    (or (chill-end-of-sequence "/\\*" "*/" 2 limit)
		(setq state 'c)))
	   ;; line comment
	   ((and chill-line-comment
		 (looking-at chill-line-comment-regexp))
	    (setq state 'ada))
	   ;; REGEXP found
	   (t
	    (setq state t)))))))
    (if (or (null state) (eq state t))
	state
      (cons state begin))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show/Hide All Comments /*...*/  (from hideif.el)


;; from outline.el with docstring fixed.
(defun chill-outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is \\n (newline character) then text is shown, while if FLAG is \\^M
\(control-M) the text is hidden."
  (let ((modp (buffer-modified-p)))
    (unwind-protect (progn
		      (subst-char-in-region from to
					    (if (= flag ?\n) ?\^M ?\n)
					    flag t))
      (set-buffer-modified-p modp))))


;; By putting this on after-revert-hook, we arrange that it only
;; does anything when revert-buffer avoids turning off the mode.
;; (That can happen in VC.)
(defun chill-before-revert-function ()
  (and chill-comment-hiding
       (chill-hide-all-comments t)))

(add-hook 'after-revert-hook 'chill-before-revert-function)


(defun chill-hide-toggle-read-only ()
  "Toggle chill-hide-read-only."
  (interactive)
  (setq chill-hide-read-only (not chill-hide-read-only))
  (message "Hide-Read-Only %s"
	   (if chill-hide-read-only "ON" "OFF"))
  (if chill-comment-hiding
      (setq buffer-read-only (or chill-hide-read-only
				 chill-outside-read-only)))
  (force-mode-line-update))


(defun chill-hide-all-comments (&optional nomsg)
  "Hide the contents of all comments /*...*/.
Turn off hiding by calling `chill-show-all-comments'."
  (interactive)
  (or nomsg (message "Hiding..."))
  (setq chill-outside-read-only buffer-read-only)
  (and chill-comment-hiding
       (chill-show-all-comments))	; Otherwise, deep confusion.
  (let ((inhibit-read-only t)
	start)
    (setq selective-display t
	  chill-comment-hiding t)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "/*" nil t)
	(or (memq (chill-in-literal) '(ada string char))
	    (progn
	      (setq start (point))
	      (chill-end-of-sequence "/\\*" "*/" 2)
	      (chill-outline-flag-region start (match-beginning 0) ?\^M))))))
  (setq buffer-read-only (or chill-hide-read-only
			     chill-outside-read-only))
  (or nomsg (message "Hiding...done")))


(defun chill-show-all-comments ()
  "Cancel the effects of `chill-hide-all-comments':
  show the contents of all comments /*...*/."
  (interactive)
  (setq buffer-read-only chill-outside-read-only
	selective-display nil)		; defaults
  (let ((inhibit-read-only t))
    (chill-outline-flag-region (point-min) (point-max) ?\n))
  (setq chill-comment-hiding nil))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands


(defun chill-electric-terminator (arg)
  "Insert character and correct line indentation, if auto newline is on."
  (interactive "*P")
  (insert-and-inherit last-command-char)
  (if chill-auto-newline
      (chill-indent-line 'NOERROR)))


(defun chill-newline-and-indent ()
  "Insert a newline, then indent according to CHILL mode.
Indentation is done using the chill-indent-line function."
  (interactive "*")
  (chill-indent-line 'NOERROR)		; indent current line
  (let ((indent (current-indentation))
	(column (current-column)))
    (delete-horizontal-space)		; eliminate trailing spaces
    (newline)
    (and (<= column indent)		; to avoid indentation problems
	 (indent-to indent)))		; with /*...*/ comments
  (if (chill-empty-line-p)
      (progn				; indent new empty line
	(insert "a")
	(chill-indent-line 'NOERROR)
	(delete-char -1))
    (chill-indent-line 'NOERROR)))	; indent new line


(defun chill-comment-region (start end)
  "Comment current region.

It is used the following rules:
   - /*{{...}}*/ is placed at region border
   - /*...*/ comment is replaced by {{...}}
   - /*{{...}}*/ comment inside region is replaced by {{...}}
   - {{...}} remains intact"
  (interactive "*r")
  (save-excursion
    (let ((the-end (chill-adjust-region-markers start end))
	  (the-begin (point)))
      ;; set beginning of region
      (and (re-search-forward "/\\*\\|\\*/" the-end t)
	   (= (preceding-char) ?/)
	   (setq the-begin (point)))
      ;; set end of region
      (goto-char the-end)
      (and (re-search-backward "/\\*\\|\\*/" the-begin t)
	   (= (following-char) ?/)
	   (setq the-end (point-marker)))
      (goto-char the-begin)
      (chill-skip-blanks-forward the-end)
      (or
       ;; or the region has only spaces and comments
       (= (point) the-end)
       ;; or there is something to comment
       (progn
	 ;; insert begin comment region /*{{
	 (goto-char
	  (or (save-excursion
		(skip-chars-backward " \t")
		(and (bolp)
		     (progn
		       (insert "/*{{\n")
		       (point))))
	      (progn
		(insert "/*{{")
		(point))))
	 ;; insert end comment region }}*/
	 (save-excursion
	   (goto-char the-end)
	   (chill-skip-blanks-backward)
	   (setq the-end (or (save-excursion
			       (skip-chars-forward " \t")
			       (and (eolp)
				    (prog1
					(point-marker)
				      (insert "\n}}*/"))))
			     (prog1
				 (point-marker)
			       (insert "}}*/")))))
	 ;; all /*...*/ or /*{{...}}*/ comment inside region
	 ;; is replaced by {{...}}
	 (while (search-forward "/*" the-end 'MOVE)
	   (delete-char -2)
	   (cond
	    ;; re-insert /* if inside a string or comment
	    ((memq (chill-in-literal) '(ada string char))
	     (insert "/*"))
	    ;; replace /*{{...}}*/ by {{...}}
	    ((looking-at "{{")
	     (chill-search-delimiter "}}*/" the-end)
	     (delete-char -2))
	    ;; replace /*...*/ by {{...}}
	    (t
	     (insert "{{")
	     (chill-search-delimiter "*/" the-end)
	     (delete-char -2)
	     (insert "}}")))))))))


(defun chill-uncomment-region (start end)
"Uncomment current region.

It is used the following rules:
   - /*{{...}}*/ comment delimiters are eliminated
   - {{...}} is replaced by /*{{...}}*/, if have nested {{...}};
     otherwise, is replaced by /*...*/
   - nested {{...}} remains intact"
  (interactive "*r")
  (save-excursion
    (let ((the-end (chill-adjust-region-markers start end))
	  (the-begin (point))
	  nested-p)
      ;; set beginning of region
      (and (re-search-forward "/\\*\\|\\*/" the-end t)
	   (= (preceding-char) ?/)
	   (setq the-begin (point)))
      ;; set end of region
      (goto-char the-end)
      (and (re-search-backward "/\\*\\|\\*/" the-begin t)
	   (= (following-char) ?/)
	   (setq the-end (point-marker)))
      (goto-char the-begin)
      ;; uncomment region
      (while (search-forward "/*{{" the-end 'MOVE)
	;; delete /*{{ delimiter
	(delete-char -4)
	;; if it is inside a string or comment
	(if (chill-in-literal)
	    ;; then re-insert /*{{
	    (insert "/*{{")
	  ;; else
	  (and (looking-at "^$")
	       (delete-char 1))
	  (setq the-begin (point))
	  (chill-search-delimiter "}}*/" the-end)
	  ;; delete }}*/ delimiter
	  (delete-char -4)
	  (and (looking-at "^$")
	       (delete-char 1))
	  (setq end (point-marker))
	  ;; all {{...}} delimiters inside region are replaced by /*...*/
	  ;; NOTE: nested {{...}} remains intact
	  (save-excursion
	    (goto-char the-begin)
	    (while (search-forward "{{" end 'MOVE)
	      (or (save-match-data (chill-in-literal))
		  (progn
		    ;; if there is nested {{...}}
		    ;;    then replace outer {{...}} by /*{{...}}*/
		    ;;    else replace {{...}} by /*...*/
		    (save-match-data
		      (save-excursion
			(setq nested-p
			      (and (re-search-forward "{{\\|}}" end t)
				   (= (preceding-char) ?{)))))
		    (replace-match (if nested-p
				       "/*{{"
				     "/*"))
		    (chill-forward-match "{{\\|}}" "}}" "}}" end nil t)
		    (replace-match (if nested-p
				       "}}*/"
				     "*/")))))))))))


(defun chill-search-delimiter (delimiter limit)
  (while (and (or (search-forward delimiter limit 'MOVE)
		  (error "No matching %s at %d" delimiter limit))
	      (memq (chill-in-literal) '(ada string char)))))


(defun chill-insert-line-comment ()
  (interactive "*")
  (and chill-line-comment
       (insert chill-line-comment " ")))


(defun chill-insert-comment ()
  (interactive "*")
  (if (looking-at "[ \t]*:")
      (progn
	(delete-horizontal-space)
	(forward-char 1)
	(insert " ")))
  (insert "/*  */")
  (backward-char 3))


(defun chill-insert-directive ()
  (interactive "*")
  (let ((here (point)))
    (skip-chars-backward " \t")
    (if (/= (preceding-char) ?:)
	(goto-char here)
      (backward-char 1)
      (delete-horizontal-space)
      (insert "  ")
      (backward-char 1))
    (insert "<>  <>")
    (chill-quick-help-region (- (point) 6) (point))
    (backward-char 3)))


(defun chill-upper ()
  "Try to expand current keyword or turn valid keywords in region to upper case.

If transient-mark-mode is on and mark is active, turn valid keywords in region
to upper case; otherwise, try to expand current keyword."
  (interactive)
  (call-interactively (if (and transient-mark-mode mark-active)
			  'chill-upper-region
			'chill-upper-keyword)))


(defun chill-upper-keyword ()
  "Try to expand current keyword."
  (interactive)
  (chill-try-expand t))


(defun chill-upper-region (start end)
  "Turn valid keywords in region to upper case."
  (interactive "*r")
  (and chill-expand-templates
       (save-excursion
	 (let ((the-end (chill-adjust-region-markers start end))
	       (start-quick (point))
	       name)
	   (chill-in-literal 'END)
	   (while (and (chill-forward-word)
		       (< (point) the-end))
	     (setq name (upcase (chill-current-word)))
	     (if (or (assoc name chill-special-alist)
		     (assoc name chill-implementation-alist))
		 (upcase-word 1)
	       (skip-syntax-forward "w")))
	   (chill-quick-help-buffer start-quick the-end)))))


(defun chill-insert-after ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "AFTER" 'space))


(defun chill-insert-at ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "AT" 'space))


(defun chill-insert-begin ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "BEGIN" 'newline))


(defun chill-insert-case ()
  (interactive)
  (chill-insert-keyword "CASE" 'space))


(defun chill-insert-code ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "CODE"))


(defun chill-insert-context ()
  (interactive)
  (chill-insert-keyword "CONTEXT" 'newline))


(defun chill-insert-cycle ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "CYCLE" 'space))


(defun chill-insert-delay ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "DELAY" 'space))


(defun chill-insert-delay-case ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "RECEIVE" 'space)
  (save-excursion
    (search-backward "RECEIVE" nil t)
    (replace-match "DELAY")))


(defun chill-insert-do-control (control)
  (chill-insert-do t)
  (insert control " ;")
  (backward-char 1))


(defun chill-insert-do-for ()
  (interactive)
  (chill-insert-do-control "FOR"))


(defun chill-insert-do-while ()
  (interactive)
  (chill-insert-do-control "WHILE"))


(defun chill-insert-do-with ()
  (interactive)
  (chill-insert-do-control "WITH"))


(defun chill-insert-do (&optional space-p)
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "DO" (if space-p
				 'space
			       'newline)))


(defun chill-insert-else ()
  (interactive)
  (insert "ELSE")
  (chill-try-expand)
  (chill-newline-and-indent))


(defun chill-insert-elsif ()
  (interactive)
  (chill-check-spec)
  (insert "ELSIF")
  (chill-try-expand)
  (insert " "))


(defun chill-insert-exceptions ()
  (interactive)
  (insert "EXCEPTIONS")
  (chill-try-expand))


(defun chill-insert-if ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "IF" 'space))


(defun chill-insert-in_line ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "IN_LINE"))


(defun chill-insert-module ()
  (interactive)
  (chill-insert-keyword "MODULE" 'newline))


(defun chill-insert-on ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "ON" 'newline))


(defun chill-insert-proc ()
  (interactive)
  (chill-insert-keyword "PROC"))


(defun chill-insert-process ()
  (interactive)
  (chill-insert-keyword "PROCESS"))


(defun chill-insert-receive ()
  (interactive)
  (chill-check-spec)
  (chill-insert-keyword "RECEIVE" 'space))


(defun chill-insert-region ()
  (interactive)
  (chill-insert-keyword "REGION" 'newline))


(defun chill-insert-returns ()
  (interactive)
  (insert "RETURNS")
  (chill-try-expand))


(defun chill-insert-struct ()
  (interactive)
  (chill-insert-keyword "STRUCT" nil 'PRESERVE))


(defun chill-try-expand-char ()
  (interactive "*")
  (let ((try-expand (and (=  (char-syntax (preceding-char)) ?w)
			 (/= (char-syntax (following-char)) ?w))))
    ;; if it is just at right of a word
    ;;   then try to expand
    (and try-expand (/= last-command-char ?\t) (chill-try-expand))
    (cond
     ;; SEMICOLON: insert semicolon and indent current line
     ((= last-command-char ?\;)
      (chill-electric-terminator nil))
     ;; NEWLINE: indent current line and insert newline
     ((= last-command-char ?\C-m)
      (chill-newline-and-indent))
     ;; TAB: (see `chill-indent-command' for docummentation)
     ((= last-command-char ?\t)
      (chill-indent-command))
     ;; <>: expand directive
     ((and (= last-command-char ?>)
	   (= (preceding-char) ?<))
      (delete-char -1)
      (chill-insert-directive))
     ;; if tried to expand and...
     ((and try-expand
	   (cond
	    ;; ...or cursor is between "" and
	    ;;       it was typed a space
	    ((= (preceding-char) ?\")
	     (and (= (following-char) ?\")
		  (memq last-command-char '(?\  ?\t))))
	    ;; ...or preceding char is open parenthesis and
	    ;;       it was typed a space or parenthesis
	    ((= (preceding-char) ?\()
	     (memq last-command-char '(?\  ?\t ?\( ?\))))
	    ;; ...or preceding char is close parenthesis and
	    ;;       it was typed a blank
	    ((= (preceding-char) ?\))
	     (= last-command-char ?\ ))))
      ;;   then do nothing
      nil)
     ;; otherwise, insert character typed
     (t
      (insert-and-inherit last-command-char)))))


(defun chill-insert-keyword (keyword &optional delimiter preserve-p)
  (let (start end word)
    (if (and (or transient-mark-mode current-prefix-arg)
	     mark-active)
	;; keyword on a region
	(progn
	  (setq start (region-beginning)
		end (chill-adjust-region-markers start (region-end)))
	  (if preserve-p
	      (progn
		(goto-char start)
		(insert keyword "\n")
		(backward-char 1)
		(setq start (point)))
	    (indent-to (current-indentation))
	    (insert keyword "\n")
	    (setq start (point))
	    (backward-char 1)))
      ;; keyword by itself
      (if (setq word (= (char-syntax (following-char)) ?w))
	  (progn
	    (insert keyword " ")
	    (setq end (point-marker))
	    (backward-char 1))
	(insert keyword)))
    ;; keyword expansion
    (chill-try-expand nil start end)
    (and word
	 (progn
	   (goto-char end)
	   (delete-char -1))))
  ;; keyword delimiter
  (cond
   ((eq delimiter 'space)
    (insert " "))
   ((eq delimiter 'newline)
    (chill-newline-and-indent))))


(defun chill-try-expand (&optional upper start end)
  (and chill-expand-templates
       (not (chill-in-literal))
       (let* ((save-point (point-marker))
	      (cw (chill-current-word))
	      (match (if (and cw upper) (upcase cw) cw))
	      (mapping (and match
			    (cdr (or (assoc match chill-special-alist)
				     (assoc match chill-implementation-alist)
				     )))))
	 (and mapping
	      (funcall mapping start end)
	      upper
	      (save-excursion
		(goto-char save-point)
		(if (= (char-syntax (preceding-char)) ?w)
		    (upcase-word -1))
		(if (= (char-syntax (following-char)) ?w)
		    (upcase-word 1)))))))


(defun chill-current-word ()
  "Return the word point is on (or a nearby word) as a string
*WITHOUT* text properties.
Return nil unless point is within or adjacent to a word."
  (and (or (memq (char-syntax (following-char)) '(?w ?_))
	   (memq (char-syntax (preceding-char)) '(?w ?_)))
       (buffer-substring-no-properties
	(save-excursion
	  (skip-syntax-backward "w_")
	  (point))
	(save-excursion
	  (skip-syntax-forward "w_")
	  (point)))))


(defun chill-check-spec ()
  (and chill-spec-file-p
       (error "Forbid on a specification file!!")))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapping functions


(defun chill-expand-indent-line (start end)
  (if (and start end)
      (chill-indent-region start end)
    (chill-indent-line 'NOERROR))
  t)


(defun chill-expand-directive (start end)
  (and (save-excursion
	 (and (re-search-backward "<>\\|[:;]" (chill-limit-backward) t)
	      (looking-at "<>")))
       (chill-indent-line 'NOERROR)))


(defun chill-expand-action (start end)
  (if chill-spec-file-p
      nil
    (save-excursion
      (let ((start-quick (chill-quick-beginning-of-word)))
	(and start end
	     (goto-char end))
	(chill-insert-semicolon)
	(chill-quick-help-region start-quick (point))))
    (chill-expand-indent-line start end)))


(defun chill-expand-argument-action (start end)
  (if chill-spec-file-p
      nil
    (let ((init (point))
	  (start-quick (chill-quick-beginning-of-word)))
      (chill-expand-struct start end)
      (or (eq (point) init)
	  (save-excursion
	    (and start end
		 (goto-char end))
	    (forward-char 1)
	    (chill-insert-semicolon)
	    (chill-quick-help-region start-quick (point)))))
    t))


(defun chill-expand-argument (start end)
  (if chill-spec-file-p
      nil
    (chill-expand-struct start end)))


(defun chill-expand-normal (start end)
  (not chill-spec-file-p))


(defun chill-expand-template (start end)
  t)


(defun chill-expand-template-and-indent (start end)
  (if chill-spec-file-p
      nil
    (chill-indent-line 'NOERROR)))


(defun chill-expand-template-do (start end)
  (if chill-spec-file-p
      nil
    (if (save-excursion (chill-DO-action-p))
	(save-excursion (chill-insert-semicolon)))
    (chill-indent-line 'NOERROR)))


(defun chill-insert-parenthesis ()
  (if (looking-at "[ \t]*(")
      nil
    (insert " ()")
    (backward-char 1)
    t))


(defun chill-expand-returns (start end)
  (let ((start-quick (chill-quick-beginning-of-word)))
    (and (chill-insert-parenthesis)
	 (chill-quick-help-region start-quick (1+ (point))))
    (chill-indent-line 'NOERROR)))


(defun chill-expand-begin (start end)
  (if chill-spec-file-p
      nil
    (chill-expand-block "END" start end)))


(defun chill-expand-proc (start end)
  (if (chill-in-declaration)
      (progn
	(chill-insert-parenthesis)
	(forward-char 1)
	nil)
    (and (not chill-spec-file-p)
	 (chill-expand-block "END" start end))
    (chill-insert-parenthesis)))


(defun chill-expand-case (start end)
  (and (chill-expand-block "ESAC" start end)
       (save-excursion
	 (insert " OF")
	 t)))


(defun chill-expand-context (start end)
  (chill-expand-block "FOR" start end 'NOSEMICOLON))


(defun chill-expand-do (start end)
  (if chill-spec-file-p
      nil
    (chill-expand-block "OD" start end)))


(defun chill-expand-if (start end)
  (and (not chill-spec-file-p)
       (not (looking-at "[ \t\n]+THEN"))
       (chill-expand-block "FI" start end)
       (save-excursion
	 (insert " THEN")
	 t)))


(defun chill-expand-else (start end)
  (and start end
       (chill-indent-line 'NOERROR)
       (chill-indent-rigidly-with-increment start end
					    (current-indentation)))
  t)


(defun chill-expand-elsif (start end)
  (if chill-spec-file-p
      nil
    (and (not (looking-at "[ \t\n]+THEN"))
	 (save-excursion
	   (insert " THEN")
	   (chill-indent-line 'NOERROR)))
    (chill-indent-rigidly-with-increment start end
					 (current-indentation))
    t))


(defun chill-expand-modulion (start end)
  (save-excursion
    ;; skip word MODULE or REGION
    (forward-word -1)
    (cond
     ;; check if word SPEC exists
     ((save-excursion
	(forward-word -1)
	(looking-at "\\<SPEC\\>"))
      )
     ;; if SPEC does not exist and
     ;;   is a specification file
     ;;   then insert it
     (chill-spec-file-p
      (insert "SPEC "))
     ;; ok, re-adjust cursor position
     (t
      (forward-word 1)))
    (chill-expand-block "END" start end)))


(defun chill-expand-on (start end)
  (if chill-spec-file-p
      nil
    (chill-quick-remove-property (- (or start (point)) 2)
				 (save-excursion (end-of-line) (point)))
    (chill-expand-block "END" start end)))


(defun chill-expand-receive (start end)
  (and (not chill-spec-file-p)
       (chill-expand-block "ESAC" start end)
       (progn
	 (insert " CASE")
	 t)))


(defun chill-expand-delay (start end)
  (cond (chill-spec-file-p
	 nil)
	((and start end)
	 (chill-expand-receive start end))
	(t
	 (chill-expand-template start end)
	 (save-excursion (chill-insert-semicolon)))))


(defun chill-expand-struct (start end)
  (if (and start end)
      ;; expansion on a region
      (let ((the-end (if (markerp end) end (copy-marker end))))
	(insert " (")
	(save-excursion
	  (and (eolp)
	       (progn
		 (delete-char 1)
		 (delete-horizontal-space)))
	  (chill-indent-region (point) the-end)
	  (goto-char the-end)
	  (skip-chars-backward "\f\n")
	  (insert ")")))
    ;; expansion by itself
    (chill-insert-parenthesis))
  t)


(defun chill-expand-timing (start end)
  (and (not chill-spec-file-p)
       (chill-expand-block "END" start end)
       (save-excursion
	 (insert " IN")
	 t)))


(defun chill-expand-code (start end)
  (if chill-spec-file-p
      nil
    (if (and start end)
	;; CODE or IN_LINE on a region
	(let ((the-end (if (markerp end) end (copy-marker end)))
	      prefix)
	  (insert " (\"")
	  (and (eolp)
	       (progn
		 (delete-char 1)
		 (delete-horizontal-space)))
	  (chill-indent-line 'NOERROR)
	  (setq prefix (buffer-substring (point)
					 (progn
					   (beginning-of-line)
					   (point))))
	  (while (progn
		   (end-of-line)
		   (insert "\");")
		   (forward-line 1)
		   (< (point) the-end))
	    (delete-horizontal-space)
	    (insert prefix)))
      ;; CODE or IN_LINE by itself
      (and (chill-insert-parenthesis)
	   (progn
	     (save-excursion
	       (forward-char 1)
	       (chill-insert-semicolon))
	     (insert "\"\"")
	     (backward-char 1)
	     (chill-indent-line 'NOERROR))))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjust some environment variables
;; (modify the settings in accordance with local CHILL environment)


(or (rassq 'chill-mode auto-mode-alist)
    (setq auto-mode-alist
	  (append '(("\\.chl$" . chill-mode)
		    ("\\.spc$" . chill-mode))
		  auto-mode-alist)
	  completion-ignored-extensions
	  (append '(".rel" ".sym")
		  completion-ignored-extensions)))


(provide 'chill-mode)


;;; chill-modee.el ends here
