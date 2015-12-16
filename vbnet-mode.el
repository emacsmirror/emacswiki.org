;;; vbnet-mode.el  --- A mode for editing Visual Basic .NET programs.
;;
;; Authors    : Fred White <fwhite@alum.mit.edu>
;;            : Dave Love <d.love@dl.ac.uk>
;;            : Kevin Whitefoot <kevin.whitefoot@nopow.abb.no>
;;            : T.K.Anderson
;;            : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : April 1996
;; Modified   : May 2011
;; Version    : 1.6
;; Keywords   : languages, basic, VB, VBNET
;; X-URL      : http://code.google.com/p/vbnetmode/
;; Last-saved : <2011-May-02 12:48:13>

;; Copyright (C) 1996 Fred White <fwhite@alum.mit.edu>
;; Copyright (C) 1998 Free Software Foundation, Inc.
;;   (additions by Dave Love and others)
;; Copyright (C) 2011 Dino Chiesa


;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.


;;; Commentary:

;; Purpose of this package: This is a mode for editing programs written
;;   in Visual Basic .NET (VB.NET).  This mode features automatic
;;   indentation of VB.NET syntax; font locking; automatic keyword
;;   capitalization; integration with compile.el, flymake.el, and
;;   imenu.el; built-in snippets for ya-snippet.el; and some minor
;;   convenience functions.

;; Installation instructions
;; --------------------------------
;;
;;  Put vbnet-mode.el somewhere in your load path, optionally byte-compile
;;  it, and add the following to your .emacs file:
;;
;;    (autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
;;    (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
;;                                  vbnet-mode)) auto-mode-alist))
;;
;;  Optionally, add a mode-hook function.  To do so, use something
;;  like this to your .emacs file:
;;
;;    (defun my-vbnet-mode-fn ()
;;      "My hook for VB.NET mode"
;;      (interactive)
;;      ;; This is an example only.
;;      ;; These statements are not required to use VB.NET, but
;;      ;; you might like them.
;;      (turn-on-font-lock)
;;      (turn-on-auto-revert-mode)
;;      (setq indent-tabs-mode nil)
;;      (require 'flymake)
;;      (flymake-mode 1)
;;      ...other mode-setup code here...
;;    )
;;    (add-hook 'vbnet-mode-hook 'my-vbnet-mode-fn)
;;
;;
;;  General
;;  ----------------------------
;;
;;  Mostly VB.NET mode will "just work."  Use `describe-mode' to see the
;;  default keybindings and the highlights of the mode.
;;
;;  Some points of interest:
;;
;;     `vbnet-mode-indent' - customizable variable setting the indent size,
;;          in spaces.  The default is 4.
;;
;;     `vbnet-mark-defun' marks the current function, if there is one.
;;
;;     `vbnet-split-line' splits the current line at point, and inserts a
;;          continuation character.
;;
;;     `vbnet-join-continued-lines' does the converse.
;;
;;     `vbnet-new-sub' - inserts a subroutine template into the buffer at
;;          point.
;;
;;     `vbnet-moveto-beginning-of-defun'
;;     `vbnet-moveto-end-of-defun'
;;     `vbnet-moveto-beginning-of-block'
;;     `vbnet-moveto-end-of-block'
;;          Functions to move within the VB.NET buffer.  The first two
;;          move to the beginning and end, respectively, of a Function
;;          or Sub.  The latter two move to the beginning and end,
;;          respectively, of the innermost containing block, whatever it
;;          is - a Function, Sub, Struct, Enum, While, etc.
;;
;;     `vbnet-close-current-block' - intelligently closes a block, For
;;          example, it inserts "End Class" when invoked if point is
;;          after a Class declaration.  This fn is naive: it
;;          will insert an "End Class" even if an "End Class" is present
;;          on the next line, and likewise for any other type of block.
;;
;;  Some of these functions are by default, bound to keystrokes when
;;  vbnet-mode loads.  Consult the documentation for each of these
;;  functions for more information.
;;
;;
;;  Fontification
;;  ----------------------------
;;
;;  There are a couple font-face name variables you can use to control
;;  the appearance of fontified vb.net buffers.  In
;;  `vbnet-namespace-face', store the name of the face for namespaces
;;  (from the Imports statement).  In `vbnet-funcall-face', store the
;;  name of the font to use for function calls.  There are also faces
;;  with the same names as these variables; they represent the default
;;  faces for these classes of code syntax.
;;
;;  You can set `vbnet-want-fontification' (via customize) to nil to
;;  skip the fontification.
;;
;;
;;  Flymake Integration
;;  ----------------------------
;;
;;  You can use flymake with vb.net mode to automatically check the
;;  syntax of your vb.net code, and highlight errors.  To do so, add a
;;  comment line like this to each .vb file that you use flymake with:
;;
;;   '  flymake-command: c:\.net3.5\vbc.exe /t:module /nologo
;;
;;  That lines specifies a command "stub".  Flymake appends the name of
;;  the file to compile, and then runs the command to check
;;  syntax.  Flymake assumes that syntax errors will be noted in the
;;  output of the command in a form that fits one of the regexs in the
;;  `compilation-error-regexp-alist-alist'.  Check the flymake module for
;;  more information on that.
;;
;;  Some rules for the command:
;;
;;    1. it must appear all on a single line.
;;
;;    2. vbnet-mode generally looks for the marker line in the first N
;;       lines of the file, where N is set in
;;       `vbnet-cmd-line-limit'.  See the documentation on that
;;       variable for more information.
;;
;;    3. the command SHOULD NOT include the name of the source file
;;       currently being edited.  This is because flymake saves a copy of
;;       the buffer into a temporary file with a unique name, and then
;;       compiles that temporary file.  The name of the temp file is
;;       automatically appended to the end of the command .
;;
;;  If you use vbc.exe as the syntax check tool (as almost everyone
;;  will), the /t:module is important.  vbnet-mode assumes that the
;;  syntax-check compile command will produce a file named
;;  NAME.netmodule, which is the default when using /t:module.  (Remember
;;  than NAME is dynamically generated).  vbnet-mode will remove the
;;  generated netmodule file after the syntax check is complete.  If you
;;  don't specify /t:module, then vbnet-mode won't know what file to
;;  delete.
;;
;;  vbnet-mode also fiddles with some other flymake things.  In
;;  particular it: adds .vb to the flymake "allowed filename masks"; adds
;;  parsing for vbc error messages; redefines the process sentinel to
;;  not treat non-zero exit status as an error (because vbc.exe returns
;;  non-zero status when syntax errors are present); and adds advice to
;;  the error parsing logic.  This all should be pretty benign for all other
;;  flymake buffers.  But it might not be.
;;
;;  You can explicitly turn the flymake integration for VB.NET off by
;;  setting `vbnet-want-flymake-fixup' to nil.
;;
;;
;;  Imenu integration
;;  ----------------------------
;;
;;  imenu provides the capability to scan a buffer to produce an "index"
;;  of the highlights or points of interest in that buffer, and then
;;  present that index as a popup menu (for example on the menubar) or
;;  completion buffer.
;;
;;  vbnet-mode integrates with imenu to produce the index for a vb.net
;;  buffer.  Automatically, there will be an "index" menu item in the
;;  menubar when you open a VB.NET buffer.  It will contain menu items
;;  corresponding to namespaces, classes, methods, functions and
;;  properties defined in that source module.
;;
;;  To turn off the vbnet+imenu integration, set vbnet-want-imenu to
;;  nil, in your vbnet mode hook function.
;;
;;  Compile Integration
;;  ----------------------------
;;
;;  vbnet-mode binds the function `vbnet-invoke-compile-interactively'
;;  to "\C-x\C-e" .  This function attempts to intellgently guess the
;;  format of the compile command to use for a buffer.  It looks in the
;;  comments at the head of the buffer for a line that begins with
;;  compile: .  If found, vbnet-mode suggests the text that follows as
;;  the compilation command when running `compile' .  If such a line is
;;  not found, vbnet-mode falls back to a msbuild or nmake command.
;;  See the documentation on `vbnet-cmd-line-limit' for further
;;  information.
;;
;;  vbnet-mode now installs an error regexp for vbc.exe into
;;  `compilation-error-regexp-alist-alist', which allows `next-error'
;;  and `previous-error' (defined in compile.el) to navigate to the next
;;  and previous compile errors in the vb buffer.
;;
;;
;;  YASnippet integration
;;  -----------------------------
;;
;;  vbnet-mode (as of v1.5b) defines some built-in snippets for
;;  convenience.  For example, if statements, ifelse, for, foreach, and
;;  so on.  You can see them on the YASnippet menu that is displayed
;;  when a vbnet-mode buffer is opened.  vbnet-mode defines this
;;  snippets happens only if ya-snippet is available.  (It is done in an
;;  `eval-after-load' clause.)  The builtin snippets will not overwrite
;;  snippets that use the same name, if they are defined in the normal
;;  way (in a compiled bundle) with ya-snippet.
;;
;;  You can explicitly turn off ya-snippet integration.  See the var,
;;  `vbnet-want-yasnippet-fixup'.
;;




;; Revisions:
;; 1.0 18-Apr-96  Initial version
;;
;; 1.1 Accomodate emacs 19.29+ font-lock-defaults
;;     Simon Marshall <Simon.Marshall@esrin.esa.it>
;;
;; 1.2 Rename to visual-basic-mode
;;
;; 1.3 Fix some indentation bugs.
;;
;; 1.3+ Changes by Dave Love: [No attempt at compatibility with
;;      anything other than Emacs 20, sorry, but little attempt to
;;      sanitize for Emacs 20 specifically.]
;;      Change `_' syntax only for font-lock and imenu, not generally;
;;      provide levels of font-locking in the current fashion;
;;      font-lock case-insensitively; use regexp-opt with the font-lock
;;      keywords; imenu support; `visual-basic-split-line', bound to
;;      C-M-j; account for single-statement `if' in indentation; add
;;      keyword "Global"; use local-write-file-hooks, not
;;      write-file-hooks.
;;
;; 1.4 September 1998
;;
;; 1.4 KJW Add begin..end, add extra keywords
;;     Add customisation for single line if.  Disallow by default.
;;     Fix if regexp to require whitespace after if and require then.
;;     Add more VB keywords.  Make begin..end work as if..endif so
;;     that forms are formatted correctly.
;;
;; 1.4.1 KJW Merged Dave Love and KJW versions.
;;     Added keywords suggested by Mickey Ferguson
;;     <MFerguson@peinc.com>
;;     Fixed imenu variable to find private variables and enums
;;
;;     Changed syntax class of =, <, > to punctuation to allow dynamic
;;     abbreviations to pick up only the word at point rather than the
;;     whole expression.
;;
;;     Fixed bug introduced by KJW adding suport for begin...end in
;;     forms whereby a single end outdented.
;;
;;     Partially fixed failure to recognise if statements with
;;     continuations (still fails on 'single line' if with
;;     continuation, ugh).
;;
;; 1.5 DPC changes February 2011
;;     Moved the `provide' statement to conventional spot, end of file.
;;
;;     Modified various `defvar' statements to be `defcustom',
;;     to allow these settings to be customized interactively.
;;
;;     Changed modeline label to the more concise "VB.NET", instead of
;;     "Visual Basic .NET"
;;
;;     Tweaked the `vbnet-continuation-regexp' slightly nto be more
;;     correct.  It needed a space before the continuation char.
;;
;;     Fixed `vbnet-defun-start-regexp' to handle Public Shared
;;     Functions.  Also renamed it to `vbnet-block-start-regexp' to
;;     reflect its true meaning, and renamed `vbnet-defun-end-regexp'
;;     similarly.  Actually, those names are irrelevant, because I put
;;     all the defconst regexps into an alist for simpler access.
;;
;;     Added "Namespace" as a keyword, and added a regexp and fn for
;;     handling namespace statements.  Also modified `vbnet-calculate-indent'
;;     to properly handle namespaces and their children.
;;
;;     Enhanced the logic for fontifying, with changes to
;;     `vbnet-font-lock-keywords-1', so that things like variables,
;;     constructor invocation, import declarations, and using statements
;;     get fontified.
;;
;;     Removed keyword fontification of String, Trim, etc.  In VB.NET,
;;     these are no longer keywords.  The whole list of keywords needs a
;;     thorough going-over.  I think it is no longer necessary with
;;     VB.NET, which has many fewer keywords.
;;
;;     Implemented indenting of VB.NET properties, getters and setters.
;;
;;     Integration with compile.el . VBnet-mode now installs an error
;;     regexp for vbc.exe into `compilation-error-regexp-alist-alist',
;;     which allows next-error to navigate to the next compile error in
;;     the vb buffer.
;;
;;     Integration with ya-snippet.el . Defines some built-in
;;     snippets for convenience.  This works only if ya-snippet is available.
;;     The builtin snippets will not overwrite snippets defined
;;     in the normal way with ya-snippet (in a compiled bundle).
;;     See also the var, `vbnet-want-yasnippet-fixup'.
;;
;;     Integration with flymake.el .  Tweaks some defuns
;;     and vars to allow flymake to work with VB.NET.  This happens only
;;     if flymake is in use.  See also the var, `vbnet-want-flymake-fixup'.
;;
;;     Removed the find-matching-* fns, they were simple and called
;;     from only one place, so added nothing.
;;
;;     New function, `vbnet-join-continued-lines', a companion to
;;     `vbnet-split-line' Also, fixed the latter to work in an edge
;;     case.
;;
;; 1.5a DPC changes February 2011
;;
;;      Added the feature where vbnet-mode scans the buffer for a line that
;;      specifies the flymake command to use.  For example, specify
;;      flymake-command: c:\.net3.5\vbc.exe /t:module /nologo
;;      in the comment at the top of the file, to tell flymake to use
;;      that command.  For info, see the var `vbnet-cmd-line-limit'.
;;
;; 1.5b DPC changes February 2011
;;
;;      Fixed bug with missing fn vbnet-find-matching-try .
;;      I missed that one during the prior refactoring.
;;
;; 1.5c DPC changes April 2011
;;
;;      Fixed imenu integration.  The imenu stuff was for old-syntax VB6,
;;      and did not handle namespaces, classes, structures, interfaces, and
;;      so on.  Rather than using `imenu-generic-expression', the integration
;;      for VB.NET uses `imenu-create-index-function'.
;;
;;      Corrected the documentation and behavior of the flymake
;;      integration.  In practice, vbnet-mode now uses the name of the
;;      temporary file, as the thing to compile.
;;
;;      Corrected indentation to handle .net attributes (eg,
;;      <DllImport>) which precede a line and use a
;;      line-continuation.  To do this, I modified
;;      `vbnet--back-to-start-of-continued-statement', which was
;;      previouisly named `vbnet-find-original-statement', to accept an
;;      optional arg telling it to NOT backup over attributes.
;;
;;      Fixed indenting for functions where the decl line is "continued", even
;;      without a preceding attribute.
;;      Example:
;;        Public Shared Function _
;;               SetForegroundWindow(ByVal handle As IntPtr) As Boolean
;;
;;      Added indentation support for classes marked
;;      NotInheritable.  Previously this keyword caused indentation to
;;      break.  Likewise for Friend functions.
;;
;;      New interactive fn, `vbnet-close-current-block', to insert the
;;      "End Xxxx" statement to close the current block.
;;
;;      New interactive fns, `vbnet-moveto-beginning-of-defun' and
;;      `vbnet-moveto-end-of-defun', that move to the beginning or end of the
;;      current Sub or Function.
;;
;;      Implement indentation and fontification for Structures and Enums.
;;
;;      Properly fontify reserved words used as variable names.  (in square
;;      brackets)
;;
;;      Updated comments on usage.
;;


;; Notes by Dave Love
;; BTW, here's a script for making tags tables that I (Dave Love) have
;; used with reasonable success.  It assumes a hacked version of etags
;; with support for case-folded regexps.  I think this is now in the
;; development version at <URL:ftp://fly.cnuce.cnr.it/pub/> and should
;; make it into Emacs after 20.4.
;;
;; #! /bin/sh
;;
;; # etags-vb: (so-called) Visual (so-called) Basic TAGS generation.
;; # Dave Love <d.love@dl.ac.uk>.  Public domain.
;; # 1997-11-21
;;
;; if [ $# -lt 1 ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options] " 1>&2
;;     exit 1
;; fi
;;
;; if [ $1 = "--help" ] || [ $1 = "-h" ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options]
;;
;; "
;;     etags --help
;; fi
;;
;; exec etags --lang=none -c '/\(global\|public\)[ \t]+\(\(const\|type\)[ \t]+\)*\([a-z_0-9]+\)/\4/' \
;;     -c '/public[ \t]+\(sub\|function\)[ \t]+\([a-z_0-9]+\)/\2/' \
;;   "$@"
;;
;; End Notes Dave Love


;; Known bugs:
;;
;;  - Doesn't know about ":" separated stmts.
;;
;;  - Doesn't recognize single line if statements if these are broken by
;;    line continuation characters.  (not sure what this means; if I did
;;    I would try to fix it.  -DPC 2011/Feb/26)
;;


;; Todo?:
;;
;;  - Handle interfaces.  how did I leave this out?
;;    need a regexp, and to handle in the imenu helper, as well
;;    as in the indentation logic.
;;
;;  - additional flymake work: allow the user to specify the command to
;;    run, through a buffer-local variable - eg, without requiring a
;;    comment in the source code.
;;
;;  - Smart completion over object fields, methods, and properties.  This
;;    would require a sourcecode analysis engine, something like
;;    NRefactory.
;;
;;  - IDE integration - not sure what this might mean.  Need suggestions
;;  - here.
;;
;;  - Change behaviour of ESC-q to recognise words used as paragraph
;;    titles and prevent them being dragged into the previous
;;    paragraph.(?)
;;
;;  - others?
;;

;;; Code:

(defvar vbnet-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar vbnet-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar vbnet-win32-p (eq window-system 'w32))
(defvar vbnet--yasnippet-has-been-fixed nil)

;; Variables you may want to customize.
(defcustom vbnet-mode-indent 4
  "*Default indentation per nesting level."
    :type 'integer :group 'vbnet)

(defcustom vbnet-want-fontification t
  "*Whether to fontify VB.NET buffers."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-want-imenu t
  "*Whether to generate a buffer index via imenu for VB.NET buffers."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-want-yasnippet-fixup t
  "*Whether to enable the builtin snippets for ya-snippet.
This is meaningful only if ya-snippet is available."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-want-flymake-fixup t
  "*Whether to enable the builtin support for flymake.
This is meaningful only if flymake is loaded."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-capitalize-keywords-p t
  "*Whether to capitalize BASIC keywords."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-wild-files (list "*.vb" "*.frm" "*.bas" "*.cls")
  "*List of Wildcard patterns for BASIC source files."
  :type 'list :group 'vbnet)

(defcustom vbnet-ide-pathname nil
  "*The full pathname of your Visual Basic exe file, if any."
  :type 'string :group 'vbnet)

;; KJW Provide for my preference in if statements
(defcustom vbnet-allow-single-line-if nil
  "*Whether to allow single line if."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-cmd-line-limit 18
  "The number of lines at the top of the file to look in, to find
the command that vbnet-mode will use to compile the current
buffer, or the command \"stub\" that vbnet-mode will use to
check the syntax of the current buffer via flymake.

If the value of this variable is zero, then vbnet-mode looks
everywhere in the file.  If the value is positive, then only in
the first N lines. If negative, then only in the final N lines.

The line should appear in a comment inside the C# buffer.


Compile
--------

In the case of compile, the compile command must be prefixed with
\"compile:\".  For example,

 // compile: csc.exe /r:Hallo.dll Arfie.cs


This command will be suggested as the compile command when the
user invokes `compile' for the first time.


Flymake
--------

In the case of flymake, the command \"stub\" string must be
prefixed with \"flymake-command:\".  For example,

  // flymake-command: DOTNETDIR\csc.exe /target:netmodule /r:foo.dll

In the case of flymake-command, the string should NOT
include the name of the file for the buffer being checked.
vbnet-mode appends the name of the source file to compile, to
this command \"stub\" before passing the command to flymake to
run it.

If for some reason the command is invalid or illegal, flymake
will report an error and disable itself.


In all cases
------------

Be sure to specify the proper path for your csc.exe, whatever
version that might be, or no path if you want to use the system
PATH search.

If the buffer depends on external libraries, then you will want
to include /R arguments to that csc.exe command.

To be clear, this variable sets the number of lines to search for
the command.  This cariable is an integer.

If the marker string (either \"compile:\" or \"flymake-command:\"
is present in the given set of lines, vbnet-mode will take
anything after the marker string as the command to run."
  :type 'integer   :group 'vbnet)


;; Fri, 01 Apr 2011
;; DPC
;; This is the kind of thing that is better done by a general-purpose
;; snippet package, customized to vbnet, and customized further for
;; the particular VBNET user.  ya-snippet.el is a good one.
;;
;; But I'm leaving this code in, as a legacy.
;;
(defvar vbnet-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
        "Public Function () As Variant\nEnd Function\n\n"
        "Public Property ()\nEnd Property\n\n")
  "*List of function templates though which ‘vbnet-new-sub’ cycles.")



;; ==================================================================
;;; imenu stuff

;; (defun vbnet-imenu-create-index-function-fake ()
;;   "producees a fake index for imenu. See the documentation for
;; `vbnet-imenu-create-index-function' for more information.
;;
;; "
;;   ;; example:
;;   ;;
;;   ;;  (("New" . #<marker at 589 in Rijndael-vb.vb>)
;;   ;;   ("New" . #<marker at 678 in Rijndael-vb.vb>)
;;   ;;   ("GetRijndaelManaged" . #<marker at 765 in Rijndael-vb.vb>)
;;   ;;   ("Run" . #<marker at 1282 in Rijndael-vb.vb>)
;;   ;;   ("Encrypt" . #<marker at 2381 in Rijndael-vb.vb>)
;;   ;;   ("Decrypt" . #<marker at 3384 in Rijndael-vb.vb>))
;;
;;
;;   '(("Somewhere in the header comment"  . 20)
;;     ("Imports"  . 375)
;;     ("Namespace Ionic.Tests.Crypto" . 447)
;;     ("Class RijndaelVb - a submenu"
;;      ("ctor"  . 597)
;;      ("A function..."  . 1282)
;;      ("etc..." . 3222))))



;; define some advice for menu construction.

;; The way imenu constructs menus from the index alist, in
;; `imenu--split-menu', is ... ah ... perplexing.  If the vbnet
;; create-index fn returns an ordered menu, and the imenu "sort" fn has
;; been set to nil, imenu still sorts the menu, according to the rule
;; that all submenus must appear at the top of any menu. Why?  I don't
;; know. This advice disables that weirdness in VB buffers.

(defadvice imenu--split-menu (around
                              vbnet--imenu-split-menu-patch
                              activate compile)
  ;; This advice will run in all buffers.  Let's may sure we
  ;; actually execute the important bits only when a VB buffer is active.
  (if (and (string-match "\\.[Vv][Bb]$"  (file-relative-name buffer-file-name))
           (boundp 'vbnet-want-imenu)
           vbnet-want-imenu)
      (let ((menulist (copy-sequence menulist))
            keep-at-top)
        (if (memq imenu--rescan-item menulist)
            (setq keep-at-top (list imenu--rescan-item)
                  menulist (delq imenu--rescan-item menulist)))
        ;; This is the part from the original imenu code
        ;; that puts submenus at the top.  huh?
        ;; --------------------------------------------
        ;; (setq tail menulist)
        ;; (dolist (item tail)
        ;;   (when (imenu--subalist-p item)
        ;;     (push item keep-at-top)
        ;;     (setq menulist (delq item menulist))))
        (if imenu-sort-function
            (setq menulist (sort menulist imenu-sort-function)))
        (if (> (length menulist) imenu-max-items)
            (setq menulist
                  (mapcar
                   (lambda (menu)
                     (cons (format "From: %s" (caar menu)) menu))
                   (imenu--split menulist imenu-max-items))))
        (setq ad-return-value
              (cons title
                    (nconc (nreverse keep-at-top) menulist))))
    ;; else
    ad-do-it))





(defun vbnet--imenu-create-index-function-helper (&optional parent-ns indent-level)
  "Helper fn for `vbnet-imenu-create-index-function-real'.

Scans for a namespace, then scans within the namespace for subs
and functions. Returns a list, suitable for use as an imenu index
alist. Leaves point after the \"End Namespace\", if it exists."

  ;; A VB.NET module consists of zero of more explicitly denoted (and
  ;; possibly nested) namespaces. (in the absence of an
  ;; explicitly-denoted namespace, the global namespace is implicitly
  ;; applied).  Within each namespace there can be zero or more
  ;; "container" things - like class, struct, or interface; each with
  ;; zero or more indexable items - like Functions, Subs, Properties,
  ;; and so on.

  ;; This fn parses the module and indexes those items, creating a
  ;; hierarchically organized list to describe them.  Each container
  ;; (ns/class/struct/etc) is represented on a separate submenu.

  ;; It works like this:
  ;;
  ;; 1. Examine each line
  ;;
  ;; 2. does the line start a container?
  ;;
  ;;    a. are we not not *in* a container? create a menu item for the
  ;;       top of the container, add to the submenu and continue
  ;;       parsing.
  ;;
  ;;    b. if already in a container, backup to start-of-line and recurse.
  ;;
  ;; 3. end of container?
  ;;
  ;;    a. are we in a container?
  ;;       add a "bottom" item to the current submenu and then add the
  ;;       submenu to the master menu.  Clear the submenu, to ready to
  ;;       find another start container. go to step 6.
  ;;
  ;;    b. not in a container, then pop out and let the parent fn call
  ;;       handle this.
  ;;
  ;; 4. Is it an indexable item?  and
  ;;    are we in a container?
  ;;    add the item to the submenu, and go to step 6.
  ;;
  ;; 5. otherwise - it is a legal line of code and we are NOT in a container.
  ;;    In this case pop out of the (presumably nested) fn call, and let
  ;;    the caller handle it.
  ;;
  ;; 6. if not popping out, go to the next line in the buffer, and
  ;;    resume at step 1.
  ;;


  (if (not indent-level) (setq indent-level ""))

  (let ((state 0)
        done
        menu-structure
        submenu
        suppress-next
        this-flavor
        this-item
        container-name
        (item-regex-tuples
         '((func-start func-end)
           (sub-start sub-end)
           (propset-start propset-end)
           (propget-start propget-end))))

    (while (not done)

      (if (eobp) (setq done t)
        (setq suppress-next nil)
        (cond
         ;; handle open-container blocks
         ((or
           (looking-at (vbnet-regexp 'class-start))
           (looking-at (vbnet-regexp 'intf-start))
           (looking-at (vbnet-regexp 'struct-start))
           (looking-at (vbnet-regexp 'enum-start))
           (looking-at (vbnet-regexp 'namespace-start)))

          (if (string= (downcase (match-string-no-properties 1)) "namespace")
              (setq this-flavor (match-string-no-properties 1)
                    this-item (match-string-no-properties 2))
            (setq this-flavor (match-string-no-properties 2)
                  this-item (match-string-no-properties 3)))

          (cond
           ((eq state 0)
            ;; we've been waiting for an open-container
            (incf state)

            ;; produce a fully-qualified name for this thing
            (setq container-name
                  (if parent-ns
                      (concat parent-ns "." this-item)
                    this-item))

            ;; add the flavor + name of the thing to the menu. This
            ;; will act as the submenu heading.
            (push (concat this-flavor " " container-name) submenu)

            ;; add an item to the submenu pointing to the top of the container
            ;; (namespace, class, etc)
            (push (cons "(top)"
                        (let ((m (make-marker)))
                          (set-marker m (match-beginning 1)))) submenu))

           ((eq state 1)
            ;; inside a container, and another one is being opened.
            ;; therefore, recurse.
            ;;(goto-char (match-beginning 0))
            (let ((child-menu
                   (vbnet--imenu-create-index-function-helper container-name
                                                              (concat indent-level "  "))))
              ;; there may be multiple children; add them all
              (if child-menu
                  (mapcar
                   '(lambda (item)
                      (push item submenu))
                   child-menu))
              (setq suppress-next t)))))


         ;; handle the container end
         ((or
           (looking-at (vbnet-regexp 'class-end))
           (looking-at (vbnet-regexp 'intf-end))
           (looking-at (vbnet-regexp 'struct-end))
           (looking-at (vbnet-regexp 'enum-end))
           (looking-at (vbnet-regexp 'namespace-end)))

          (cond
           ((eq state 1)
            (decf state)
            (push (cons "(bottom)"
                        (let ((m (make-marker)))
                          (set-marker m (match-end 0)))) submenu)
            (push (nreverse submenu) menu-structure)
            ;; prepare to start again:
            (setq submenu nil))

           ((eq state 0)
            ;; close of parent container.
            (setq done t))))


         ;; handle indexable items within the container (class/enum/struct/etc)
         ((eq state 1)  ;; we're inside a container
          (let (found)
            (dolist (pair item-regex-tuples)
              (if (and (not found)
                       (looking-at (vbnet-regexp (car pair))))
                  ;; capture 1 - protection modifier
                  ;; capture 2 - type of thing (Sub, Function, etc)
                  ;; capture 3 - name of the thing
                  (progn
                    (setq found t)
                    (push (cons
                           (concat
                            (match-string-no-properties 3)
                            " ("
                            (downcase (substring (match-string-no-properties 2) 0 1))
                            ")")
                           (let ((m (make-marker)))
                             (set-marker m (match-beginning 1)))) submenu)
                    ;; advance to the corresponding end of the indexable thing
                    (re-search-forward (vbnet-regexp (cadr pair)) nil t))))))

         (t
          ;; not done yet...
          (setq done nil))))

      (if (and (not done)
               (not suppress-next))
          (vbnet-next-line-of-code)))

    (nreverse menu-structure)))





(defun vbnet-imenu-create-index-function ()
  "a function called by imenu to create an index for the current
VB.NET buffer, conforming to the format specified in
`imenu--index-alist' .  To produce the index, which lists the
classes, functions, methods, and properties for the current
buffer, this function scans the entire buffer.

imenu calls this fn only when the buffer has been updated.

See `imenu-create-index-function' for more information."
  (save-excursion
    (save-restriction
      (widen)
      ;; start at the top
      (goto-char (point-min))
      (vbnet-next-line-of-code)
      (let ((index-alist
             (vbnet--imenu-create-index-function-helper)))

        ;; If the index menu contains exactly one element, and it is a
        ;; namespace menu, then remove it.  This simplifies the menu,
        ;; and results in no loss of information: all types get
        ;; fully-qualified names anyway. This may cover the majority of
        ;; cases, because often a VB source module defines either one class, or
        ;; a set of related classes inside a single namespace.

        ;; To remove that namespace, we need to prune & graft the tree.
        ;; Remove the ns hierarchy level, but also remove the 1st and
        ;; last elements in the sub-menu, which represent the top and
        ;; bottom of the namespace.

        (if (and
             (= 1 (length index-alist))
             (consp (car index-alist))
             (let ((tokens (split-string
                            (car (car index-alist))
                            "[ \t]" t)))
               (and (<= 1 (length tokens))
                    (string= (downcase
                              (nth 0 tokens)) "namespace"))))
            (nreverse (cdr (nreverse (cddar index-alist))))
          index-alist)))))


;; ==================================================================




(defvar vbnet-mode-syntax-table nil)
(if vbnet-mode-syntax-table
    ()
  (setq vbnet-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" vbnet-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" vbnet-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" vbnet-mode-syntax-table)
  (modify-syntax-entry ?\= "." vbnet-mode-syntax-table)
  (modify-syntax-entry ?\< "." vbnet-mode-syntax-table)
  ; Make =, etc., punctuation so that dynamic abbreviations work properly
  (modify-syntax-entry ?\> "." vbnet-mode-syntax-table))


(defvar vbnet-mode-map nil)

(if vbnet-mode-map ()
  (setq vbnet-mode-map (make-sparse-keymap))
  (define-key vbnet-mode-map "\t" 'vbnet-indent-line)
  (define-key vbnet-mode-map "\r" 'vbnet-newline-and-indent)

  ;;It is bound to C->, <C-M-end>, C-M-e, ESC <C-end>.
  ;;It is bound to C-<, <C-M-home>, C-M-a, ESC <C-home>.

  (define-key vbnet-mode-map "\M-\C-a"            'vbnet-moveto-beginning-of-defun)
  (define-key vbnet-mode-map (kbd "ESC <C-home>") 'vbnet-moveto-beginning-of-defun)
  (define-key vbnet-mode-map (kbd "C-<")          'vbnet-moveto-beginning-of-defun)

  (define-key vbnet-mode-map "\M-\C-e"            'vbnet-moveto-end-of-defun)
  (define-key vbnet-mode-map (kbd "ESC <C-end>")  'vbnet-moveto-end-of-defun)
  (define-key vbnet-mode-map (kbd "C->")          'vbnet-moveto-end-of-defun)

  ;;(define-key vbnet-mode-map "\M-\C-a" 'vbnet-moveto-beginning-of-defun)
  ;; (define-key vbnet-mode-map "\M-\C-e" 'vbnet-moveto-end-of-defun)
  ;; (define-key vbnet-mode-map "\M-\C-end" 'vbnet-moveto-end-of-defun)
  ;; (define-key vbnet-mode-map "\C->" 'vbnet-moveto-end-of-defun)


  (define-key vbnet-mode-map "\M-\C-h" 'vbnet-mark-defun)
  (define-key vbnet-mode-map "\M-\C-\\" 'vbnet-indent-region)
  (define-key vbnet-mode-map "\C-c/" 'vbnet-close-current-block)
  (define-key vbnet-mode-map "\M-q" 'vbnet-fill-or-indent)
  (define-key vbnet-mode-map "\M-\C-j" 'vbnet-split-line)
  (define-key vbnet-mode-map (kbd "M-RET") 'vbnet-split-line)
  (define-key vbnet-mode-map (kbd "ESC <C-return>") 'vbnet-join-continued-lines)
  (cond (vbnet-winemacs-p
         (define-key vbnet-mode-map '(control C) 'vbnet-start-ide))
        (vbnet-win32-p
         (define-key vbnet-mode-map (read "[?\\S-\\C-c]") 'vbnet-start-ide)))
  (if vbnet-xemacs-p
      (progn
        (define-key vbnet-mode-map "\M-G" 'vbnet-grep)
        (define-key vbnet-mode-map '(meta backspace) 'backward-kill-word)
        (define-key vbnet-mode-map '(control meta /) 'vbnet-new-sub))))


;; These abbrevs are valid only in a code context.
(defvar vbnet-mode-abbrev-table nil)

(defvar vbnet-mode-hook ())


;; Is there a way to case-fold all regexp matches?
;; Change KJW Add enum, , change matching from 0 or more to zero or one for public etc.


;; Includes the compile-time #if variation.
;; KJW fixed if to require a whitespace so as to avoid matching, for
;; instance, iFileName and to require then.

;; Two versions; one recognizes single line if just as though it were
;; a multi-line and the other does not.  Modified again to remove the
;; requirement for 'Then' so as to allow it to match if statements that
;; have continuations.
;;(defconst vbnet-if-regexp
;;   "^[ \t]*#?[Ii]f[ \t]+.*[ \t]+[Tt]hen[ \t]*.*\\('\\|$\\)")



;; alist of regexps for various structures in a vbnet file.
(eval-and-compile
  (defconst vbnet-regexp-alist
    (list

     ;; These elements in the list are used for fontification as well as
     ;; indenting. The former is delegated to font-lock.el, while the
     ;; latter is done directly by this mode.

     `(block-start  ;; general-purpose block start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Ff]riend\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Ss]tatic\\)"
         "[ \t]+"
         "\\([Ss]ub\\|"
         "[Ff]unction\\|"
         "[Ss]tructure\\|"
         "[Pp]roperty\\|"
         "[Ii]nterface\\|"
         "[Tt]ype\\|"
         "[Ee]num\\|"
         "[Cc]lass\\|"
         "[Mm]odule\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of thing
         "[ \t]*"
         "\(?"))  ;; optional open-paren

     `(block-end
       ,(concat
         "^[ \t]*[Ee]nd "
         "\\("
         "[Ss]ub\\|"
         "[Ff]unction\\|"
         "[Ss]tructure\\|"
         "[Pp]roperty\\|"
         "[Ii]nterface\\|"
         "[Tt]ype\\|"
         "[Ee]num\\|"
         "[Cc]lass\\|"
         "[Mm]odule\\)"))

     `(block-flavor
       ,(concat
         "\\b\\("
         "[Ss]ub\\|"
         "[Ff]unction\\|"
         "[Pp]roperty\\|"
         "[Ii]nterface\\|"
         "[Tt]ype\\|"
         "[Ee]num\\|"
         "[Cc]lass\\|"
         "[Mm]odule\\)\\b"))

     `(intf-start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\|"
         "[Ff]riend\\(?: [Ss]hared\\)?\\|"
         "[Ss]tatic\\)"
         "[ \t]+"
         "\\([Ii]nterface\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of interface
         "[ \t]*"
         "\(?"))  ;; open-paren

     '(intf-end      "^[ \t]*[Ee]nd +[Ii]nterface")

     `(func-start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\|"
         "[Ff]riend\\(?: [Ss]hared\\)?\\|"
         "[Ss]tatic\\)"
         "[ \t]+"
         "\\([Ff]unction\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of func
         "[ \t]*"
         "\(?"))  ;; open-paren

     '(func-end      "^[ \t]*[Ee]nd +[Ff]unction")

     `(sub-start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\|"
         "[Ss]tatic\\|"
         "[Ff]riend\\)"
         "[ \t]+"
         "\\([Ss]ub\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of sub
         "[ \t]*"
         "\(?"))  ;; optional open-paren

     '(sub-end      "^[ \t]*[Ee]nd +[Ss]ub")

     `(prop-start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\(?: [Ss]hared\\)?[ \t]+\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?[ \t]+\\|"
         "\\)"                                   ;; no qualifier at all
         "\\([Pp]roperty\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of prop
         ))

     '(prop-end      "^[ \t]*[Ee]nd +[Pp]roperty")

     `(class-start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\b\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Pp]rivate\\b\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Ss]tatic\\b\\|"
         "\\)"
         "[ \t]*"
         "\\([Cc]lass\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of class
         "[ \t]*"))  ;; optional ws

     '(class-end      "^[ \t]*[Ee]nd +[Cc]lass")

     `(struct-start
       ,(concat
         "^[ \t]*"
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Ff]riend\\(?: [Ss]hared\\)?\\(?: [Nn]ot[Ii]nheritable\\)?\\|"
         "[Ss]tatic\\)"
         "[ \t]+"
         "\\([Ss]tructure\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of struct
         "[ \t]*"))  ;; optional ws

     '(struct-end      "^[ \t]*[Ee]nd +[Ss]tructure")

     `(enum-start
       ,(concat
         "^[ \t]*"
         "\\([Pp]ublic\\|"
         "[Pp]rivate\\|"
         "[Ff]riend\\)"
         "[ \t]+"
         "\\([Ee]num\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of enum
         "\\(?:\\([ \t]+[Aa]s\\)\\([ \t]+[^ \t\(\n]+\\)\\)?" ;; optional base type
         "[ \t]*"))  ;; optional trailing ws

     '(enum-end      "^[ \t]*[Ee]nd +[Ee]num")

     `(namespace-start
       ,(concat
         "^[ \t]*"
         "\\([Nn]amespace\\)"
         "[ \t]+"
         "\\([^ \t\(\n]+\\)" ;; name of ns
         "[ \t]*"))

     '(namespace-end   "^[ \t]*[Ee]nd[ \t]+[Nn]amespace\\b")

     '(if              "^[ \t]*#?\\([Ii]f\\)[ \t]+.*[ \t_]+")
     '(ifthen          "^[ \t]*#?\\([Ii]f\\)\\b.+\\<[Tt]hen\\>\\s-\\S-+")
     '(else            "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
     '(endif           "[ \t]*#?[Ee]nd[ \t]*[Ii]f")
     '(end-of-attr-and-continuation    "^.*>[ \t]+_[ \t]*$")
     '(continuation    "^.* _[ \t]*$")
     '(label           "^[ \t]*[a-zA-Z0-9_]+:$")
     '(select          "^[ \t]*\\([Ss]elect\\)[ \t]+[Cc]ase")
     '(case            "^[ \t]*[Cc]ase")
     '(select-end      "^[ \t]*[Ee]nd[ \t]+[Ss]elect")
     '(for             "^[ \t]*[Ff]or\\b")
     '(next            "^[ \t]*[Nn]ext\\b")
     '(do              "^[ \t]*[Dd]o\\b")
     '(loop            "^[ \t]*[Ll]oop\\b")
     '(while           "^[ \t]*\\([Ww]hile\\)\\b")
     '(end-while       "^[ \t]*[Ee]nd[ \t]+[Ww]hile\\b")
     '(wend            "^[ \t]*[Ww]end\\b")
     '(with            "^[ \t]*\\([Ww]ith\\)\\b")
     '(end-with        "^[ \t]*[Ee]nd[ \t]+[Ww]ith\\b")
     '(try             "^[ \t]*\\([Tr]ry\\)\\b")
     '(catch           "^[ \t]*[Cc]atch\\b")
     '(finally         "^[ \t]*[Ff]inally\\b")
     '(end-try         "^[ \t]*[Ee]nd[ \t]+[Tt]ry\\b")
     '(class           "^[ \t]*[Cc]lass\\b")
     '(end-class       "^[ \t]*[Ee]nd[ \t]+[Cc]lass\\b")
     '(module          "^[ \t]*[Mm]odule\\b")
     '(end-module      "^[ \t]*[Ee]nd[ \t]+[Mm]odule\\b")
     '(using           "^[ \t]*\\([Uu]sing\\)\\b")
     '(end-using       "^[ \t]*[Ee]nd[ \t]+[Uu]sing\\b")
     '(blank           "^[ \t]*$")
     '(comment         "^[ \t]*\\s<.*$")

     '(propget-start   "^[ \t]*\\([Gg]et\\)[ \t]*$")
     '(propget-end     "^[ \t]*[Ee]nd[ \t]+[Gg]et\\b")

     '(propset-start   "^[ \t]*\\([Ss]et\\)[ \t]*(")
     '(propset-end     "^[ \t]*[Ee]nd[ \t]+[Ss]et\\b")


     ;; =======================================================
     ;; the following elements are used for fontification, only.

     '(funcall         "\\b\\([[:alpha:]_][[:alnum:]_.]+\\)[ \t]*(")

     '(import          "^[ \t]*[Ii]mports[ \t]+\\([[:alpha:]_][[:alnum:]_.]+\\)[ \t]*$")

     ;; Some of these regexps match on partial lines.
     ;; The Dim regexp is one example.  This allows the AS fragment and
     ;; its following typename to be fontified independently.


     `(field         ;; Public foo As Integer
       ,(concat
         "\\(?:[Pp]ublic\\|[Pp]rivate\\|[Ff]riend\\)"
         "[ \t]+"                            ;; ws
         "\\([^- \t\(]+\\)"                  ;; name of field
         "[ \t]+"                            ;; ws
         "\\([Aa]s\\)\\([ \t]+[^- \t\(\n]+\\)" ;; type decl
         "[ \t]*"                            ;; optional trailing ws
         ))

     `(dim            ;; Dim X
       ,(concat
         "^[ \t]*"
         "[Dd]im[ \t]+"
         "\\([[:alpha:]_][[:alnum:]_]+\\)"
         "\\(?:([^)]*)\\)?"       ;; optional array dimension (no capture)
         ))

     `(as             ;; As X  ||   As New X
       ;; The As keyword may appear in a func/sub argument list or in a Dim
       ,(concat
         "[ \t]+[Aa]s"
         "\\(?:[ \t]+[Nn]ew\\)?"  ;; optional New keyword (no capture)
         "[ \t]+\\([-A-Za-z.0-9_]+\\)"))

     `(assign            ;; x = foo
       ;; partial match, leave point after equals sign
       ,(concat
         "^[ \t]*"
         "\\("
         "[[:alpha:]_][[:alnum:]_.]+\\|" ;; variable name
         "\\[[[:alpha:]_][[:alnum:]_.]+\\]" ;; var name in square brackets (for resvd words)
         "\\)"
         "[ \t]*"                 ;; optional ws
         "\\(?:([^)]*)\\)?"       ;; optional array dimension (no capture)
         "[ \t]*"                 ;; optional ws
         "="
         "[ \t]*"                 ;; optional white space
         ))

     `(using-simple       ;; Using x
       ;; partial match on Using statement, up to and including the variable
       ,(concat
         "^[ \t]*"
         "[Uu]sing[ \t]+"
         "\\([[:alpha:]_][[:alnum:]_.]+\\)"  ;; variable name
         "[ \t]*"))

     `(new         ;; New foo()
       ,(concat
         "[ \t]*"
         "[Nn]ew[ \t]+"                     ;; New keyword (no capture)
         "\\([[:alpha:]_][[:alnum:]_.]+\\)" ;; constructor
         ))

     `(constant   "\\(\\b\\(?:[1-9][0-9.]*\\|[0-9]\\)\\b\\|&H[0-9A-F]+\\)")

     )))



(defun vbnet-regexp (symbol)
  "Retrieves a regexp from the `vbnet-regexp-alist' corresponding
to the given symbol. There's probably a nifty way to do this with
a fast hash table, but an alist works fine for this purpose. It's
fast enough.
"
  (let ((elt (assoc symbol vbnet-regexp-alist)))
    (if elt (cadr elt) nil)))


;; This is some approximation of the set of reserved words in Visual Basic.
(eval-and-compile
  (defvar vbnet-all-keywords
    '("Add" "Aggregate" "And" "App" "AppActivate" "Application" "Array" "As"
      "Asc" "AscB" "Atn" "Attribute"
      "Beep" "Begin" "BeginTrans" "Boolean" "ByVal" "ByRef"
      "Catch" "CBool" "CByte" "CCur"
      "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "CVErr" "CVar" "Call"
      "Case" "ChDir" "ChDrive" "Character" "Choose" "Chr" "ChrB"
      "Class" "Clipboard" "Close" "Collection"  "Columns"
      "Command" "CommitTrans" "CompactDatabase" "Component" "Components"
      "Const" "Container" "Containers" "Cos" "CreateDatabase" "CreateObject"
      "CurDir" "Currency"
      "DBEngine" "DDB" "Data" "Database" "Databases"
      "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day"
      "Debug" "Declare" "Deftype" "DeleteSetting" "Dim" "Dir" "Do"
      "DoEvents" "Domain"
      "Double" "Dynaset" "EOF" "Each" "Else" "End" "EndProperty"
      "Enum" "Environ" "Erase" "Err" "Error" "Exit" "Exp" "FV" "False" "Field"
      "Fields" "FileAttr" "FileCopy" "FileDateTime" "FileLen" "Fix" "Font" "For"
      "Form" "FormTemplate" "Format" "Forms" "FreeFile" "FreeLocks" "Friend"
      "Function"
      "Get" "GetAllSettings" "GetAttr" "GetObject" "GetSetting" "Global" "GoSub"
      "GoTo" "Group" "Groups" "Hex" "Hour" "IIf" "IMEStatus" "IPmt" "IRR"
      "If" "Implements" "InStr" "Input" "Int" "Integer" "Is" "IsArray" "IsDate"
      "IsEmpty" "IsError" "IsMissing" "IsNull" "IsNumeric" "IsObject" "Kill"
      "LBound" "LCase" "LOF" "LSet" "LTrim" "Left" "Len" "Let" "Like" "Line"
      "Load" "LoadPicture" "LoadResData" "LoadResPicture" "LoadResString" "Loc"
      "Lock" "Log" "Long" "Loop" "MDIForm" "MIRR" "Me" "MenuItems"
      "MenuLine" "Mid" "Minute" "MkDir" "Month" "MsgBox"
      "NPV" "NPer" "Name" "Namespace"
      "New" "Next" "Not" "Now" "Nothing" "Object" "Oct" "On" "Open"
      "OpenDatabase"
      "Operator" "Option" "Optional"
      "Or" "PPmt" "PV" "Parameter" "Parameters" "Partition"
      "Picture" "Pmt" "Print" "Printer" "Printers" "Private" "ProjectTemplate"
      "Property"
      "Properties" "Public" "Put" "QBColor" "QueryDef" "QueryDefs"
      "RSet" "RTrim" "Randomize" "Rate" "ReDim" "Recordset" "Recordsets"
      "RegisterDatabase" "Relation" "Relations" "Rem" "RepairDatabase"
      "Reset" "Resume" "Return" "Right" "RmDir" "Rnd" "Rollback" "RowBuffer"
      "SLN" "SYD" "SavePicture" "SaveSetting" "Screen" "Second" "Seek"
      "SelBookmarks" "Select" "SelectedComponents" "SendKeys" "Set"
      "SetAttr" "SetDataAccessOption" "SetDefaultWorkspace" "Sgn" "Shell"
      "Sin" "Single" "Snapshot" "Space" "Spc" "Sqr" "Static" "Step" "Stop" "Str"
      "Structure"
      "StrComp" "StrConv"
      ;;"String"
      "Sub" "SubMenu" "Switch" "Tab" "Table"
      "TableDef" "TableDefs" "Tan" "Then" "Time" "TimeSerial" "TimeValue"
      "Timer" "To"
      ;;"Trim"
      "True" "Try" "Type" "TypeName" "UBound" "UCase" "Unload"
      "Unlock" "Using" "Val" "Variant" "VarType" "Verb" "Weekday" "Wend"
      "While" "Width" "With" "Workspace" "Workspaces" "Write" "Year"
      "NotInheritable" "Shared" "OrElse"
      "Overridable" "WithEvents" "Finally" "Imports" "Compare" "Handles"
      "Of" "Module"
      )))

(defvar vbnet-namespace-face 'vbnet-namespace-face)
(defface vbnet-namespace-face
  '((((background light))
     (:foreground "DarkSalmon"))
    (((background dark))
     (:background "DarkSalmon")))
  "Face for namespace names (in the Imports statement) in VB.NET buffers."
  :group 'vbnet)

(defvar vbnet-funcall-face 'vbnet-funcall-face)
(defface vbnet-funcall-face
  '((((background light))
     (:foreground "dim gray"))
    (((background dark))
     (:background "grey")))
  "Face for function calls in VB.NET buffers."
  :group 'vbnet)

;; (make-face 'vbnet-diagnostic-face)
;; (set-face-foreground 'vbnet-diagnostic-face "chartreuse")
;; (defvar vbnet-diagnostic-face 'vbnet-diagnostic-face
;;   "Face name to use for diagnostic purposes in VB.NET buffers.")


;; The setup for fontifying. See font-lock.el, in particular the documentation
;; for `font-lock-keywords'.  This form uses mostly the form of font-lock-keywords
;; like this:   (MATCHER . HIGHLIGHT), where HIGHLIGHT is MATCH-HIGHLIGHT and
;; MATCH-HIGHLIGHT is of the form: (SUBEXP FACENAME [OVERRIDE [LAXMATCH]]) .
;;

(defvar vbnet-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; constant numeric values
     (list (vbnet-regexp 'constant)
           '(1 font-lock-constant-face nil t))

     ;; using statements
     (list (vbnet-regexp 'using-simple)
           '(1 font-lock-variable-name-face))

     ;; new (invoking constructors)
     (list (vbnet-regexp 'new)
           '(1 font-lock-type-face))

     ;; Dim statements
     (list (vbnet-regexp 'dim)
           '(1 font-lock-variable-name-face)
           '(2 font-lock-type-face nil t))

     ;; Field - as in a struct
     (list (vbnet-regexp 'field)
           '(1 font-lock-variable-name-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-type-face nil t))

     ;; As fragment
     (list (vbnet-regexp 'as)
           '(1 font-lock-type-face nil t))

     ;; Assignment statements
     (list (vbnet-regexp 'assign)
           '(1 font-lock-variable-name-face))

     ;; function declarations
     (list (vbnet-regexp 'func-start)
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-function-name-face))

     ;; sub declaration
     (list (vbnet-regexp 'sub-start)
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-function-name-face))

     ;; class decl
     (list (vbnet-regexp 'class-start)
           '(1 font-lock-keyword-face nil t)  ;; protection modifiers
           '(2 font-lock-keyword-face nil t)  ;; "Class"
           '(3 font-lock-type-face))    ;; name of type

     ;; struct decl
     (list (vbnet-regexp 'struct-start)
           '(1 font-lock-keyword-face nil t)  ;; protection modifiers
           '(2 font-lock-keyword-face nil t)  ;; "Structure"
           '(3 font-lock-type-face))  ;; name of type

     ;; enum decl
     (list (vbnet-regexp 'enum-start)
           '(1 font-lock-keyword-face nil t)  ;; protection
           '(2 font-lock-keyword-face nil t)  ;; "Enum"
           '(3 font-lock-type-face)  ;; name of enum
           '(4 font-lock-keyword-face)  ;; "As" (optional)
           '(5 font-lock-type-face))  ;; derived type (optional)

     ;; namespace decl
     (list (vbnet-regexp 'namespace-start)
           '(1 font-lock-keyword-face nil t)
           '(2 vbnet-namespace-face))

     ;; function call - must be placed after the entries for using statements,
     ;; func decls, and sub decls
     (list (vbnet-regexp 'funcall)
           ;;'(1 font-lock-function-name-face))
           '(1 vbnet-funcall-face))

     ;; imports
     (list (vbnet-regexp 'import)
           '(1 vbnet-namespace-face))

     ;; Statement labels
     (cons (vbnet-regexp 'label)
           'font-lock-keyword-face)

     ;; Case values
     ;; String-valued cases get font-lock-string-face regardless.
     (list "^[ \t]*case[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

     ;; Any keywords you like.
     (list (concat "\\<" (regexp-opt
                          '("Dim" "If" "Then" "Else" "ElseIf" "End If") t)
                   "\\>")
           1 'font-lock-keyword-face))))


(defvar vbnet-font-lock-keywords-2
  (append vbnet-font-lock-keywords-1
          (eval-when-compile
            `((,(concat "\\<" (regexp-opt vbnet-all-keywords t) "\\>")
               1 font-lock-keyword-face)))))


(defvar vbnet-font-lock-keywords vbnet-font-lock-keywords-1)

(put 'vbnet-mode 'font-lock-keywords 'vbnet-font-lock-keywords)

(defun vbnet-enable-font-lock ()
  ;; Emacs 19.29 requires a window-system else font-lock-mode errs out.
  (cond ((or vbnet-xemacs-p window-system)

         ;; In win-emacs this sets font-lock-keywords back to nil!
         (if vbnet-winemacs-p
             (font-lock-mode 1))

         ;; Accomodate emacs 19.29+
         ;; From: Simon Marshall <Simon.Marshall@esrin.esa.it>
         (cond ((boundp 'font-lock-defaults)
                (make-local-variable 'font-lock-defaults)
                (setq font-lock-defaults
                      `((vbnet-font-lock-keywords
                         vbnet-font-lock-keywords-1
                         vbnet-font-lock-keywords-2)
                        nil t ((,(string-to-char "_") . "w")))))
               (t
                (make-local-variable 'font-lock-keywords)
                (setq font-lock-keywords vbnet-font-lock-keywords)))

         (if vbnet-winemacs-p
             (font-lock-fontify-buffer)
           (font-lock-mode 1)))))


;; KJW should add some odds and bobs here to cover "end if". One way
;; could be to create the abbreviations by removing whitespace then we
;; could put "end if", "end with" and so on in the keyword table
;; Another idea would be to make it intelligent enough to substitute
;; the correct end for the construct (with, select, if)
;; Is this what the abbrev table hook entry is for?
(defun vbnet-construct-keyword-abbrev-table ()
  (if vbnet-mode-abbrev-table
      nil
    (let ((words vbnet-all-keywords)
          (word nil)
          (list nil))
      (while words
        (setq word (car words)
              words (cdr words))
        (setq list (cons (list (downcase word) word) list)))

      (define-abbrev-table 'vbnet-mode-abbrev-table list))))


;; Would like to do this at compile-time.
(vbnet-construct-keyword-abbrev-table)


(defun vbnet-in-code-context-p ()
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
                  (beginning-of-line)
                  (point)))
           (list
            (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))          ; inside string.
           (null (nth 4 list))))))      ; inside comment


(defun vbnet-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
        (if (vbnet-in-code-context-p)
            vbnet-mode-abbrev-table)))


(defun vbnet-newline-and-indent (&optional count)
  "Insert a newline, updating indentation."
  (interactive)
  (save-excursion
    (expand-abbrev)
    (vbnet-indent-line))
  (call-interactively 'newline-and-indent))


(defun vbnet-moveto-beginning-of-block ()
  "Moves to the line containing the start of the smallest containing block,
regardless whether it is a Function, Sub, Class, Namespace, etc.

See also, the related functions,  `vbnet-moveto-end-of-block',
 `vbnet-moveto-beginning-of-defun', and `vbnet-moveto-end-of-defun'."
  (interactive)
  (if (re-search-backward (vbnet-regexp 'block-start) 0 t)
      (back-to-indentation)))


(defun vbnet-moveto-end-of-block ()
  "Moves to the line containing the end of the smallest containing block,
regardless whether it is a Function, Sub, Class, Namespace, etc.

See also, the related functions,  `vbnet-moveto-beginning-of-block',
 `vbnet-moveto-beginning-of-defun', and `vbnet-moveto-end-of-defun'."
  (interactive)
  (if (re-search-forward (vbnet-regexp 'block-end) nil t)
      (back-to-indentation)))



(defun vbnet-close-current-block ()
  "Inserts the \"End Xxxx\" (etc) string to close the current
containing block, whether it is a Sub, Class, Function,
Namespace, Struct, If, While, For, Enum, Using, etc.

It looks backwards in the source code to find the innermost \"block\"
that is open, and inserts the appropriate ending syntax for that block.

The logic is naive. If you invoke this fn when point is within a
class declaration, it will insert \"End Class\" even if there is
an \"End Class\" on the line immediately following point.  So
don't do that."
  (interactive)
  (let ((orig-point (point))
        (block-regex-tuples ;; regex regex idx count
         (list '(prop-start prop-end 2 0)
               '(select select-end 1 0)
               '(with end-with 1 0)
               '(using end-using 1 0)
               '(if endif 1 0)
               '(for next "Next" 0)
               '(while end-while 1 0)
               '(sub-start sub-end 2 0)
               '(try end-try 1 0)
               '(func-start func-end 2 0)
               '(intf-start intf-end 2 0)
               '(class-start class-end 2 0)
               '(struct-start struct-end 2 0)
               '(enum-start enum-end 2 0)
               '(propset-start propset-end 1 0)
               '(propget-start propget-end 1 0)
               '(namespace-start namespace-end 1 0)
               ))
        block-end
        done)

    ;; The approach here used to determine the smallest containing block:

    ;;   1. goto the previous line.
    ;;
    ;;   2. if (bobp), quit.
    ;;
    ;;   3. for each type of block, check if the line is an "end block"
    ;;      of that type. If so, increment the count for that block type,
    ;;      and continue to step 1.  otheriwse, continue to step 3.
    ;;
    ;;   4. for each type of block, check if the line is a "start block"
    ;;      of that type.  If so, and if the count is zero, this is the
    ;;      start of the smallest containing block.  If it is the start
    ;;      of a block, but the count of ends for that block type is
    ;;      non-zero, decrement the appropriate count and continue to
    ;;      step 1.
    ;;

    (while (not done)

      (let (found
            eol)

        ;; steo 1
        (vbnet-previous-line-of-code)

        ;; steo 2
        (if (bobp) (setq done t)

          (setq eol (save-excursion
                      (end-of-line)
                      (point)))

          ;; steo 3: check for end-blocks, incf nesting counts as necessary
          (dolist (tuple block-regex-tuples)
            (if (not found)
                (if (re-search-forward (vbnet-regexp (cadr tuple)) eol t)
                    (progn
                      (incf (cadddr tuple))
                      (setq found t)))))


          ;; steo 4: check for begin-blocks, checking the nesting count
          (if (not found)
              (dolist (tuple block-regex-tuples)
                (if (not found)
                    (if (re-search-forward (vbnet-regexp (car tuple)) eol t)
                        (if (eq (cadddr tuple) 0)
                            (setq found t
                                  done t
                                  block-end
                                  (let ((value (caddr tuple)))
                                    (if (integerp value)
                                        (concat "End " (match-string value))
                                      value)))

                          (decf (cadddr tuple))
                          (setq found t)))))))))

    (goto-char orig-point)

    ;; If I was ambitious, I could insert logic here to determine if
    ;; there is already an "End Xxx" for the given block. But that would
    ;; be hard to do correctly, and I'm not that ambitious. I'll settle
    ;; for properly documenting the naivete of this fn.

    (if block-end
        (progn
          (insert block-end)
          (vbnet-indent-line)
          ;; (back-to-indentation)
          (move-end-of-line 1)
          (just-one-space 0)
          ))))


(defun vbnet--moveto-boundary-of-defun (goto-top)
  "Move to a boundary of the Function (or Sub) that surrounds point.

If GOTO-TOP is non-nil, then move to the top of the
Function (or Sub).  Otherwise, move to the bottom of the
Function (or Sub).

If the original point is not within a Function or Sub, returns nil, and
does not move point."
  (let* ((orig-point (point))
         (block-regex-tuples
          '((func-start func-end)
            (sub-start sub-end)
            (propset-start propset-end)
            (propget-start propget-end)))
         get-regex-fn1
         get-regex-fn2
         test-done-fn
         move-fn
         found
         done)

    (if goto-top
        (setq get-regex-fn1 'cadr
              get-regex-fn2 'car
              test-done-fn 'bobp
              move-fn 'vbnet-previous-line-of-code)
      (setq get-regex-fn1 'car
            get-regex-fn2 'cadr
            test-done-fn 'eobp
            move-fn 'vbnet-next-line-of-code))

    (while (not done)
      (let (eol)
        (funcall move-fn)
        (if (funcall test-done-fn) (setq done t)
          (setq eol (save-excursion (end-of-line) (point)))
          (dolist (regex-pair block-regex-tuples)
            (if (not done)
                (if (re-search-forward (vbnet-regexp (funcall get-regex-fn1 regex-pair)) eol t)
                    (setq done t))))

          (if (not done)
              (dolist (regex-pair block-regex-tuples)
                (if (not done)
                    (if (re-search-forward (vbnet-regexp (funcall get-regex-fn2 regex-pair)) eol t)
                        (progn
                          (setq done t found t)
                          (vbnet-indent-line)
                          (if goto-top
                              (back-to-indentation)
                            (move-end-of-line 1))))))))))

    (if (not found)
        (progn
          (goto-char orig-point)))

    found))



(defun vbnet-moveto-beginning-of-defun ()
  "Move to the top of the Function (or Sub) that surrounds point.

If the original point is not within a Function or Sub, throw
an error.

See also, `vbnet-moveto-end-of-defun'.

--------

NB: Emacs has a fn called `beginning-of-defun' which is designed
to do the same thing, for Lisp code.  It allows for alternative
logic to search to the beginning of the containing function, via
the variable `beginning-of-defun-function'.  So, if things worked
nicely, ‘vbnet-mode’ could simply set that variable to this
function.

But, `beginning-of-defun' does additional things after
calling the custom function.  Not sure why, and VBnet-mode doesn't
want that extra stuff for navigating in VB.NET code.  So, we don't
use that facility.  In fact, I'm not sure of the utility of that
extension mechanism, but, whatever."
  (interactive)
  (let (debug-on-error)
    ;; Set debug-on-error to nil here. In case of an error, I just want
    ;; this interactive fn to ding the bell and post a message.  Not
    ;; sure why "enter the debugger on error" by default is appropriate.
    ;;
    ;; See doc on `eval-expression-debug-on-error' for more background.
    ;;
    (if (not (vbnet--moveto-boundary-of-defun t))
        (error "Not in Function, not in Sub"))))



(defun vbnet-moveto-end-of-defun ()
  "Move to the bottom of the Function (or Sub) that surrounds point.

If the original point is not within a Function or Sub, throw an error.

See also, `vbnet-moveto-beginning-of-defun'.

--------

NB: Emacs has a fn called `end-of-defun' which is designed
to do the same thing, for Lisp code.  Also, it allows for alternative
logic to search to the end of the containing function, via
the variable `end-of-defun-function'.  So, if things worked
nicely, ‘vbnet-mode’ could simply set that variable to this
function.

But, `end-of-defun' does additional things after
calling the custom function.  Not sure why, and VBnet-mode doesn't
want that extra stuff for navigating in VB.NET code.  So, we don't
use that facility.  In fact, I'm not sure of the utility of that
extension mechanism, but, whatever."
  (interactive)
  (let (debug-on-error)
    ;; Set debug-on-error to nil here. In case of an error, I just want
    ;; this interactive fn to ding the bell and post a message.  Not
    ;; sure why "enter the debugger on error" by default is appropriate.
    ;;
    ;; See doc on `eval-expression-debug-on-error' for more background.
    ;;
    (if (not (vbnet--moveto-boundary-of-defun nil))
        (error "Not in Function, not in Sub"))))




(defun vbnet-mark-defun ()
  "Set mark to the bottom of the Function (or Sub) that surrounds point,
then set point to the top of the Function (or Sub).

If the original point is not within a Function or Sub, returns nil."
  (interactive)
  ;; (beginning-of-line) ;; ?huh?
  (let ((orig-point (point)))
    (condition-case ()
        (progn
          (vbnet-moveto-end-of-defun)
          (set-mark (point))
          (goto-char orig-point)
          (vbnet-moveto-beginning-of-defun)
          (if vbnet-xemacs-p
              (zmacs-activate-region)))
      (progn
        (goto-char orig-point)
        (error "Not in a Function or Sub")))))



(defun vbnet-indent-defun ()
  (interactive)
  (save-excursion
    (vbnet-mark-defun)
    (call-interactively 'vbnet-indent-region)))


(defun vbnet-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s<+[ \t]*"))
      (if (looking-at comment-re)
          (let ((fill-prefix
                 (buffer-substring
                  (progn (beginning-of-line) (point))
                  (match-end 0))))

            (while (and (not (bobp))
                        (looking-at (vbnet-regexp 'comment)))
              (forward-line -1))
            (if (not (bobp)) (forward-line 1))

            (let ((start (point)))

              ;; Make all the line prefixes the same.
              (while (and (not (eobp))
                          (looking-at comment-re))
                (replace-match fill-prefix)
                (forward-line 1))

              (if (not (eobp))
                  (beginning-of-line))

              ;; Fill using fill-prefix
              (fill-region-as-paragraph start (point))))))))



(defun vbnet-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at (vbnet-regexp 'comment)))
         (vbnet-fill-long-comment))
        (t
         (vbnet-indent-defun))))


(defun vbnet-new-sub ()
  "Insert template for a new subroutine.
Repeat to cycle through alternatives.

This is probably better handled with a dedicated template module,
like ya-snippet."
  (interactive)
  (beginning-of-line)
  (let ((templates (cons (vbnet-regexp 'blank)
                         vbnet-defn-templates))
        (tem nil)
        (bound (point)))
    (while templates
      (setq tem (car templates)
            templates (cdr templates))
      (cond ((looking-at tem)
             (replace-match (or (car templates)
                                ""))
             (setq templates nil))))

    (search-backward "()" bound t)))



(defun vbnet-untabify ()
  "Used to convert any tabs present in the file, to spaces."
  (if (eq major-mode 'vbnet-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun vbnet-get-tag-around-point ()
  (if (and (not (bobp))
           (save-excursion
             (backward-sexp)
             (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
        (e (save-excursion
             (forward-sexp)
             (point))))
    (buffer-substring s e)))


(defun vbnet-grep (tag)
  "Search BASIC source files in current directory for TAG."
  (interactive
   (list (let* ((def (vbnet-get-tag-around-point))
                (tag (read-string
                      (format "Grep for [%s]: " def))))
           (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag
                (mapconcat 'identity vbnet-wild-files " "))))


;;; IDE Connection.

(defun vbnet-buffer-project-file ()
  "Return a guess as to the project file associated with the current buffer."
  (car (directory-files (file-name-directory (buffer-file-name)) t "\\.vbp")))

(defun vbnet-start-ide ()
  "Start Visual Basic (or your favorite IDE, (after Emacs, of course))
on the first project file in the current directory.
Note: it's not a good idea to leave Visual Basic running while you
are editing in Emacs, since Visual Basic has no provision for reloading
changed files."
  (interactive)
  (let (file)
    (cond ((null vbnet-ide-pathname)
           (error "No pathname set for Visual Basic.  See vbnet-ide-pathname"))
          ((null (setq file (vbnet-buffer-project-file)))
           (error "No project file found"))
          ((fboundp 'win-exec)
           (iconify-emacs)
           (win-exec vbnet-ide-pathname 'win-show-normal file))
          ((fboundp 'start-process)
           (iconify-frame (selected-frame))
           (start-process "*VisualBasic*" nil vbnet-ide-pathname file))
          (t
           (error "No way to spawn process!")))))



;;; Indentation-related stuff.

(defun vbnet-indent-region (start end)
  "Apply indentation according to Visual Basic .NET syntax, for
each line in the region.

See also `vbnet-indent-line'."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
                (< (point) end))
      (if (not (looking-at (vbnet-regexp 'blank)))
          (vbnet-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
         (zmacs-deactivate-region))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))


(defun vbnet-previous-line-of-code ()
  "Move to the previous non-blank, non-comment line in the buffer."
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at (vbnet-regexp 'blank))
                  (looking-at (vbnet-regexp 'comment))))
    (forward-line -1)))


(defun vbnet-next-line-of-code ()
  "Move to the next non-blank, non-comment line in the buffer."
  (if (not (eobp))
      (forward-line 1)) ;; moves to bol
  (while (and (not (eobp))
              (or (looking-at (vbnet-regexp 'blank))
                  (looking-at (vbnet-regexp 'comment))))
    (forward-line 1)))




(defun vbnet--back-to-start-of-continued-statement (&optional dont-backup-over-attributes)
  "If the current line is a continuation, move back to the original statement.

Do not backup over attributes if the optional arg,
DONT-BACKUP-OVER-ATTRIBUTES, is t."
  (let ((here (point)))

    (vbnet-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at (vbnet-regexp 'continuation))
                (not (and dont-backup-over-attributes
                          (looking-at (vbnet-regexp 'end-of-attr-and-continuation))))
                )
      (setq here (point))
      (vbnet-previous-line-of-code))
    (goto-char here)))


(defun vbnet-find-matching-stmt (open-regexp close-regexp)
  "Search backwards to find a matching statement. Attempts to properly
handle nested blocks."
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (vbnet-previous-line-of-code)
      (vbnet--back-to-start-of-continued-statement t) ;; don't backup over attributes!
      (cond ((looking-at close-regexp)
             (setq level (+ level 1)))
            ((looking-at open-regexp)
             (setq level (- level 1)))))))



(defun vbnet--get-indent-column-for-continued-line (original-point)
  "Calculate indent for a line which follows a continuation line.

Upon entry, the point must be positioned on the line *prior to
the one to be indented*, and ORIGINAL-POINT refers to the line
being indented.

Indent continuation lines according to some rules.

   1. If the continuation line is a .NET Attribute, (eg
      <DllImport(...)> then indent the following line to the same
      column.

   2. if the continued line has an open paren pair, then
      indent the following line to the first open paren on the
      previous line.

   3. otherwise, indent one word in."
  (let ((starting (point)))

    (cond
     ;; does the preceding line end an attribute?
     ((looking-at (vbnet-regexp 'end-of-attr-and-continuation))
      (vbnet--back-to-start-of-continued-statement)
      (back-to-indentation)
      (current-column))

     (t
      (vbnet--back-to-start-of-continued-statement)

      (let* ((orig-stmt (point))
           (matching-open-paren
            (condition-case ()
                (save-excursion
                  (goto-char original-point)
                  (beginning-of-line)
                  (backward-up-list 1)
                  ;; Only if point is now w/in cont. block.
                  (if (<= orig-stmt (point))
                      (current-column)))
              (error nil))))

      (cond (matching-open-paren ;; found?
             (1+ matching-open-paren))
            (t
             ;; Else, after first word on original line.
             (back-to-indentation)
             (forward-word 1)
             (while (looking-at "[ \t]")
               (forward-char 1))
             (current-column))))))))



(defun vbnet-calculate-indent ()
  "Calculate the indentation for the current point in a vb.net buffer."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond

       ;; special case the beginning-of-buffer
       ((bobp)
        0)

       ;; The following indents class start to the 0th column.
       ;; In VB.NET, this is wrong, due to enclosing namespaces.

       ;; ((or
       ;;   ;;              (looking-at (vbnet-regexp 'block-start))
       ;;   (looking-at (vbnet-regexp 'class))
       ;;   (looking-at (vbnet-regexp 'label))
       ;;   ;;              (looking-at (vbnet-regexp 'block-end)))
       ;;   (looking-at (vbnet-regexp 'end-class)))
       ;;  0)

       ;; these can be nested
       ((looking-at (vbnet-regexp 'namespace-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'namespace-start)
                                  (vbnet-regexp 'namespace-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'class-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'class-start)
                                  (vbnet-regexp 'class-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'struct-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'struct-start)
                                  (vbnet-regexp 'struct-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'enum-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'enum-start)
                                  (vbnet-regexp 'enum-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'prop-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'prop-start)
                                  (vbnet-regexp 'prop-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'propget-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'propget-start)
                                  (vbnet-regexp 'propget-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'propset-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'propset-start)
                                  (vbnet-regexp 'propset-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'func-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'func-start)
                                  (vbnet-regexp 'func-end))
        (current-indentation))

       ((looking-at (vbnet-regexp 'sub-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'sub-start)
                                  (vbnet-regexp 'sub-end))
        (current-indentation))

       ;; The outdenting stmts, which simply match their original.
       ((or (looking-at (vbnet-regexp 'else))
            (looking-at (vbnet-regexp 'endif)))
        (vbnet-find-matching-stmt (vbnet-regexp 'if)
                                  (vbnet-regexp 'endif))
        (current-indentation))

       ((or (looking-at (vbnet-regexp 'catch))
            (looking-at (vbnet-regexp 'finally)))
        (vbnet-find-matching-stmt (vbnet-regexp 'try)
                                  (vbnet-regexp 'end-try))
        (current-indentation))

       ;; All the other matching pairs act alike.
       ((looking-at (vbnet-regexp 'next))
        (vbnet-find-matching-stmt (vbnet-regexp 'for)
                                  (vbnet-regexp 'next))
        (current-indentation))

       ((looking-at (vbnet-regexp 'loop))
        (vbnet-find-matching-stmt (vbnet-regexp 'do)
                                  (vbnet-regexp 'loop))
        (current-indentation))

       ((looking-at (vbnet-regexp 'wend))
        (vbnet-find-matching-stmt (vbnet-regexp 'while)
                                  (vbnet-regexp 'wend))
        (current-indentation))

       ((looking-at (vbnet-regexp 'end-while))
        (vbnet-find-matching-stmt (vbnet-regexp 'while)
                                  (vbnet-regexp 'end-while))
        (current-indentation))

       ((looking-at (vbnet-regexp 'end-with))
        (vbnet-find-matching-stmt (vbnet-regexp 'with)
                                  (vbnet-regexp 'end-with))
        (current-indentation))

       ((looking-at (vbnet-regexp 'end-try))
        (vbnet-find-matching-stmt (vbnet-regexp 'try)
                                  (vbnet-regexp 'end-try))
        (current-indentation))

       ((looking-at (vbnet-regexp 'end-using))
        (vbnet-find-matching-stmt (vbnet-regexp 'using)
                                  (vbnet-regexp 'end-using))
        (current-indentation))

       ((looking-at (vbnet-regexp 'select-end))
        (vbnet-find-matching-stmt (vbnet-regexp 'select)
                                  (vbnet-regexp 'select-end))
        (current-indentation))

       ;; A case of a select is somewhat special.
       ((looking-at (vbnet-regexp 'case))
        (vbnet-find-matching-stmt (vbnet-regexp 'select)
                                  (vbnet-regexp 'select-end))
        (+ (current-indentation) vbnet-mode-indent))

       (t
        ;; Other cases which depend on the previous line.
        (vbnet-previous-line-of-code)

        ;; Skip over label lines, which always have 0 indent.
        (while (looking-at (vbnet-regexp 'label))
          (vbnet-previous-line-of-code))

        (cond

         ((looking-at (vbnet-regexp 'continuation))
          (vbnet--get-indent-column-for-continued-line original-point))

         (t
          (vbnet--back-to-start-of-continued-statement t) ;; why?

          (let ((indent (current-indentation)))
            ;; All the various +indent regexps.
            (cond

             ((looking-at (vbnet-regexp 'block-start))
              (+ indent vbnet-mode-indent))

             ((or (looking-at (vbnet-regexp 'class-start))
                  (looking-at (vbnet-regexp 'struct-start))
                  (looking-at (vbnet-regexp 'enum-start))
                  (looking-at (vbnet-regexp 'namespace-start))
                  (looking-at (vbnet-regexp 'propget-start))
                  (looking-at (vbnet-regexp 'propset-start))
                  (looking-at (vbnet-regexp 'module)))
              (+ indent vbnet-mode-indent))

             ((and (or (looking-at (vbnet-regexp 'if))
                       (looking-at (vbnet-regexp 'else)))
                   (not (and vbnet-allow-single-line-if
                             (looking-at (vbnet-regexp 'ifthen)))))
              (+ indent vbnet-mode-indent))

             ((or (looking-at (vbnet-regexp 'select))
                  (looking-at (vbnet-regexp 'case)))
              (+ indent vbnet-mode-indent))

             ((or (looking-at (vbnet-regexp 'try))
                  (looking-at (vbnet-regexp 'catch))
                  (looking-at (vbnet-regexp 'finally)))
              (+ indent vbnet-mode-indent))

             ((or (looking-at (vbnet-regexp 'do))
                  (looking-at (vbnet-regexp 'for))
                  (looking-at (vbnet-regexp 'while))
                  (looking-at (vbnet-regexp 'with))
                  (looking-at (vbnet-regexp 'using)))
              (+ indent vbnet-mode-indent))

             (t
              ;; By default, just copy indent from prev line.
              indent))))))))))




(defun vbnet-indent-to-column (col)
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at (vbnet-regexp 'blank)))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))


(defun vbnet-indent-line ()
  "Indent current line for Visual Basic syntax.
This assumes that the previous non-blank line is indented properly.

See also `vbnet-indent-region'."
  (interactive)
  (vbnet-indent-to-column (vbnet-calculate-indent)))



;; ========================================================================
;; line continuation helpers

(defun vbnet-split-line ()
  "Split line at point, adding continuation character or continuing
a comment. In Abbrev mode, any abbrev before point will be expanded.

See also `vbnet-join-continued-lines'"
  (interactive)
  (save-excursion
    (let* ((opoint (point))
           (bol (progn (beginning-of-line) (point)))
           (pps-list
            (parse-partial-sexp bol opoint)))

      ;; Dispatch on syntax at this position.
      (cond ((nth 4 pps-list)   ; in comment
             (indent-new-comment-line))
            ((nth 3 pps-list)   ; in string
             (error "Can't break line inside a string"))
            (t
             (just-one-space)           ; insure one space
             (insert "_")               ; apply the continuation
             (vbnet-newline-and-indent))))))


 (defun vbnet-join-continued-lines ()
  "Join the split line at point, removing the continuation
character and concatenating the current line and the following
line. At exit, point is moved to the end of the joined line.

See also `vbnet-split-line'
"
  (interactive)
  (save-excursion
    (let* ((opoint (point))
           (bol (progn (beginning-of-line) (point)))
           (eol (progn (end-of-line) (point)))
           (pps-list
            (parse-partial-sexp bol eol)))

      ;; Dispatch on syntax at this position.
      (cond ((nth 4 pps-list)   ; in comment
             nil)
            ((nth 3 pps-list)   ; in string
             nil)

            (t
             (cond
              ((equal opoint eol)
               (backward-char 4))
              ((equal opoint (1- eol))
               (backward-char 3))
              ((equal (1+ opoint) (1- eol))
               (backward-char 2))
              (t
               (goto-char opoint)))

             ;; is it a continuation?
             (if (looking-at ".+ _$")
                 (progn
                   (end-of-line)
                   (delete-char -1)
                   (delete-char 1)
                   (just-one-space))))))))


;; ========================================================================
;;; Some experimental functions

;;; Load associated files listed in the file local variables block
(defun vbnet-load-associated-files ()
"Load files that are useful to have around when editing the source
of the file that has just been loaded.

The buffer must have a local variable, `vbnet-associated-files',
that is a list of strings, naming the \"associated\" files to be
opened into editing buffers.  If the file name is relative it is
relative to the directory containing the current buffer.  If the
file is already loaded into an editing buffer, nothing happens;
this prevents circular references from causing trouble.

After an associated file is loaded, if it is a VB.NET module and
if it has the appropriate variable set, its associated files list
will be processed in turn."

  (if (boundp 'vbnet-associated-files)
      (let ((files vbnet-associated-files)
            (file nil))
        (while files
          (setq file (car files)
                files (cdr files))
          (message "Load associated file: %s" file)
          (vbnet-load-file-ifnotloaded file default-directory)))))



(defun vbnet-load-file-ifnotloaded (file default-directory)
  "Load (edit) FILE if not already loaded.
If FILE is relative then DEFAULT-DIRECTORY provides the path."
  (let((file-absolute (expand-file-name file default-directory)))
    (if (get-file-buffer file-absolute) ; don't do anything if the buffer is already loaded
        ()
      (find-file-noselect file-absolute ))))


;; ========================================================================
;; YA-snippet integration

(defun vbnet-fixup-yasnippet ()
  "Load snippets into ya-snippet for VB.NET, if they do not already exist."
  (if (not vbnet--yasnippet-has-been-fixed)
      (if (fboundp 'yas/snippet-table-fetch)
          (let ((snippet-table (yas/snippet-table 'vbnet-mode))
                (keymap (if yas/use-menu
                            (yas/menu-keymap-for-mode mode)
                          nil))
                (yas/require-template-condition nil)
                (builtin-snips
                 '(
                   ("wl" "System.Console.WriteLine(${1://thing to write})
" "WriteLine (...)" nil)
                   ("prop" "   Private _${1:Name} as ${2:Type}
   Public Property ${1:Name}() As ${2:Type}

      Get
         Return m${1:Name}
      End Get

      Set(ByVal value As ${2:Type})
            m${1:Name} = value
      End Set

   End Property ' ${1:Name}

" "Property ... { ... }" nil)
                   ("ife" "If ${1:predicate} Then
  ${2:// then clause}
Else
  ${3:// else clause}
End If" "If ... Then  ... Else  ... End If" nil)
                   ("if" "If ${1:predicate} Then
  ${2:// then clause}
End If
" "If ... Then  ... End If" nil)
                   ("fore" "Dim ${1:var} As ${2:type}
For Each $1 In ${3:IEnumerable}
    ${4:'' body...}
Next

" "For Each ... Next" nil)
                   ("for" "For ${1:index} As Integer = 0 To ${2:finish}
    ${3:''body}
Next $1

" "for (...) { ... }" nil)
                   ("args" "        Dim i As Integer
        For i = 0 To args.Length - 1
            Select Case(args(i))
                Case \"-b\":
                    If (_boolValue = True) Then
                        Throw New ArgumentException(args(i))
                    End If
                    _boolValue = True

                Case \"-s\":
                    i += 1
                    If (args.Length <= i) Then
                        Throw New ArgumentException(args(i))
                    End If
                    If Not (Me._stringValue Is Nothing) Then
                        Throw New ArgumentException(args((i - 1)))
                    End If
                    _stringValue = args(i)

                Case \"-n\":
                    i += 1
                    If (args.Length <= i) Then
                        Throw New ArgumentException(args(i))
                    End If
                    If (Me._intValue <> 0) Then
                        Throw New ArgumentException(args((i - 1)))
                    End If
                    If args(i).StartsWith(\"0x\") Then
                        Me._intValue = Integer.Parse(args(i).Substring(2), NumberStyles.AllowHexSpecifier)
                    Else
                        Me._intValue = Integer.Parse(args(i))
                    End If

                case \"-?\":
                    Throw New ArgumentException(args(i))

                Case Else:
                    Throw New ArgumentException(args(i))

            End Select
        Next i
        If (Me._intValue = 0) Then
            Me._intValue = Me.DefaultIntValue
        End If

" "Select Case(args(i) ..." nil)
                   )))

            (setq vbnet--yasnippet-has-been-fixed t)

            ;; TODO: Fix. when vbnet-mode is loaded after yasnippet,
            ;; vbnet-mode does not show up on the yasnippet menu.
            ;; This code naively attempts to insert it manually.
            ;; To be correct, Should check to see if it is already there,
            ;; to handle the case when vbnet is loaded before yasnippet.

            (when yas/use-menu
              (define-key
                yas/menu-keymap
                (vector 'vbnet-mode)
                `(menu-item "VB.Net" ,keymap)))

            ;; Insert the snippets from above into the table if they
            ;; are not already defined.
            (mapcar
             '(lambda (item)
                (let* ((full-key (car item))
                       (existing-snip
                        (yas/snippet-table-fetch snippet-table full-key)))
                  (if (not existing-snip)
                      (let* ((key (file-name-sans-extension full-key))
                             (name (caddr item))
                             (condition (nth 3 item))
                             (template (yas/make-template (cadr item)
                                                          (or name key)
                                                          condition)))
                        (yas/snippet-table-store snippet-table
                                                 full-key
                                                 key
                                                 template)
                        (when yas/use-menu
                          (define-key keymap (vector (make-symbol full-key))
                            `(menu-item ,(yas/template-name template)
                                        ,(yas/make-menu-binding (yas/template-content template))
                                        :keys ,(concat key yas/trigger-symbol))))))))
             builtin-snips)))))


;; ========================================================================
;; flymake integration

(defun vbnet-flymake-init ()
  (vbnet-flymake-init-impl
   'flymake-create-temp-inplace t t 'vbnet-flymake-get-cmdline))

(defun vbnet-flymake-init-impl (create-temp-f use-relative-base-dir use-relative-source get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
        (temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
    ;;(message "vbnet/init:temp source file: %s" temp-source-file-name)
    (setq args (flymake-get-syntax-check-program-args
                temp-source-file-name "."
                use-relative-base-dir use-relative-source
                get-cmdline-f))
    args))


(defun vbnet-flymake-cleanup ()
  "Delete the temporary .netmodule file created in syntax checking,
then call through to flymake-simple-cleanup."
  (if flymake-temp-source-file-name
      (progn
        ;;(message "vbnet/clean:temp source file: %s" flymake-temp-source-file-name)

        (let* ((netmodule-name
                (concat (file-name-sans-extension flymake-temp-source-file-name)
                        ".netmodule"))
               (expanded-netmodule-name (expand-file-name netmodule-name ".")))
          (if (file-exists-p expanded-netmodule-name)
              (flymake-safe-delete-file expanded-netmodule-name)))
        ))
  (flymake-simple-cleanup))

(defvar vbnet-flymake-vbc-arguments
  (list "/t:module" "/nologo")
  "A list of arguments to use with the vbc.exe
compiler, when using flymake with a
direct vbc.exe build for syntax checking purposes.")



(defun vbnet-split-string-respecting-quotes (s)
  "splits a string into tokens, respecting double quotes
For example, the string 'This is \"a string\"' will be split into 3 tokens.

More pertinently, the string
   'csc /t:module /R:\"c:\abba dabba\dooo\Foo.dll\"'

...will be split into 3 tokens.

This fn also removes quotes from the tokens that have them. This is for
compatibility with flymake and the process-start fn."
  (let ((local-s s)
        (my-re-1 "[^ \"]+\"[^\"]+\"\\|[^ \"]+")
        (my-re-2 "\\([^ \"]+\\)\"\\([^\"]+\\)\"")
        (tokens))
    (while (string-match my-re-1 local-s)
      (let ((token (match-string 0 local-s))
            (remainder (substring local-s (match-end 0))))
        (if (string-match my-re-2 token)
            (setq token (concat (match-string 1 token) (match-string 2 token))))
        (message "token: %s" token)
        (setq tokens (append tokens (list token)))
        (setq local-s remainder)))
  tokens))


(defun vbnet-get-value-from-comments (marker-string line-limit)
  "Get a string from the header comments in the current buffer.

This is used to extract the flymake command and the compile
command from the comments.

It looks for MARKER-STRING and returns the string that
follows it, or returns nil if that string is not found.

E.g. when MARKER-STRING is \"flymake-command\", and the following
line is found at the top of the buffer:

     flymake-command: vbc.exe /r:Hallo.dll

... then this command will return the string:

     \"vbc.exe /r:Hallo.dll\"

LINE-LIMIT is interpreted in the same way as `vbnet-cmd-line-limit'."

  (let (start search-limit found)
    ;; determine what lines to look in
    (save-excursion
      (save-restriction
        (widen)
        (cond ((> line-limit 0)
               (goto-char (setq start (point-min)))
               (forward-line line-limit)
               (setq search-limit (point)))
              ((< line-limit 0)
               (goto-char (setq search-limit (point-max)))
               (forward-line line-limit)
               (setq start (point)))
              (t                        ;0 => no limit (use with care!)
               (setq start (point-min))
               (setq search-limit (point-max))))))

    ;; look in those lines
    (save-excursion
      (save-restriction
        (widen)
        (let ((re-string
               (concat "\\b" marker-string "[ \t]*:[ \t]*\\(.+\\)$")))
          (if (and start
                   (< (goto-char start) search-limit)
                   (re-search-forward re-string search-limit 'move))

              (buffer-substring-no-properties
               (match-beginning 1)
               (match-end 1))))))))



(defun vbnet-flymake-get-cmdline (source base-dir)
  "Gets the cmd line for running a flymake session in a VB.NET buffer.
This gets called by flymake itself.

The fn looks in the buffer for a line that looks like:

  flymake-command: <command goes here>

  (It should be embedded into a comment)

Typically the command will be a line that runs nmake.exe,
msbuild.exe, or vbc.exe, with various options. It should
eventually run the VB.NET compiler, or something else that emits
error messages in the same form as the VB.NET compiler.

In general, you should use a target type of \"module\" (eg,
/t:module) to allow vbnet-flymake to clean up the products of the
build.

See `vbnet-cmd-line-limit' for a way to restrict where vbnet-mode
will search for the command.

If this string is not found, then this fn will fallback to a
generated vbc.exe command."
  (let ((explicitly-specified-command
         (vbnet-get-value-from-comments "flymake-command" vbnet-cmd-line-limit)))
    (cond
     (explicitly-specified-command
      ;; the marker string was found in the buffer
      (let ((tokens (vbnet-split-string-respecting-quotes explicitly-specified-command)))
        ;; implicitly append the name of the temporary source file
        (list (car tokens) (append (cdr tokens) (list flymake-temp-source-file-name)))))

     (t
      ;; fallback
      (list "vbc.exe"
            (append (vbnet-flymake-get-final-vbc-arguments
                     vbnet-flymake-vbc-arguments)
                    (list source)))))))



(defun vbnet-flymake-get-final-vbc-arguments (initial-arglist)
  "Gets the arguments used by VBC.exe for flymake runs.
This may inject a /t:module into an arglist, where it is not present.

It burps if a different /t argument is found."
  (interactive)
  (let ((args initial-arglist)
        arg
        (found nil))
    (while args
      (setq arg (car args))
      (cond
       ((string-equal arg "/t:module") (setq found t))
       ((string-match "^/t:" arg)
        (setq found t)
        (message "vbnet-mode: WARNING /t: option present in arglist, and not /t:module; fix this.")))

      (setq args (cdr args)))

    (setq args
          (if found
              initial-arglist
            (append (list "/t:module") initial-arglist)))

    (if (called-interactively-p 'any)
        (message "result: %s" (prin1-to-string args)))

    args))



(defvar vbnet-flymake-vbc-error-pattern
  "^[ \t]*\\([_A-Za-z0-9][^(]+\\.[Vv][Bb]\\)(\\([0-9]+\\)) : \\(\\(error\\|warning\\) BC[0-9]+:[ \t\n]*\\(.+\\)\\)"

  "Regexp to find error messages in the output of VBC.exe.  Used for Flymake integration.")


(defun vbnet-flymake-install ()
  "Change flymake variables and fns to work with VBNET.

This fn does four things:

1. add a VB.NET entry to the ‘flymake-allowed-file-name-masks’,
   or replace it if it already exists.

2. add a VB.NET entry to ‘flymake-err-line-patterns’.
   This isn't strictly necessary because of item #4.

3. redefine ‘flymake-process-sentinel’ to NOT check the process
   exit status.  Vbc.exe  returns a 1 when there are compile-time
   errors.  This causes flymake to disable itself, which we don't want.

4. provide advice to ‘flymake-parse-line’, specifically set up for
   VB.NET buffers.  This allows optimized searching for errors
   in vbc.exe output.

It's necessary to invoke this function only once, not every time
‘vbnet-mode’ is invoked.  ‘vbnet-mode’ uses `eval-after-load' to call it
once, after flymake has loaded."

  (flymake-log 2 "vbnet-flymake-install")

  ;; 1. add a VB.NET entry to the flymake-allowed-file-name-masks

  (let* ((key "\\.vb\\'")
         (vbnet-entry (assoc key flymake-allowed-file-name-masks)))
    (if vbnet-entry
        (setcdr vbnet-entry '(vbnet-flymake-init vbnet-flymake-cleanup))
      (add-to-list
       'flymake-allowed-file-name-masks
       (list key 'vbnet-flymake-init 'vbnet-flymake-cleanup))))


  ;; 2. add a VB.NET entry to flymake-err-line-patterns
  ;;
  ;; The value of each entry is a list, (STRING IX1 IX2 IX3 IX4), where
  ;; STRING is the regex, and the other 4 values are indexes into the
  ;; regex captures for the filename, line, column, and error text,
  ;; respectively.
  (add-to-list
   'flymake-err-line-patterns
   (list vbnet-flymake-vbc-error-pattern 1 2 nil 3))


  ;; 3.  override the definition for flymake-process-sentinel
  ;;
  ;; DPC - 2011 Feb 26
  ;; Redefining a function is a bit unusual, but I think it is necessary
  ;; to remove the check on process exit status.  For VBC.exe, it gives
  ;; a 1 status when compile errors result. This means flymake turns
  ;; itself off, which we don't want. This really ought to be tunable in
  ;; flymake, but I guess no one asked for that feature yet.
  (defun flymake-process-sentinel (process event)
    "Sentinel for syntax check buffers."
    (when (memq (process-status process) '(signal exit))
      (let* ((exit-status       (process-exit-status process))
             (command           (process-command process))
             (source-buffer     (process-buffer process))
             (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer))))

        (flymake-log 2 "process %d exited with code %d"
                     (process-id process) exit-status)
        (condition-case err
            (progn
              (flymake-log 3 "cleaning up using %s" cleanup-f)
              (when (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (funcall cleanup-f)))

              (delete-process process)
              (setq flymake-processes (delq process flymake-processes))

              (when (buffer-live-p source-buffer)
                (with-current-buffer source-buffer

                  (flymake-parse-residual)
                  ;;(flymake-post-syntax-check exit-status command)
                  (flymake-post-syntax-check 0 command)
                  (setq flymake-is-running nil))))
          (error
           (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                                  source-buffer (error-message-string err))))
             (flymake-log 0 err-str)
             (with-current-buffer source-buffer
               (setq flymake-is-running nil))))))))


  ;; 4. define some advice for the error parsing
  (defadvice flymake-parse-line (around
                                 flymake-for-vbnet-parse-line-patch
                                 activate compile)
    ;; This advice will run in all buffers.  Let's may sure we
    ;; actually execute the important stiff only when a VB buffer is active.
    (if (string-match "\\.[Vv][Bb]$"  (file-relative-name buffer-file-name))

        (let (raw-file-name
              e-text
              result
              (pattern (list vbnet-flymake-vbc-error-pattern 1 2 nil 3))
              (line-no 0)
              (err-type "e"))
          (if (string-match (car pattern) line)
              (let* ((file-idx (nth 1 pattern))
                     (line-idx (nth 2 pattern))
                     (e-idx (nth 4 pattern)))
                (flymake-log 3 "parse line: fx=%s lx=%s ex=%s"
                             file-idx line-idx e-idx)
                (setq raw-file-name (if file-idx (match-string file-idx line) nil))
                (setq line-no       (if line-idx (string-to-number (match-string line-idx line)) 0))
                (setq e-text      (if e-idx
                                      (match-string e-idx line)
                                    (flymake-patch-e-text (substring line (match-end 0)))))
                (or e-text (setq e-text "<no error text>"))
                (if (and e-text (string-match "^[wW]arning" e-text))
                    (setq err-type "w"))
                (flymake-log 3 "parse line: fx=%s/%s lin=%s/%s text=%s"
                             file-idx raw-file-name
                             line-idx line-no
                             e-text)
                (setq ad-return-value
                      (flymake-ler-make-ler raw-file-name line-no err-type e-text nil nil))
                )))

      ;; else - not in a vb.net buffer
      ad-do-it)))


;; ========================================================================
;; compile integration


(defun vbnet-guess-compile-command ()
  "set `compile-command' intelligently depending on the
current buffer, or the contents of the current directory."
  (interactive)
  (set (make-local-variable 'compile-command)

       (cond
        ((or (file-expand-wildcards "*.csproj" t)
             (file-expand-wildcards "*.vcproj" t)
             (file-expand-wildcards "*.vbproj" t)
             (file-expand-wildcards "*.shfbproj" t)
             (file-expand-wildcards "*.sln" t))
         "msbuild ")

        ;; sometimes, not sure why, the buffer-file-name is
        ;; not set.  Can use it only if set.
        (buffer-file-name
         (let ((filename (file-name-nondirectory buffer-file-name)))
           (cond

            ;; editing a vb file - check for an explicitly-specified command
            ((string-equal (substring buffer-file-name -3) ".vb")
             (let ((explicit-compile-command
                    (vbnet-get-value-from-comments "compile" vbnet-cmd-line-limit)))
               (or explicit-compile-command
                   (concat "nmake " ;; assume a makefile exists
                           (file-name-sans-extension filename)
                           ".exe"))))

            ;; something else - do a typical .exe build
            (t
             (concat "nmake "
                     (file-name-sans-extension filename)
                     ".exe")))))
        (t
         ;; punt
         "nmake "))))


(defun vbnet-invoke-compile-interactively ()
  "Wrapper for the `compile' function.  This simply
checks to see if `compile-command' has been previously set, and
if not, invokes `vbnet-guess-compile-command' to set the value.
Then it invokes the `compile' function, interactively.

The effect is to guess the compile command only once, per buffer.

I tried doing this with advice attached to the `compile'
function, but because of the interactive nature of the fn, it
didn't work the way I wanted it to. So this fn should be bound to
the key sequence the user likes for invoking compile, like ctrl-c
ctrl-e."
  (interactive)
  (cond
   ((not (boundp 'vbnet-local-compile-command-has-been-set))
    (vbnet-guess-compile-command)
    (set (make-local-variable 'vbnet-local-compile-command-has-been-set) t)))
  ;; local compile command has now been set
  (call-interactively 'compile))



(eval-after-load "compile"
  '(progn
     (let ((e1
            ;; VBC.exe example compiler error message:
            ;; c:\dev\dotnet\test\Module.vb(25) : error BC30451: Name 'arf' is not declared.
            '(msvbc
              "^[ \t]*\\([-_A-Za-z0-9][^\n(]*\\.vb\\)(\\([0-9]+\\)) ?: +\\(error\\|warning\\) BC[0-9]+:"
              1 2 nil)))
       (add-to-list 'compilation-error-regexp-alist-alist e1)
       (add-to-list 'compilation-error-regexp-alist (car e1)))))




;; ========================================================================
;; finally, the mode

(defun vbnet-mode ()
  "A mode for editing Microsoft Visual Basic .NET programs.
This is version 1.5 of the mode.

This mode features automatic indentation of VB.NET syntax, font
locking, keyword capitalization, integration with compile.el,
integration with flymake.el, integration with ya-snippet.el, and
some minor convenience functions.

As for those functions, here are some points of interest:

    `vbnet-mode-indent' - customizable variable setting the indent size,
         in spaces. The default is 4.

    `vbnet-mark-defun' marks the current function, if there is one.

    `vbnet-split-line' splits the current line at point, and inserts a
         continuation character.

    `vbnet-join-continued-lines' does the converse.

    `vbnet-new-sub' - inserts a subroutine template into the buffer at
         point.

    `vbnet-moveto-beginning-of-defun'
    `vbnet-moveto-end-of-defun'
    `vbnet-moveto-beginning-of-block'
    `vbnet-moveto-end-of-block'
         Functions to move within the VB.NET buffer. The first two
         move to the beginning and end, respectively, of a Function
         or Sub. The latter two move to the beginning and end,
         respectively, of the innermost containing block, whatever it
         is - a Function, Sub, Struct, Enum, While, etc.

    `vbnet-close-current-block' - intelligently closes a block, For
         example, it inserts \"End Class\" when invoked if point is
         after a Class declaration. This fn is naive: it
         will insert an \"End Class\" even if an \"End Class\" is present
         on the next line.

Consult the documentation for each of these functions for more
information.

For the syntax highlighting, it does not (yet?) support in-line
XML syntax, nor LINQ syntax.

Here's a summary of the key bindings:

\\{vbnet-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map vbnet-mode-map)
  (setq major-mode 'vbnet-mode)
  (setq mode-name "VB.NET")
  (set-syntax-table vbnet-mode-syntax-table)

  (add-hook 'local-write-file-hooks 'vbnet-untabify)

  (setq local-abbrev-table vbnet-mode-abbrev-table)
  (if vbnet-capitalize-keywords-p
      (progn
        (make-local-variable 'pre-abbrev-expand-hook)
        (add-hook 'pre-abbrev-expand-hook 'vbnet-pre-abbrev-expand-hook)
        (abbrev-mode 1)))

  (make-local-variable 'comment-start)
  (setq comment-start "' ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'vbnet-indent-line)

  ;; These vars are defined in lisp.el - not sure if
  ;; this is helpful. See the documentation on
  ;; `vbnet-moveto-beginning-of-defun' for why.
  (set (make-local-variable 'beginning-of-defun-function)
       'vbnet-moveto-beginning-of-defun)

  (set (make-local-variable 'end-of-defun-function)
       'vbnet-moveto-end-of-defun)

  ;;(make-local-variable 'vbnet-associated-files)
  ;; doing this here means we need not check to see if it is bound later.
  (add-hook 'find-file-hooks 'vbnet-load-associated-files)

  ;; compile
  (local-set-key "\C-x\C-e"  'vbnet-invoke-compile-interactively)


  (run-hooks 'vbnet-mode-hook)

  ;; post-hook setup.
  ;; Run this stuff here because it's conditional on things that may
  ;; have been modified in the hook functions.

  ;; imenu
  (if vbnet-want-imenu
      (progn
        ;; There are two ways to do imenu indexing. One is to provide a
        ;; function, via `imenu-create-index-function'.  The other is to
        ;; provide imenu with a list of regexps via
        ;; `imenu-generic-expression'; imenu will do a "generic scan" for you.
        ;; vbnet-mode uses the former method.
        ;;
        (setq imenu-create-index-function 'vbnet-imenu-create-index-function)
        (imenu-add-menubar-index)))

  ;; fontification
  (if vbnet-want-fontification
      (vbnet-enable-font-lock))

  ;; yasnippet
  (eval-after-load "yasnippet"
    (if vbnet-want-yasnippet-fixup
        (vbnet-fixup-yasnippet)))

  ;; flymake
  (eval-after-load "flymake"
    '(progn
       (if vbnet-want-flymake-fixup
           (vbnet-flymake-install))))


  )


(provide 'vbnet-mode)

;;; vbnet-mode.el ends here
