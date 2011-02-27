;;; vbnet-mode.el  --- A mode for editing Visual Basic .NET programs.
;;
;; Authors    : Fred White <fwhite@alum.mit.edu>
;;            : Dave Love <d.love@dl.ac.uk>
;;            : Kevin Whitefoot <kevin.whitefoot@nopow.abb.no>
;;            : T.K.Anderson
;;            : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : April 1996
;; Modified   : February 2011
;; Version    : 1.5
;; Keywords   : languages, basic, VB, VBNET
;; X-URL      : http://code.google.com/p/vbnetmode/
;; Last-saved : <2011-February-26 19:34:28>

;; Copyright (C) 1996 Fred White <fwhite@alum.mit.edu>
;; Copyright (C) 1998 Free Software Foundation, Inc.
;;   (additions by Dave Love and others)


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
;;   indentation, font locking, keyword capitalization, integration with
;;   compile.el, integration with flymake.el, integration with
;;   ya-snippet.el, and some minor convenience functions.

;; Installation instructions
;;
;;  Put vbnet-mode.el somewhere in your load path, optionally compile
;;  it, and add the following to your .emacs file:
;;
;;    (autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
;;    (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
;;                                  vbnet-mode)) auto-mode-alist))
;;
;;  And optionally, add something like this to your .emacs file as well:
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
;;     correct. It needed a space before the continuation char.
;;
;;     Fixed `vbnet-defun-start-regexp' to handle Public Shared Functions.
;;     Also renamed it to `vbnet-block-start-regexp' to reflect its
;;     true meaning, and renamed `vbnet-defun-end-regexp' similarly.
;;
;;     Put all the defconst regexps into an alist for simpler access.
;;
;;     Added "Namespace" as a keyword, and added a regexp and fn for
;;     handling namespace statements. Also modified `vbnet-calculate-indent'
;;     to properly handle namespaces and their children.
;;
;;     Enhanced the logic for fontifying, with changes to
;;     `vbnet-font-lock-keywords-1', so that things like variables,
;;     constructor invocation, import declarations, and using statements
;;     get fontified.
;;
;;     Removed keyword fontification of String, Trim, etc.  In VB.NET,
;;     these are no longer keywords. The whole list of keywords needs a
;;     thorough going-over. I think it is no longer necessary with
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
;;     See also the var, `vbnet-want-fixup-yasnippet'.
;;
;;     Integration with flymake.el .  Tweaks some defuns
;;     and vars to allow flymake to work with VB.NET. This happens only
;;     if flymake is in use. See also the var, `vbnet-want-fixup-flymake'.
;;
;;     Removed the find-matching-* fns, they were simple and called
;;     from only one place, so added nothing.
;;
;;     New function, `vbnet-join-continued-lines', a companion to
;;     `vbnet-split-line' Also, fixed the latter to work in an edge
;;     case.
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
;;    line continuation characters. (not sure what this means; if I did
;;    I would try to fix it.  -DPC 2011/Feb/26)
;;


;; todo?:
;;
;;  fwd/back-compound-statement
;;
;;  smart completion over object fields, methods, and properties.
;;
;;  IDE integration
;;
;;  Change behaviour of ESC-q to recognise words used as paragraph
;;  titles and prevent them being dragged into the previous
;;  paragraph.
;;
;;  etc.


;;; Code:

(defvar vbnet-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar vbnet-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar vbnet-win32-p (eq window-system 'w32))

;; Variables you may want to customize.
(defcustom vbnet-mode-indent 4
  "*Default indentation per nesting level."
    :type 'integer :group 'vbnet)

(defcustom vbnet-fontify-p t
  "*Whether to fontify Basic buffers."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-want-fixup-yasnippet t
  "*Whether to enable the builtin snippets for ya-snippet. This is meaningful
only if ya-snippet is available."
  :type 'boolean :group 'vbnet)

(defcustom vbnet-want-fixup-flymake t
  "*Whether to enable the builtin supprot for flymake. This is meaningful
only if flymake is loaded."
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
  "*Whether to allow single line if"
  :type 'boolean :group 'vbnet)


;; This is the kind of thing that is better done by a general-purpose
;; snippet package, customized to vbnet, and customized further for
;; the particular VBNET user.  ya-snippet.el is a good one.
(defvar vbnet-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
        "Public Function () As Variant\nEnd Function\n\n"
        "Public Property ()\nEnd Property\n\n")
  "*List of function templates though which vbnet-new-sub cycles.")


(defvar vbnet-imenu-generic-expression
  '((nil "^\\s-*\\(public\\|private\\)*\\s-+\\(declare\\s-+\\)*\\(sub\\|function\\)\\s-+\\(\\sw+\\>\\)"
         4)
    ("Constants"
     "^\\s-*\\(private\\|public\\|global\\)*\\s-*\\(const\\s-+\\)\\(\\sw+\\>\\s-*=\\s-*.+\\)$\\|'"
     3)
    ("Variables"
     "^\\(private\\|public\\|global\\|dim\\)+\\s-+\\(\\sw+\\>\\s-+as\\s-+\\sw+\\>\\)"
     2)
    ("Types" "^\\(public\\s-+\\)*type\\s-+\\(\\sw+\\)" 2)))



(defvar vbnet-mode-syntax-table nil)
(if vbnet-mode-syntax-table
    ()
  (setq vbnet-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" vbnet-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" vbnet-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" vbnet-mode-syntax-table)
  (modify-syntax-entry ?\= "." vbnet-mode-syntax-table)
  (modify-syntax-entry ?\< "." vbnet-mode-syntax-table)
  (modify-syntax-entry ?\> "." vbnet-mode-syntax-table)) ; Make =, etc., punctuation so that dynamic abbreviations work properly


(defvar vbnet-mode-map nil)
(if vbnet-mode-map
    ()
  (setq vbnet-mode-map (make-sparse-keymap))
  (define-key vbnet-mode-map "\t" 'vbnet-indent-line)
  (define-key vbnet-mode-map "\r" 'vbnet-newline-and-indent)
  (define-key vbnet-mode-map "\M-\C-a" 'vbnet-beginning-of-defun)
  (define-key vbnet-mode-map "\M-\C-e" 'vbnet-end-of-defun)
  (define-key vbnet-mode-map "\M-\C-h" 'vbnet-mark-defun)
  (define-key vbnet-mode-map "\M-\C-\\" 'vbnet-indent-region)
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
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\|"
         "[Ss]tatic\\|"
         "[Ff]riend\\)"
         "[ \t]+"
         "\\([Ss]ub\\|"
         "[Ff]unction\\|"
         "[Pp]roperty\\|"
         "[Tt]ype\\|"
         "[Ee]num\\|"
         "[Cc]lass\\|"
         "[Mm]odule\\)"
         "[ \t]+"
         "\\([^ \t\(]+\\)" ;; name of thing
         "[ \t]*"
         "\(?"))  ;; optional open-paren

     `(block-end
       ,(concat
         "^[ \t]*[Ee]nd "
         "\\("
         "[Ss]ub\\|"
         "[Ff]unction\\|"
         "[Pp]roperty\\|"
         "[Tt]ype\\|"
         "[Ee]num\\|"
         "[Cc]lass\\|"
         "[Mm]odule\\)"))

     `(func-start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\|"
         "[Ss]tatic\\|"
         "[Ff]riend\\)"
         "[ \t]+"
         "\\([Ff]unction\\)"
         "[ \t]+"
         "\\([^ \t\(]+\\)" ;; name of func
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
         "\\([^ \t\(]+\\)" ;; name of sub
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
         "\\([^ \t\(]+\\)" ;; name of prop
         ))

     '(prop-end      "^[ \t]*[Ee]nd +[Pp]roperty")

     `(class-start
       ,(concat
         "^[ \t]*" ;; leading whitespace
         "\\([Pp]ublic\\(?: [Ss]hared\\)?\\|"
         "[Pp]rivate\\(?: [Ss]hared\\)?\\|"
         "[Ss]tatic\\)"
         "[ \t]+"
         "\\([Cc]lass\\)"
         "[ \t]+"
         "\\([^ \t\(]+\\)" ;; name of class
         "[ \t]*"))  ;; optional ws

     '(class-end      "^[ \t]*[Ee]nd +[Cc]lass")

     `(namespace-start
       ,(concat
         "^[ \t]*"
         "\\([Nn]amespace\\)"
         "[ \t]+"
         "\\([^ \t\(]+\\)" ;; name of ns
         "[ \t]*"))

     '(namespace-end   "^[ \t]*[Ee]nd[ \t]+[Nn]amespace\\b")

     '(if              "^[ \t]*#?[Ii]f[ \t]+.*[ \t_]+")
     '(ifthen          "^[ \t]*#?[Ii]f.+\\<[Tt]hen\\>\\s-\\S-+")
     '(else            "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
     '(endif           "[ \t]*#?[Ee]nd[ \t]*[Ii]f")
     '(continuation    "^.* _[ \t]*$")
     '(label           "^[ \t]*[a-zA-Z0-9_]+:$")
     '(select          "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
     '(case            "^[ \t]*[Cc]ase")
     '(select-end      "^[ \t]*[Ee]nd[ \t]+[Ss]elect")
     '(for             "^[ \t]*[Ff]or\\b")
     '(next            "^[ \t]*[Nn]ext\\b")
     '(do              "^[ \t]*[Dd]o\\b")
     '(loop            "^[ \t]*[Ll]oop\\b")
     '(while           "^[ \t]*[Ww]hile\\b")
     '(wend            "^[ \t]*[Ww]end\\b")
     '(with            "^[ \t]*[Ww]ith\\b")
     '(end-with        "^[ \t]*[Ee]nd[ \t]+[Ww]ith\\b")
     '(try             "^[ \t]*[Tr]ry\\b")
     '(catch           "^[ \t]*[Cc]atch\\b")
     '(finally         "^[ \t]*[Ff]inally\\b")
     '(end-try         "^[ \t]*[Ee]nd[ \t]+[Tt]ry\\b")
     '(class           "^[ \t]*[Cc]lass\\b")
     '(end-class       "^[ \t]*[Ee]nd[ \t]+[Cc]lass\\b")
     '(module          "^[ \t]*[Mm]odule\\b")
     '(end-module      "^[ \t]*[Ee]nd[ \t]+[Mm]odule\\b")
     '(using           "^[ \t]*[Uu]sing\\b")
     '(end-using       "^[ \t]*[Ee]nd[ \t]+[Uu]sing\\b")
     '(blank           "^[ \t]*$")
     '(comment         "^[ \t]*\\s<.*$")

     '(propget-start   "^[ \t]*[Gg]et[ \t]*$")
     '(propget-end     "^[ \t]*[Ee]nd[ \t]+[Gg]et\\b")

     '(propset-start   "^[ \t]*[Ss]et[ \t]*(")
     '(propset-end     "^[ \t]*[Ee]nd[ \t]+[Ss]et\\b")


     ;; =======================================================
     ;; the following elements are used for fontification, only.

     '(funcall         "\\b\\([[:alpha:]_][[:alnum:]_.]+\\)[ \t]*(")

     '(import          "^[ \t]*[Ii]mports[ \t]+\\([[:alpha:]_][[:alnum:]_.]+\\)[ \t]*$")

     ;; some of these regexps match on partial lines.
     ;; The Dim regexp is one example.  This allows the AS fragment and
     ;; its following typename to be fontified independently.

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
         "\\([[:alpha:]_][[:alnum:]_.]+\\)" ;; variable name
         "[ \t]*"                 ;; optional white space
         "\\(?:([^)]*)\\)?"       ;; optional array dimension (no capture)
         "[ \t]*"                 ;; optional white space
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

     `(constant   "\\b\\([1-9][0-9.]*\\|[0-9]\\)\\b")

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


(make-face 'vbnet-namespace-face)
(set-face-foreground 'vbnet-namespace-face "DarkSalmon")
(defvar vbnet-namespace-face 'vbnet-namespace-face
  "Face name to use for namespace names (in the Imports statement)
in VB.NET buffers.")

(make-face 'vbnet-funcall-face)
(set-face-foreground 'vbnet-funcall-face "grey")
(defvar vbnet-funcall-face 'vbnet-funcall-face
  "Face name to use for function calls in VB.NET buffers.")

;; (make-face 'vbnet-diagnostic-face)
;; (set-face-foreground 'vbnet-diagnostic-face "chartreuse")
;; (defvar vbnet-diagnostic-face 'vbnet-diagnostic-face
;;   "Face name to use for diagnostic purposes in VB.NET buffers.")


;; logic for fontifying
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
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-type-face))

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


(defun vbnet-beginning-of-block ()
  (interactive)
  (re-search-backward (vbnet-regexp 'block-start)))

(defun vbnet-end-of-block ()
  (interactive)
  (re-search-forward (vbnet-regexp 'block-end)))

(defun vbnet-mark-defun ()
  (interactive)
  (beginning-of-line)
  (vbnet-end-of-defun)
  (set-mark (point))
  (vbnet-beginning-of-defun)
  (if vbnet-xemacs-p
      (zmacs-activate-region)))

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
  "Insert template for a new subroutine.  Repeat to cycle through alternatives."
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
  "Do not allow any tabs into the file."
  (if (eq major-mode 'vbnet-mode)
      (untabify (point-min) (point-max)))
  nil)



(defun vbnet-default-tag ()
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
   (list (let* ((def (vbnet-default-tag))
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

See also `vbnet-indent-line'.

"
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
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at (vbnet-regexp 'blank))
                  (looking-at (vbnet-regexp 'comment))))
    (forward-line -1)))


(defun vbnet-find-original-statement ()
  "If the current line is a continuation, move back to the original stmt."
  (let ((here (point)))
    (vbnet-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at (vbnet-regexp 'continuation)))
      (setq here (point))
      (vbnet-previous-line-of-code))
    (goto-char here)))

(defun vbnet-find-matching-stmt (open-regexp close-regexp)
  "Search backwards to find a matching statement. Attempts to properly
handle nested blocks. "
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (vbnet-previous-line-of-code)
      (vbnet-find-original-statement)
      (cond ((looking-at close-regexp)
             (setq level (+ level 1)))
            ((looking-at open-regexp)
             (setq level (- level 1)))))))


(defun vbnet-calculate-indent ()
  "Calculates the indentation for the current point in a vb.net buffer."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond

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
        (vbnet-find-matching-try)
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
          (vbnet-find-original-statement)
          ;; Indent continuation line under matching open paren,
          ;; or else one word in.
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
            (cond (matching-open-paren
                   (1+ matching-open-paren))
                  (t
                   ;; Else, after first word on original line.
                   (back-to-indentation)
                   (forward-word 1)
                   (while (looking-at "[ \t]")
                     (forward-char 1))
                   (current-column)))))

         (t
          (vbnet-find-original-statement) ;; why?

          (let ((indent (current-indentation)))
            ;; All the various +indent regexps.
            (cond

             ((looking-at (vbnet-regexp 'block-start))
              (+ indent vbnet-mode-indent))

             ((or (looking-at (vbnet-regexp 'class-start))
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
  "Indent current line for Visual Basic syntax.  This assumes that the
previous non-blank line is indented properly.

See also `vbnet-indent-region'.
"
  (interactive)
  (vbnet-indent-to-column (vbnet-calculate-indent)))



;; ========================================================================
;; line continuation helpers

(defun vbnet-split-line ()
  "Split line at point, adding continuation character or continuing
a comment. In Abbrev mode, any abbrev before point will be expanded.

See also `vbnet-join-continued-lines'

"
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

The file must have a local variable that lists the files to be
loaded.  If the file name is relative it is relative to the
directory containing the current buffer.  If the file is already
loaded nothing happens, this prevents circular references causing
trouble.  After an associated file is loaded its associated files
list will be processed."
  (if (boundp 'vbnet-associated-files)
      (let ((files vbnet-associated-files)
            (file nil))
        (while files
          (setq file (car files)
                files (cdr files))
          (message "Load associated file: %s" file)
          (vbnet-load-file-ifnotloaded file default-directory)))))



(defun vbnet-load-file-ifnotloaded (file default-directory)
  "Load file if not already loaded.
If file is relative then default-directory provides the path"
  (let((file-absolute (expand-file-name file default-directory)))
    (if (get-file-buffer file-absolute) ; don't do anything if the buffer is already loaded
        ()
      (find-file-noselect file-absolute ))))


;; ========================================================================
;; YA-snippet integration

(defun vbnet-fixup-yasnippet ()
  "Sets snippets into ya-snippet for VB.NET, if they do not already exist.
"
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


        ;; TODO: Fix. when vbnet-mode is loaded after yasnippet,
        ;; vbnet-mode does not show up on the yasnippet menu.
        ;; This code naively attempts to insert it manually.
        ;; To be correct, Should check to see if it is already there.

        (when yas/use-menu
          (define-key
            yas/menu-keymap
            (vector 'vbnet-mode)
            `(menu-item "VB.Net" ,keymap)))

        ;; insert those snips into the table if they
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
         builtin-snips))))

(eval-after-load "yasnippet"
  (if vbnet-want-fixup-yasnippet
      (vbnet-fixup-yasnippet)))


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

    (setq args (flymake-get-syntax-check-program-args
                temp-source-file-name "."
                use-relative-base-dir use-relative-source
                get-cmdline-f))
    args))


(defun vbnet-flymake-cleanup ()
  "Delete the temporary .netmodule file created in syntax checking,
then call through to flymake-simple-cleanup."
  (if flymake-temp-source-file-name
  (let* ((netmodule-name
          (concat (file-name-sans-extension flymake-temp-source-file-name)
                              ".netmodule"))
         (expanded-netmodule-name (expand-file-name netmodule-name ".")))
    (if (file-exists-p expanded-netmodule-name)
        (flymake-safe-delete-file expanded-netmodule-name))))
    (flymake-simple-cleanup))

(defvar vbnet-flymake-vbc-arguments
  (list "/t:module" "/nologo")
  "A list of arguments to use with the vbc.exe
compiler, when using flymake with a
direct vbc.exe build for syntax checking purposes.")


(defun vbnet-flymake-get-cmdline (source base-dir)
  "Gets the cmd line for running a flymake session in a VB.NET buffer.

If this fn were smart it would parse the comment header at the top of the file
to look for a string that specifies the flymake command line to use.

It isn't (yet) that smart.

"
  ;; this should be a fallback
  (list "vbc.exe"
        (append (vbnet-flymake-get-final-vbc-arguments) (list source))))


(defun vbnet-flymake-get-final-vbc-arguments ()
  "Gets the arguments used by VBC.exe for flymake runs. "
  vbnet-flymake-vbc-arguments )

(defvar vbnet-vbc-error-pattern
  "^[ \t]*\\([_A-Za-z0-9][^(]+\\.[Vv][Bb]\\)(\\([0-9]+\\)) : \\(\\(error\\|warning\\) BC[0-9]+:[ \t\n]*\\(.+\\)\\)"

  "Regexp to find error messages in the output of VBC.exe")


(defun vbnet-flymake-install ()
  "Change flymake variables and fns to work with VBNET.

This fn does four things:

1. add a VB.NET entry to the flymake-allowed-file-name-masks,
   or replace it if it already exists.

2. add a VB.NET entry to flymake-err-line-patterns.
   This isn't strictly necessary because of item #4.

3. redefine flymake-process-sentinel to NOT check the process
   exit status.  Vbc.exe  returns a 1 when there are compile-time
   errors. This causes flymake to disable itself, which we don't want.

4. provide advice to flymake-parse-line, specifically set up for
   VB.NET buffers.  This allows optimized searching for errors
   in vbc.exe output.

It's necessary to invoke this function only once, not every time
vbnet-mode is invoked. vbnet-mode uses `eval-after-load' to call it
once, after flymake has loaded.
"

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
   (list vbnet-vbc-error-pattern 1 2 nil 3))


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
                                 flymake-for-csharp-parse-line-patch
                                 activate compile)
    ;; This advice will be used in all buffers.  Let's may sure we
    ;; actually execute it only when a VB buffer is active.
    (if (string-match "\\.[Vv][Bb]$"  (file-relative-name buffer-file-name))

        (let (raw-file-name
              e-text
              result
              (pattern (list vbnet-vbc-error-pattern 1 2 nil 3))
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
                      (flymake-ler-make-ler raw-file-name line-no err-type e-text nil 0))
                )
            ;; else
            ad-do-it))))
  )

(eval-after-load "flymake"
  '(progn
     (if vbnet-want-fixup-flymake
         (vbnet-flymake-install))))


;; ========================================================================
;; compile integration

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

This mode features automatic indentation, font locking, keyword
capitalization, integration with compile.el, integration with
flymake.el, integration with ya-snippet.el, and some minor
convenience functions.

Commands:
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

  (if vbnet-fontify-p
      (vbnet-enable-font-lock))

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression vbnet-imenu-generic-expression)

  (set (make-local-variable 'imenu-syntax-alist)
       `((,(string-to-char "_") . "w")))
  (set (make-local-variable 'imenu-case-fold-search) t)

  ;;(make-local-variable 'vbnet-associated-files)
  ;; doing this here means we need not check to see if it is bound later.
  (add-hook 'find-file-hooks 'vbnet-load-associated-files)

  (run-hooks 'vbnet-mode-hook))



(provide 'vbnet-mode)

;;; vbnet-mode.el ends here

