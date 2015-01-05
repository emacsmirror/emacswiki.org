;;; cc-mode+.el --- Extensions to `c-mode.el' & `cc-mode.el'.
;;
;; Filename: cc-mode+.el
;; Description: Extensions to `c-mode.el' & `cc-mode.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2015, Drew Adams, all rights reserved.
;; Created: Mon Aug 30 13:01:25 1999
;; Version: 0
;; Last-Updated: Thu Jan  1 10:26:41 2015 (-0800)
;;           By: dradams
;;     Update #: 76
;; URL: http://www.emacswiki.org/cc-mode+.el
;; Keywords: c, c++, programming
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `cc-align', `cc-cmds', `cc-defs', `cc-engine', `cc-langs',
;;   `cc-menus', `cc-mode', `cc-styles', `cc-vars', `custom',
;;   `derived', `easymenu', `imenu', `imenu+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `c-mode.el' & `cc-mode.el'.
;;
;;
;; Note: This code is quite OLD, and is likely OBSOLETE/USELESS now.
;;       You might find it useful in some way to mine - or not. ;-)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/10/23 dadams
;;     Applied renaming: imenu-add-defs-to-menubar to imenup-add-defs-to-menubar.
;; 2007/05/18 dadams
;;     Require cl.el only at compile time, and only for Emacs < 20.
;;
;; RCS $Log: cc-mode+.el,v $
;; RCS Revision 1.6  2001/01/08 22:28:26  dadams
;; RCS Adapted file header for Emacs Lisp Archive.
;; RCS
;; RCS Revision 1.5  2001/01/03 17:31:35  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.4  2001/01/03 00:33:46  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.3  2001/01/02 23:26:08  dadams
;; RCS Optional require of imenu+.el via 3rd arg=t now.
;; RCS
;; RCS Revision 1.2  2000/11/01 15:41:07  dadams
;; RCS Put imenu-add-defs-to-menubar inside condition-case, in c-mode-common-hook.
;; RCS
;; RCS Revision 1.1  2000/09/13 20:06:14  dadams
;; RCS Initial revision
;; RCS
; Revision 1.3  1999/08/30  13:15:44  dadams
; Added: cc-imenu-c-generic-expression, cc-imenu-c++-generic-expression.
;
; Revision 1.2  1999/08/30  12:13:40  dadams
; *** empty log message ***
;
; Revision 1.1  1999/08/30  11:53:00  dadams
; Initial revision
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (when (< emacs-major-version 20)(require 'cl))) ;; cddr, when
(require 'cc-mode)
(require 'imenu+ nil t) ;; (no error if not found): imenu-add-defs-to-menubar

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq imenup-generic-expression
                   c-imenu-generic-expression)
             (condition-case nil
                 (imenu-add-defs-to-menubar) ; no error if not defined
               (error nil))))

;; This is the expression for C++ mode, but it's used for C too.
(defvar c-imenu-generic-expression
  (`
   ((nil
     (,
      (concat
       "^"                              ; beginning of line is required
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"    ; type specs; there can be no
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"    ; more than 3 tokens, right?

       "\\("                            ; last type spec including */&
       "[a-zA-Z0-9_:]+"
       "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
       "\\)?"                           ; if there is a last type spec
       "\\("                            ; name; take that into the imenu entry
       "[a-zA-Z0-9_:~]+"                ; member function, ctor or dtor...
                                        ; (may not contain * because then
                                        ; "a::operator char*" would become "char*"!)
       "\\|"
       "\\([a-zA-Z0-9_:~]*::\\)?operator"
       "[^a-zA-Z1-9_][^(]*"             ; ...or operator
       " \\)"
       "[ \t]*([^)]*)[ \t\n]*[^         ;]" ; require something other than a ; after
                                        ; the (...) to avoid prototypes.  Can't
                                        ; catch cases with () inside the parentheses
                                        ; surrounding the parameters
                                        ; (like "int foo(int a=bar()) {...}"

       )) 6)
    ("Class"
     (, (concat
         "^"                            ; beginning of line is required
         "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
         "class[ \t]+"
         "\\([a-zA-Z0-9_]+\\)"          ; this is the string we want to get
         "[ \t]*[:{]"
         )) 2)
    ;; For finding prototypes, structs, unions, enums.
    ("Prototypes"
     (,
      (concat
       "^"                              ; beginning of line is required
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"    ; type specs; there can be no
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"    ; more than 3 tokens, right?

       "\\("                            ; last type spec including */&
       "[a-zA-Z0-9_:]+"
       "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
       "\\)?"                           ; if there is a last type spec
       "\\("                            ; name; take that into the imenu entry
       "[a-zA-Z0-9_:~]+"                ; member function, ctor or dtor...
                                        ; (may not contain * because then
                                        ; "a::operator char*" would become "char*"!)
       "\\|"
       "\\([a-zA-Z0-9_:~]*::\\)?operator"
       "[^a-zA-Z1-9_][^(]*"             ; ...or operator
       " \\)"
       "[ \t]*([^)]*)[ \t\n]*           ;"      ; require ';' after
                                        ; the (...) Can't
                                        ; catch cases with () inside the parentheses
                                        ; surrounding the parameters
                                        ; (like "int foo(int a=bar());"
       )) 6)
    ("Struct"
     (, (concat
         "^"                            ; beginning of line is required
         "\\(static[ \t]+\\)?"          ; there may be static or const.
         "\\(const[ \t]+\\)?"
         "struct[ \t]+"
         "\\([a-zA-Z0-9_]+\\)"          ; this is the string we want to get
         "[ \t]*[{]"
         )) 3)
    ("Enum"
     (, (concat
         "^"                            ; beginning of line is required
         "\\(static[ \t]+\\)?"          ; there may be static or const.
         "\\(const[ \t]+\\)?"
         "enum[ \t]+"
         "\\([a-zA-Z0-9_]+\\)"          ; this is the string we want to get
         "[ \t]*[{]"
         )) 3)
    ("Union"
     (, (concat
         "^"                            ; beginning of line is required
         "\\(static[ \t]+\\)?"          ; there may be static or const.
         "\\(const[ \t]+\\)?"
         "union[ \t]+"
         "\\([a-zA-Z0-9_]+\\)"          ; this is the string we want to get
         "[ \t]*[{]"
         )) 3)
    ))
  "*Imenu generic expression for C mode.  See `imenu-generic-expression'.")

(defvar cc-imenu-c-generic-expression c-imenu-generic-expression
  "*Imenu generic expression for C mode.  See `imenu-generic-expression'.")

(defvar cc-imenu-c++-generic-expression c-imenu-generic-expression
  "*Imenu generic expression for C++ mode.  See `imenu-generic-expression'.")


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cc-mode+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cc-mode+.el ends here
