;;; any-ini-mode.el --- keyword highlighting for .ini files etc based on a 'source of truth'

;;; Copyright (C) 2002, 2003 Robert Fitzgerald <robert.fitzgerald@t-online.de>

;;; Author: Robert Fitzgerald <robert.fitzgerald@t-online.de>
;;; Created: Mar 2003
;;; Version: 1.0.4
;;; Keywords: convenience 

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.

;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program ; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; Commentary:
 
;;; When a file is visted in <any>.ini mode the _valid_ section and parameter names
;;; are highlighted as keywords, the comments are highlighted as comments and everything 
;;; else is displayed in your normal font. 

;;; The list of valid section and parameter names is built dynamically, based on a 
;;; canonical file ('source of truth') that contains all valid options.

;;; In this way, you can easily spot a mis-typed name when you're editting your files, 
;;; rather than having to wait for your application to misbehave.

;;; It's also useful if, like me, you have users who can't spell 'log_file_path' or
;;; colleagues who insist on creating parameters called 'defaulttofirstbackupdirectory'.
 
;;; You may also define, among other things, a valid comment character and a
;;; valid assignment charcter.

;;; Collectively, the definition of a canonical file and a comment character etc 
;;; define a 'style' of .ini file.
;;;
;;; You may set up a default style for all <any>.ini mode buffers, or, more usefully,
;;; you may set up several styles that will be automatically applied, based on the name
;;; of the file being visited.

;;; any-ini-mode now includes support for `imenu'.
;;;
;;; `imenu-generic-expression' is set up for each buffer to find the section names
;;; and, by default, a menu of section names is automatically added to the menubar.
;;;
;;; If you setup `speedbar' correctly, this means that you can also navigate the 
;;; sections with `speedbar'.
;;;
;;; See the `any-ini-imenu' customization group and the documentation for `imenu'
;;; and `speedbar' for more details.

;;; Finally, you may want to try running the `any-ini-toggle-errorcheck-mode' command.
;;; See the docstring for this command for an explantion.

;;; Customization is via the `any-ini' group in the `local' customization section.

;;;
;;; To load any-ini-mode on startup, copy this file to your load-path and add this to 
;;; your .emacs file -
;;;
;;;    (require 'any-ini-mode)
;;;
;;; You may also want to specify that <any>.ini mode should apply by default
;;; to all .ini and .conf files, for example. Here's how -
;;;
;;;    (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
;;;    (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode))
;;;

;;; Example:
;;;
;;; You deal with three applications - larry, mo and curly.
;;; Each requires a config file - larry.conf, mo.ini and .curlyrc - and none of the parameter
;;; names that are valid in a mo.ini file are valid in a larry.conf or a .curlyrc, etc. etc.
;;; 
;;; No problem.
;;;
;;; First, create a canonical config file for each app (the names of these files are unimportant).
;;; This is simply a template containing all the valid sections and parameters.
;;; eg.
;;;
;;; --------------------------------------------------------------------------------------
;;; | # canon.larry.conf - canonical config file for the larry app                       |
;;; | # this file is maintained by the larry developers in ~devel/larry/etc              |
;;; | # Applies to larry v6.6.6 and up                                                   |
;;; | # Last updated 12-24-2001 by groucho                                               |
;;; |                                                                                    |
;;; | [USER]                                                                             |
;;; | editor =                                                                           |
;;; | #group =           # deprecated v6.6.6                                             |
;;; | home =                                                                             |
;;; |                                                                                    |
;;; | [BACKUP]                                                                           |
;;; | path =                                                                             |
;;; | always =           #                                                               |
;;; | never =            # NB - only one of these should be set TRUE in a live file      |
;;; | sometimes =        #                                                               |
;;; |                                                                                    |
;;; --------------------------------------------------------------------------------------
;;;
;;; Next, create 3 styles (call them larry.conf, mo.ini and .curlyrc) in `any-ini-styles-alist' 
;;; and set the canonical file for each type.  You can also specify, for example, that a mo.ini
;;; uses `<' and `>' to bracket section names and that a .curlyrc uses `:' as its assignment 
;;; character.
;;;
;;; Finally, add this to your .emacs file -
;;; 
;;;    (require 'any-ini-mode) 
;;;    (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
;;;    (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode))
;;;    (add-to-list 'auto-mode-alist '("\\.curlyrc" . any-ini-mode))
;;;

;;; Change Log:

;;; Changes from 1.0.3 to 1.0.4:

;;; * Improved doc strings for `any-ini-param-name-regexp' and `any-ini-section-name-regexp'
;;;   Based on feedback from Dave Pearson and others

;;; Changes from 1.0.2 to 1.0.3:

;;; * Fixed up for use with `describe-mode'.
;;;   Thanks to Vagn Johansen

;;; Changes from 1.0.1 to 1.0.2:

;;; * Add an `imenu' menu for section names
;;;   See `any-ini-setup-imenu' for details.

;;; Changes from 1.0.0 to 1.0.1:

;;; * Use new `any-ini-regexp-opt' function in place of standard `regexp-opt' function.
;;;   Necessary because of occasional memory problems with `regexp-opt' during parsing 
;;;   of very large (1500+ lines) files.

;;; Customization:

(defgroup any-ini nil
  "Major mode for editing .ini files with customizable keyword highlighting."
  :tag "<any>.ini"
  :group 'local)

(defgroup any-ini-faces nil
  "Faces for <any>.ini mode.

Comments use `font-lock-comment-face'."
  :tag "<any>.ini faces"
  :group 'any-ini)

(defgroup any-ini-imenu nil
  "<any>.ini interaction with `imenu'.

See Info Node `(emacs)Imenu' for more details."
  :tag "<any>.ini imenu"
  :group 'any-ini)

(defcustom any-ini-canonical-ini-files '(("~/.any.ini.canon" nil))
  "*Alist of files to be parsed for valid section and parameter names.

Each description has the form (FILENAME TYPE).

Files may be parsed in three ways, based on TYPE :

:is-canonical-file (nil)

The file will be treated as a canonical file.

It will be parsed twice, once to extract the section names and once
to extract the parameter names.

The section names will be extracted using `any-ini-section-start-chars',
`any-ini-section-name-regexp' and `any-ini-section-end-chars'.

The parameter names will be extracted using `any-ini-param-name-regexp'.
Parameter names will be extracted only if they are followed by an
assignment character (see `any-ini-assignment-chars'), possibly with
intervening whitespace. It is not necessary, however, for the parameters
to have any values assigned.

:is-list-of-section-names (1)

The file will be treated as a simple list of valid section names.
The first whitespace-delimited word on each line will be checked against
`any-ini-section-name-regexp' and those that pass will be extracted.

:is-list-of-paramter-names (2)

The file will be treated as a simple list of valid parameter names.
The first whitespace-delimited word on each line will be checked against
`any-ini-param-name-regexp' and those that pass will be extracted.

In all cases,  whitespace is ignored and `any-ini-comment-start-chars' is
used to disregard comments."
  :group 'any-ini
  :type '(repeat (list :tag "Canonical files" :value ("~/.any.ini.canon" nil)
		       (file  :tag "Filename")
		       (choice :tag "Type" 
			       (const :tag "Canonical file" nil)
			       (const :tag "List of section names" 1)
			       (const :tag "List of parameter names" 2)))))

(defcustom any-ini-comment-start-chars '( ?\# )
  "Characters that may start a comment.

Multi-line comments are not handled; a comment is assumed
to end with a linefeed or formfeed.

Default value is `#'."
  :group 'any-ini
  :type '(repeat :tag "Comment chars" (character :value ?\# :tag "Char")))

(defcustom any-ini-param-name-regexp "[-_A-Za-z0-9]+"
  "Regexp to describe valid parameter name.

This regexp is used only when a canonical file is parsed for valid
parameter names.

The actual keyword highlighting of parameter names is based on the
list of parameter names thus created, _not_ on this regexp.

The regexp should describe the characters that make up a valid
parameter _name_ only (eg. My_Param-11 ). The mode itself will
take care of handling any surrounding whitespace and/or assignment
character.

Default value is \"[-_A-Za-z0-9]+\".

See also `any-ini-assignment-chars'."
  :group 'any-ini
  :type '(regexp :tag "Params regexp" :value "[-_A-Za-z0-9]+"))

(defcustom any-ini-assignment-chars '( ?\= )
  "Characters that show the assignment of a value to a parameter.

Parameter names will be extracted from a canonical file only if they are
followed by an assignment character, possibly with intervening whitespace.
It is not necessary, however, for the parameter to have any values assigned.

The same rules apply to highlighting a parameter name in an `any-ini-mode'
buffer.

Default value is `='."
  :group 'any-ini
  :type '(repeat :tag "Assignment chars" (character :value ?\= :tag "Char")))

(defcustom any-ini-section-name-regexp "[-_A-Za-z0-9]+"
  "Regexp to describe valid section name.

This regexp is used only when a canonical file is parsed for valid
section names.

The actual keyword highlighting of section names is based on the
list of section names thus created, _not_ on this regexp.

This regexp should describe the characters that make up a valid
section _name_ only (eg. MY_SECTION-22 ). The mode itself will
take care of handling any surrounding brackets.

See `any-ini-section-start-chars' and `any-ini-section-end-chars' for 
the definition of the bracketting characters.

Default value is \"[-_A-Za-z0-9]+\"."
  :group 'any-ini
  :type '(regexp :tag "Sections regexp" :value "[-_A-Za-z0-9]+"))

(defcustom any-ini-section-start-chars '( ?\[ )
  "Characters that may begin a section heading.

NB - No attempt is made to pair-up the characters that start and end a
section heading.  It's assumed that a section heading that is begun
by a valid starting character may be ended by _any_ valid ending character. 

Default value is `['.

See also `any-ini-section-end-chars'."
  :group 'any-ini
  :type '(repeat :tag "Section start chars" (character :value ?\[ :tag "Char")))
 
(defcustom any-ini-section-end-chars '( ?\] )
  "Characters that may end a section heading.

NB - No attempt is made to pair-up the characters that start and end a
section heading.  It's assumed that a section heading that is begun
by a valid starting character may be ended by _any_ valid ending character. 

Default value is `]'.

See also `any-ini-section-start-chars'."
  :group 'any-ini
  :type '(repeat :tag "Section end chars" (character :value ?\]  :tag "Char")))

(defcustom any-ini-styles-alist nil
  "List of <any>.ini styles that may be used in preference to the default style.

Each item has the form -

\(STYLENAME CANON_FILES COMMENT_CHARS PARAM_REGEXP ASSIGNERS SECTION_REGEXP SECT_STARTERS SECT_ENDERS KEYWORDS)

STYLENAME: Each style list should be given a meaningful name. When a file is
visited in <any>.ini mode, `any-ini-styles-alist' will be searched for a STYLENAME
that matches the name of the file.  If a match is found, the style settings in
the list will be applied. If not, the default style will be used.

CANON_FILES: See the documentation for `any-ini-canonical-ini-files'
COMMENT_CHARS: See the documentation for `any-ini-comment-start-chars'
PARAM_REGEXP: See the documentation for `any-ini-param-name-regexp'
ASSIGNERS: See the documentation for `any-ini-assignment-chars'
SECTION_REGEXP: See the documentation for `any-ini-section-name-regexp'
SECT_STARTERS: See the documentation for `any-ini-section-start-chars'
SECT_ENDERS: See the documentation for `any-ini-section-end-chars'."
  :group 'any-ini
  :type '(repeat (list
	  (string :tag "Style name" :value "new.ini")
	  (repeat :tag "Canonical files" (list :value ("~/.new.ini.canon" nil)
			(file  :tag "Filename")
			(choice :tag "Type" 
				(const :tag "Canonical file" nil)
				(const :tag "List of section names" 1)
				(const :tag "List of parameter names" 2))))
	  (repeat :tag "Comment chars" (character :value ?\# :tag "Char"))
	  (regexp :tag "Params regexp" :value "[-_A-Za-z0-9]+")
	  (repeat :tag "Assignment chars" (character :value ?\= :tag "Char"))
	  (regexp :tag "Sections regexp" :value "[-_A-Za-z0-9]+")
	  (repeat :tag "Section start chars" (character :value ?\[ :tag "Char"))
	  (repeat :tag "Section end chars" (character :value ?\] :tag "Char" )))))

(defcustom any-ini-mode-mode-hook nil
  "Normal hook run when entering <any>.ini mode."
  :type 'hook
  :group 'any-ini)

(defcustom any-ini-param-face  'font-lock-function-name-face
  "*Font that <any>.ini mode will use to highlight parameter names."
  :type 'face
  :group 'any-ini-faces)

(defcustom any-ini-section-face  'font-lock-keyword-face
  "*Font that <any>.ini mode will use to highlight section names."
  :type 'face
  :group 'any-ini-faces)

(defcustom any-ini-assigner-face  'default
  "*Font that <any>.ini mode will use to highlight the assignment character."
  :type 'face
  :group 'any-ini-faces)

(defcustom any-ini-value-face  'default
  "*Font that <any>.ini mode will use to highlight parameter values."
  :type 'face
  :group 'any-ini-faces)

(defcustom any-ini-section-chars-face  'default
  "*Font that <any>.ini mode will use to highlight section names."
  :type 'face
  :group 'any-ini-faces)

(defcustom any-ini-imenu-show-flag t
  "Use `imenu' \(if available\) to add a menu of section names to menubar.

Non-nil means that a menu of section names will be automatically created
for each new <any>.ini mode buffer through a call to `imenu-add-to-menubar'.

The menu name is defined by `any-ini-imenu-name'.

`imenu-generic-expression' is _always_ initialised in <any>.ini mode
buffers, regardless of the state of this flag.  This means that the
standard `imenu' commands should always be available.

See Info Node `(emacs)Imenu' for more details."
  :group 'any-ini-imenu
  :type 'boolean)

(defcustom any-ini-imenu-name "Sections"
  "Name to use for menu of section names."
  :group 'any-ini-imenu
  :type 'string)

;;; Constants:

(defconst any-ini-buffer-errorcheck nil
  "Buffer-local version of this var indicates if a buffer is using
any-ini-errorcheck-mode.")

(defconst any-ini-style nil
  "Keyword highlighting style to apply to a file.

This variable should be set by `any-ini-set-my-style'.
Setting it directly won't have the desired effect. 

When `any-ini-style' is nil, the default style is applied.
See also `any-ini-set-my-style' and `any-ini-styles-alist'.

*NB - You may be tempted to set this as a file variable
eg.

;;   -*- mode: any-ini; any-ini-style: \"apache\" -*-

but, unfortunately, this won't have the desired affect either.
Hopefully, this technique will be available soon.

At the moment, the following _will_ work ...

;;   -*- mode: any-ini; eval: (any-ini-set-my-style \"apache\") -*-

... but then you'll have to deal with `enable-local-eval' etc.")

;;; Code:

(defun any-ini-set-my-style (&optional mystyleoverride)
  "*Set keyword highlighting style for a file.

MYSTYLEOVERRIDE is a string representing the name of a highlighting style.
If the style is found in `any-ini-styles-alist' it will be applied to the
current buffer.

If MYSTYLEOVERRIDE is `nil', the name of the currently-visited file will
be taken as the required style name and will be searched for in `any-ini-styles-alist'.

If a matching style is not found for MYSTYLEOVERRIDE or the filename, the 
default style will be applied.

If this function is called with a prefix argument and MYSTYLEOVERRIDE is `nil', 
the default style will be applied, regardless of name of the visited file.

The default style is the style described by the global values of -

`any-ini-canonical-ini-files', 
`any-ini-comment-start-chars', 
`any-ini-param-name-regexp'
`any-ini-assignment-chars', 
`any-ini-section-name-regexp', 
`any-ini-section-start-chars'
`any-ini-section-end-chars'
"
  (interactive
   (if any-ini-styles-alist
       (list 
	(completing-read "Apply any-ini-style :" any-ini-styles-alist nil t))
     (message "any-ini-mode : No styles defined.")
     nil))
  (if (not (eq major-mode 'any-ini-mode))
      (message "Major mode is not any-ini-mode.")
    (make-local-variable 'any-ini-style)
    (if (and current-prefix-arg (string= mystyleoverride ""))
	(setq any-ini-style nil)
      (if (string= mystyleoverride "")
	  (setq any-ini-style nil)
	(setq any-ini-style mystyleoverride))
      (if (and any-ini-style (stringp any-ini-style))
	  ()
	(if buffer-file-name
	    (setq any-ini-style (file-name-nondirectory buffer-file-name)))))
    (any-ini-apply-style)
    (font-lock-mode -1)
    (font-lock-mode t)))

(defun any-ini-apply-style (&optional re-read-canons)
  "Setup and apply style for a buffer, based on local `any-ini-style'.

If `any-ini-style' is nil, or is not the name of a style from `any-ini-styles-alist',
then the default style will be applied.

If RE-READ-CANONS is non-nil, the relevant canon files will be re-parsed and the
stored font-lock-keywords for the style updated.

Otherwise, the canon files will only be parsed if they have not previously been
parsed in this Emacs session."

  (let (mystylealist nofontlocksyet)
    (if any-ini-style
	(setq mystylealist (assoc any-ini-style any-ini-styles-alist)))
    (cond ((not mystylealist) 
	   (kill-local-variable 'any-ini-font-lock-keywords)
	   (kill-local-variable 'any-ini-canonical-ini-files)
	   (kill-local-variable 'any-ini-comment-start-chars)
	   (kill-local-variable 'any-ini-param-name-regexp)
	   (kill-local-variable 'any-ini-assignment-chars)
	   (kill-local-variable 'any-ini-section-name-regexp)
	   (kill-local-variable 'any-ini-section-start-chars)
	   (kill-local-variable 'any-ini-section-end-chars)
	   (setq any-ini-style nil))
	  (t
	   (make-local-variable 'any-ini-font-lock-keywords)
	   (make-local-variable 'any-ini-canonical-ini-files)
	   (make-local-variable 'any-ini-comment-start-chars)
	   (make-local-variable 'any-ini-param-name-regexp)
	   (make-local-variable 'any-ini-assignment-chars)
	   (make-local-variable 'any-ini-section-name-regexp)
	   (make-local-variable 'any-ini-section-start-chars)
	   (make-local-variable 'any-ini-section-end-chars)
	   (setq any-ini-font-lock-keywords nil)
	   (if re-read-canons
	       ()
	     (let ((mysavedfontlock (get 'any-ini-styles-alist any-ini-style)))
	       (if mysavedfontlock 
		   (setq any-ini-font-lock-keywords mysavedfontlock)
		 (setq nofontlocksyet t))))
	   (setq any-ini-canonical-ini-files (nth 1 mystylealist))
	   (setq any-ini-comment-start-chars (nth 2 mystylealist))
	   (setq any-ini-param-name-regexp (nth 3 mystylealist))
	   (setq any-ini-assignment-chars (nth 4 mystylealist))
	   (setq any-ini-section-name-regexp (nth 5 mystylealist))
	   (setq any-ini-section-start-chars (nth 6 mystylealist))
	   (setq any-ini-section-end-chars (nth 7 mystylealist))))
    (any-ini-setup-local-syntax-table)
    (any-ini-read-keywords re-read-canons)
    (any-ini-setup-imenu)
    (make-local-variable 'comment-start)
    (if any-ini-comment-start-chars
	(setq comment-start (string (nth 0 any-ini-comment-start-chars)))
      (setq comment-start "#"))
    (make-local-variable 'comment-end)
    (setq comment-end "")    
    (setq mode-name (concat "<any>" (if (not any-ini-style) 
					".ini" 
				      (concat " " any-ini-style)) 
			    " file"))
    (when (or nofontlocksyet re-read-canons)
      (put 'any-ini-styles-alist any-ini-style any-ini-font-lock-keywords))))

(defvar any-ini-local-syntax-table nil
  "Syntax table used while in <any>.ini mode.

Spaces and tabs are defined as whitespace, linefeeds and formfeeds are defined 
as comment-ending characters and the defined `any-ini-comment-start-chars' are
defined as comment-starting characters.")

(defun any-ini-setup-local-syntax-table ()
  "Setup local syntax table based on settings for `any-ini-comment-start-chars'.

Spaces and tabs are defined as whitespace, linefeeds and formfeeds are defined 
as comment-ending characters and the defined `any-ini-comment-start-chars' are
defined as comment-starting characters.

Syntax table is stored as `any-ini-local-syntax-table'."
  (make-local-variable 'any-ini-local-syntax-table)
  (setq any-ini-local-syntax-table (make-syntax-table))
  (set-syntax-table any-ini-local-syntax-table)
  (modify-syntax-entry ?\  " ")
  (modify-syntax-entry ?\t " ")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\f ">")
  (if (not any-ini-comment-start-chars)
      (modify-syntax-entry ?\# "<")
    (dolist (commentchar any-ini-comment-start-chars)
      (modify-syntax-entry commentchar "<"))))

(defvar any-ini-map nil
  "Keymap for <any>.ini mode.")
(if any-ini-map
    ()
  (setq any-ini-map (make-sparse-keymap)))

(defvar any-ini-abbrev-table nil
  "Abbrev table used while in <any>.ini mode.")
(define-abbrev-table 'any-ini-abbrev-table ())

(defvar any-ini-font-lock-keywords nil
  "Current list of keywords for highlighting in default <any>.ini mode.

See also `any-ini-read-keywords' and `any-ini-reread-keywords'.")

(defun any-ini-re-read-keywords-etc ()
  "*Re-parse the canonical files for the current style and update all style
info based on the current customization settings.

Having updated the style info, this fuction will then cycle through
all buffers and also update those that may be affected by the re-read.

Calls `any-ini-apply-style' in this buffer with RE-READ-CANONS as t
and `any-ini-apply-style' for the other buffers with no argument."
  (interactive)
  (if (not (eq major-mode 'any-ini-mode))
      (message "Major mode is not any-ini-mode.")
    (message "any-ini-reread-keywords : working ....")
    (any-ini-apply-style t)
    (save-excursion
      (let ((updating-style any-ini-style))
	(dolist (somebuffer (buffer-list))
		(set-buffer somebuffer)
		(when (and (eq major-mode 'any-ini-mode) (eq any-ini-style updating-style))
		  (any-ini-apply-style)
		  (font-lock-mode -1)
		  (font-lock-mode t)))))
    (message "any-ini-reread-keywords : DONE.")))

(defun any-ini-read-keywords (&optional forceread)
  "Create keyword-map for <any>.ini mode, based on current canon files.

Can be called from `any-ini-reread-keywords', in which case FORCEREAD
will be non-nil, the relevant canonical files will be re-read and any current
font-lock-keyword info for the current style will be overwritten.

Otherwise, no action will be taken if a current font-lock-keywords list exists
for the current style.

The font-lock-keyword lists for the default style are stored in the global version
of `any-ini-font-lock-keywords'. The keywords for the styles from `any-ini-styles-alist'
are stored in local versions of `any-ini-font-lock-keywords' and also in a plist
of `any-ini-styles-alist'.

At the moment, this data is not saved between Emacs sessions - instead, it is recreated
for each style, the first time the style is activated in a session."
  (if (and any-ini-font-lock-keywords (not forceread))
      ()
    (setq any-ini-font-lock-keywords nil)
    (dolist (mykeyfile any-ini-canonical-ini-files)
      (setq any-ini-font-lock-keywords 
	    (append 
	     (any-ini-append-keys-from mykeyfile (list nil
						       nil
						       any-ini-comment-start-chars
						       any-ini-param-name-regexp
						       any-ini-assignment-chars
						       any-ini-section-name-regexp
						       any-ini-section-start-chars
						       any-ini-section-end-chars)
				       ) 
	     any-ini-font-lock-keywords)))
    (setq any-ini-font-lock-keywords
	  (append (list (cons 
			 (concat "\\^.*\\([" 
				 (if any-ini-comment-start-chars 
				     any-ini-comment-start-chars
				  (string ?\#) )
				 "].*$\\)") 
			 (list 2 'font-lock-comment-face nil nil))
			)
		  any-ini-font-lock-keywords )))
  (setq font-lock-defaults (list any-ini-font-lock-keywords nil t nil nil)))

(defun any-ini-append-keys-from (kfile mystylealist)
  "Read a file and set the section and/or param names it contains as keywords
in `any-ini-font-lock-keywords'."
  (let (mykeyfile myfiletype myparselist isinifile my-font-lock-keywords)
    (setq mykeyfile (nth 0 kfile))
    (setq myfiletype (nth 1 kfile))
    (cond 
     ( (eq myfiletype 1) (setq myparselist '(t)))       ;; Sections list
     ( (eq myfiletype 2) (setq myparselist '(nil)))     ;; Params list
     ( t (setq isinifile t)             ;; .ini file, parse twice
	 (setq myparselist '(t nil)))
     )
    (when (file-exists-p mykeyfile)
      (save-current-buffer
	(let (mykeywordslist 
	      mykeyword 
	      font
	      (my-any-ini-comment-start-chars (nth 2 mystylealist))
	      (my-any-ini-param-name-regexp (nth 3 mystylealist))
	      (my-any-ini-assignment-chars (nth 4 mystylealist))
	      (my-any-ini-section-name-regexp (nth 5 mystylealist))
	      (my-any-ini-section-start-chars (nth 6 mystylealist))
	      (my-any-ini-section-end-chars (nth 7 mystylealist)))
	  (set-buffer (get-buffer-create "*any-ini-temp*"))
	  (insert-file-contents mykeyfile nil nil nil t)
	  (dolist (sectionfile myparselist)
	    (goto-char (point-min))
	    (condition-case e
		(while t
		  (cond ((and isinifile sectionfile)
			 (re-search-forward 
			  (concat "\\(^[ \t]*\\)"
				  "\\([" 
				  (if my-any-ini-section-start-chars 
				      my-any-ini-section-start-chars 
				      (string ?\[)) 
				  "]\\)"
				  "\\(" 
				  (if my-any-ini-section-name-regexp 
				      my-any-ini-section-name-regexp
				    "[-_A-Za-z0-9]+")
				  "\\)"
				  "\\(["
				  (if my-any-ini-section-end-chars
				      my-any-ini-section-end-chars
				    (string ?\]))
				  "]\\)"
				  "\\(.*$\\)"
				  )
			  )
			 (setq mykeyword (match-string 3)))
			((and isinifile (not sectionfile))
			 (re-search-forward 
			  (concat "\\(^[ \t]*\\)\\("
				  (if my-any-ini-param-name-regexp
				      my-any-ini-param-name-regexp
				    "[-_A-Za-z0-9]+")
				  "\\)\\([ \t]*"
				  "["
				  (if my-any-ini-assignment-chars
				      my-any-ini-assignment-chars
				    (string ?\=))
				  "]"
				  ".*$\\)")
			  )
			 (setq mykeyword (match-string 2)))
			(sectionfile
			 (re-search-forward 
			  (concat "\\(^[ \t]*\\)\\("
				  (if my-any-ini-section-name-regexp
				      my-any-ini-section-name-regexp
				    "[-_A-Za-z0-9]+")
				  "\\)$")
			  )
			 (setq mykeyword (match-string 2)))
			((not sectionfile)
			 (re-search-forward 
			  (concat "\\(^[ \t]*\\)\\("
				  (if my-any-ini-param-name-regexp
				      my-any-ini-param-name-regexp
				    "[-_A-Za-z0-9]+")
				  "\\)$")
			  )
			 (setq mykeyword (match-string 2))))
		  (when mykeyword
		    (setq mykeywordslist (append (list mykeyword) mykeywordslist))))
	      (search-failed))
	    (setq my-font-lock-keywords
		  (append (list 
			   (cons 
			    (any-ini-keywords-regexp mykeywordslist sectionfile mystylealist) 
			    (if sectionfile
				(list 
				 (list 1 'any-ini-section-chars-face nil nil)
				 (list 2 'any-ini-section-face nil nil)
				 (list 3 'any-ini-section-chars-face nil nil)
				 )
			    (list (list 2 'any-ini-param-face nil nil) 
				  (list 4 'any-ini-assigner-face nil nil)
				  (list 6 'any-ini-value-face nil nil)
				  (list 7 'font-lock-comment-face nil nil)
				  ))			    
			    )
			   )
			  my-font-lock-keywords )))
	  (kill-buffer (current-buffer)))))
    my-font-lock-keywords))

(defun any-ini-keywords-regexp (keywords aresections mystylealist)
  "Create regexp to describe sections/params for font-locking.

NB - First part of the regexps (searching for whitespace etc at the beginning of a line),
is wrapped in parentheses to ensure that the keyword part of the regexp is always be `subexp2'."
  (let (
	(my-any-ini-comment-start-chars (nth 2 mystylealist))
	(my-any-ini-param-name-regexp (nth 3 mystylealist))
	(my-any-ini-assignment-chars (nth 4 mystylealist))
	(my-any-ini-section-name-regexp (nth 5 mystylealist))
	(my-any-ini-section-start-chars (nth 6 mystylealist))
	(my-any-ini-section-end-chars (nth 7 mystylealist)))
    (if aresections
	(concat "\\(^[ \t]*["
		(if my-any-ini-section-start-chars
		    my-any-ini-section-start-chars
		  (string ?\[))
		"]\\)" 
		(any-ini-regexp-opt keywords t) 
		"\\([" 
		(if my-any-ini-section-end-chars
		    my-any-ini-section-end-chars
		  (string ?\]))
		"]\\)"
		"\\([ \t]*$\\|[ \t]+["
		(if my-any-ini-comment-start-chars
		    my-any-ini-comment-start-chars
		  (string ?\#))
		"].*$\\)"
		)
      (concat "\\(^[ \t]*\\)"
	      (any-ini-regexp-opt keywords t) 
	      "\\([ \t]*\\)"
	      "\\([" 
	      (if my-any-ini-assignment-chars 
		  my-any-ini-assignment-chars
		(string ?\=))
	      "]\\)"
	      "\\([ \t]*\\)"
	      "\\([^"
	      (if my-any-ini-comment-start-chars
		  my-any-ini-comment-start-chars
		(string ?\#))
	      "\n]*\\)"
	      "\\(["
	      (if my-any-ini-comment-start-chars
		  my-any-ini-comment-start-chars
		(string ?\#))
	      "].*\\|[\n]\\)"
	      ))))

(defun any-ini-regexp-opt (strings paren)
  "Replacement for standard `regexp-opt' function.

This replacement is necessary since the standard function can occasionally cause a memory 
error when setting a file's mode from the auto-mode-alist functionality.

Error was \"Variable binding depth exceeds max-specpdl-size\" and happened only with
large (1500+ lines) canonical files."
  (let ((open-paren (if paren "\\(" ""))
        (close-paren (if paren "\\)" "")))
    (concat open-paren
            (mapconcat 'regexp-quote strings "\\|")
            close-paren)))

(defun any-ini-toggle-errorcheck-mode ()
  "*Temorarily set fonts in this buffer to highlight spelling errors more clearly.

Resets the comment face and param-related faces in this buffer to `font-lock-string-face'.
The idea is that this will make it easier to spot errors in a large or heavily-commented 
file, as they should show up in `default' face against a relatively neutral background.

The fonts affected are - 

`font-lock-comment-face'
`any-ini-param-face'
`any-ini-assigner-face'
`any-ini-value-face'
"
  (interactive)
  (if (not (eq major-mode 'any-ini-mode))
      (message "Major mode is not any-ini-mode.")
    (make-variable-buffer-local 'any-ini-buffer-errorcheck)
    (cond ((eq any-ini-buffer-errorcheck 1)
	   (kill-local-variable 'font-lock-comment-face)
	   (kill-local-variable 'any-ini-param-face)
	   (kill-local-variable 'any-ini-assigner-face)
	   (kill-local-variable 'any-ini-value-face)
	   (setq any-ini-buffer-errorcheck 0)
	   )
	  (t
	   (make-local-variable 'font-lock-comment-face)
	   (make-local-variable 'any-ini-param-face)
	   (make-local-variable 'any-ini-assigner-face)
	   (make-local-variable 'any-ini-value-face)
	   (setq font-lock-comment-face 'font-lock-string-face)
	   (setq any-ini-param-face 'font-lock-string-face)
	   (setq any-ini-assigner-face 'font-lock-string-face)
	   (setq any-ini-value-face 'font-lock-string-face)
	   (setq any-ini-buffer-errorcheck 1)
	   ))
    (any-ini-apply-style)
    (font-lock-mode -1)
    (font-lock-mode t)))

(defun any-ini-setup-imenu ()
  "Setup and display an `imenu' menu of section names for current buffer.

Sets up `imenu-generic-expression' \(see Info Node `(emacs)Imenu'\)
based on current settings of 

`any-ini-section-name-regexp', 
`any-ini-section-start-chars',
`any-ini-section-end-chars'

The title of the menu is read from `any-ini-imenu-name'.

Display of the menu is controlled by `any-ini-imenu-show-flag'.

See Info Node `(emacs)Imenu' for more details."
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression
	(list
	 (list nil  
	       (concat "\\(^[ \t]*\\)"
		       "\\([" 
		       (if any-ini-section-start-chars 
			   any-ini-section-start-chars 
			 (string ?\[)) 
		       "]\\)"
		       "\\(" 
		       (if any-ini-section-name-regexp 
			   any-ini-section-name-regexp
			 "[-_A-Za-z0-9]+")
		       "\\)"
		       "\\(["
		       (if any-ini-section-end-chars
			   any-ini-section-end-chars
			 (string ?\]))
		       "]\\)"
		       "\\(.*$\\)"
		       )
	       3)
	 )
	)
  (if any-ini-imenu-show-flag
      (if (featurep 'imenu)
	  (imenu-add-to-menubar 'any-ini-imenu-name)
	(message "any-ini-mode: imenu NOT available."))))

;;;###autoload
(defun any-ini-mode ()
  "*Major mode for editing config files with syntax highlighting based on a 'source of truth'.

You may set up a default style for all <any>.ini mode buffers, or, more usefully,
you may set up several styles that will be automatically applied based on the name
of the file being visited.

See `any-ini-set-my-style' and `any-ini-styles-alist' for more details.

Turning on <any>.ini mode runs the normal hook `any-ini-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map any-ini-map)
  (setq local-abbrev-table any-ini-abbrev-table)
  (setq major-mode 'any-ini-mode)
  (any-ini-set-my-style)
  (run-hooks 'any-ini-mode-hook))

;; Try to pull in imenu if it exists.
(condition-case nil
    (require 'imenu)
  (error nil))

(provide 'any-ini-mode)

;;; any-ini-mode.el ends here

