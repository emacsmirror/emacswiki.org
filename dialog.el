;;; dialog.el --- dialog box interface using widgets, frames and windows

;; Copyright (C) 2008 Vinicius Jose Latorre

;; Time-stamp: <2008/01/22 18:35:17 vinicius>
;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: convenience, extensions, hypermedia
;; Version: 0.2
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package implements a dialog box interface using widgets,
;; frames and windows.
;;
;; `dialog' package uses the widget package which handle all low level
;; stuff for deal with buttons, fields, etc.  The buttons, fields,
;; etc. are created in a buffer.
;;
;; `dialog' package simplifies the use of the widget package, as the
;; easymenu package simplifies the menu creation/management.
;;
;; `dialog' package provides a mechanism to navigate through dialogs.
;;
;; This package was tested on Emacs 22 and 23.
;;
;;
;; Using dialog
;; ------------
;;
;; As an example, here is a very simple dialog specification:
;;
;;    (require 'dialog)
;;
;;    (dialog-define hello1
;;      '(:style window
;;               [navigation 1 1 :tag "Navigation"]
;;               [text 3 1 "Hello World 1!!"]
;;               [button-quit 5 1]
;;               [button-previous 5 10 :tag "Hello :("])
;;      "This is a Hello World example.")
;;
;;    (dialog-define hello2
;;      '(:style window
;;               [navigation 1 1 :tag "Navigation"]
;;               [text 3 1 "Hello World 2!!"]
;;               [button-quit 5 1]
;;               [button-next 5 10 hello1 :tag "Hello :)"])
;;      "This is another Hello World example.")
;;
;;    (hello2)  ; or (dialog-run 'hello2) run dialog hello2
;;
;; The following screen is displayed when hello2 executes:
;;
;; ------------------------------------------------------------ hello2
;;  Navigation: hello2
;;
;;  Hello World 2!!
;;
;;  [Quit]   [Hello :)]
;; ------------------------------------------------------------ hello2
;;
;; If [Quit] button is pressed (by mouse ou keyboard), the dialog box
;; quits.  If [Hello :)] button is pressed, the dialog hello executes
;; as seen below.
;;
;; ------------------------------------------------------------- hello
;;  Navigation: [hello2] :: hello
;;
;;  Hello World 1!!
;;
;;  [Quit]   [Hello :(]
;; ------------------------------------------------------------- hello
;;
;; If [Hello :(] or [hello2] button is pressed, the dialog hello2
;; executes as seen above.
;;
;;
;; Interface Functions
;; -------------------
;;
;; -- Macro: dialog-define dialog spec doc
;;	Declare a dialog called DIALOG with items described in SPEC.
;;	DIALOG does not need to be quoted.
;;
;;	Second argument SPEC is the dialog specification.
;;
;;	Third argument DOC is the dialog documentation.
;;
;;	See _Defining a Dialog Box_ section for SPEC documentation.
;;	See also _Dialog Derivation_ section.
;;
;; -- Function: dialog-doc-string dialog
;; -- Function: dialog-documentation dialog
;;	Get the documentation string for DIALOG.
;;
;; -- Function: dialog-make-empty dialog
;;	Define a new, empty dialog with name DIALOG.
;;	If the dialog already exists, it is left unmodified.
;;	Return DIALOG.
;;
;; -- Function: dialog-run dialog
;;	Execute DIALOG.  See `dialog-define'.
;;
;; -- Function: dialog-spec dialog
;;	Get the DIALOG specification.  See `dialog-define'.
;;
;; -- Function: dialog-update-text sym
;;	Update text field associated with symbol SYM.
;;	See `dialog-define'.
;;
;; -- Function: dialogp object
;;	Return t if OBJECT is a dialog object.
;;
;; -- Function: set-dialog-doc-string dialog doc
;; -- Function: set-dialog-documentation dialog doc
;;	Set the documentation string for DIALOG to DOC.
;;
;; -- Function: set-dialog-spec dialog spec
;;	Set the DIALOG specification.  See `dialog-define'.
;;
;;
;; Defining a Dialog Box
;; ---------------------
;;
;; A dialog box is defined by a list which has the following form:
;;
;;    (STYLE FIELD...)
;;
;; Where STYLE specifies how dialog will be opened and dialog
;; derivation (more about this below), and FIELD is a vector which
;; specifies a dialog field.
;;
;; Valid values for STYLE are:
;;
;;    :style window
;;	Use the current frame with only one window.
;;
;;    :style split-window-horizontally
;;    :style (split-window-horizontally . ARG)
;;	Split current window horizontally and select the window at
;;	left.  ARG is optional; if specified, it is passed as argument
;;	to `split-window-horizontally' function (which see).
;;	ARG must be an integer.
;;
;;    :style split-window-vertically
;;    :style (split-window-vertically . ARG)
;;	Split current window vertically and select the window above.
;;	ARG is optional; if specified, it is passed as argument for
;;	`split-window-vertically' function (which see).
;;	ARG must be an integer.
;;
;;    :style frame
;;    :style (frame . POSITION)
;;	Make a new frame.  POSITION is optional; it specifies the
;;	position of the upper left corner of the new frame.
;;	POSITION can have the following values:
;;
;;	(X . Y)	the position in pixels.
;;
;;	point	the current point position.
;;
;;	mouse	the current mouse position.
;;
;;	center	the new frame is centralized in the selected frame.
;;
;;	frame	the upper left corner of the selected frame.
;;
;;	If POSITION is omitted, the frame position is given by the
;;	system where Emacs is running.
;;
;;	If there isn't a windowing system, it behaves as `window'.
;;
;;    :parent DIALOG
;;    :parent (DIALOG . DECORATION)
;;	This dialog derives from dialog DIALOG (the parent dialog).
;;	DECORATION specifies what to do with decorations (box, hline
;;	and vline fields).  DECORATION can have the following values:
;;
;;	keep		keep all parent decoration.
;;
;;	kill		kill all parent decoration.
;;
;;	kill-overlap	kill parent decoration only when overlaps with
;;			some derived dialog field (decoration or not).
;;
;;	The DECORATION default value is keep.
;;
;;	See _Dialog Derivation_ section.
;;
;; STYLE can be omitted, the default value is `:style window'.
;;
;; The window configuration is saved just before the dialog box
;; activation and it is restored just after dialog box termination.
;;
;; There exist the following FIELD types:
;;
;;    box
;;    button
;;    button-cancel
;;    button-next
;;    button-ok
;;    button-previous
;;    button-quit
;;    button-reset
;;    checkbox
;;    editable
;;    hline
;;    menu
;;    navigation
;;    radio
;;    text
;;    vline
;;
;; FIELD has the following forms:
;;
;;    [box LINE COLUMN LINE2 COLUMN2
;;	:tag TAG]
;;
;;	Draw a box which diagonal vertices are at LINE and COLUMN, and
;;	at LINE2 and COLUMN2.
;;	LINE(2) starts from 1.  COLUMN(2) starts from 0.
;;	TAG contains the characters used to draw the box border.
;;	If TAG is omitted, the default value is ".-|++++".
;;	The TAG string specifies:
;;
;;	".-|++++"
;;	 :::::::
;;	 ::::::+--- bottom left corner
;;	 :::::+---- bottom right corner
;;	 ::::+----- top left corner
;;	 :::+------ top right corner
;;	 ::+------- vertical
;;	 :+-------- horizontal
;;	 +--------- null box (LINE = LINE2 and COLUMN = COLUMN2)
;;
;;    [button LINE COLUMN
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a button at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	If TAG is omitted, "Button" is used.
;;	When pressed, it executes FUNCTION, if FUNCTION is
;;	specified.  If FUNCTION is omitted, nothing happens.
;;	See _Field Keywords_ section below.
;;
;;    [button-cancel LINE COLUMN
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a cancel button at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	If TAG is omitted, "Cancel" is used.
;;	When pressed, it takes the following steps:
;;	1. Discard all temporary dialog values;
;;	2. Execute FUNCTION, if FUNCTION is specified;
;;	3. Finish the current dialog, that is, return to previous
;;	   dialog, if exists one.
;;	See _Field Keywords_ section below.
;;
;;    [button-next LINE COLUMN DIALOG
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a next button at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	If TAG is omitted, "Next" is used.
;;	If DIALOG is not a dialog, nothing happens.
;;	If DIALOG is a dialog, when pressed, it takes the following
;;	steps:
;;	1. Execute FUNCTION, if FUNCTION is specified;
;;	2. Go to next DIALOG.
;;	See _Field Keywords_ section below.
;;
;;    [button-ok LINE COLUMN
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify an ok button at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	If TAG is omitted, "Ok" is used.
;;	When pressed, it takes the following steps:
;;	1. All temporary dialog values are saved into
;;	   corresponding variables;
;;	2. Execute FUNCTION, if FUNCTION is specified;
;;	3. Finish the current dialog, that is, return to previous
;;	   dialog, if exists one.
;;	See _Field Keywords_ section below.
;;
;;    [button-previous LINE COLUMN
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a previous button at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	If TAG is omitted, "Previous" is used.
;;	If there isn't a previous dialog, nothing happens.
;;	If there isn a previous dialog, when pressed, it takes the
;;	following steps:
;;	1. Execute FUNCTION, if FUNCTION is specified;
;;	2. Go to previous dialog.
;;	See _Field Keywords_ section below.
;;
;;    [button-quit LINE COLUMN
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a quit button at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	If TAG is omitted, "Quit" is used.
;;	When pressed, it takes the following steps:
;;	1. Discard all temporary dialog values;
;;	2. Execute FUNCTION, if FUNCTION is specified;
;;	3. Finish all dialog chain.
;;	See _Field Keywords_ section below.
;;
;;    [button-reset LINE COLUMN
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a reset button at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	If TAG is omitted, "Reset" is used.
;;	When pressed, it takes the following steps:
;;	1. Reset all temporary dialog values, that is, restore the
;;	   original value for each temporary dialog variable;
;;	2. Execute FUNCTION, if FUNCTION is specified.
;;	See _Field Keywords_ section below.
;;
;;    [checkbox LINE COLUMN VAR
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a checkbox at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	VAR is a symbol variable which will hold the checkbox value.
;;	If TAG is omitted, it is created only the checkbox.
;;	If TAG is specified, the first character indicates if the TAG
;;	is positioned at left or right of the checkbox.  If the first
;;	character is `?-', the TAG is positioned at left of the
;;	checkbox, that is:
;;
;;	TAG []
;;
;;	If the first character is not `?-', the TAG is positioned at
;;	right of the checkbox, that is:
;;
;;	[] TAG
;;
;;	The first character of the TAG is discarded, so, the minimum
;;	TAG length is 2.
;;	When pressed, it takes the following steps:
;;	1. Store VALUE into a temporary dialog variable;
;;	2. Execute FUNCTION passing VALUE as argument, if
;;	   FUNCTION is specified.
;;	See _Field Keywords_ section below.
;;
;;    [editable LINE COLUMN KIND VAR
;;	:tag TAG :notify FUNCTION :help-echo HELP
;;	:size SIZE :action FUNCTION :secret CHAR]
;;
;;	Specify an editable field at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	VAR is a symbol variable which will hold the editable value.
;;	KIND specifies the kind of editable field, it can have the
;;	following values:
;;
;;	character	a character field.
;;
;;	coding-system	a MULE coding-system field.
;;
;;	color		choose a color name (with sample).
;;
;;	directory	a directory name field.
;;
;;	file		a file name field.
;;
;;	float		a floating point number field.
;;
;;	integer		an integer number field.
;;
;;	key-sequence	a key sequence field.
;;
;;	number		a number (floating point or integer) field.
;;
;;	regexp		a regular expression field.
;;
;;	sexp		an arbitrary Lisp expression field.
;;
;;	string		a string field.
;;
;;	symbol		a Lisp symbol field.
;;
;;	text		a multiline text area field.
;;
;;	variable	a Lisp variable field.
;;
;;	See _Field Keywords_ section below.
;;
;;    [hline LINE COLUMN LENGTH
;;	:tag TAG]
;;
;;	Draw a horizontal line starting at LINE and COLUMN until LINE
;;	and (COLUMN + LENGTH - 1).
;;	LINE starts from 1.  COLUMN starts from 0.
;;	TAG is a string which the very first character is used to draw
;;	the line.  If TAG is omitted, the default value is "-".
;;
;;    [menu LINE COLUMN VAR ALIST
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a menu at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	VAR is a symbol variable which will hold the menu value.
;;	ALIST is an association list which has the following form:
;;
;;	(VALUE . MENU-ITEM)
;;
;;	Where VALUE is the value which will be stored in VAR when this
;;	menu item is selected; MENU-ITEM is a string shown as the menu
;;	item.  VALUE can be a symbol or a string.
;;	When a menu item is selected, it takes the following steps:
;;	1. Store VALUE into a temporary dialog variable;
;;	2. Execute FUNCTION passing VALUE as argument, if FUNCTION is
;;	   specified.
;;	See _Field Keywords_ section below.
;;
;;    [navigation LINE COLUMN
;;	:tag TAG :help-echo HELP]
;;
;;	Specify a navigation field bar at LINE and COLUMN which shows
;;	all dialogs before the current one.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	It has the following generic form:
;;
;;	TAG: [dialog1] :: [dialog2] :: ... :: [dialogN-1] :: dialogN
;;
;;	Where TAG, if specified, is given by :tag keyword; [dialog1],
;;	[dialog2] until [dialogN-1] are buttons which go to dialog
;;	correspondent when the button is pressed.
;;	See _Field Keywords_ section below.
;;
;;    [radio LINE COLUMN VAR VALUE
;;	:tag TAG :notify FUNCTION :help-echo HELP]
;;
;;	Specify a radio at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	VAR is a symbol variable which will hold the radio value.
;;	VALUE is the value used when this radio is selected.
;;	If TAG is omitted, it is created only the radio.
;;	If TAG is specified, the first character indicates if the TAG
;;	is positioned at left or right of the radio.  If the first
;;	character is `?-', the TAG is positioned at left of the
;;	radio, that is:
;;
;;	TAG ( )
;;
;;	If the first character is not `?-', the TAG is positioned at
;;	right of the radio, that is:
;;
;;	( ) TAG
;;
;;	The first character of the TAG is discarded, so, the minimum
;;	TAG length is 2.
;;	When pressed, it takes the following steps:
;;	1. Store VALUE into a temporary dialog variable;
;;	2. Update all radio which share the same VAR;
;;	3. Execute FUNCTION passing VALUE as argument, if
;;	   FUNCTION is specified.
;;	See _Field Keywords_ section below.
;;
;;    [text LINE COLUMN TEXT
;;	:size SIZE]
;;
;;	Specify a TEXT string to be inserted at LINE and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	TEXT can be a string, a symbol or a list.  If TEXT is a symbol
;;	variable, the variable value must be a string.  If TEXT is a
;;	symbol function or a function, the function will be evaluated
;;	without parameters and should returns a string.  If TEXT is a
;;	list, the list header should be a function, this function will
;;	be evaluated and the list tail will be the parameters for this
;;	function; this function should return a string.
;;	If TEXT is a symbol, `dialog-update-text' can be used by a
;;	function updates this field.
;;	See _Field Keywords_ section below.
;;
;;    [vline LINE COLUMN LENGTH
;;	:tag TAG]
;;
;;	Draw a vertical line starting at LINE and COLUMN until (LINE +
;;	LENGTH - 1) and COLUMN.
;;	LINE starts from 1.  COLUMN starts from 0.
;;	TAG is a string which the very first character is used to draw
;;	the line.  If TAG is omitted, the default value is "|".
;;
;;
;; Field Keywords
;; --------------
;;
;; The keywords specified in a field are optionals.
;; Below is the keyword documentation.
;;
;;    :action FUNCTION
;;	Specify a function FUNCTION which is activated when RET key is
;;	pressed.  It is passed as argument the value of the editable
;;	field.  FUNCTION must return a value.
;;	If the returned value is nil, it means that something goes
;;	wrong, so the point stays in the current editable field.
;;	If the returned value is not nil, the point goes to the next
;;	field.
;;
;;    :help-echo HELP
;;	Specifies how to display a message whenever you move to the
;;	field via keyboard or move the mouse over it.  HELP is either
;;	a string to display, a function of one argument, the field
;;	widget, which should return a string to display, or a form
;;	that evaluates to such a string.
;;
;;    :notify FUNCTION
;;	Specify a function FUNCTION which is activated at each change
;;	of the editable field.  It is passed as argument the value of
;;	the field.
;;
;;    :secret CHAR
;;	Character used to display the value.  You can set this to
;;	e.g. `?*' if the field contains a password or other secret
;;	information.  By default, this is `nil', and the value is not
;;	secret.
;;
;;    :size SIZE
;;	Specify the SIZE of string to be displayed.
;;	It can have the following values:
;;
;;	integer			the size of string.
;;
;;	(COLUMNS . LINES)	rectangular text area, both values
;;				are integers greater than zero.
;;
;;    :tag TAG
;;	Usually, specify a field label.
;;	Some fields use TAG differently, see the field documentation
;;	above.
;;
;;
;; Dialog Derivation
;; -----------------
;;
;; Sometimes you need to create a dialog B which is almost the same as
;; another dialog A, but it should add some extra fields in A, or it
;; should remove some fields from A.  This is what the dialog
;; derivation do, that is, a way to add/remove some fields from a
;; dialog in order to create a new one.
;;
;; To derive a dialog from another one, just specify the :parent in a
;; dialog definition.  For example:
;;
;;    (dialog-define example2
;;      '(:style window
;;	      [navigation 1 1 :tag "Navigation"]
;;	      [text 3 1 "Hello World 1!!"]
;;	      [button-quit 5 1]
;;	      [button-previous 5 10 :tag "Hello :("])
;;      "This is the parent dialog.")
;;
;;    (dialog-define example1
;;      '(:style (frame . mouse) :parent example2
;;	      ;; this is a new button
;;	      [button-quit 7 1 :tag "New Quit Button"]
;;	      ;; this text field removes the "Hello :(" button
;;	      [text 5 10 " "])
;;      "This is the derived dialog.")
;;
;; So, if the new dialog element overlaps one of parent dialog
;; elements, the parent dialog element is removed.
;;
;; The :parent specification have the following values:
;;
;;    :parent DIALOG
;;    :parent (DIALOG . DECORATION)
;;
;; Where DIALOG is the parent dialog and DECORATION specifies what to do
;; with decoration fields, that is, box, hline and vline fields.
;;
;; DECORATION can have the following values:
;;
;;    keep		keep all parent decoration.
;;
;;    kill		kill all parent decoration.
;;
;;    kill-overlap	kill parent decoration only when overlaps with
;;			some derived dialog field (decoration or not).
;;
;; The DECORATION default value is keep.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of `dialog' options, please,
;; see the options declaration in the code for a long documentation.
;;
;; `dialog-frame-min-width'	Specify frame minimum width, measured
;;				in characters.
;;
;; `dialog-frame-min-height'	Specify frame minimum height, measured
;;				in lines.
;;
;; `dialog-extra-columns'	Specify extra number of columns,
;;				measured in characters.
;;
;; `dialog-extra-lines'		Specify extra number of lines,
;;				measured in lines.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq dialog-frame-min-width 50)
;;
;;    This way always keep your default settings when you enter a new
;;    Emacs session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET dialog-frame-min-width RET 50 RET
;;
;;    This way keep your settings only during the current Emacs
;;    session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Options* option,
;;	 then click on *Customize Emacs*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Convenience* group,
;;	 expand *Dialog* group
;;	 and then customize `dialog' options.
;;    Through this way, you may choose if the settings are kept or not
;;    when you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v dialog-frame-min-width RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not
;;    when you leave out the current Emacs session.
;;
;;
;; Todo List
;; ---------
;;
;; - output a rectangular text area
;; - edit a rectangular text area
;; - hints and tips section
;; - scrolling list/vector (probably like a button which opens another
;;   frame/window)
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Christoph Conrad <christoph.conrad@gmx.de> for dialog
;; derivation suggestion.
;;
;; Thanks to Per Abrahamsen <abraham@dina.kvl.dk> (and to all people
;; who contributed with him) for developing widget and custom
;; packages.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


(eval-when-compile
  (require 'cus-edit)
  (require 'wid-edit)
  (require 'widget))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interface


(defgroup dialog nil
  "Dialog group."
  :tag "Dialog"
  :link '(emacs-library-link :tag "Source Lisp File" "dialog.el")
  :version "23"
  :group 'convenience
  :group 'extensions
  :group 'hypermedia)


(defcustom dialog-extra-columns 6
  "*Specify extra number of columns, measured in characters.

Used to adjust point position and frame centralisation."
  :type  'integer
  :group 'dialog)


(defcustom dialog-extra-lines 0
  "*Specify extra number of lines, measured in lines.

Used to adjust point position and frame centralisation."
  :type  'integer
  :group 'dialog)


;; I got these values by trial and error in my system.
;; If you got different values, please, send me an email.
(defcustom dialog-frame-min-width 27
  "*Specify frame minimum width, measured in characters."
  :type  'integer
  :group 'dialog)


(defcustom dialog-frame-min-height 3
  "*Specify frame minimum height, measured in lines."
  :type  'integer
  :group 'dialog)


;;;###autoload
(put 'dialog-define 'lisp-indent-function 'defun)
;;;###autoload
(defmacro dialog-define (dialog spec doc)
  "Declare a dialog called DIALOG with items described in SPEC.
DIALOG does not need to be quoted.

Second argument SPEC is the dialog specification.

Third argument DOC is the dialog documentation.

The SPEC argument value should have the following form:

   (STYLE FIELD...)

Where STYLE specifies how dialog will be opened and FIELD is a
vector which specifies a dialog field.

Valid values for STYLE are:

   :style window
	Use the current frame with only one window.

   :style split-window-horizontally
   :style (split-window-horizontally . ARG)
	Split current window horizontally and select the window
	at left.  ARG is optional; if specified, it is passed as
	argument to `split-window-horizontally' function (which
	see).  ARG must be an integer.

   :style split-window-vertically
   :style (split-window-vertically . ARG)
	Split current window vertically and select the window
	above.  ARG is optional; if specified, it is passed as
	argument for `split-window-vertically' function (which
	see).  ARG must be an integer.

   :style frame
   :style (frame . POSITION)
	Make a new frame.  POSITION is optional; it specifies the
	position of the upper left corner of the new frame.
	POSITION can have the following values:

	(X . Y)	the position in pixels.

	point	the current point position.

	mouse	the current mouse position.

	center	the new frame is centralized in the selected frame.

	frame	the upper left corner of the selected frame.

	If POSITION is omitted, the frame position is given by
	the system where Emacs is running.

	If there isn't a windowing system, it behaves as `window'.

   :parent DIALOG
   :parent (DIALOG . DECORATION)
	This dialog derives from dialog DIALOG (the parent
	dialog).  DECORATION specifies what to do with
	decorations (box, hline and vline fields).
	DECORATION can have the following values:

	keep		keep all parent decoration.

	kill		kill all parent decoration.

	kill-overlap	kill parent decoration only when overlaps with
			some derived dialog field (decoration or not).

	The DECORATION default value is keep.

	See _Dialog Derivation_ section.

STYLE can be omitted, the default value is `window'.

The window configuration is saved just before the dialog box
activation and it is restored just after dialog box termination.

There exist the following FIELD types:

   box
   button
   button-cancel
   button-next
   button-ok
   button-previous
   button-quit
   button-reset
   checkbox
   editable
   hline
   menu
   navigation
   radio
   text
   vline

FIELD has the following forms:

   [box LINE COLUMN LINE2 COLUMN2
	:tag TAG]

	Draw a box which diagonal vertices are at LINE and
	COLUMN, and at LINE2 and COLUMN2.
	LINE(2) starts from 1.  COLUMN(2) starts from 0.
	TAG contains the characters used to draw the box border.
	If TAG is omitted, the default value is \".-|++++\".
	The TAG string specifies:

	\".-|++++\"
	 :::::::
	 ::::::+--- bottom left corner
	 :::::+---- bottom right corner
	 ::::+----- top left corner
	 :::+------ top right corner
	 ::+------- vertical
	 :+-------- horizontal
	 +--------- null box (LINE = LINE2 and COLUMN = COLUMN2)

   [button LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, \"Button\" is used.
	When pressed, it executes FUNCTION, if FUNCTION is
	specified.  If FUNCTION is omitted, nothing happens.
	See _Field Keywords_ section below.

   [button-cancel LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a cancel button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, \"Cancel\" is used.
	When pressed, it takes the following steps:
	1. Discard all temporary dialog values;
	2. Execute FUNCTION, if FUNCTION is specified;
	3. Finish the current dialog, that is, return to previous
	   dialog, if exists one.
	See _Field Keywords_ section below.

   [button-next LINE COLUMN DIALOG
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a next button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, \"Next\" is used.
	If DIALOG is not a dialog, nothing happens.
	If DIALOG is a dialog, when pressed, it takes the
	following steps:
	1. Execute FUNCTION, if FUNCTION is specified;
	2. Go to next DIALOG.
	See _Field Keywords_ section below.

   [button-ok LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify an ok button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, \"Ok\" is used.
	When pressed, it takes the following steps:
	1. All temporary dialog values are saved into
	   corresponding variables;
	2. Execute FUNCTION, if FUNCTION is specified;
	3. Finish the current dialog, that is, return to previous
	   dialog, if exists one.
	See _Field Keywords_ section below.

   [button-previous LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a previous button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, \"Previous\" is used.
	If there isn't a previous dialog, nothing happens.
	If there isn a previous dialog, when pressed, it takes
	the following steps:
	1. Execute FUNCTION, if FUNCTION is specified;
	2. Go to previous dialog.
	See _Field Keywords_ section below.

   [button-quit LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a quit button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, \"Quit\" is used.
	When pressed, it takes the following steps:
	1. Discard all temporary dialog values;
	2. Execute FUNCTION, if FUNCTION is specified;
	3. Finish all dialog chain.
	See _Field Keywords_ section below.

   [button-reset LINE COLUMN
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a reset button at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	If TAG is omitted, \"Reset\" is used.
	When pressed, it takes the following steps:
	1. Reset all temporary dialog values, that is, restore
	   the original value for each temporary dialog variable;
	2. Execute FUNCTION, if FUNCTION is specified.
	See _Field Keywords_ section below.

   [checkbox LINE COLUMN VAR
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a checkbox at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the checkbox value.
	If TAG is omitted, it is created only the checkbox.
	If TAG is specified, the first character indicates if the TAG
	is positioned at left or right of the checkbox.  If the first
	character is `?-', the TAG is positioned at left of the
	checkbox, that is:

	TAG []

	If the first character is not `?-', the TAG is positioned at
	right of the checkbox, that is:

	[] TAG

	The first character of the TAG is discarded, so, the minimum
	TAG length is 2.
	When pressed, it takes the following steps:
	1. Store VALUE into a temporary dialog variable;
	2. Execute FUNCTION passing VALUE as argument, if
	   FUNCTION is specified.
	See _Field Keywords_ section below.

   [editable LINE COLUMN KIND VAR
	:tag TAG :notify FUNCTION :help-echo HELP
	:size SIZE :action FUNCTION :secret BOOL]

	Specify an editable field at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the editable value.
	KIND specifies the kind of editable field, it can have the
	following values:

	character	a character field.

	coding-system	a MULE coding-system field.

	color		choose a color name (with sample).

	directory	a directory name field.

	file		a file name field.

	float		a floating point number field.

	integer		an integer number field.

	key-sequence	a key sequence field.

	number		a number (floating point or integer) field.

	regexp		a regular expression field.

	sexp		an arbitrary Lisp expression field.

	string		a string field.

	symbol		a Lisp symbol field.

	text		a multiline text area field.

	variable	a Lisp variable field.

	See _Field Keywords_ section below.

   [hline LINE COLUMN LENGTH
	:tag TAG]

	Draw a horizontal line starting at LINE and COLUMN until
	LINE and (COLUMN + LENGTH - 1).
	LINE starts from 1.  COLUMN starts from 0.
	TAG is a string which the very first character is used to
	draw the line.  If TAG is omitted, the default value is
	\"-\".

   [menu LINE COLUMN VAR ALIST
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a menu at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the menu value.
	ALIST is an association list which has the following form:

	(VALUE . MENU-ITEM)

	Where VALUE is the value which will be stored in VAR when
	this menu item is selected; MENU-ITEM is a string shown
	as the menu item.  VALUE can be a symbol or a string.
	When a menu item is selected, it takes the following
	steps:
	1. Store VALUE into a temporary dialog variable;
	2. Execute FUNCTION passing VALUE as argument, if
	   FUNCTION is specified.
	See _Field Keywords_ section below.

   [navigation LINE COLUMN
	:tag TAG :help-echo HELP]

	Specify a navigation field bar at LINE and COLUMN which
	shows all dialogs before the current one.
	LINE starts from 1.  COLUMN starts from 0.
	It has the following generic form:

	TAG: [dialog1] :: [dialog2] :: ... :: [dialogN-1] :: dialogN

	Where TAG, if specified, is given by :tag keyword;
	[dialog1], [dialog2] until [dialogN-1] are buttons which
	go to the dialog correspondent when the button is pressed.
	See _Field Keywords_ section below.

   [radio LINE COLUMN VAR VALUE
	:tag TAG :notify FUNCTION :help-echo HELP]

	Specify a radio at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	VAR is a symbol variable which will hold the radio value.
	VALUE is the value used when this radio is selected.
	If TAG is omitted, it is created only the radio.
	If TAG is specified, the first character indicates if the TAG
	is positioned at left or right of the radio.  If the first
	character is `?-', the TAG is positioned at left of the
	radio, that is:

	TAG ( )

	If the first character is not `?-', the TAG is positioned at
	right of the radio, that is:

	( ) TAG

	The first character of the TAG is discarded, so, the minimum
	TAG length is 2.
	When pressed, it takes the following steps:
	1. Store VALUE into a temporary dialog variable;
	2. Update all radio which share the same VAR;
	3. Execute FUNCTION passing VALUE as argument, if
	   FUNCTION is specified.
	See _Field Keywords_ section below.

   [text LINE COLUMN TEXT
	:size SIZE]

	Specify a TEXT string to be inserted at LINE and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	TEXT can be a string, a symbol or a list.  If TEXT is a
	symbol variable, the variable value must be a string.  If
	TEXT is a symbol function or a function, the function
	will be evaluated without parameters and should returns a
	string.  If TEXT is a list, the list header should be a
	function, this function will be evaluated and the list
	tail will be the parameters for this function; this
	function should return a string.
	If TEXT is a symbol, `dialog-update-text' can be used by a
	function updates this field.
	See _Field Keywords_ section below.

   [vline LINE COLUMN LENGTH
	:tag TAG]

	Draw a vertical line starting at LINE and COLUMN until
	(LINE + LENGTH - 1) and COLUMN.
	LINE starts from 1.  COLUMN starts from 0.
	TAG is a string which the very first character is used to
	draw the line.  If TAG is omitted, the default value is
	\"|\".


Field Keywords
--------------

The keywords specified in a field are optionals.
Below is the keyword documentation.

   :action FUNCTION
	Specify a function FUNCTION which is activated when RET
	key is pressed.  It is passed as argument the value of
	the editable field.  FUNCTION must return a value.
	If the returned value is nil, it means that something
	goes wrong, so the point stays in the current editable
	field.  If the returned value is not nil, the point goes
	to the next field.

   :help-echo HELP
	Specifies how to display a message whenever you move to
	the field via keyboard or move the mouse over it.  HELP
	is either a string to display, a function of one
	argument, the field widget, which should return a string
	to display, or a form that evaluates to such a string.

   :notify FUNCTION
	Specify a function FUNCTION which is activated at each
	change of the editable field.  It is passed as argument
	the value of the field.

   :secret CHAR
	Character used to display the value.  You can set this to
	e.g. `?*' if the field contains a password or other
	secret information.  By default, this is `nil', and the
	value is not secret.

   :size SIZE
	Specify the SIZE of string to be displayed.
	It can have the following values:

	integer			the size of string.

	(COLUMNS . LINES)	rectangular text area, both values
				are integers greater than zero.

   :tag TAG
	Usually, specify a field label.
	Some fields use TAG differently, see the field
	documentation above.


Dialog Derivation
-----------------

Sometimes you need to create a dialog B which is almost the same
as another dialog A, but it should add some extra fields in A, or
it should remove some fields from A.  This is what the dialog
derivation do, that is, a way to add/remove some fields from a
dialog in order to create a new one.

To derive a dialog from another one, just specify the :parent in a
dialog definition.  For example:

   (dialog-define example2
     '(:style window
	      [navigation 1 1 :tag \"Navigation\"]
	      [text 3 1 \"Hello World 1!!\"]
	      [button-quit 5 1]
	      [button-previous 5 10 :tag \"Hello :(\"])
     \"This is the parent dialog.\")

   (dialog-define example1
     '(:style (frame . mouse) :parent example2
	      ;; this is a new button
	      [button-quit 7 1 :tag \"New Quit Button\"]
	      ;; this text field removes the \"Hello :(\" button
	      [text 5 10 \" \"])
     \"This is the derived dialog.\")

So, if the new dialog element overlaps one of parent dialog elements,
the parent dialog element is removed.

The :parent specification have the following values:

   :parent DIALOG
   :parent (DIALOG . DECORATION)

Where DIALOG is the parent dialog and DECORATION specifies what to do
with decoration fields, that is, box, hline and vline fields.

DECORATION can have the following values:

   keep		keep all parent decoration.

   kill		kill all parent decoration.

   kill-overlap	kill parent decoration only when overlaps with some
		derived dialog field (decoration or not).

The DECORATION default value is keep.


Example
-------

As an example, here is a very simple dialog specification:

   (require 'dialog)

   (dialog-define hello1
     '(:style window
	      [navigation 1 1 :tag \"Navigation\"]
	      [text 3 1 \"Hello World 1!!\"]
	      [button-quit 5 1]
	      [button-previous 5 10 :tag \"Hello :(\"])
     \"This is a Hello World example.\")

   (dialog-define hello2
     '(:style window
	      [navigation 1 1 :tag \"Navigation\"]
	      [text 3 1 \"Hello World 2 !!\"]
	      [button-quit 5 1]
	      [button-next 5 10 hello1 :tag \"Hello :)\"])
     \"This is another Hello World example.\")

   (hello2)  ; or (dialog-run 'hello2) run dialog hello2"
  (list 'dialog-do-define (list 'quote dialog) spec doc))


;;;###autoload
(defun dialog-run (dialog)
  "Execute DIALOG.  See `dialog-define'."
  (when (dialogp dialog)
    (funcall dialog)))


;;;###autoload
(defun dialog-make-empty (dialog)
  "Define a new, empty dialog with name DIALOG.
If the dialog already exists, it is left unmodified.
Return DIALOG."
  (unless (dialogp dialog)
    (dialog-set dialog t t 'ignore nil))
  dialog)


;;;###autoload
(defun dialogp (object)
  "Return t if OBJECT is a dialog object."
  (and (symbolp object)		      ; it is a symbol...
       (boundp object)		      ; and symbol's value is not void...
       (fboundp object)		      ; and symbol's function is not void...
       (get object 'dialog-spec)      ; and symbol's property has `dialog-spec'
       (get object 'dialog-documentation) ; and also `dialog-documentation'.
       t))


(defun dialog-documentation (dialog)
  "Get the documentation string for DIALOG."
  (when (dialogp dialog)
    (get dialog 'dialog-documentation)))


(defun set-dialog-documentation (dialog doc)
  "Set the documentation string for DIALOG to DOC."
  (when (dialogp dialog)
    (put dialog 'dialog-documentation
	 (if (stringp doc) (purecopy doc) ""))))


(defalias 'dialog-doc-string     'dialog-documentation)
(defalias 'set-dialog-doc-string 'set-dialog-documentation)


(defun dialog-spec (dialog)
  "Get the DIALOG specification.  See `dialog-define'."
  (when (dialogp dialog)
    (get dialog 'dialog-spec)))


(defun set-dialog-spec (dialog spec)
  "Set the DIALOG specification.  See `dialog-define'."
  (when (dialogp dialog)
    (dialog-do-define1 dialog spec (dialog-documentation dialog))))


(defvar dialog-internal-sym-text-alist)	; forward declaration

(defun dialog-update-text (sym)
  "Update text field associated with symbol SYM.
See `dialog-define'."
  (dolist (field (cdr (assq sym dialog-internal-sym-text-alist)))
    (dialog-insert-text field)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions --- Dialog definition


;; Global var
(defvar dialog-frame-alist nil)


;; Local vars
(defvar dialog-internal-window-config   nil)
(defvar dialog-internal-style           nil)
(defvar dialog-internal-style-arg       nil)
(defvar dialog-internal-max-line        0)
(defvar dialog-internal-max-column      0)
(defvar dialog-internal-previous-dialog nil)
(defvar dialog-internal-next-dialog     nil)
(defvar dialog-internal-dialog          nil)
;; ALIST: (var tmp wid) or (var tmp (wid . val))
(defvar dialog-internal-variable-alist  nil)
(defvar dialog-internal-variable-count  0)
;; ALIST: (sym field...)
(defvar dialog-internal-sym-text-alist  nil)

;; this var is a work around due to a `set-frame-position' problem,
;; that is, when current frame has menu-bar-mode and/or tool-bar-mode
;; on, and the new frame has both of them off, the new frame is
;; positioned higher (relative to y axis) than it should.
(defvar dialog-internal-y-offset 0)	; HACK


;; Style vector index
(defconst dialog-style-type       0)
(defconst dialog-style-arg        1)
(defconst dialog-style-parent     2)
(defconst dialog-style-decoration 3)


;; Field vector index
(defconst dialog-field-type       0)
(defconst dialog-field-create     1)
(defconst dialog-field-line       2)
(defconst dialog-field-column     3)
(defconst dialog-field-line-end   4)
(defconst dialog-field-line2      4)
(defconst dialog-field-column-end 5)
(defconst dialog-field-column2    5)
(defconst dialog-field-arg        6)
(defconst dialog-field-notify     7)
(defconst dialog-field-tag        8)
(defconst dialog-field-help       9)
(defconst dialog-field-size       10)
(defconst dialog-field-action     11)
(defconst dialog-field-secret     12)


(defun dialog-buffer-name (dialog)
  (format "*Dialog %s*" dialog))


(defun dialog-set (dialog spec parsed fun doc)
  "Set unconditionally DIALOG symbol.

SPEC is the dialog specification.

PARSED is the dialog specification parsed.

FUN is the dialog activation function.

DOC is the dialog documentation.

See `dialog-define'."
  (set  dialog parsed)
  (fset dialog fun)
  (put  dialog 'dialog-spec spec)
  (put  dialog 'dialog-documentation
	(if (stringp doc) (purecopy doc) ""))
  (put  dialog 'dialog-derived-fields nil)
  dialog)


(defun dialog-do-define (dialog spec doc)
  "Like `dialog-define', but DIALOG is evaluated as a normal argument."
  (unless (dialogp dialog)
    (dialog-do-define1 dialog spec doc))
  dialog)


(defun dialog-do-define1 (dialog spec doc)
  "Like `dialog-do-define', but don't test if DIALOG is a dialog object."
  (dialog-set dialog
	      spec
	      (dialog-overlap-between-fields
	       dialog
	       (dialog-parse-spec dialog spec))
	      (list 'lambda ()
		    (list 'dialog-do-execute
			  (list 'quote dialog)))
	      doc))


(defun dialog-do-execute (dialog)
  "Execute DIALOG."
  (let ((window-config (current-window-configuration))
	(frame-y-offset			; HACK
	 (* (frame-char-height)
	    (+ (if (and menu-bar-mode
			(frame-parameter nil 'menu-bar-lines))
		   (* 2 (frame-parameter nil 'menu-bar-lines))
		 0)
	       (if (and tool-bar-mode
			(frame-parameter nil 'tool-bar-lines))
		   (1+ (* 2 (frame-parameter nil 'tool-bar-lines)))
		 0))))
	(frame-char (cons (frame-char-width) (frame-char-height)))
	(frame-size (cons (frame-pixel-width) (frame-pixel-height)))
	(frame-pos  (cons (frame-parameter nil 'left)
			  (frame-parameter nil 'top)))
	(mouse-pos  (cdr (mouse-pixel-position)))
	(point-pos  (cons (+ dialog-extra-columns
			     (car (window-edges))
			     (current-column))
			  (+ dialog-extra-lines
			     (cadr (window-edges))
			     (count-lines (window-start) (point))
			     (if (= (current-column) 0) 1 0))))
	(buffer (get-buffer-create (dialog-buffer-name dialog)))
	(style  (aref (car (symbol-value dialog)) dialog-style-type))
	(arg    (aref (car (symbol-value dialog)) dialog-style-arg))
	(previous-dialog dialog-internal-dialog)
	(previous-style  dialog-internal-style))
    ;; initialize buffer
    (set-buffer buffer)
    (kill-all-local-variables)
    (let ((inhibit-read-only t)
	  (ol (overlay-lists)))
      ;; delete all the overlays.
      (mapc 'delete-overlay (car ol))
      (mapc 'delete-overlay (cdr ol))
      (erase-buffer))
    ;; initialize local vars
    (set (make-local-variable 'dialog-internal-window-config)   window-config)
    (set (make-local-variable 'dialog-internal-style)           style)
    (set (make-local-variable 'dialog-internal-style-arg)       arg)
    (set (make-local-variable 'dialog-internal-max-line)        1)
    (set (make-local-variable 'dialog-internal-max-column)      0)
    (set (make-local-variable 'dialog-internal-previous-dialog) previous-dialog)
    (set (make-local-variable 'dialog-internal-next-dialog)     nil)
    (set (make-local-variable 'dialog-internal-dialog)          dialog)
    (set (make-local-variable 'dialog-internal-variable-alist)  nil)
    (set (make-local-variable 'dialog-internal-variable-count)  0)
    (set (make-local-variable 'dialog-internal-sym-text-alist)  nil)
    ;; HACK
    (set (make-local-variable 'dialog-internal-y-offset) frame-y-offset)
    ;; hooks
    (dialog-add-hooks)
    ;; create fields
    (dolist (field (dialog-derive-fields dialog))
      (dialog-goto-line-column (aref field dialog-field-line)
			       (aref field dialog-field-column))
      (funcall (aref field dialog-field-create)
	       field dialog previous-dialog))
    ;; adjust window/frame
    (dialog-pop-to-buffer dialog style arg buffer
			  t previous-style
			  frame-char frame-size frame-pos
			  point-pos mouse-pos)
    ;; start widget
    (use-local-map widget-keymap)
    (widget-setup)))


(defun dialog-pop-to-buffer (dialog style style-arg buffer
				    &optional create-p previous-style
				    frame-char frame-size frame-pos
				    point-pos mouse-pos)
  "Create dialog window and then display dialog buffer in it.

DIALOG is the dialog.

STYLE is the dialog style.

STYLE-ARG is the dialog style argument.

BUFFER is the dialog buffer.

PREVIOUS-STYLE is the previous dialog style.

CREATE-P indicates if buffer is being created now;
otherwise, the buffer already exists and will be used now.

See `dialog-make-frame' for documentation about FRAME-CHAR,
FRAME-SIZE, FRAME-POS, POINT-POS and MOUSE-POS arguments."
  ;; handle dialog style
  (cond
   ;; split-window-horizontally
   ((eq style 'split-window-horizontally)
    (when (eq previous-style 'split-window-horizontally)
      (delete-windows-on (current-buffer)))
    (split-window-horizontally style-arg))
   ;; split-window-vertically
   ((eq style 'split-window-vertically)
    (when (eq previous-style 'split-window-vertically)
      (delete-windows-on (current-buffer)))
    (split-window-vertically style-arg))
   ;; window-system and frame
   ((and create-p window-system (eq style 'frame))
    (dialog-make-frame dialog buffer style-arg
		       frame-char frame-size frame-pos
		       point-pos mouse-pos))
   ;; window or (frame and not (create-p and window-system))
   (t
    (delete-other-windows)))
  ;; display buffer in recent window
  (let (pop-up-windows)
    (pop-to-buffer buffer))
  ;; eventually, fit window to buffer
  (when (and (eq style 'split-window-vertically)
	     (null style-arg))		; honor user setting
    (fit-window-to-buffer)))


(defun dialog-make-frame (dialog buffer position
				 frame-char frame-size frame-pos
				 point-pos mouse-pos)
  "Make a DIALOG frame displaying BUFFER.

POSITION is the kind of frame position.  It can be:
   (X . Y)	the position in pixels.
   point	the current point position.
   mouse	the current mouse position.
   center	the new frame is centralized in the selected frame.
   frame	the upper left corner of the selected frame.

FRAME-CHAR is the frame char dimensions in pixel.  It has the form:
   (frame-char-width . frame-char-height)

FRAME-SIZE is the frame size in pixel.  It has the form:
   (frame-width . frame-height)

FRAME-POS is the frame position in pixel.  It has the form:
   (left . top)

POINT-POS is the point position in characters.  It has the form:
   (column . line)

MOUSE-POS is the mouse position in pixel. It has the form:
   (left . top)"
  (goto-char (point-min))
  (let* ( ;; frame width
	 (col  (max dialog-frame-min-width
		    dialog-internal-max-column))
	 ;; frame height
	 (lin  (+ 2
		  (max dialog-frame-min-height
		       dialog-internal-max-line)))
	 ;; frame left
	 (left (cond ((consp position)
		      (car position))
		     ((eq position 'point)
		      (+ (car frame-pos)
			 (* (car frame-char)
			    (car point-pos))))
		     ((eq position 'mouse)
		      (+ (or (car mouse-pos) 0)
			 (car frame-pos)))
		     ((eq position 'frame)
		      (car frame-pos))
		     ((eq position 'center)
		      (+ (car frame-pos)
			 (/ (- (car frame-size)
			       (* (+ col
				     dialog-extra-columns)
				  (car frame-char)))
			    2)))
		     (t 0)))
	 ;; frame top
	 (top  (+ dialog-internal-y-offset ; HACK
		  (cond ((consp position)
			 (cdr position))
			((eq position 'point)
			 (+ (cdr frame-pos)
			    (* (cdr frame-char)
			       (cdr point-pos))))
			((eq position 'mouse)
			 (+ (or (cdr mouse-pos) 0)
			    (cdr frame-pos)))
			((eq position 'frame)
			 (cdr frame-pos))
			((eq position 'center)
			 (+ (cdr frame-pos)
			    (/ (- (cdr frame-size)
				  (* (+ lin
					dialog-extra-lines)
				     (cdr frame-char)))
			       2)))
			(t 0))))
	 ;; frame name
	 (name  (format " .: %s :. " (symbol-name dialog)))
	 ;; The frame
	 (frame (select-frame
		 (make-frame
		  (list (cons 'title  name)
			(cons 'name   name)
			(cons 'width  col)
			(cons 'height lin)
			(cons 'left   left)
			(cons 'top    top)
			'(user-size      . t)
			'(user-position  . t)
			'(menu-bar-lines . nil)
			'(tool-bar-lines . nil)
			'(minibuffer     . nil))))))
    (dialog-add-frame-alist frame buffer)))


(defun dialog-create-text (field dialog previous-dialog)
  "Create a text FIELD."
  (dialog-add-symbol-alist field)
  (dialog-insert-text field))


(defun dialog-insert-text (field)
  "Insert the text of the text FIELD."
  (dialog-goto-line-column (aref field dialog-field-line)
			   (aref field dialog-field-column))
  (dialog-insert (aref field dialog-field-size)
		 (dialog-text-eval (aref field dialog-field-arg))))


(defun dialog-create-button-text (field default)
  "Create a text button FIELD.

DEFAULT is the default tag."
  (dialog-insert nil "[" (or (aref field dialog-field-tag) default) "]"))


(defun dialog-create-hline (field dialog previous-dialog)
  "Create a horizontal line FIELD."
  (dialog-create-hline1 (aref field dialog-field-line)
			(aref field dialog-field-column)
			(aref field dialog-field-arg)
			(aref (aref field dialog-field-tag) 0)))


(defun dialog-create-hline1 (line column length hchar)
  "Create horizontal line at LINE and COLUMN with LENGTH characters HCHAR."
  (dialog-goto-line-column line column)
  (dialog-insert nil (make-string length hchar)))


(defun dialog-create-vline (field dialog previous-dialog)
  "Create a vertical line FIELD."
  (dialog-create-vline1 (aref field dialog-field-line)
			(aref field dialog-field-column)
			(aref field dialog-field-arg)
			(aref (aref field dialog-field-tag) 0)))


(defun dialog-create-vline1 (line column length vchar)
  "Create vertical line at LINE and COLUMN with LENGTH characters VCHAR."
  (let ((vstr (char-to-string vchar)))
    (dotimes (i length)
      (dialog-goto-line-column (+ line i) column)
      (dialog-insert nil vstr))))


(defun dialog-create-box (field dialog previous-dialog)
  "Create a box FIELD."
  (let ((lower-lin (min (aref field dialog-field-line)
			(aref field dialog-field-line2)))
	(upper-lin (1- (max (aref field dialog-field-line)
			    (aref field dialog-field-line2))))
	(lower-col (min (aref field dialog-field-column)
			(aref field dialog-field-column2)))
	(upper-col (1- (max (aref field dialog-field-column)
			    (aref field dialog-field-column2))))
	(border (aref field dialog-field-tag)))
    (dialog-goto-line-column upper-lin upper-col)
    (cond
     ;; null box
     ((and (= lower-lin upper-lin) (= lower-col upper-col))
      (dialog-insert nil (char-to-string (aref border 0))))
     ;; horizontal line
     ((= lower-lin upper-lin)
      (dialog-create-hline1 lower-lin lower-col
			    (- upper-col lower-col)
			    (aref border 1)))
     ;; vertical line
     ((= lower-col upper-col)
      (dialog-create-vline1 lower-lin lower-col
			    (- upper-lin lower-lin)
			    (aref border 2)))
     ;; box
     (t
      (let ((hlen (1+ (- upper-col lower-col)))
	    (hstr (make-string (- upper-col lower-col 1)
			       (aref border 1)))
	    (vstr (char-to-string (aref border 2)))
	    (llin (1+ lower-lin)))
	;; top border
	(dialog-goto-line-column lower-lin lower-col)
	(dialog-insert nil (char-to-string (aref border 3))
		       hstr
		       (char-to-string (aref border 4)))
	;; bottom border
	(dialog-goto-line-column upper-lin lower-col)
	(dialog-insert nil (char-to-string (aref border 6))
		       hstr
		       (char-to-string (aref border 5)))
	;; vertical borders
	(dotimes (i (- upper-lin lower-lin 1))
	  (dialog-goto-line-column (+ llin i) lower-col)
	  (dialog-insert nil vstr)
	  (dialog-goto-line-column (+ llin i) upper-col)
	  (dialog-insert nil vstr)))))))


(defun dialog-create-navigation (field dialog previous-dialog)
  (let ((beg (current-column))
	dlist previous)
    ;; get previous dialogs
    (save-excursion
      (while dialog-internal-previous-dialog
	(setq previous dialog-internal-previous-dialog)
	(when (dialog-set-buffer previous)
	  (setq dlist (cons previous dlist)))))
    ;; insert tag, if exists
    (when (aref field dialog-field-tag)
      (dialog-insert nil (aref field dialog-field-tag) ": "))
    ;; insert buttons to previous dialogs
    (dolist (dlg (nreverse dlist))
      (dialog-delete-region (+ (length (symbol-name dlg)) 6))
      (widget-create 'push-button
		     :notify (dialog-create-goto-function dlg)
		     :help-echo (aref field dialog-field-help)
		     (symbol-name dlg))
      (widget-insert " :: "))
    ;; insert current dialog
    (dialog-insert nil (symbol-name dialog))))


(defun dialog-create-button-if (condition field dfun dtag)
  "Create a button FIELD, depending on CONDITION.

If CONDITION is non-nil, create a button FIELD;
otherwise, create a text button.

DFUN is the button action.  It should be a function or nil.

DTAG is the button tag.  It should be a string."
  (if condition
      (dialog-create-button1 field dfun dtag)
    (dialog-create-button-text field dtag)))


(defun dialog-create-button (field dialog previous-dialog)
  "Create a button FIELD."
  (dialog-create-button1 field 'dialog-action-quit "Button"))


(defun dialog-create-button-cancel (field dialog previous-dialog)
  "Create a cancel button FIELD."
  (dialog-create-button1 field 'dialog-action-cancel "Cancel"))


(defun dialog-create-button-next (field dialog previous-dialog)
  "Create a next button FIELD."
  (let ((dialog (aref field dialog-field-arg)))
    (dialog-create-button-if
     (dialogp dialog)
     field
     (dialog-create-goto-function dialog) "Next")))


(defun dialog-create-button-ok (field dialog previous-dialog)
  "Create an ok button FIELD."
  (dialog-create-button1 field 'dialog-action-save-and-cancel "Ok"))


(defun dialog-create-button-previous (field dialog previous-dialog)
  "Create a previous button FIELD."
  (dialog-create-button-if
   previous-dialog
   field
   'dialog-action-goto-previous "Previous"))


(defun dialog-create-button-quit (field dialog previous-dialog)
  "Create a quit button FIELD."
  (dialog-create-button1 field 'dialog-action-quit "Quit"))


(defun dialog-create-button-reset (field dialog previous-dialog)
  "Create a reset button FIELD."
  (dialog-create-button1 field 'dialog-action-reset "Reset"))


(defun dialog-create-button1 (field dfun dtag)
  "Create a button FIELD.

DFUN is the button action.  It should be a function or nil.

DTAG is the button tag.  It should be a string."
  (let ((tag (or (aref field dialog-field-tag) dtag)))
    (dialog-delete-region (+ (length tag) 2))
    (widget-create 'push-button
		   :notify (let ((fun
				  (aref field dialog-field-notify)))
			     (if fun
				 (list 'lambda '(&rest ignore)
				       (list 'funcall fun)
				       (list dfun))
			       dfun))
		   :help-echo (aref field dialog-field-help)
		   tag)))


(defun dialog-create-menu (field dialog previous-dialog)
  "Create a menu FIELD."
  (let* ((var   (car (aref field dialog-field-arg)))
	 (tmp   (dialog-make-temp-var var))
	 (alist (cdr (aref field dialog-field-arg)))
	 (tag   (aref field dialog-field-tag))
	 (fun   (aref field dialog-field-notify))
	 (max   0))
    (mapc #'(lambda (item)
	      (setq max (max max (length (cdr item)))))
	  alist)
    (dialog-delete-region (+ (length tag) 2 (max max 20)))
    (dialog-add-variable-alist
     var tmp
     (apply 'widget-create 'menu-choice
	    :tag tag
	    :format (if tag "%[%t%]: %v" "%v")
	    :value (symbol-value tmp)
	    :help-echo (aref field dialog-field-help)
	    :notify (dialog-create-function
		     'dialog-internal-function-notify
		     'widget
		     (list 'quote tmp)
		     (dialog-arg-function fun))
	    :void '(choice-item :format "%[%t%]"
				:tag    "Can't display value!")
	    ;; menu items
	    (mapcar #'(lambda (item)
			(list 'choice-item
			      :format "%[%t%]"
			      :value  (car item)
			      :tag    (concat
				       (cdr item)
				       (make-string
					(- max (length (cdr item)))
					?\ ))))
		    alist)))))


(defun dialog-create-checkbox (field dialog previous-dialog)
  "Create a checkbox FIELD."
  (let* ((var (aref field dialog-field-arg))
	 (tmp (dialog-make-temp-var var))
	 (fun (aref field dialog-field-notify))
	 (tag (aref field dialog-field-tag)))
    (when (and tag (= (aref tag 0) ?-))
      (dialog-insert nil (substring tag 1) " "))
    (dialog-delete-region 1)
    (dialog-add-variable-alist
     var tmp
     (widget-create 'checkbox
		    :help-echo (aref field dialog-field-help)
		    :notify (dialog-create-function
			     'dialog-internal-function-notify-value
			     (list 'not tmp)
			     (list 'quote tmp)
			     (dialog-arg-function fun))
		    (symbol-value tmp)))
    (when (and tag (/= (aref tag 0) ?-))
      (dialog-insert nil " " (substring tag 1)))))


(defun dialog-create-radio (field dialog previous-dialog)
  "Create a radio FIELD."
  (let* ((var (car (aref field dialog-field-arg)))
	 (val (cdr (aref field dialog-field-arg)))
	 (tmp (dialog-make-temp-var var))
	 (fun (aref field dialog-field-notify))
	 (tag (aref field dialog-field-tag)))
    (when (and tag (= (aref tag 0) ?-))
      (dialog-insert nil (substring tag 1) " "))
    (dialog-delete-region 1)
    (dialog-add-variable-alist
     var tmp
     (widget-create 'radio-button
		    :help-echo (aref field dialog-field-help)
		    :value     (eq (symbol-value tmp) val)
		    :notify (dialog-create-function
			     'dialog-update-radio
			     (list 'quote var)
			     (list 'quote val)
			     (dialog-arg-function fun)))
     val t)
    (when (and tag (/= (aref tag 0) ?-))
      (dialog-insert nil " " (substring tag 1)))))


(defun dialog-create-editable (field dialog previous-dialog)
  "Create an editable FIELD."
  (let* ((kind     (car (aref field dialog-field-arg)))
	 (var      (cdr (aref field dialog-field-arg)))
	 (number-p (and (memq kind '(number integer float)) t))
	 (tmp      (dialog-make-temp-var var))
	 (notify   (aref field dialog-field-notify))
	 (action   (aref field dialog-field-action))
	 (size     (aref field dialog-field-size))
	 (tag      (or (aref field dialog-field-tag)
		       (capitalize (symbol-name kind))))
	 (waction  (if action
		       (dialog-create-action-function
			kind action number-p)
		     (dialog-default-action-function kind)))
	 (wnotify  (dialog-create-function
		    'dialog-internal-function-notify
		    'widget
		    (list 'quote tmp)
		    (dialog-arg-function notify)
		    number-p)))
    (dialog-delete-region (+ (length tag) (or size 1) 2))
    (dialog-add-variable-alist
     var tmp
     (widget-create kind
		    :help-echo (aref field dialog-field-help)
		    :tag       tag
		    :size      size
		    :secret    (aref field dialog-field-secret)
		    :action    waction
		    :notify    wnotify)
     (symbol-value var) t)
    (when size
      (dialog-insert nil " "))))


(defun dialog-create-goto-function (dialog)
  "Create a widget function which goes to dialog DIALOG."
  (dialog-create-function
   'dialog-action-goto-dialog (list 'quote dialog)))


(defun dialog-create-function (fun &rest args)
  "Create a widget function which calls FUN with arguments ARGS."
  (list 'lambda '(widget &rest args)
	(apply 'list fun args)))


(defun dialog-create-action-function (kind action number-p)
  "Create an `:action' widget function."
  (list 'lambda '(widget &optional event)
	(list 'when
	      (list 'funcall (dialog-arg-function action)
		    (list 'dialog-widget-value 'widget number-p))
	      (list (dialog-default-action-function kind)
		    'widget 'event))))


(defun dialog-default-action-function (kind)
  "Return the default `:action' widget function.

KIND is the widget kind."
  (cond ((eq kind 'coding-system) 'widget-coding-system-action)
	((eq kind 'color)         'widget-color-action)
	(t                        'widget-field-action)))


(defun dialog-arg-function (fun)
  "Return function FUN as an argument."
  (cond ((null fun)      nil)		    ; no function
	((symbolp fun)   (list 'quote fun)) ; symbol function
	((functionp fun) fun)		    ; lambda function
	(t               nil)))		    ; no function


(defun dialog-internal-function-notify (widget sym-var fun
					       &optional numberp)
  (dialog-internal-function-notify-value
   (dialog-widget-value widget numberp)
   sym-var fun))


(defun dialog-widget-value (widget &optional numberp)
  "Return the WIDGET value.

NUMBERP indicates if WIDGET is a numeric widget."
  (if (string= (widget-apply widget :value-get) "")
      (if numberp 0 "")
    (widget-value widget)))


(defun dialog-internal-function-notify-value (value sym-var fun)
  (set sym-var value)
  (when fun
    (funcall fun value)))


(defun dialog-text-eval (arg)
  "Evaluate ARG to string.

If ARG is a string, return the string.
If ARG is a symbol variable, get the variable value.
If ARG is a symbol function or a function, the function is
evaluated without argument.
If ARG is a list and the list header is a function, the function
is evaluated with list tail as the arguments.
Any other value, return an empty string.
If the result of the variable or function evaluation is not a
string, it evaluates recursively until a string is returned."
  (let ((val (cond
	      ((symbolp arg)		; symbol
	       (cond
		((boundp arg)  (symbol-value arg))
		((fboundp arg) (funcall arg))
		(t             "")))
	      ((functionp arg)		; function
	       (funcall arg))
	      ((and (listp arg)		; list
		    (functionp (car arg)))
	       (apply (car arg) (cdr arg)))
	      ((stringp arg)		; string
	       arg)
	      (t			; anything else
	       ""))))
    (if (stringp val)
	val
      (dialog-text-eval val))))


(defun dialog-insert (size &rest args)
  "Insert strings in ARGS until SIZE characters.
If SIZE is nil, all strings in ARGS are inserted.
If SIZE is lesser than or equal to zero, nothing happens."
  (when (or (null size) (> size 0))
    (if size
	;; limit the length of all strings in ARGS to SIZE
	(let ((alist args)
	      (nchar size)
	      len last)
	  (while alist
	    (setq len (length (car alist)))
	    (cond ((> nchar len)
		   (setq nchar (- nchar len)))
		  ((< nchar len)
		   (setcar alist
			   (if (> nchar 1)
			       (substring (car alist) 0 (1- nchar))
			     (char-to-string ?\x8BB)))
		   (setcdr alist
			   (if (> nchar 1)
			       (cons (char-to-string ?\x8BB) nil)
			     nil))
		   (setq nchar 0)
		   (setq alist nil))
		  (t
		   (setq nchar 0)
		   (setcdr alist nil)))
	    (setq last  alist
		  alist (cdr alist)))
	  (when (> nchar 0)
	    (setcdr last (cons (make-string nchar ?\s) nil))))
      ;; calculate the lenght of all strings in ARGS
      (setq size 0)
      (dolist (arg args)
	(setq size (+ size (length arg)))))
    ;; insert ARGS in SIZE columns
    (dialog-delete-region size)
    (apply 'widget-insert args)))


(defun dialog-goto-line-column (line column)
  "Goto line LINE and then move point to column COLUMN.
See `dialog-goto-line' and `dialog-move-to-column'."
  (dialog-goto-line line)
  (dialog-move-to-column column))


(defun dialog-goto-line (line)
  "Like `goto-line', but LINE can go beyond end of buffer."
  (if (<= line dialog-internal-max-line)
      (goto-line line)
    (goto-char (point-max))
    (widget-insert (make-string (- line dialog-internal-max-line) ?\n))
    (setq dialog-internal-max-line line)))


(defun dialog-move-to-column (column)
  "Like `move-to-column'."
  (move-to-column column t)
  (dialog-internal-max-column))


(defun dialog-delete-region (length)
  "Delete text between point and LENGTH characters forward."
  (delete-region (save-excursion
		   (move-to-column (+ (current-column) length) t)
		   (dialog-internal-max-column)
		   (point))
		 (point)))


(defun dialog-internal-max-column ()
  "Set the maximum column number into `dialog-internal-max-column'."
  (setq dialog-internal-max-column (max dialog-internal-max-column
					(current-column))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions --- Parsing


(defconst dialog-style-values
  '(
    split-window-horizontally
    split-window-vertically
    window
    frame
    )
  "List of valid dialog style.")


(defconst dialog-style-frame-values
  '(
    point
    mouse
    center
    frame
    )
  "List of valid :position values for :style frame.")


(defconst dialog-editable-field-list
  '(
    character
    coding-system
    color
    directory
    file
    float
    integer
    key-sequence
    number
    regexp
    sexp
    string
    symbol
    text
    variable
    )
  "List of valid editable fields.")


(defconst dialog-parent-decor-values
  '(
    keep
    kill
    kill-overlap
    )
  "List of valid :parent decoration values.")


(defconst dialog-create-field-alist
  '(
    (box             . dialog-create-box)
    (button          . dialog-create-button)
    (button-cancel   . dialog-create-button-cancel)
    (button-next     . dialog-create-button-next)
    (button-ok       . dialog-create-button-ok)
    (button-previous . dialog-create-button-previous)
    (button-quit     . dialog-create-button-quit)
    (button-reset    . dialog-create-button-reset)
    (checkbox        . dialog-create-checkbox)
    (editable        . dialog-create-editable)
    (hline           . dialog-create-hline)
    (menu            . dialog-create-menu)
    (navigation      . dialog-create-navigation)
    (radio           . dialog-create-radio)
    (text            . dialog-create-text)
    (vline           . dialog-create-vline)
    )
  "Alist which associates dialog type field with a field creation function.")


(defun dialog-parse-spec (dialog spec)
  "Parse SPEC for DIALOG and return a parsed structure."
  (let ( ;; Parse dialog keywords
	(keywords (dialog-parse-spec-keywords dialog spec)))
    (cons (car keywords)
	  ;; Parse dialog fields
	  (dialog-parse-spec-fields dialog (cdr keywords)))))


(defun dialog-parse-spec-keywords (dialog spec)
  "Parse SPEC for DIALOG keywords."
  (let (keyword arg style value parent decoration)
    (while (and spec
		(cdr spec)
		(keywordp (setq keyword (car spec))))
      (setq arg  (cadr spec)
	    spec (cddr spec))
      (cond
       ;; :parent DIALOG
       ;; :parent (DIALOG . DECORATION)
       ((eq keyword :parent)
	(if (consp arg)
	    (setq parent     (car arg)
		  decoration (cdr arg))
	  (setq parent     arg
		decoration 'keep))
	(unless (and parent (symbolp parent)
		     (memq decoration dialog-parent-decor-values))
	  (dialog-error
	   dialog ":parent keyword value `%s' is not valid"
	   arg)))
       ;; :style window
       ;; :style split-window-vertically
       ;; :style (split-window-vertically . ARG)
       ;; :style split-window-horizontally
       ;; :style (split-window-horizontally . ARG)
       ;; :style frame
       ;; :style (frame . (X . Y))
       ;; :style (frame . point)
       ;; :style (frame . mouse)
       ;; :style (frame . center)
       ;; :style (frame . frame)
       ((eq keyword :style)
	(if (consp arg)
	    (setq style (car arg)
		  value (cdr arg))
	  (setq style arg
		value nil))
	;; check style
	(unless (memq style dialog-style-values)
	  (dialog-error
	   dialog ":style keyword value `%s' is not a valid style"
	   style))
	;; check value
	(cond
	 ((null value))
	 ((eq style 'window)
	  (dialog-error
	   dialog ":style window does not have any argument"))
	 ((memq style '(split-window-horizontally
			split-window-vertically))
	  (unless (integerp value)
	    (dialog-error
	     dialog ":style %s argument value `%s' is not an integer"
	     style value)))
	 ((eq style 'frame)
	  (unless (or (memq value dialog-style-frame-values)
		      (and (consp value)
			   (integerp (car value))
			   (integerp (cdr value))))
	    (dialog-error
	     dialog ":style frame argument value `%s' is not a valid position"
	     value)))))
       ;; otherwise, error!!!
       (t
	(dialog-error
	 dialog "`%s' is not a valid keyword"
	 keyword))))
    (when (or
	   ;; if :style is omitted...
	   (null style)
	   ;; or if it's not a windowing manager and style is frame...
	   (and (not window-system) (eq style 'frame)))
      ;; ...force `window' instead of `frame'
      (setq style 'window
	    value nil))
    ;; return dialog style structure
    (cons (vector style value parent decoration) spec)))


(defun dialog-parse-spec-fields (dialog spec)
  "Parse DIALOG for SPEC fields."
  (let (parsed)
    ;; Parse dialog fields
    (when (null spec)
      (dialog-error
       dialog "specification must have at least one field"))
    (dolist (field spec)
      ;; A field must be a vector...
      (unless (vectorp field)
	(dialog-error
	 dialog "field specification must be a vector"))
      ;; ...and it must have a minimum length.
      (unless (>= (length field) 3)
	(dialog-error
	 dialog "invalid vector field specification"))
      (setq parsed (cons (dialog-parse-field field dialog) parsed)))
    (nreverse parsed)))


(defun dialog-parse-field (field dialog)
  "Parse one FIELD of DIALOG."
  (let ((type       (aref field 0))
	(line-end   0)
	(column-end 0)
	arg keywords)
    ;; parse LINE and COLUMN first
    (dialog-parse-field-line-column field dialog)
    (cond
     ;; [text LINE COLUMN TEXT :size SIZE]
     ((eq type 'text)
      (dialog-index-is-inside 3 field dialog)
      (let ((text (aref field 3)))
	(unless (and text
		     (or (stringp text)
			 (symbolp text)
			 (listp text)))
	  (dialog-error-field
	   dialog field "TEXT must be string, symbol or list"))
	(setq arg (copy-sequence text)))
      (setq keywords (dialog-parse-field-keywords
		      '(:size) 4 field dialog))
      ;; set field boundary
      (let ((size (nth 3 keywords)))
	(setq line-end   (+ (aref field 1)
			    (if (consp size)
				(car size)
			      1))
	      column-end (+ (aref field 2)
			    (cond ((null size)
				   (if (stringp arg)
				       (length arg)
				     100)) ; occupy rest of line
				  ((consp size)
				   (cdr size))
				  (t
				   size))))))
     ;; [navigation LINE COLUMN :tag TAG :help-echo HELP]
     ((eq type 'navigation)
      (dialog-parse-field-line-column field dialog)
      (setq keywords (dialog-parse-field-keywords
		      '(:tag :help-echo) 3 field dialog))
      ;; set field boundary
      (setq line-end   (1+ (aref field 1))
	    column-end (+ (aref field 2) 100)))	; occupy rest of line
     ;; [button          LINE COLUMN :tag TAG :notify FUNCTION :help-echo HELP]
     ;; [button-ok       LINE COLUMN :tag TAG :notify FUNCTION :help-echo HELP]
     ;; [button-cancel   LINE COLUMN :tag TAG :notify FUNCTION :help-echo HELP]
     ;; [button-reset    LINE COLUMN :tag TAG :notify FUNCTION :help-echo HELP]
     ;; [button-quit     LINE COLUMN :tag TAG :notify FUNCTION :help-echo HELP]
     ;; [button-previous LINE COLUMN :tag TAG :notify FUNCTION :help-echo HELP]
     ((memq type '(button button-ok button-cancel button-reset
			  button-quit button-previous))
      (setq keywords (dialog-parse-field-keywords
		      '(:tag :notify :help-echo) 3 field dialog))
      ;; set field boundary
      (setq line-end   (1+ (aref field 1))
	    column-end (+ (aref field 2) 2
			  (cond ((nth 1 keywords)
				 (length (nth 1 keywords)))
				((eq type 'button)          6)
				((eq type 'button-ok)       2)
				((eq type 'button-cancel)   6)
				((eq type 'button-reset)    5)
				((eq type 'button-quit)     4)
				((eq type 'button-previous) 8)))))
     ;; [button-next LINE COLUMN DIALOG
     ;;       :tag TAG :notify FUNCTION :help-echo HELP
     ((eq type 'button-next)
      (dialog-index-is-inside 3 field dialog)
      (setq arg (aref field 3))
      (unless (and arg (symbolp arg))
	(dialog-error-field
	 dialog field "`%s' is not a DIALOG symbol" arg))
      (setq keywords (dialog-parse-field-keywords
		      '(:tag :notify :help-echo) 4 field dialog))
      ;; set field boundary
      (setq line-end   (1+ (aref field 1))
	    column-end (+ (aref field 2) 2
			  (if (nth 1 keywords)
			      (length (nth 1 keywords))
			    4))))
     ;; [hline LINE COLUMN LENGTH :tag TAG]
     ;; [vline LINE COLUMN LENGTH :tag TAG]
     ((memq type '(hline vline))
      (dialog-index-is-inside 3 field dialog)
      (setq arg (aref field 3))
      (unless (and arg (integerp arg) (> arg 0))
	(dialog-error-field
	 dialog field "LENGTH must be an integer greater than zero"))
      (setq keywords (dialog-parse-field-keywords
		      '(:tag) 4 field dialog))
      (unless (nth 1 keywords)		; TAG default value
	(setcar (cdr keywords) (if (eq type 'hline) "-" "|")))
      (unless (> (length (nth 1 keywords)) 0)
	(dialog-error-field
	 dialog field "TAG can't be an empty string"))
      ;; set field boundary
      (setq line-end   (if (eq type 'hline)
			   (1+ (aref field 1))
			 (+ (aref field 1) arg))
	    column-end (if (eq type 'hline)
			   (+ (aref field 2) arg)
			 (1+ (aref field 2)))))
     ;; [box LINE COLUMN LINE2 COLUMN2 :tag TAG]
     ((eq type 'box)
      (dialog-parse-field-line-column field dialog 3)
      (setq keywords (dialog-parse-field-keywords
		      '(:tag) 5 field dialog))
      (unless (nth 1 keywords)		; TAG default value
	(setcar (cdr keywords) ".-|++++"))
      (unless (>= (length (nth 1 keywords)) 7)
	(dialog-error-field
	 dialog field "TAG length must be equal or greater than 7"))
      ;; adjust boundary values
      (setq line-end   (1+ (max (aref field 1) (aref field 3)))
	    column-end (1+ (max (aref field 2) (aref field 4))))
      (aset field 1 (min (aref field 1) (aref field 3)))
      (aset field 2 (min (aref field 2) (aref field 4))))
     ;; [menu LINE COLUMN VAR ALIST
     ;;       :tag TAG :notify FUNCTION :help-echo HELP
     ;; ALIST: (atom . string)
     ((eq type 'menu)
      (dialog-index-is-inside 4 field dialog)
      (setq arg (aref field 3))
      (unless (and arg (symbolp arg) (boundp arg))
	(dialog-error-field
	 dialog field "VARIABLE must be a symbol variable"))
      ;; (VAR ALIST)
      (setq arg (cons arg (aref field 4)))
      (unless (and (cdr arg)
		   (let ((is-alist t))
		     (mapc #'(lambda (item)
			       (setq is-alist
				     (and is-alist
					  (consp item)
					  ;; option value
					  (or (symbolp (car item))
					      (stringp (car item)))
					  ;; option tag
					  (and (stringp (cdr item))
					       ;; get max option len
					       (setq column-end
						     (max column-end
							  (length
							   (cdr item))))))))
			   (cdr arg))
		     is-alist))
	(dialog-error-field
	 dialog field
	 "ALIST must be an alist of (symbol . string) or (string . string)"))
      (setq keywords (dialog-parse-field-keywords
		      '(:tag :notify :help-echo) 5 field dialog))
      ;; set field boundary
      (setq line-end   (1+ (aref field 1))
	    column-end (+ column-end (aref field 2)
			  (if (nth 1 keywords)
			      (+ (length (nth 1 keywords)) 2)
			    0))))
     ;; [checkbox LINE COLUMN VAR
     ;;       :tag TAG :notify FUNCTION :help-echo HELP
     ((eq type 'checkbox)
      (dialog-index-is-inside 3 field dialog)
      (setq arg (aref field 3))
      (unless (and arg (symbolp arg) (boundp arg))
	(dialog-error-field
	 dialog field "VARIABLE must be a symbol variable"))
      (setq keywords (dialog-parse-field-keywords
		      '(:tag :notify :help-echo) 4 field dialog))
      ;; set field boundary
      (setq line-end   (1+ (aref field 1))
	    column-end (+ (aref field 2) 1
			  (if (nth 1 keywords)
			      (length (nth 1 keywords))
			    0))))
     ;; [radio LINE COLUMN VAR VALUE
     ;;       :tag TAG :notify FUNCTION :help-echo HELP]
     ((eq type 'radio)
      (dialog-index-is-inside 4 field dialog)
      (setq arg (aref field 3))
      (unless (and arg (symbolp arg) (boundp arg))
	(dialog-error-field
	 dialog field "VARIABLE must be a symbol variable"))
      ;; (VAR . VALUE)
      (setq arg (cons arg (aref field 4)))
      (setq keywords (dialog-parse-field-keywords
		      '(:tag :notify :help-echo) 5 field dialog))
      ;; set field boundary
      (setq line-end   (1+ (aref field 1))
	    column-end (+ (aref field 2) 3
			  (if (nth 1 keywords)
			      (length (nth 1 keywords))
			    0))))
     ;; [editable LINE COLUMN KIND VAR
     ;;       :tag TAG :notify FUNCTION :help-echo HELP
     ;;       :size SIZE :action FUNCTION :secret CHAR]
     ((eq type 'editable)
      (dialog-index-is-inside 4 field dialog)
      (let ((var  (aref field 4))
	    (kind (aref field 3)))
	(unless (and (symbolp kind)
		     (memq kind dialog-editable-field-list))
	  (dialog-error-field
	   dialog field
	   "KIND must be a symbol which is contained in `dialog-editable-field-list'"))
	(unless (and var (symbolp var) (boundp var))
	  (dialog-error-field
	   dialog field "VARIABLE must be a symbol variable"))
	;; (KIND . VAR)
	(setq arg (cons kind var))
	(setq keywords (dialog-parse-field-keywords
			'(:tag :notify :help-echo :size
			       :secret :action)
			5 field dialog)))
      ;; set field boundary
      (let ((size (nth 3 keywords))
	    (tag  (or (nth 1 keywords)
		      (symbol-name (aref field 3)))))
	(setq line-end   (+ (aref field 1)
			    (if (consp size)
				(car size)
			      1))
	      column-end (+ (aref field 2)
			    (if tag
				(+ (length tag) 2)
			      0)
			    (cond ((null size)
				   100) ; occupy rest of line
				  ((consp size)
				   (cdr size))
				  (t
				   size))))))
     ;; Otherwise, error!
     (t
      (dialog-error dialog "`%s' is not a valid field type" type)))
    ;; return parsed structure
    (apply
     'vector
     (aref field 0)			    ; type symbol
     (cdr (assq (aref field 0)		    ;
		dialog-create-field-alist)) ; creation fun
     (aref field 1)			    ; LINE
     (aref field 2)			    ; COLUMN
     line-end				    ; LINE END, LINE2
     column-end				    ; COLUMN END, COLUMN2
     arg				; TEXT, DIALOG, LENGTH,
					;  (VAR ALIST), (VAR . VALUE),
					;  (KIND . VAR),  VAR
     keywords)				; :notify
					; :tag
					; :help-echo
					; :size
					; :action
					; :secret
    ))


(defun dialog-parse-field-keywords (valid-keywords index field dialog)
  "Parse FIELD keywords of DIALOG.
VALID-KEYWORDS is a list of valid keywords for FIELD.
INDEX is the initial FIELD index to start parsing."
  (let ((flen (length field))
	keyword arg
	notify tag help size action secret)
    (while (< (1+ index) flen)
      (setq keyword (aref field index)
	    arg     (aref field (1+ index))
	    index   (+ index 2))
      ;; check semantics
      (cond
       ((not (memq keyword valid-keywords))
	(dialog-error-field
	 dialog field "`%s' is not a valid keyword"
	 keyword))
       ;; :notify FUNCTION
       ;; :action FUNCTION
       ((memq keyword '(:notify :action))
	(if (eq keyword :notify)
	    (setq notify arg)
	  (setq action arg))
	(unless (and arg
		     (or (symbolp arg)
			 (functionp arg)))
	  (dialog-error-field
	   dialog field "%s value must be a function or symbol"
	   keyword)))
       ;; :tag TAG
       ((eq keyword :tag)
	(setq tag arg)
	(unless (and arg (stringp arg))
	  (dialog-error-field
	   dialog field ":tag value must be string")))
       ;; :help-echo HELP
       ((eq keyword :help-echo)
	  (setq help arg)
	(unless (and arg
		     (or (stringp arg)
			 (functionp arg)))
	  (dialog-error-field
	   dialog field ":help-echo value must be string or function")))
       ;; :size SIZE
       ((eq keyword :size)
	(setq size arg)
	(when (and (consp arg)
		   (integerp (car arg))
		   (integerp (cdr arg)))
	  (dialog-error-field
	   dialog field ":size value not implemented yet: (integer . integer)"))
	(unless (or (integerp arg)
		    (and (consp arg)
			 (integerp (car arg))
			 (integerp (cdr arg))))
	  (dialog-error-field
	   dialog field ":size value must be integer or (integer . integer)")))
       ;; :secret CHAR
       ((eq keyword :secret)
	(setq secret arg)
	(unless (or (null arg)
		    (char-valid-p arg))
	  (dialog-error-field
	   dialog field ":secret value must be nil or character")))))
    ;; return list of keyword values
    (list notify tag help size action secret)))


;; Check field -line and -column values
(defun dialog-parse-field-line-column (field dialog &optional start)
  "Parse line and column FIELD components of DIALOG.
If START is specified, it's the initial index of line and column
components."
  (let ((istr  (if start "2" ""))
	(index (or start 1)))
    (unless (< (1+ index) (length field))
      (dialog-error-field
       dialog field "LINE%s or COLUMN%s isn't specified"
       istr istr))
    ;; LINE, LINE2
    (unless (and (integerp (aref field index))
		 (> (aref field index) 0))
      (dialog-error-field
       dialog field "LINE%s must be an integer greater than zero"
       istr))
    ;; COLUMN, COLUMN2
    (unless (and (integerp (aref field (1+ index)))
		 (>= (aref field (1+ index)) 0))
      (dialog-error-field
       dialog field "COLUMN%s must be a non-negative integer"
       istr))))


(defun dialog-index-is-inside (index field dialog)
  "Check if INDEX is inside FIELD vector."
  (unless (< index (length field))
    (dialog-error-field
     dialog field "invalid vector field specification")))


(defun dialog-error (dialog mess &rest args)
  "Give an error message with header \"Dialog `D': \"."
  (apply 'error (format "Dialog `%%s': %s." mess)
	 dialog args))


(defun dialog-error-field (dialog field mess &rest args)
  "Give an error message with header \"Dialog `D' F field: \"."
  (apply 'error (format "Dialog `%%s' %%s field: %s." mess)
	 dialog (aref field 0) args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions --- Derivation


(defsubst dialog-field-decoration-p (field)
  "Return t if FIELD is a decoration field."
  (memq (aref field dialog-field-type) '(box vline hline)))


(defun dialog-field-overlap-p (field1 field2)
  "Return t if field FIELD1 overlaps field FIELD2."
  ;; LL LC o------+|        p is inside when:
  ;;       |      ||           LL <= p.line   < UL
  ;;       |      ||           LC <= p.column < UC
  ;;       +------+|
  ;;       --------o UL UC
  (not (or (>= (aref field1 dialog-field-line)	     ; LL1 >= UL2
	       (aref field2 dialog-field-line-end))  ;
	   (<= (aref field1 dialog-field-line-end)   ; UL1 <= LL2
	       (aref field2 dialog-field-line))	     ;
	   (>= (aref field1 dialog-field-column)     ; LC1 >= UC2
	       (aref field2 dialog-field-column-end)) ;
	   (<= (aref field1 dialog-field-column-end)  ; UC1 <= LC2
	       (aref field2 dialog-field-column)))))  ;


(defun dialog-overlap-between-fields (dialog dialog-value)
  "Check if the DIALOG-VALUE fields overlap each other."
  (let ((fl (cdr dialog-value))
	f decor non-decor)
    (while fl
      (setq f (car fl))
      (if (dialog-field-decoration-p f)
	  ;; f decoration
	  (setq decor (cons f decor))
	;; f non-decoration
	(dolist (n (cdr fl))
	  (when (and (not (dialog-field-decoration-p n))
		     (dialog-field-overlap-p f n))
	    (dialog-error-field
	     dialog n "this field overlaps the preceding field `%s'"
	     (aref f dialog-field-type))))
	;; f non-decoration without overlaping
	(setq non-decor (cons f non-decor)))
      (setq fl (cdr fl)))
    (list (car dialog-value)		; style
	  (nreverse decor)		; decor field
	  (nreverse non-decor))))	; non-decor field


(defun dialog-exclude-overlap (pfields dfields)
  "Return a field list which the fields do not overlap themselves.
Exclude all PFIELDS field which overlaps with any field in DFIELDS list."
  (let (ok overlap)
    ;; get all pfields which does not overlap any dfields
    (dolist (p pfields)
      (setq overlap nil)
      (dolist (f dfields)
	(when (dialog-field-overlap-p p f)
	  (setq overlap t)))
      (unless overlap
	(setq ok (cons p ok))))
    (nreverse ok)))


(defun dialog-overlap-between-dialogs (fields fparent)
  "Return a field list which the fields do not overlap themselves.
Exclude all FPARENT field which overlaps with any field in FIELDS list."
  (let ((dpolicy (car fields))
	(ppolicy (aref (nth 0 fparent) dialog-style-decoration))
	decor non-decor)
    ;; handle decoration
    (cond
     ((eq dpolicy 'kill)		; kill all decoration
      (setq decor (nth 1 fields)))
     ((eq dpolicy 'keep)		; keep all decoration
      (setq decor   (append (nth 1 fparent) (nth 1 fields))
	    dpolicy ppolicy))
     ((eq dpolicy 'kill-overlap)	; kill decor when overlap
      (when (eq ppolicy 'kill) (setq dpolicy 'kill))
      ;; exclude all parent decor which overlap any field decor
      (setq decor (dialog-exclude-overlap (nth 1 fparent)
					  (nth 1 fields)))
      ;; exclude all parent decor which overlap any field non-decor
      (setq decor (dialog-exclude-overlap decor (nth 2 fields)))))
    ;; handle non-decoration
    (setq non-decor
	  (append
	   ;; exlude all parent fields which overlap any dialog field
	   (dialog-exclude-overlap (nth 2 fparent) (nth 2 fields))
	   (nth 2 fields)))
    ;; return overlap result
    (list dpolicy			; decoration policy
	  decor				; decor field
	  non-decor)))			; non-decor field


(defun dialog-derive-fields (dialog)
  "Return a field list derived from DIALOG."
  (or (get dialog 'dialog-derived-fields)
      (let ((f (cons (aref (car (symbol-value dialog))
			   dialog-style-decoration)
		     (cdr (symbol-value dialog))))
	    (p dialog))
	(while (setq p (aref (car (symbol-value p))
			     dialog-style-parent))
	  (setq f (dialog-overlap-between-dialogs f (symbol-value p))))
	(setq f (append (nth 1 f) (nth 2 f)))
	(put dialog 'dialog-derived-fields f)
	f)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions --- Actions


(defun dialog-action-quit (&rest dummy)
  "Dialog action to quit all dialog chain."
  (dialog-remove-hooks)
  (let (previous config)
    (while dialog-internal-previous-dialog
      (setq previous dialog-internal-previous-dialog)
      (dialog-kill-buffer)
      (dialog-set-buffer previous))
    (setq config dialog-internal-window-config)
    (dialog-kill-buffer)
    (set-window-configuration config)))


(defun dialog-action-save-and-cancel (&rest dummy)
  "Dialog action to save values and cancel current dialog."
  (mapc #'(lambda (item)
	    (set (nth 0 item) (symbol-value (nth 1 item))))
	dialog-internal-variable-alist)
  (dialog-action-cancel))


(defun dialog-action-reset (&rest dummy)
  "Dialog action to reset values of current dialog."
  (mapc #'(lambda (item)
	    (let ((val (symbol-value (nth 0 item))))
	      (set (nth 1 item) val)
	      (if (symbolp (nth 2 item))
		  (widget-value-set (nth 2 item) val)
		(dolist (wid (cddr item))
		  (widget-value-set (car wid) (eq val (cdr wid)))))))
	dialog-internal-variable-alist)
  (when dialog-internal-variable-alist
    (widget-setup)))


(defun dialog-action-cancel (&rest dummy)
  "Dialog action to cancel current dialog."
  (if dialog-internal-previous-dialog
      (dialog-action-goto-previous)
    (dialog-action-quit)))


(defun dialog-action-goto-previous (&rest dummy)
  "Dialog action to cancel current dialog and goto previous dialog."
  (let ((previous dialog-internal-previous-dialog))
    (when previous
      (dialog-kill-buffer)
      (dialog-set-buffer previous)
      (dialog-pop-to-buffer dialog-internal-dialog
			    dialog-internal-style
			    dialog-internal-style-arg
			    (current-buffer)))))


(defun dialog-action-goto-dialog (dialog)
  "Dialog action to cancel current dialog and goto DIALOG dialog."
  (when (dialogp dialog)
    (let ((buffer (dialog-buffer-name dialog)))
      (if (get-buffer buffer)
	  ;; buffer already exists in the dialog chain
	  (let (previous)
	    (while (not (eq dialog-internal-dialog dialog))
	      (setq previous dialog-internal-previous-dialog)
	      (dialog-kill-buffer)
	      (dialog-set-buffer previous))
	    (dialog-pop-to-buffer dialog-internal-dialog
				  dialog-internal-style
				  dialog-internal-style-arg
				  (current-buffer))
	    (setq dialog-internal-next-dialog nil))
	;; new dialog buffer
	(setq dialog-internal-next-dialog dialog)
	(dialog-do-execute dialog)))))


(defun dialog-update-radio (var value fun)
  "Update all radio widget associated with variable VAR.

VALUE is the value used to update.

FUN is a function activated at end of update all radio widget.
FUN can be a symbol function or a lambda function.
FUN is called without argument."
  (let ((item (assq var dialog-internal-variable-alist)))
    (when item
      (set (nth 1 item) value)
      (dolist (wid (cddr item))
	(widget-value-set (car wid) (eq (cdr wid) value)))
      (widget-setup)))
  (when fun
    (funcall fun value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal funcitons --- Misc


(defun dialog-add-hooks ()
  "Add buffer and window hooks."
  (add-hook 'kill-buffer-hook 'dialog-hook-buffer)
  (add-hook 'window-configuration-change-hook 'dialog-hook-window))


(defun dialog-remove-hooks ()
  "Remove buffer and window hooks."
  (remove-hook 'kill-buffer-hook 'dialog-hook-buffer)
  (remove-hook 'window-configuration-change-hook 'dialog-hook-window))


(defun dialog-hook-window ()
  "If `delete-window' command is activated, quit all dialog chain."
  (when (and (eq this-command 'delete-window)
	     (dialogp dialog-internal-dialog))
    (dialog-action-quit)))


(defun dialog-hook-buffer ()
  "If `kill-buffer' command is activated, quit all dialog chain."
  (when (and (eq this-command 'kill-buffer)
	     (dialogp dialog-internal-dialog))
    (dialog-action-quit)))


(defun dialog-hook-frame (frame)
  "If `delete-frame' command is activated, quit all dialog chain at FRAME.

FRAME is the frame which will be deleted."
  (let (buffer next)
    (when (and (frame-live-p frame)
	       (setq buffer (cdr (assq frame dialog-frame-alist))))
      (dialog-delete-frame-alist frame)
      ;; delete frames and buffers whose depend on this frame
      (save-excursion
	(set-buffer buffer)
	(setq next dialog-internal-next-dialog)
	;; adjust previous dialog
	(save-excursion
	  (when (dialog-set-buffer dialog-internal-previous-dialog)
	    (setq dialog-internal-next-dialog nil)))
	;; delete frames and buffers in the next dialog chain
	(delete-windows-on buffer)
	(kill-buffer buffer)	       ; current frame is being delete
	(while (and next (dialog-set-buffer next))
	  (setq next dialog-internal-next-dialog)
	  (dialog-kill-buffer))))))


(defun dialog-kill-buffer (&optional buffer)
  "Kill a dialog BUFFER.

If BUFFER is nil, kill the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))	  ; buffer object
  (when (setq buffer (get-buffer buffer)) ; buffer name string
    (save-excursion
      (set-buffer buffer)
      (let (frame)
	(if (and (eq dialog-internal-style 'frame)
		 (setq frame
		       (car (rassoc buffer dialog-frame-alist))))
	    ;; `dialog-hook-frame' kills this buffer
	    (delete-frame frame t)
	  (delete-windows-on buffer)
	  (kill-buffer buffer))))))


(defun dialog-set-buffer (dialog)
  "Make the DIALOG buffer current for editing operations."
  (let ((buffer (get-buffer (dialog-buffer-name dialog))))
    (and buffer (set-buffer buffer))
    buffer))


(defun dialog-add-frame-alist (frame buffer)
  "Add the association of FRAME and BUFFER."
  (unless (assq frame dialog-frame-alist)
    (setq dialog-frame-alist (cons (cons frame buffer)
				   dialog-frame-alist))
    (add-hook 'delete-frame-functions 'dialog-hook-frame)))


(defun dialog-delete-frame-alist (&optional frame)
  "Delete the association of a buffer with FRAME."
  (setq dialog-frame-alist (assq-delete-all (or frame
						(selected-frame))
					    dialog-frame-alist))
  (unless dialog-frame-alist
    (remove-hook 'delete-frame-functions 'dialog-hook-frame)))


(defun dialog-make-temp-var (var)
  "If VAR is not a local temporary variable symbol, make it."
  (or (nth 1 (assq var dialog-internal-variable-alist))
      (let ((tmp (make-local-variable
		  (intern
		   (format
		    "dialog--temp--<%02d>"
		    (setq dialog-internal-variable-count
			  (1+ dialog-internal-variable-count)))))))
	(set tmp (symbol-value var))
	tmp)))


(defun dialog-add-variable-alist (var tmp wid &optional value multiple-wid-p)
  "Add the association of variables VAR and TMP.

WID is the widget which uses TMP.

Optional VALUE is the default value associated with TMP.

MULTIPLE-WID-P indicates if TMP is used in more than one widget."
  (let ((item (assq var dialog-internal-variable-alist)))
    (cond
     ((null item)
      (setq dialog-internal-variable-alist
	    (cons (list var tmp
			(if multiple-wid-p
			    (cons wid value)
			  wid))
		  dialog-internal-variable-alist)))
     (multiple-wid-p
      (setcdr (cdr item) (cons (cons wid value) (cddr item)))))))


(defun dialog-add-symbol-alist (field)
  "If text FIELD has a symbol, add the association of the symbol with FIELD."
  (let ((sym (aref field dialog-field-arg)))
    (when (and sym (symbolp sym))
      (let ((item (assq sym dialog-internal-sym-text-alist)))
	(if item
	    (setcdr (cdr item) (cons field (cddr item)))
	  (setq dialog-internal-sym-text-alist
		(cons (list sym field)
		      dialog-internal-sym-text-alist)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'dialog)


;;; dialog.el ends here
