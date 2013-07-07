;;; light.el --- lightning completion
;; Copyright (c) 2005
;; Mark Haiman, Nick Reingold, John Palmieri

;; Authors:   Mark Haiman <mhaiman@macaulay.ucsd.edu>, 
;;            Nick Reingold, 
;;            John Palmieri <palmieri@math.washington.edu>
;; Maintainer: John Palmieri <palmieri@math.washington.edu>
;;             URL: http://www.math.washington.edu/~palmieri/Emacs/light.html
;; Keywords: completion
;; Version:  0.72 of Fri Sep 30 11:54:37 PDT 2005


;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: 
;;   
;;   This package provides dynamic completion, with an optional idle
;; time.  This means that, for instance, if I have a directory
;; ~palmieri, and this directory contains the files  
;;    alphabet   alpo   bozo   catfood.aux   catfood.dvi  catfood.tex
;; then if I type "~palmieri/" (in any buffer) and hit the key to 
;; start lightning completion on file names, the following happens:
;;  If I hit "a" or "al", then after a delay, "alp" gets inserted, and
;;     I'm still in completion mode.  By `after a delay', I mean that
;;     if Emacs is idle for a specified length of time (default is
;;     half a second, but it is customizable), then completion kicks
;;     in.  As long as you keep typing, it doesn't complete.
;;  If I hit "b" or "bo" or "boz", then after a delay, "bozo" gets
;;     inserted, and completion stops.
;;  If I hit "c" or "ca" or (etc.), then after a delay, "catfood.tex"
;;     gets inserted, and completion stops (assuming that "aux" and
;;     "dvi" are elements of the list completion-ignored-extensions).
;;  If I hit any other letter or number, I get a beep.
;;  If I hit C-f, I cycle through the different possible completions.
;;     C-b cycles backwards.
;;  If I hit C-c, completion stops 
;;  If I hit SPACE, completion stops if I have a valid completion so
;;     far; otherwise, hitting SPACE acts like the TAB key.
;;  If I hit TAB, it completes as far as possible (so if I type
;;     "a" and hit TAB, then "alp" gets inserted, just as if I hit "a"
;;     and waited for 0.5 seconds).  If there are several choices,
;;     hitting TAB opens up a new buffer called *Completions* which
;;     lists the possible completions (so if I have "alp" and hit TAB,
;;     the *Completions* buffer shows "alphabet" and "alpo" as my two
;;     options).
;;  If I hit "a" and wait (so that "alp" is inserted) then I hit DEL,
;;     then "alp" is erased, and I'm still in completion mode (i.e.,
;;     partial completions are remembered in a stack, and DEL pops to
;;     the previous level).
;;  If I hit "C-h", a brief help message pops up (temporarily) in the
;;     minibuffer.
;;  If I hit "C-c", then completion stops.
;;  If I hit any other control character, completion stops and that 
;;     control character does whatever its ordinary binding tells it
;;     to.  For example, hitting "C-a" stops completion and moves to
;;     the beginning of the line, while "C-l" stops completion and
;;     redraws the screen.
;;
;; Customization: the following options are customizable via
;;
;;   M-x customize-group lightning-completion
;;
;; lc-complete-idle-time: after this much time has elapsed, try to
;;   complete.  This defaults to 0.5 seconds.
;;
;; lc-clean-up: this determines what happens if you type an invalid
;;   competion.  Suppose in the above example, you type "albatross".
;;   If lc-clean-up is nil (the default), then after a delay, Emacs
;;   beeps at you and warns you that this is not a valid completion.
;;   If lc-clean-up is non-nil, then it also deletes the things you
;;   typed that made it invalid, so it would delete everything but
;;   "al", and then in fact would insert "p" to give "alp".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; There are two main ways to use lightning completion.  One of them
;; is by calling functions which complete on specific things, and the
;; other is by turning on the lightning completion option.
;;
;; 1. Functions for completion on specific things.
;;
;; This file defines the following functions:
;;
;;   function                            starts completion on 
;;   -------------------------------     ---------------------
;;   lc-complete-file-name               file names
;;   lc-complete-buffer-name             buffer names
;;   lc-complete-word                    words, using ispell
;;   lc-complete-lisp-object             lisp objects
;;   lc-complete-lisp-function           lisp functions
;;   lc-complete-lisp-variable           lisp variables
;;   lc-complete-buffer-contents         buffer contents
;;   lc-complete-kill-ring               contents of kill ring
;;
;; These all start the appropriate sort of lightning completion.  For
;; example, if you're writing e-mail and you want to include the
;; pathname for a file, you can run lc-complete-file-name to
;; start completion on file names.
;;
;; There is a customizable variable lc-ctrl-x-c-is-completion.  If it
;; is non-nil, then `C-x c' is the prefix key for these commands.  In
;; particular, if it is on, then 
;;
;;   C-x c F   runs   lc-complete-file-name
;;   C-x c b   runs   lc-complete-buffer-name
;;   C-x c i   runs   lc-complete-word
;;   C-x c o   runs   lc-complete-lisp-object
;;   C-x c f   runs   lc-complete-lisp-function
;;   C-x c v   runs   lc-complete-lisp-variable
;;   C-x c k   runs   lc-complete-kill-ring
;;   C-x c y   runs   lc-complete-buffer-contents
;;   C-x c u   runs   lc-complete-a-la-mode
;;
;; (The last function is described below.)
;;
;; Also, the function
;;
;;   lc-complete-a-la-mode
;;
;; "guesses" on how to complete depending on the context (well, it actually 
;; looks at the variable completing-insert-function, which should be set by 
;; the current mode).  For instance, this will complete on menu items
;; in Info, and who knows what else.
;;
;; There is a customizable variable lc-ctrl-backslash-completes-a-la-mode.
;; If it is non-nil, then `C-\' runs lc-complete-a-la-mode.
;;
;; All of the completion functions are based on the all-purpose completion
;; function
;;
;;   completing-insert
;;
;; See its documentation string for a description.  The function
;; completing-insert-ispell-word provides a good example of how to use
;; this when there is an easily available list of possible
;; completions.  The ispell package provides the function lookup-words
;; which does this.  To use this with lightning completion, one only
;; has to write a function that acts as a wrapper for lookup-words and
;; is suitable for use as the TABLE argument in completing-insert.
;;
;; For other types of completion, one has to write the code to produce
;; the list of completions oneself.  We provide two examples of this.
;; One is completion on buffer contents (in a section toward the end
;; of this file).  There is a function
;;
;;   lc-complete-buffer-contents
;;
;; which does what it sounds like: namely, it completes on the
;; contents of the current buffer.  If you give it a prefix argument,
;; it prompts for the buffer name on whose contents you want to
;; complete.  The second example we provide is on TeX command
;; sequences--this is part of the Ultra-TeX package and is contained
;; in the file ultex.el.  This works by reading a list of command
;; sequences from a file, and completing on the contents of that list.
;; ultex.el also provides a number of other features, which are
;; described therein.
;;
;;
;; 2. Enabling lightning completion.
;;
;; There is a customizable variable toggle-lightning-completion.  If
;; non-nil, then this allows one to enable lightning completion in
;; various situations in the minibuffer.  One can specify which
;; situations by customizing lightning-completion-list.  This allows
;; you to toggle lightning completion in each of the following
;; contexts:
;;   files, functions, commands, variables, user variables,
;;   lisp objects, info menu items, buffer names, query replace,
;;   miscellany
;; For example, if you enable lightning completion on files (by
;; turning on both toggle-lightning-completion and the appropriate
;; part of lightning-completion-list), then every time you enter the
;; minibuffer and are prompted for a file name, you will be using
;; lightning completion on file names. 
;;
;; You can disable lightning completion on individual functions by
;; customizing the variable lc-dark-commands, which is a list of
;; commands.  For example, if you want to disable lightning completion
;; when using insert-file, then add insert-file to this list.  Then
;; even if you've enabled lightning completion on file names, it will
;; be off when running insert-file. If you want two versions of a
;; command like insert-file, only one of which uses lightning
;; completion, then make an alias to insert-file, and disable
;; lightning completion on that function:
;;    (defalias 'insert-file-dark 'insert-file)
;; and add insert-file-dark to lc-dark-commands.  I personally have
;; functions like dired-create-directory and dired-do-symlink in my
;; list of dark commands.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version history
;;
;; 0.72 (30-Sep-2005) GNU Emacs 22 bug fix.
;; 0.71 (05-Jun-2002) GNU Emacs 21 bug fixes.
;; 0.70 (25-Oct-2001) completing-insert-ispell-word, and added aliases
;;          lc-complete-BLAH for the old functions completing-insert-BLAH.
;; 0.68 (09-Feb-2001) Patch to work with GNU Emacs 21--take
;;          minibuffer-prompt-width into account.
;; 0.65 (25-Sep-2000) Patch to work with XEmacs 21.2--avoid
;;          set-window-configuration.
;; 0.63 (29-Jun-1999) Changed my e-mail address, URL.
;; 0.61 (10-May-1999) Made a bit more compatible with Emacs 19.  Fixed 
;;          bug with meta keys in light-mode-map.
;; 0.60 (29-Apr-1999) Separated light from ultra-tex package.
;; 0.55 (24-Apr-1999) Fixed bug in completing-insert-file-name: was
;;          ignoring lc-complete-idle-time.  Also reformatted a little
;;          documentation.
;; 0.54 (12-Feb-1999) Fixed bug in lightning-completion
;; 0.53 (26-Jan-1999) Version number increased to keep up with ultex.el
;; 0.52 (19-Jan-1999) Version number increased to keep up with ultex.el
;; 0.51 (14-Jan-1999) Tried to clean up interaction with old versions
;;          of custom.
;; 0.50 (21-Dec-1998) Implemented customization stuff.  Implemented
;;          delay time completion.  Tried to clean up query-replace
;;          behavior.  No longer compatible with GNU Emacs 18, or even
;;          distributed with Emacs 18 compatibility files: use version
;;          0.41 or earlier if you have to use Emacs 18.
;; 0.41 (28-Sep-1998) Version number increased to keep up with ultex.el
;; 0.40 (24-Sep-1998) Version number increased to keep up with ultex.el
;; 0.39 (03-Sep-1998) Changed default keybinding to make backspace do
;;          the right thing when using XEmacs.
;; 0.37 (17-Apr-1998) New variable lc-emacs-20-p: non-nil if using
;;          Emacs 20.
;; 0.36 (07-Apr-1998) Version number increased to keep up with ultex.el
;; 0.35 (27-Mar-1998) Version number increased to keep up with ultex.el
;; 0.34 (23-Mar-1998) Version number increased to keep up with ultex.el
;; 0.33 (31-Oct-1997) Version number increased to keep up with ultex.el
;; 0.32 (02-Sep-1997) Fixed typo: ctrl-t was bound to lc-display-key,
;;          rather than ctrl-i (TAB).
;; 0.31 (01-Aug-1997) Version number increased to keep up with ultex.el
;; 0.30 (30-Jul-1997) Changed version number when distribution was
;;          reorganized.
;; 0.26 (23-Jul-1997) made sure return and linefeed were bound
;;           explicitly to 'lc-exit-and-then (because of problems with
;;           XEmacs otherwise).
;; 0.25 (28-Feb-1997) fixed a few bugs, and tried to make the
;;           *Completions* buffer work better with XEmacs.
;;             Incorporated buffer completion stuff into this file,
;;           instead of leaving it in its own file.
;; 0.24 (14-Feb-1997) bug fixes and so forth.  Now works better with
;;           resize-minibuffer, for instance.
;; 0.23 (16-Jan-1997) replaced buffer-substring with
;;           buffer-substring-no-properties everywhere.
;;           Also played around with . and .. to fix various bugs,
;;           make things faster when looking in big directories.
;;           Introduced new commands, lc-make-command-dark and
;;           lc-make-command-light, documented above.
;; 0.22.1 (02-Dec-1996) bug fix in lc-display-completions
;; 0.22 (27-Nov-1996) changed lightnify completely, so that you don't
;;           need to rebind any keys or anything.  Thanks to Richard
;;           Stallman for the idea.  This version is no longer
;;           compatible with GNU Emacs 18.  If you want to use
;;           lightning completion with that version of Emacs, load the
;;           file light18.el.  In Emacs 18, the function lightnify has
;;           been renamed lightnify18.  (Sorry about the lack of
;;           backward compatibility.)
;; 0.21 (14-Nov-1996) changed light-mode-map, made *Completions*
;;           buffer work better (can select with mouse, for example)
;; 0.2 (28-Oct-1996) started numbering the versions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Customization
;;

(defconst lc-version-string "0.71"
  "Version of lightning completion package.")

(defconst lc-version lc-version-string
  "Version of lightning completion package.")

(defvar lc-custom-p t
  "Non-nil if there appears to be a newish version of the custom
library available.")

;;  :set, :tag

;; if the custom package is not available, make sure defgroup and
;; defcustom are defined anyway.
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom)
	   (fboundp 'custom-declare-variable)
	   (fboundp 'custom-initialize-set)
	   (fboundp 'custom-handle-keyword))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (setq lc-custom-p nil)
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (make-face (, var))))
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

(defgroup lightning-completion nil
  "Lightning completion mode: dynamic completion on lisp objects, file
names, etc."
  :tag "Lightning completion"
  :prefix "lc"
  :link '(custom-manual "(light)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/light.html")
  :group 'abbrev)

(defgroup lightning-completion-keys nil
  "Key bindings for lightning completion mode."
  :prefix "lc"
  :group 'lightning-completion)

;;;; group: lightning-completion

(defconst lc-xemacs-p
  (string-match "XEmacs\\|Lucid" emacs-version)
  "Non-nil if using XEmacs.")

(defconst lc-emacs-20-p
  (and (boundp 'emacs-major-version)
       (= emacs-major-version 20))
  "Non-nil if using Emacs 20.")

(defconst lc-emacs-21-p
  (and (boundp 'emacs-major-version)
       (not lc-xemacs-p)
       (>= emacs-major-version 21))
  "Non-nil if using GNU Emacs 21 or later.")

(defcustom lc-complete-idle-time-default 0.5
  "*After this much idle time has elapsed, try to complete.
Measured in seconds.  Set this to 0 to achieve old lightning
completion behavior."
  :type '(number)
  :group 'lightning-completion)

(defvar lc-complete-idle-time nil
  "*After this much idle time has elapsed, try to complete.
Measured in seconds.  This variable may be changed by various
functions.  Set lc-complete-idle-time-default or
lightning-completion-list instead.")

(make-variable-buffer-local 'lc-complete-idle-time)

(defcustom toggle-lightning-completion nil
  "Toggle lightning completion.
If on, you should customize lightning-completion-list to specify
contexts in which to use lightning completion.  If off, you can still
run functions like completing-insert-file-name or
completing-insert-according-to-mode to use lightning completion."
  :type '(boolean)
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'minibuffer-setup-hook 'lightning-completion)
	   (remove-hook 'minibuffer-setup-hook 'lightning-completion))
	 (set symbol value))
  :group 'lightning-completion)

(defconst lightning-completion-list-default
  '((files)
    (functions)
    (commands)
    (variables)
    (user-variables) 
    (lisp-objects) 
    (info-menu-items) 
    (buffers)
    (query)
    (misc))
  "default value of lightning-completion-list")

(defun lc-convert-completion-list (list)
  "Convert LIST (which should be lightning-completion-list-external)
to a list of (symbol . boolean) pairs."
  (let ((lc-list lightning-completion-list-default)
	(temp list)
	answer)
    (if (< (length temp) (length lc-list))
	(setq temp (append temp (make-list
				 (- (length lc-list) (length temp))
				 nil))))
    (while lc-list
      (setq answer (cons (cons (caar lc-list) (car temp)) answer)
	    lc-list (cdr lc-list)
	    temp (cdr temp)))
    (reverse answer)))

(defun lc-unconvert-completion-list (list)
  "Convert LIST (which should be lightning-completion-list)
to a list of boolean values."
  (mapcar 'cdr list))

(defcustom lightning-completion-list-external (make-list 10 nil)
  "Enable lightning completion in specific contexts.  If nil or a
negative number, turn off completion in that context.  If t, turn on
completion.  If a non-negative number, turn on completion with that
number for the idle time."
  :tag "Lightning completion list"
  :type '(list (choice :tag "Files          "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Functions      "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Commands       "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Variables      "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "User variables "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Lisp objects   "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Info menu items"
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Buffer names   "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Query replace  "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default))))
	       (choice :tag "Miscellany     "
		       (const :tag "On" t)
		       (const :tag "Off" nil)
		       (number :tag "Delay time"
			       :value nil
			       :default-get
			       (lambda (widget)
				 (prin1-to-string
				  lc-complete-idle-time-default)))))
  :set (lambda (symbol value)
	 (setq lightning-completion-list
	       (lc-convert-completion-list value))
	 (set symbol value))
  :group 'lightning-completion)

(defvar lightning-completion-list
  (lc-convert-completion-list lightning-completion-list-external)
  "List of things on which to complete.
This is a list, each element of which looks like (SITUATION)
or (SITUATION . t).  In the former case, lightning completion is off
in SITUATION, and in the latter case, lightning completion is on in
SITUATION.  You can modify this list directly, but it is better
customize it or to use the lightnify function: (lightnify SITUATION)
toggles lightning completion for SITUATION.")

(defcustom lc-clean-up nil
  "*If on, when there is no valid completion, remove the invalid characters.
If off, just give warning when no valid completion."
  :type '(boolean)
  :group 'lightning-completion)

(defcustom lc-ignored-file-extensions-external
  completion-ignored-extensions
  "File extensions to ignore when doing lightning completion"
  :type '(repeat string)
  :tag "Lc Ignored File Extensions"
  :set (lambda (symbol value)
	 (setq lc-ignored-file-extensions
	       (concat "\\(" 
		       (mapconcat 'regexp-quote value "\\|")
		       "\\)$"))
	 (set symbol value))
  :group 'lightning-completion)

(defvar lc-ignored-file-extensions
  (concat "\\(" 
	  (mapconcat 'regexp-quote
		     lc-ignored-file-extensions-external
		     "\\|")
	  "\\)$")
  "Regular expression of file extensions to ignore when doing
lightning completion.")

(defcustom lc-dark-commands nil
  "List of commands for which lightning completion is disabled.
Example of use: 
   (defalias 'find-file-dark 'find-file)
defines a function called find-file-dark which acts the same as
find-file.  Then if you add find-file-dark to this list of commands,
it will not use lightning completion on file names, even if find-file
does."
  :type '(repeat function)
  :group 'lightning-completion)

(defcustom lc-dark-recursive-minibufs nil
  "If a number N, then don't use lightning completion when
recursion-depth is bigger than N.  If nil, then no restrictions on
when to use lightning completion."
  :type '(choice (const nil) integer)
  :group 'lightning-completion)

(defcustom lc-override-flag nil
  "If non-nil, override other local maps when using lightning completion.
You may want to turn this on if using outline mode or some other minor
mode which uses C-c as prefix.  This may not be necessary, though."
  :type '(boolean)
  :group 'lightning-completion)

;;;; group: lightning-completion-keys

(defconst key
  '(choice string
	   (vector (repeat :inline t (choice symbol character)))
	   (vector (repeat symbol character)))
  "customization type for key sequence")

(defcustom lc-keep-key " "
  "Key to keep current completion in Light mode.  Default is SPC,
which may not be very visible in the customization buffer."
  :type key
  :group 'lightning-completion-keys)
  
(defcustom lc-del-key [backspace]
  "Key to delete last completion unit in Light mode."
  :type key
  :group 'lightning-completion-keys)

(defcustom lc-stop-key [(control c)]
  "Key to exit Light mode."
  :type key
  :group 'lightning-completion-keys)

(defcustom lc-quote-key [(control q)]
  "Key to quote next char in Light mode."
  :type key
  :group 'lightning-completion-keys)

(defcustom lc-help-key [(control h)]
  "Help key in Light mode."
  :type key
  :group 'lightning-completion-keys)

(defvar lc-use-old-lc-keymap nil
  "If non-nil, use old lightning completion key bindings.")

(if (null lc-use-old-lc-keymap)
    (progn
      (defcustom lc-cycle-key [(control f)]
	"Key to cycle to next completion in Light mode."
	:type key
	:group 'lightning-completion-keys)
      (defcustom lc-back-cycle-key [(control b)]
	"Key to cycle to previous completion in Light mode."
	:type key
	:group 'lightning-completion-keys)
      (defcustom lc-display-key [tab]
	"Key to display all completions in Light mode."
	:type key
	:group 'lightning-completion-keys))
  (defcustom lc-cycle-key [tab]
    "Key to cycle to next completion in Light mode."
    :type key
    :group 'lightning-completion-keys)
  (defcustom lc-back-cycle-key [(control u)]
    "Key to cycle to previous completion in Light mode."
    :type key
    :group 'lightning-completion-keys)
  (defcustom lc-display-key [(control m)]
    "Key to display all completions in Light mode."
    :type key
    :group 'lightning-completion-keys))

(defvar lc-completions-map (make-sparse-keymap)
  "Key map for lightning completion functions.")

(defcustom lc-ctrl-x-c-is-completion nil
  "Toggle whether `C-x c' is the prefix key for the various lightning
completion commands.  If on,

  C-x c f   runs   completing-insert-lisp-function
  C-x c v   runs   completing-insert-lisp-variable
  C-x c F   runs   completing-insert-file-name
  C-x c i   runs   completing-insert-ispell-word
  C-x c k   runs   completing-insert-kill
  C-x c y   runs   completing-insert-buffer contents
  C-x c C-h   lists all of the key bindings starting with C-x c

If turned off, `C-x c' does nothing."
  :type '(boolean)
  :set (lambda (symbol value)
	 (if value
	     (define-key ctl-x-map "c" lc-completions-map)
	   (define-key ctl-x-map "c" nil))
	 (set symbol value))
  :group 'lightning-completion-keys)

(define-key lc-completions-map "f" 'completing-insert-lisp-function)
(define-key lc-completions-map "v" 'completing-insert-lisp-variable)
(define-key lc-completions-map "o" 'completing-insert-lisp-object)
(define-key lc-completions-map "F" 'completing-insert-file-name)
(define-key lc-completions-map "i" 'completing-insert-ispell-word)
(define-key lc-completions-map "u" 'completing-insert-according-to-mode)
(define-key lc-completions-map "b" 'completing-insert-buffer-name)
(define-key lc-completions-map "k" 'completing-insert-kill)
(define-key lc-completions-map "y" 'completing-insert-buffer-contents)

(defcustom lc-ctrl-backslash-completes-a-la-mode nil
  "Toggle whether `C-\\' runs the `completing-insert-according-to-mode'.
If turned off, `C-\\' does nothing."
  :type '(boolean)
  :set (lambda (symbol value)
	 (if value
	     (global-set-key "\C-\\" 'completing-insert-according-to-mode)
	   (global-set-key "\C-\\" nil))
	 (set symbol value))
  :group 'lightning-completion-keys)

;; set variables correctly if not using a new version of custom.
(if (not lc-custom-p)
    (progn
      (if toggle-lightning-completion
	  (add-hook 'minibuffer-setup-hook 'lightning-completion))
      (setq lightning-completion-list
	    (lc-convert-completion-list
	     lightning-completion-list-external))
      (setq lc-ignored-file-extensions
	    (concat "\\(" 
		    (mapconcat 'regexp-quote
			       lc-ignored-file-extensions-external
			       "\\|")
		    "\\)$"))
      (if lc-ctrl-x-c-is-completion
	  (define-key ctl-x-map "c" lc-completions-map))
      (if lc-ctrl-backslash-completes-a-la-mode
	  (global-set-key "\C-\\"
			  'completing-insert-according-to-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up light-mode, light-mode-map, etc. 
;;

(defvar light-mode nil
  "Non-nil if using Light mode as a minor mode")
(make-variable-buffer-local 'light-mode)
(or (assq 'light-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(light-mode " Light")
				 minor-mode-alist)))

(defvar light-mode-map nil
  "Minor mode map for lightning completion.")
(if light-mode-map
    nil
  (let ((i 31)
	(map (copy-keymap minibuffer-local-completion-map))
	(meta-map (make-keymap)))
    (substitute-key-definition 'switch-to-completions
			       'lc-switch-to-completions
			       map)
    (substitute-key-definition 'advertised-switch-to-completions
			       'lc-advertised-switch-to-completions
			       map)
    (defalias 'lc-advertised-switch-to-completions
      'lc-switch-to-completions)
    (substitute-key-definition 'exit-minibuffer
			       'lc-exit-and-then
			       map)
    (substitute-key-definition 'keyboard-quit
			       'lc-exit-and-then
			       map)
    (substitute-key-definition 'abort-recursive-edit
			       'lc-exit-and-then
			       map)
    (substitute-key-definition 'minibuffer-keyboard-quit
			       'lc-exit-and-then
			       map)
    (substitute-key-definition 'next-history-element
			       'lc-cycle-forward
			       map)
    (substitute-key-definition 'previous-history-element
			       'lc-cycle-backward
			       map)
    (substitute-key-definition 'minibuffer-completion-help
			       'lc-try-to-complete
			       map)
    (substitute-key-definition 'minibuffer-complete
			       'lc-try-to-complete
			       map)
    (if (keymapp (lookup-key map [menu-bar minibuf]))
	(progn
	  (define-key map [menu-bar light]
	    (cons "Light" (make-sparse-keymap "Light")))
	  (define-key map [menu-bar light tab]
	    '("List Completions" . lc-try-to-complete))
	  (defalias 'lc-exit-and-then-alias 'lc-exit-and-then)
	  (define-key map [menu-bar light quit]
	    '("Quit" . lc-exit-and-then-alias))
	  (define-key map [menu-bar light return]
	    '("Enter" . lc-exit-and-then-alias))
	  (define-key map [menu-bar minibuf] 'undefined)))
    (define-key map [escape] meta-map)
    (while (<= (setq i (1+ i)) 126)
      (or (lookup-key map (vector (list 'control i)))
	  (define-key map (vector (list 'control i))
	    'lc-exit-and-then))
      (or (lookup-key map (vector (list 'meta i)))
	  (progn
	    (define-key meta-map (char-to-string i) 'lc-exit-and-then)
	    (define-key map (vector (list 'meta i))
	      'lc-exit-and-then)))
      (define-key map (char-to-string i) 'lc-self-insert-char))
    (define-key map [return] 'lc-exit-and-then)
    (define-key map [linefeed] 'lc-exit-and-then)
    (define-key map [(control j)] 'lc-exit-and-then)
    (define-key map [(control g)] 'lc-exit-and-then)
    (define-key map [(control m)] 'lc-exit-and-then)
    (define-key map (char-to-string 127) 'lc-exit-and-then)
    (if lc-keep-key
	(define-key map lc-keep-key 'lc-keep-if-complete))
    (if lc-del-key
	(define-key map lc-del-key 'lc-delete))
    (if lc-del-key
	(progn
	  (substitute-key-definition 'delete-backward-char
				     'lc-delete
				     map)
	  (substitute-key-definition 'delete-backward-char
				     'lc-delete
				     map
				     global-map)))
    (if lc-cycle-key
	(define-key map lc-cycle-key 'lc-cycle-forward)) 
    (if lc-back-cycle-key
	(define-key map lc-back-cycle-key 'lc-cycle-backward))
    (if lc-display-key
	(define-key map lc-display-key 'lc-try-to-complete))
    (if lc-stop-key
	(define-key map lc-stop-key 'lc-quit))
    (if lc-quote-key
	(define-key map lc-quote-key 'lc-quote-char))
    (if lc-help-key
	(define-key map lc-help-key 'lc-help))
    (setq light-mode-map map)))

(defvar lc-completion-list-mode-map nil
  "Local map for completion list buffers (for use with lightning completion).")
(or lc-completion-list-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-2] 'lc-mouse-choose-completion)
      (define-key map [down-mouse-2] nil)
      (define-key map "\C-m" 'lc-choose-completion)
      (define-key map "\e\e\e" 'delete-completion-window)
      (define-key map [left] 'previous-completion)
      (define-key map [right] 'next-completion)
      (setq lc-completion-list-mode-map map)))

(and (boundp 'minor-mode-map-alist)
     (or (assq 'light-mode minor-mode-map-alist)
	 (setq minor-mode-map-alist
	       (cons (cons 'light-mode light-mode-map)
		     minor-mode-map-alist))))
(make-variable-buffer-local 'light-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous variables
;;

(defvar lc-old-overriding-keymap nil)
(make-variable-buffer-local 'lc-old-overriding-keymap)

(defvar lc-stack nil
  "top to agree with buffer, except when cycling")
(make-variable-buffer-local 'lc-stack)
(defvar lc-table nil
  "table for lightning completion")
(make-variable-buffer-local 'lc-table)
(defvar lc-predicate nil
  "predicate for lightning completion")
(make-variable-buffer-local 'lc-predicate)
(defvar lc-hook nil
  "hook for lightning completion")
(make-variable-buffer-local 'lc-hook)
(defvar lc-cycle nil
  "nil if not cycling.  If cycling, equal to (completion-vector . number)")
(make-variable-buffer-local 'lc-cycle)
(defvar lc-prev-windows nil
  "state before opening *Completions* window -- used with
set-window-configuration to kill *Completions* buffer")
(defvar lc-display-filter nil
  "used in completing-insert as the display argument")
(make-variable-buffer-local 'lc-display-filter)
(defvar lc-last-display-time nil
  "`time' measured by stack top eq-ness")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main functions
;;

(defun completing-insert (table pred init &optional hook message display)
  "Lightning-complete string before point in the buffer, relative to
completion TABLE; allowing only completions that satisfy PRED.  These
are used exactly as they are by `completing-read', which means this:
  TABLE may be an alist, an obarray, or a function-symbol.  For an
alist, PRED applies to the entries (conses).  For an obarray, PRED
applies to the symbols.  A function symbol will be called with a
STRING as first arg, PRED as second arg and third arg nil, t, or
`lambda'; according to third arg, the function is supposed to return
the common completion of STRING, all its completions, or the
truth-value of its completeness.  In particular the function can be
like 'read-file-name-internal, with PRED the name of a directory.
  Third arg INIT is the number of characters before point to complete
as the initial string.  Barf immediately if this is no match.  If
negative, we are resuming, so return nil unless situation at last quit
agrees with buffer before point; then restore that situation.
  Optional arg HOOK is run on successful completion; gets same kind of
argument as PRED, or the complete string if TABLE is a function symbol.
  On entering, message \"Completing <optional arg MESSAGE>...\" is
displayed.
  Optional arg DISPLAY is a function to call on each possible
completion before displaying.  If the DISPLAY function returns nil,
that string is NOT displayed."
  (condition-case nil
      (if (not
	   (or
	    (and (>= init 0)		; starting fresh
		 (prog1			; if so, reset things and be t
		     t
		   (setq lc-stack nil)
		   (let ((grab (buffer-substring-no-properties
				(- (point) init) (point)))
			 (n 0))
		     (while (<= n init)
		       (setq lc-stack (cons (substring grab 0 n) lc-stack))
		       (setq n (1+ n)))) ; completions=part grabs
		   (setq lc-table table lc-predicate pred
			 lc-hook hook lc-cycle nil
			 lc-display-filter display)))
	    ;; see if resuming state is consistent:
	    (and
	     lc-stack
	     (let ((state (and lc-cycle
			       (if (string-match
				    (concat "^" (regexp-quote
						 (car lc-stack)))
				    (aref (car lc-cycle) (cdr lc-cycle)))
				   t 'state))))
	       (and (or (eq state 'state)
			(and
			 (>= (point)
			     (+ (point-min) (length (car lc-stack))))
			 (string= (car lc-stack)
				  (buffer-substring-no-properties
				   (- (point) (length (car lc-stack)))
				   (point)))))
		    (or (null state)
			(looking-at
			 (regexp-quote
			  (substring (aref (car lc-cycle) (cdr lc-cycle))
				     (if (eq state 'state)
					 0 (match-end 0))))))))
	     (eq table lc-table)
	     (equal pred lc-predicate)
	     (equal hook lc-hook)
	     (equal display lc-display-filter))))
	  nil				; trying to resume inconsistently
	(setq light-mode t)
	(and lc-override-flag
	     (keymapp (lookup-key (current-local-map)
				  (char-to-string lc-stop-key)))
	     (not (minibuffer-window-active-p (minibuffer-window)))
	     (setq lc-old-overriding-keymap overriding-terminal-local-map
		   overriding-terminal-local-map light-mode-map))
	(add-hook 'mouse-leave-buffer-hook
		  (function (lambda nil (lc-quit 'mouse))))
	(lc-redraw-modeline)
	(set-buffer-modified-p (buffer-modified-p)) ; update mode line
	(setq lc-prev-windows (current-window-configuration))
	(if (or (> 0 init)
		(string= (car lc-stack) "") ; don't try to complete ""
		(let ((stat (lc-complete-stack-top "")))
		  (or (stringp stat) (prog1 nil (lc-quit stat)))))
	    (progn
	      (lc-message (concat "Completing" (if message " ") message "..."))
	      (while nil)))		; no-op
	t)				; return t except for bad resume
    (quit (setq unread-command-events
		(list (lc-character-to-event ?\C-g))))))

(defun lc-quit (arg &optional quick)
  "Exit lightning completion mode.
ARG nil means because of error.  ARG t means because successful.  ARG
other means intentional quit without being complete.  Interactively,
you get the last."
  (interactive '(lambda))
  (remove-hook 'mouse-leave-buffer-hook
	       (function (lambda nil (lc-quit 'mouse))))
  (and lc-override-flag
       (keymapp (lookup-key (current-local-map)
			    (char-to-string lc-stop-key)))
       (not (minibuffer-window-active-p (minibuffer-window)))
       (setq overriding-terminal-local-map lc-old-overriding-keymap))
  (set-buffer-modified-p (buffer-modified-p)) ; update mode line
  (lc-redraw-modeline)
  (setq lc-complete-idle-time lc-complete-idle-time-default)
  (if toggle-lightning-completion
      (add-hook 'minibuffer-setup-hook 'lightning-completion))
  (setq light-mode nil)
  (or arg (ding))			; yell if an error
  (or (eq arg 'mouse)
      (and lc-prev-windows
	   (get-buffer-window lc-completion-buffer-name)
	   (= 1 (length (get-buffer-window-list (current-buffer))))
	   (or (null lc-xemacs-p)
	       (null (minibuffer-window-active-p (minibuffer-window))))
	   (progn
	     (let ((pt (point)))
	       (set-window-configuration lc-prev-windows)
	       (if lc-emacs-21-p (goto-char pt))
	       (setq lc-prev-windows nil)))))
  (and (eq arg 'choose)
       (looking-at (regexp-quote (car lc-stack)))
       (forward-char (length (car lc-stack))))
  (if (or (eq arg t) (eq arg 'choose))
      (let ((name (car lc-stack)))
	(setq lc-stack nil)		; no resume after success
	(lc-message "Completed.")	; do here in case hook sends a message
	(if lc-hook			; on success, call possible hook
	    (funcall lc-hook
		     (cond ((vectorp lc-table) ; table is an obarray
			    (intern-soft name lc-table))
			   ((listp lc-table) ; table is an alist
			    (assoc name lc-table))
			   (t name))))	; table is a function
	(if (> (current-column) fill-column)  (run-hooks 'auto-fill-hook)))
    ;; unsuccessful quit:
    (setq lc-last-display-time nil)
    (lc-message (if arg "Stopped completing." "Can't complete.")
		unread-command-events)))

(defun lc-message (str &optional quick)
  "Same as message except in the minibuffer: then put message at point,
sit, and erase message.  Optional arg QUICK shortens the sit-for"
  (interactive)
  (if (not (eq (selected-window) (minibuffer-window)))
      (message str)
    ;; in minibuffer!
    (if (and (boundp 'resize-minibuffer-mode)
	     resize-minibuffer-mode)
	(resize-minibuffer-window))
    (setq str (concat " [" str "]"))
    (let ((inhibit-quit t))
      (save-excursion (insert str))
      (sit-for (if quick 0 2))
      (delete-char (length str)))))

(defun lc-switch-stack-top (str)
  "Replace top of stack with STR, fixing buffer."
  (let ((inhibit-quit t))
    (delete-backward-char (length (car lc-stack)))
    (insert str)
    (rplaca lc-stack str)))

(defun lc-pop-stack nil
  "Pop the stack, fixing buffer."
  (let ((inhibit-quit t))
    (delete-backward-char (length (car lc-stack)))
    (setq lc-stack (cdr lc-stack))
    (insert (car lc-stack))))

(defun lc-complete-stack-top (more &optional no-modify)
  "If possible, replace what's on top of stack, and before point, with
the common completion of that extended by MORE, returning that.  Return
nil if no match.  If result is complete and unique, return t.  If
optional arg NO-MODIFY is non-nil, don't modify the stack--just see if
it would be complete."
  (let* ((str (concat (car lc-stack) more))
	 ;; t:use real table. nil:truly no completions. alist:the completions
	 (all (or (symbolp lc-table)
		  (and (> (length str) 0) (= (aref str 0) ? ))
		  (mapcar 'list (all-completions str lc-table lc-predicate))))
	 (try (and all (try-completion
			str
			(if (eq all t) lc-table all)
			(if (eq all t) lc-predicate))))
	 (str (if (eq try t) str try)))
    (and try
	 (progn
	   (or no-modify
	       (lc-switch-stack-top str))
	   (or (eq try t)
	       (try-completion str
			       (if (eq all t) lc-table all)
			       (if (eq all t) lc-predicate)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions bound to keys  (see also lc-quit above)
;;

;; bound to control characters
(defun lc-exit-and-then nil
  "Intentional unsuccessful quit, then put back char to be read again."
  (interactive)
  (setq unread-command-events (list last-command-event))
  (lc-quit 'lambda))

;; bound to printing characters
(defun lc-self-insert-char nil
  "Update lc-stack, insert this char, and if idle for
lc-complete-idle-time, run lc-idle-complete."
  (interactive)
  (lc-stop-cycling)
  (setq lc-stack (cons (concat
			(car lc-stack)
			(char-to-string last-command-char))
		       lc-stack))
  (insert last-command-char)
  (if (null lc-complete-idle-time)
      (setq lc-complete-idle-time lc-complete-idle-time-default))
  (if (or (zerop lc-complete-idle-time)
	  (sit-for lc-complete-idle-time))
      (lc-idle-complete)))

(defun lc-idle-complete nil
  "Complete as far as possible.  If no valid completions, beep.
If no valid completions and the customizable variable lc-clean-up is
non-nil, then delete characters until a valid string remains."
  (interactive)
  (let ((top (lc-complete-stack-top "")))
    (cond ((eq top t)
	   (lc-quit t))
	  ((null top)
	   (ding)
	   (lc-message "No match." t)
	   (if lc-clean-up
	       (progn
		 (while (and lc-stack (null top))
		   (lc-pop-stack)
		   (setq top (lc-complete-stack-top "")))
		 (lc-idle-complete)))))))

;; another option for printing characters
(defun lc-self-insert-char-2 nil
  "Just insert this char and update the stack.  If you want a
variant on the behavior of lightning completion, after you load
light.el, run 

(substitute-key-definition 'lc-self-insert-char 
			   'lc-self-insert-char-2
			   light-mode-map)

Then typing an alphanumeric key will insert it, and hitting TAB will
complete.  This is different from ordinary Emacs completion because
you can use it outside of the minibuffer."
  (interactive)
  (lc-stop-cycling)
  (setq lc-stack (cons (concat (car lc-stack) (char-to-string
					       last-command-char))
		       lc-stack))
  (insert last-command-char))

;; bound to lc-keep-key (space, by default)
(defun lc-keep-if-complete nil
  "Quit with success if current stack top is complete.  Otherwise self-insert."
  (interactive)
  (if lc-cycle
      (let ((try t))
	(lc-absorb-cycling)
	(if (or (not (symbolp lc-table))
		(setq try (funcall lc-table (car lc-stack) lc-predicate nil)))
	    (if (eq try t) (lc-quit t)
	      (lc-switch-stack-top try))
	  (lc-pop-stack)
	  (lc-self-insert-char)))
    (if (cond ((listp lc-table) 
	       (assoc (car lc-stack) lc-table))
	      ((vectorp lc-table)
	       (or (and (eq 'obarray lc-table)
			(string= "nil" (car lc-stack)))
		   (intern-soft (car lc-stack) lc-table)))
	      (t (funcall lc-table (car lc-stack) lc-predicate 'lambda)))
	(lc-quit t)
      (if (lc-complete-stack-top
	   (if lc-stack " " (concat (car lc-stack) " "))
	   t)
	  (lc-self-insert-char)
	(lc-try-to-complete)))))

;; bound to lc-del-key (DEL, by default)
(defun lc-delete nil
  "Go back one completion unit.  If cycling this means stop it.
If there is no previous unit, quit quietly."
  (interactive)
  (if lc-cycle (lc-stop-cycling)
    (if (null (cdr lc-stack)) (lc-quit 'lambda)
      (lc-pop-stack))))

;; bound to lc-quote-key (C-q, by default)
(defun lc-quote-char nil
  "Quote the next key as printing character for lightning completion."
  (interactive)
  (let ((inhibit-quit t))
    (lc-message "^Q- ")
    (setq last-command-char (read-quoted-char))
    (lc-self-insert-char)))

;; bound to lc-help-key (C-h, by default)
(defun lc-help nil
  "Tell the completion control keys as a message."
  (interactive)
  (message
   (concat
    (and lc-keep-key (concat "Keep="
			     (lc-key-description lc-keep-key) " "))
    (and lc-del-key (concat "Del="
			    (lc-key-description lc-del-key) " "))
    (and lc-cycle-key (concat "Cycle="
			      (lc-key-description lc-cycle-key)
			      (and lc-back-cycle-key
				   (concat "," (lc-key-description
						 lc-back-cycle-key)))
			      " "))
    (and lc-display-key (concat "Show-All="
				(lc-key-description lc-display-key) " "))
    (and lc-stop-key (concat "Stop="
			     (lc-key-description lc-stop-key) " "))
    (and lc-quote-key (concat "Quote="
			      (lc-key-description lc-quote-key) " "))
    (and lc-help-key (concat "Help="
			     (lc-key-description lc-help-key) " ")))))

(defalias 'lc-key-description 'key-description)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cycling stuff.
;;

;; bound to lc-cycle-key (C-f, by default)
(defun lc-cycle-forward (arg)
  "Start cycling through completions, or cycle forward if already cycling."
  (interactive (list 1))
  (if lc-cycle
      (progn (lc-cycle-remove)
	     (rplacd lc-cycle (% (+ (length (car lc-cycle)) (cdr lc-cycle) arg)
				 (length (car lc-cycle)))))
    (setq lc-cycle
	  (cons (apply 'vector
		       (all-completions (car lc-stack) lc-table lc-predicate))
		0))
    (setq arg 0))
  (if (= 0 (length (car lc-cycle)))
      (progn (setq lc-cycle nil) (lc-message "No visible completions."))
    (delete-backward-char (length (car lc-stack)))
    (save-excursion
      (insert (aref (car lc-cycle) (cdr lc-cycle))))
    (if (string-match (concat "^" (regexp-quote (car lc-stack)))
		      (aref (car lc-cycle) (cdr lc-cycle)))
	(forward-char (match-end 0)))))

;; bound to lc-back-cycle-key (C-b, by default)
(defun lc-cycle-backward nil
  "Start cycling through completions, or cycle backward if already cycling."
  (interactive)
  (lc-cycle-forward -1))

(defun lc-stop-cycling nil
  "Stop cycling, delete cycle shown."
  (and lc-cycle 
       (progn (lc-cycle-remove)
	      (setq lc-cycle nil))))

(defun lc-cycle-remove nil
  "Remove last cycle shown."
  (if (string-match (concat "^" (regexp-quote (car lc-stack)))
		    (aref (car lc-cycle) (cdr lc-cycle)))
      (delete-char (- (length (aref (car lc-cycle) (cdr lc-cycle)))
		      (match-end 0)))
    (delete-char (length (aref (car lc-cycle) (cdr lc-cycle))))
    (insert (car lc-stack))))

(defun lc-absorb-cycling nil
  "Stop cycling and push cycle shown on stack."
  (and lc-cycle 
       (progn
	 (lc-cycle-remove)
	 (setq lc-stack (cons (car lc-stack) lc-stack))
	 (lc-switch-stack-top (aref (car lc-cycle) (cdr lc-cycle)))
	 (setq lc-cycle nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff for completions buffer.
;;

;; bound to lc-display-key (TAB, by default)
(defun lc-try-to-complete nil
  "Try to complete.  Complete as far as possible.
If there are choices, pop up buffer with list.  If there are no valid
completions, ding."
  (interactive)
  (let ((old (car lc-stack))
	(top (lc-complete-stack-top "" t)))
    ;;  (top (lc-complete-stack-top "")))
    (cond ((string= old top)
	   (lc-display-completions))
	  ((eq top t)
	   (lc-complete-stack-top "")   ;; new
	   (lc-quit t))
	  ((null top)
	   (ding)
	   (lc-message "No match." t)
	   (if lc-clean-up
	       (progn
		 (while (and lc-stack (null top))
		   (lc-pop-stack)
		   (setq top (lc-complete-stack-top "")))
		 (lc-idle-complete))))
	  (t
	   (lc-complete-stack-top "")))))

(defvar lc-completion-buffer-name " *Completions*"
  "Name of buffer in which to display list of completions")

(defun lc-display-completions (&optional jump)
  "Show possible completions, just like `minibuffer-completion-help'"
  (interactive)
  (lc-stop-cycling)
  (if (and (not (equal jump 'jump))
	   (equal lc-last-display-time (car lc-stack))
	   (get-buffer-window lc-completion-buffer-name))
      (let ((ow (selected-window))	; successive displays scroll
	    (w (get-buffer-window lc-completion-buffer-name)))
	(select-window w)
	(condition-case nil
	    (if (<= (point-max) (window-end))
		(goto-char (point-min))
	      (scroll-up))
	  (error (goto-char (point-min))))
	(select-window ow))
    (setq lc-last-display-time (car lc-stack))
    (let ((all (all-completions (car lc-stack) lc-table lc-predicate))
	  results ans)
      (if (not (fboundp lc-display-filter)) nil
	(while all
	  (setq ans (funcall lc-display-filter (car all)))
	  (and ans
	       (setq results (cons ans results)))
	  (setq all (cdr all)))
	(setq all (nreverse results)))
      (if all
	  (lc-display-completions-internal all)
	(lc-message "No visible completions.")))))

(defun lc-switch-to-completions ()
  "Select the completion list window."
  (interactive)
  ;; Make sure we have a completions window.
  (lc-display-completions 'jump)
  (select-window (get-buffer-window lc-completion-buffer-name))
  (goto-char (point-min))
  (search-forward "\n\n")
  (forward-line 1))

(defun lc-choose-completion ()
  "Choose the completion that point is in or next to.
Just like choose-completion, except this calls
lc-choose-completion-string instead of choose-completion-string."
  (interactive)
  (let (beg end completion (buffer completion-reference-buffer)
	(base-size completion-base-size))
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
    (if (null beg)
	(error "No completion here"))
    (setq beg (previous-single-property-change beg 'mouse-face))
    (setq end (or (next-single-property-change end 'mouse-face)
		  (point-max)))
    (setq completion (buffer-substring-no-properties beg end))
    (let ((owindow (selected-window)))
      (if (and (one-window-p t 'selected-frame)
	       (window-dedicated-p (selected-window)))
	  ;; This is a special buffer's frame
	  (iconify-frame (selected-frame))
	(or (window-dedicated-p (selected-window))
	    (bury-buffer)))
      (select-window owindow))
    (lc-choose-completion-string completion buffer base-size)))

(defun lc-mouse-choose-completion (event)
  "Click on an alternative in the `*Completions*' buffer to choose it.
Just like mouse-choose-completion, except this calls
lc-choose-completion-string instead of choose-completion-string."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((buffer (window-buffer))
        choice
	base-size)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-start event))))
      (if completion-reference-buffer
	  (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
	(goto-char (posn-point (event-start event)))
	(let (beg end)
	  (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	      (setq end (point) beg (1+ (point))))
	  (if (null beg)
	      (error "No completion here"))
	  (setq beg (previous-single-property-change beg 'mouse-face))
	  (setq end (or (next-single-property-change end 'mouse-face)
			(point-max)))
	  (setq choice (buffer-substring-no-properties beg end)))))
    (let ((owindow (selected-window)))
      (select-window (posn-window (event-start event)))
      (if (and (one-window-p t 'selected-frame)
	       (window-dedicated-p (selected-window)))
	  ;; This is a special buffer's frame
	  (iconify-frame (selected-frame))
	(or (window-dedicated-p (selected-window))
	    (bury-buffer)))
      (select-window owindow))
    (lc-choose-completion-string choice buffer base-size)))

(defun lc-choose-completion-string (choice &optional buffer base-size)
  "Like choose-completion-string (from simple.el), with some stuff to
make it work well (it says here) with lightning completion."
  (let ((buffer (or buffer completion-reference-buffer)))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
	     (or (not (active-minibuffer-window))
		 (not (equal buffer
			     (window-buffer (active-minibuffer-window))))))
	(error "Minibuffer is not active for completion")
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
	  (delete-region (+ base-size (point-min)) (point))
	(choose-completion-delete-max-match choice))
      (insert choice)
;       (remove-text-properties (- (point) (length choice)) (point)
; 			      '(mouse-face nil))
      (if (string-match (regexp-quote (car lc-stack)) choice)
	  (setq lc-stack (cons choice lc-stack))
	(setq lc-stack (cons (concat (car lc-stack) choice)
			     lc-stack)))
      ;; choice may be part of a multiline string (e.g. in ultra-tex),
      ;; so complete
      (if (lc-complete-stack-top "" t)
	  (lc-complete-stack-top ""))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
	(set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice.
      (if (and (equal buffer (window-buffer (minibuffer-window)))
	       minibuffer-completion-table)
	   ;; If this is reading a file name, and the file name chosen
	   ;; is a directory, don't exit the minibuffer.
	  (if (and (eq minibuffer-completion-table 'read-file-name-internal)
		   (file-directory-p (buffer-string)))
	      (select-window (active-minibuffer-window))
	    (exit-minibuffer))
	(and lc-prev-windows
	       (lc-quit 'choose))))))

(defvar lc-completion-fixup-function nil
  "A function to customize how completions are identified in completion lists.
`lc-completion-setup-function' calls this function with no arguments
each time it has found what it thinks is one completion.
Point is at the end of the completion in the completion list buffer.
If this function moves point, it can alter the end of that completion.")

(defvar lc-completion-message-function
  'lc-completion-default-message-function 
  "A function to give the text at the top of the *Completions*
buffer.  Called by `lc-completion-setup-function'.")

(defun lc-completion-default-message-function nil
  "Standard message function for lc-completion-setup-function."
  (if (lc-window-system)
      (insert (substitute-command-keys
	       "Click \\[lc-mouse-choose-completion] on a completion to select it.\n")))
  (insert (substitute-command-keys
	   "In this buffer, type \\[lc-choose-completion] to \
select the completion near point.\n\n"))
  (forward-line 1))

(defun lc-completion-setup-function ()
  "Like completion-setup-function (from simple.el), except with
slightly different messages."
  (save-excursion
    (let ((mainbuf (current-buffer)))
      (set-buffer standard-output)
      (completion-list-mode)
      (make-local-variable 'completion-reference-buffer)
      (setq completion-reference-buffer mainbuf)
      ;; The value 0 is right in most cases, but not for file name completion.
      ;; so this has to be turned off.
      ;;      (setq completion-base-size 0)
      (goto-char (point-min))
      (if lc-completion-message-function
	  (funcall lc-completion-message-function))
      (while (re-search-forward "[^ \t\n]+\\( [^ \t\n]+\\)*" nil t)
	(let ((beg (match-beginning 0))
	      (end (point)))
	  (if lc-completion-fixup-function
	      (funcall lc-completion-fixup-function))
	  (put-text-property beg (point) 'mouse-face 'highlight)
	  (goto-char end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities
;;

(defun word-grabber nil
  "Move point to just after the word point is in or after, and
return length of word."
  (skip-chars-forward "^ \n\t\f\"`'();{}")
  (- (point) (save-excursion (skip-chars-backward "^ \n\t\f\"`'();{}")
			     (point))))

(defun point-adjust-hook (arg)
  "Intended to be used when lc-table is an alist whose elements look
like `(<string> <number> . <hook>)'. Move point forward <number>
chars, and then run <hook> (if non-nil)."
  (forward-char (car (cdr arg)))
  (if (cdr (cdr arg)) (funcall (cdr (cdr arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry points for completion on various things.  see also
;; completing-insert-buffer-contents below.
;;

(defun completing-insert-lisp-object nil
  "Complete lisp object in buffer at point."
  (interactive)
  (let ((time (lc-complete-p 'lisp-objects)))
    (if (and (numberp time)
	     (<= 0 time))
	(setq lc-complete-idle-time time)
      (setq lc-complete-idle-time lc-complete-idle-time-default)))
  (completing-insert obarray nil (word-grabber) nil "lisp objects"))

(defun completing-insert-lisp-function nil
  "Complete lisp object in buffer at point."
  (interactive)
  (let ((time (lc-complete-p 'functions)))
    (if (and (numberp time)
	     (<= 0 time))
	(setq lc-complete-idle-time time)
      (setq lc-complete-idle-time lc-complete-idle-time-default)))
  (completing-insert obarray 'fboundp (word-grabber) nil "functions"))

(defun completing-insert-lisp-variable nil
  "Complete lisp object in buffer at point."
  (interactive)
  (let ((time (lc-complete-p 'variables)))
    (if (and (numberp time)
	     (<= 0 time))
	(setq lc-complete-idle-time time)
      (setq lc-complete-idle-time lc-complete-idle-time-default)))
  (completing-insert obarray 'boundp (word-grabber) nil "variables"))

(defun completing-insert-buffer-name nil
  "Complete buffer name in buffer at point."
  (interactive)
  (let ((time (lc-complete-p 'buffers)))
    (if (and (numberp time)
	     (<= 0 time))
	(setq lc-complete-idle-time time)
      (setq lc-complete-idle-time lc-complete-idle-time-default)))
  (completing-insert (mapcar (function (lambda (x) (list (buffer-name x))))
			     (buffer-list))
		     nil (word-grabber) nil "buffer names"))

(defun completing-insert-kill nil
  "Complete something from the kill ring in buffer at point."
  (interactive)
  (let ((time (lc-complete-p 'kill)))
    (if (and (numberp time)
	     (<= 0 time))
	(setq lc-complete-idle-time time)
      (setq lc-complete-idle-time lc-complete-idle-time-default)))
  (completing-insert
   (mapcar 'list
	   (apply 'append
		  (mapcar
		   (function
		    (lambda (x)
		      (cons x (and (string-match "\\s-+" x)
				   (list (substring x (match-end 0)))))))
		   kill-ring)))
   nil 0 nil "recent kills"))

(defun completing-insert-ispell-word nil
  "Complete the current word using ispell."
  (interactive)
  (completing-insert 'lc-lookup-words nil
			(word-grabber) nil
			"words"))

(defun lc-lookup-words (string pred flag)
  "Complete STRING a la ispell-complete-word.  Suitable for use with
the function completing-insert.  PRED will always be nil--it's there
for compatibility purposes.  If FLAG is non-nil, return all possible
completions.  If FLAG is nil, complete as far as possible.  If there
is a unique completion, return it.  If STRING equals the unique
completion, return t."
  (require 'ispell)
  (let ((word-list (lookup-words string))
	(guess string))
    (if flag word-list
      (if (zerop (length word-list))
	  nil
	(if (= 1 (length word-list))
	    (or (string= string (car word-list))
		(car word-list))
	  (while (and (not (string= guess (car word-list)))
		      (not (member nil
				   (mapcar
			      (function
			       (lambda (word)
				 (string-match (regexp-quote
						(substring
						 (car word-list)
						 0 (1+ (length
							guess))))
					       word)))
			      word-list))))
	    (setq guess (substring (car word-list)
				 0 (1+ (length guess)))))
	  guess)))))

(defalias 'lc-complete-lisp-object 'completing-insert-lisp-object)
(defalias 'lc-complete-lisp-function 'completing-insert-lisp-function)
(defalias 'lc-complete-lisp-variable 'completing-insert-lisp-variable)
(defalias 'lc-complete-buffer-name 'completing-insert-buffer-name)
(defalias 'lc-complete-kill-ring 'completing-insert-kill)
(defalias 'lc-complete-file-name 'completing-insert-file-name)
(defalias 'lc-complete-buffer-contents 'completing-insert-buffer-contents)
(defalias 'lc-complete-word 'completing-insert-ispell-word)
(defalias 'lc-complete-a-la-mode 'completing-insert-according-to-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion a la mode
;;

(defun completing-insert-according-to-mode nil
  "Start lightning completion.  If possible, resumes stopped completion.  
Otherwise, in the minibuffer, uses its table and predicate (slightly
modified for file name reading).  Failing that, calls
`completing-insert-function' if the mode has it set.  Final default is
lisp-object completion."
  (interactive)
  (cond ((completing-insert lc-table lc-predicate -1 lc-hook) nil)
	((and (minibuffer-window-active-p (minibuffer-window))
	      minibuffer-completion-table)
	 (let* ((table (if (eq minibuffer-completion-table
			       'read-file-name-internal)
			   'lc-read-file-name-internal
			 minibuffer-completion-table))
		(message
		 (cond ((eq table 'lc-read-file-name-internal)
			"file names")
		       ((and (listp table) (bufferp (cdr (car table))))
			"buffers")
		       ((eq obarray table)
			(cond ((not
				(and (boundp
				      'minibuffer-completion-predicate)
				     minibuffer-completion-predicate))
			       "lisp objects")
			      ((eq 'fboundp minibuffer-completion-predicate)
			       "functions")
			      ((eq 'commandp minibuffer-completion-predicate)
			       "commands")
			      ((eq 'boundp minibuffer-completion-predicate)
			       "variables")
			      ((eq 'user-variable-p
				   minibuffer-completion-predicate)
			       "user variables")))
		       (t "something")))
		(display (and (eq table 'lc-read-file-name-internal)
			      'lc-file-display-filter)))
	   (or (completing-insert table minibuffer-completion-predicate
				  -1)
	       (completing-insert table minibuffer-completion-predicate
				  (progn (goto-char (point-max))
					 (- (point) (point-min)))
				  nil message display))))
	;; I moved this here to make existing minibuffer
	;; completion info take precedence over stopped completion.
	;; -- Nick Reingold 5/24/92
	((completing-insert lc-table lc-predicate -1
			    lc-hook lc-display-filter) nil)
	(completing-insert-function
	 (call-interactively completing-insert-function))
	(t (completing-insert-lisp-object))))

(defvar completing-insert-function nil
  "Function to be called by M-x completing-insert-according-to-mode, 
if non-nil")
(make-variable-buffer-local 'completing-insert-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;lightnification
;;

;; Add this function to minibuffer-setup-hook, or (even better)
;; customize the variable toggle-lightning-completion, to turn on
;; lightning completion.
(defun lightning-completion ()
  (interactive)
  (let ((pred minibuffer-completion-predicate)
	complete-p message table display)
    (cond ((and lc-dark-recursive-minibufs
		(< (if (numberp lc-dark-recursive-minibufs)
		       lc-dark-recursive-minibufs 1)
		   (recursion-depth)))
	   (setq complete-p nil))
	  ((memq this-command lc-dark-commands)
	   (setq complete-p nil))
	  ((eq minibuffer-history-variable 'file-name-history)
	   (setq complete-p (lc-complete-p 'files)
		 message "file names"
		 table 'lc-read-file-name-internal
		 display 'lc-file-display-filter))
	  ((eq 'fboundp minibuffer-completion-predicate)
	   (setq complete-p (lc-complete-p 'functions)
		 message "functions"
		 table obarray))
	  ((eq 'commandp minibuffer-completion-predicate)
	   (setq complete-p (lc-complete-p 'commands)
		 message "commands"
		 table obarray))
	  ((eq 'boundp minibuffer-completion-predicate)
	   (setq complete-p (lc-complete-p 'variables)
		 message "variables"
		 table obarray))
	  ((eq 'user-variable-p minibuffer-completion-predicate)
	   (setq complete-p (lc-complete-p 'user-variables)
		 message "user variables"
		 table obarray))
	  ((and (eq minibuffer-completion-table obarray)
		(not (and (boundp 'minibuffer-completion-predicate)
			  minibuffer-completion-predicate)))
	   (setq complete-p (lc-complete-p 'lisp-objects)
		 message "lisp objects"
		 table obarray))
	  ((eq 'Info-complete-menu-item minibuffer-completion-table)
	   (setq complete-p (lc-complete-p 'info-menu-items)
		 message "Info menu items"
		 table minibuffer-completion-table))
	  ((eq minibuffer-history-variable 'query-replace-history)
	   (setq complete-p (lc-complete-p 'query)
		 message "buffer contents"
		 table 'buffer-completion-internal
		 pred (car (cdr (buffer-list)))))
	  ((and (listp minibuffer-completion-table)
		(listp (car minibuffer-completion-table))
		(bufferp (cdr (car minibuffer-completion-table))))
	   (setq complete-p (lc-complete-p 'buffers)
		 message "buffers"
		 table minibuffer-completion-table))
	  (minibuffer-completion-table
	   (setq complete-p (lc-complete-p 'misc)
		 message "something"
		 table minibuffer-completion-table)))
    (if complete-p
	(progn
	  (setq lc-complete-idle-time complete-p)
	  (or (completing-insert table pred -1)
	      (completing-insert table pred
				 (progn (goto-char (point-max))
					(- (point)
					   (point-min)
					   (lc-minibuffer-prompt-width)))
				 nil message display))))))

(defun query-replace-read-args (string regexp-flag)
  (lc-query-replace-read-args string regexp-flag))

(defun lc-query-replace-read-args (string regexp-flag)
  (let (from to)
    (if query-replace-interactive
	(setq from (car (if regexp-flag regexp-search-ring search-ring)))
      (setq from (read-from-minibuffer (format "%s: " string)
				       nil nil nil
				       'query-replace-history)))
    (remove-hook 'minibuffer-setup-hook 'lightning-completion)
    (condition-case ()
	(setq to (read-from-minibuffer (format "%s %s with: " string from)
				       nil nil nil
				       'query-replace-history))
	(quit
	 (if toggle-lightning-completion
	     (add-hook 'minibuffer-setup-hook 'lightning-completion))
	 (error "Quit")))
    (if toggle-lightning-completion
	(add-hook 'minibuffer-setup-hook 'lightning-completion))
    (list from to current-prefix-arg)))

(defun lc-complete-p (arg)
  "Non-nil if one should do lightning completion in environment ARG,
as determined by the value of the variable lightning-completion-list.
More specifically, either nil or a non-negative number."
  (let ((flag (cdr (assoc arg lightning-completion-list))))
    (cond
     ((eq flag t)
      lc-complete-idle-time-default)
     ((and (numberp flag)
	   (<= 0 flag))
      flag)
     ((and (numberp flag)
	   (> 0 flag))
      nil)
     ((eq flag nil)
      nil))))

;; Perhaps not so useful any more.  Customize
;; lightning-completion-list instead.
(defun lightnify (arg &rest args)
  "Toggle lightning completion on ARG--possible values are 'files,
'functions, 'commands, 'variables, 'user-variables, 'lisp-objects,
'info-menu-items, 'buffers, 'query, 'misc.  Also: argument 'all will
enable lightning completion on everything imaginable.  Argument 'none
will turn off lightning completion on everything.  It's probably
better to customize the variable lightning-completion-list than to use
this function."
  (cond ((equal arg 'none)
	 (setq lightning-completion-list
	       lightning-completion-list-default))
	((equal arg 'all)
	 (setq lightning-completion-list
	       (mapcar '(lambda (x) (append x t))
			  lightning-completion-list-default)))
	(t
	 (let ((arg-list (cons arg args)) old)
	   (while arg-list
	     (setq old (assoc (car arg-list) lightning-completion-list)
		   arg-list (cdr arg-list))
	     (if old
		 (progn
		   (setq lightning-completion-list 
			 (delete old lightning-completion-list))
		   (if (cdr old)
		       (setq lightning-completion-list
			     (cons (list (car old))
				   lightning-completion-list))
		     (setq lightning-completion-list
			   (cons (cons (car old) t)
				 lightning-completion-list))))))
	   lightning-completion-list)))
  (setq lightning-completion-list-external
	(lc-unconvert-completion-list lightning-completion-list)))

;; It's probably better to customize the variable lc-dark-commands.
(defun lc-make-command-dark (command)
  "Turn off lightning completion for COMMAND."
  (interactive "CTurn off lighting completion for command: ")
  (or (member command lc-dark-commands)
      (setq lc-dark-commands (cons command lc-dark-commands))))

;; The argument to `interactive' is taken from cancel-debug-on-entry.
;; This way Emacs completes on the elements of lc-dark-commands.
;; It's probably better to customize the variable lc-dark-commands.
(defun lc-make-command-light (command)
  "Turn on lightning completion for COMMAND, reversing the effects of
lc-make-command-dark.  (This will not enable lightning completion in
general--you need to call lightnify to do that.)"
  (interactive
   (list (let ((name
		(completing-read "Enable lightning completion for command: " 
				 (mapcar 'list
					 (mapcar 'symbol-name
						 lc-dark-commands))
				 nil t nil)))
	   (if name (intern name)))))  
  (setq lc-dark-commands (delete command lc-dark-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file completion stuff
;;

(defun completing-insert-file-name (&optional dir init)
  "Complete file name in buffer at point.  Non-interactively, use directory
DIR (nil for current default-directory); start with INIT chars before point."
  (interactive (list nil (word-grabber)))
  (let ((time (lc-complete-p 'files)))
    (if (and (numberp time)
	     (<= 0 time))
	(setq lc-complete-idle-time time)
      (setq lc-complete-idle-time lc-complete-idle-time-default)))
  (completing-insert 'lc-read-file-name-internal
		     (or dir default-directory) (or init 0)
		     nil "file names" 'lc-file-display-filter))

(defconst lc-literal-file-regexp
  "\\(\\(^\\|/\\)\\(~[^/]*\\|\\.\\.?\\)\\|\\${?[a-zA-Z0-9]*\\)$"
  "Regexp for file names which don't get completed, yet.")
(defconst lc-expand-this-file-regexp
  "\\(\\${[a-zA-Z0-9]*}\\|\\(^\\|/\\)\\.\\.?/\\)$"
  "Regexp for file names which get expanded before completion.")

(defun lc-read-file-name-internal (str dir action)
  "\"Internal\" subroutine for `completing-insert-file-name'. Do not
call this."
  (let (str-dir real-str)
    (cond ((and (null action) (string-match lc-literal-file-regexp str))
	   str)
	  ((progn (setq real-str (lc-expand-file-name
				  (substitute-in-file-name str) dir)
			str-dir (file-name-directory real-str))
		  (not (file-directory-p str-dir)))
	   nil)
	  ((eq action t)
	   (mapcar (function (lambda (x)
			       (expand-file-name x str-dir)))
		   (read-file-name-internal str dir action)))
	  ((file-directory-p real-str)
	   real-str)
	  (t
	   (let* ((exp (string-match lc-expand-this-file-regexp str))
		  (str (if exp real-str str))
		  (ans (read-file-name-internal str dir action)))
	     (if (null action)
		 (if (and exp (eq ans t)) str ans)
	       (and (not exp) ans)))))))

(defun lc-expand-file-name (name &optional dir)
  "Like expand-file-name, except that if first arg NAME is something
like `bozo/.' then return `bozo/'.  expand-file-name, in contrast,
would return `bozo'."
  (concat (expand-file-name name dir)
	  (if (or (and (< 1 (length name))
			  (string= "/." (substring name -2)))
		  (and (< 2 (length name))
			  (string= "/.." (substring name -3))))
	      "/")))

(defun lc-file-display-filter (fn)
  (cond ((string-match lc-ignored-file-extensions fn)
	 nil)
	((file-directory-p fn)
	 (let ((dir (if (file-directory-p (car lc-stack))
			(car lc-stack)
		      (directory-file-name (car lc-stack)))))
	   (if (string= fn (lc-expand-file-name "./" dir))
	       "./"
	     (if (string= fn (lc-expand-file-name "../" dir))
		 "../"
	       (concat (file-name-nondirectory (directory-file-name fn))
		       "/")))))
	(t (file-name-nondirectory fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer completion stuff.
;;
;; This section (which used to be the file bufcomp.el) adapts
;; lightning completion to complete on reasonably balanced substrings
;; of a buffer.  The main entry point is
;;   (completing-insert-buffer-contents BUF)
;; where BUF is interactively the current buffer or, with arg, a buffer
;; specified by the user.

(defun buffer-sub-hunk (start end)
  "Return substring of current buffer from START at least up to END, extended
sufficiently to be balanced if possible, but in any case not to include
more than one non-blank line past END."
  (save-excursion
    (goto-char end)
    (skip-chars-forward "\n")
    (skip-chars-forward "^\n")
    (save-restriction
      (narrow-to-region start (point))
      (goto-char start)
      (let (n)
	(while (< (point) end)
	  (condition-case what (goto-char (setq n (scan-sexps (point) 1)))
	    (error (if (or (null n) (= ?U (aref (car (cdr what)) 0)))
		       (goto-char (point-max))
		     (forward-char 1))))))
      (buffer-substring-no-properties start (point)))))

(defvar buf-comp-internal-last nil)	; last return of a try-type call

(defun buffer-completion-internal (str buf action)
  "Internal subroutine for `completing-insert-buffer-contents'.  Do
not call this.
  Used like `read-file-name-internal' but for completing STR as a
substring of buffer BUF.  Completing with space as last char matches
anything, as long as the match is unique.  ACTION nil means common
part of proper extensions of STR, up to next sexp boundary, t means
list of some of these extensions.  Other means return nil (no
substring is ever considered complete)."
  (and
   (memq action '(nil t))		; never complete so keep is disabled
   (save-window-excursion
     (let* ((obuf (prog1 (current-buffer) (set-buffer buf)))
	    inhibit-quit case-fold-search find (l (length str)))
       (prog2
	   (if (eq buf obuf)		; hide completion in progress
	       (progn (setq inhibit-quit t)
		      (delete-backward-char (length (car lc-stack)))))
	   (if action
	       (let ((oball (make-vector 37 0)) (n 700))
		 (save-excursion
		   (goto-char (point-min))
		   (while (and (< 0 (setq n (1- n)))
			       (search-forward str nil t))
		     (intern (buffer-sub-hunk (match-beginning 0)
					      (min (point-max) (1+ (point))))
			     oball))
		   (if (< 0 n) (all-completions "" oball)
		     '("Completions too numerous to mention!"))))
	     (setq			; this arranges that identical repeats
	      buf-comp-internal-last	; of a try call do no work, speeding
	      (if (eq str buf-comp-internal-last) str ; up lc-complete-stack-top.
		(save-excursion
		  (goto-char (point-min))
		  (or
		   (and
		    (search-forward str nil t)
		    (setq find (buffer-sub-hunk (match-beginning 0) (point)))
		    (progn
		      (while (and (> (length find) l) (search-forward str nil t))
			(setq find (try-completion
				    ""
				    (list (list find)
					  (list
					   (buffer-substring-no-properties
					    (match-beginning 0)
					    (min (point-max)
						 (+ (match-beginning 0)
						    (length find)))))))))
		      find))
		   (and (string-match "\\s-" (substring str -1))
			(search-forward (setq str (substring str 0 -1)) nil t)
			(setq find (buffer-sub-hunk (match-beginning 0)
						    (min (point-max)
							 (1+ (point)))))
			(progn
			  (setq l (1- l))
			  (while (and (> (length find) l)
				      (search-forward str nil t))
			    (setq find (try-completion
					""
					(list
					 (list find)
					 (list
					  (buffer-substring-no-properties
					   (match-beginning 0)
					   (min (point-max)
						(+ (match-beginning 0)
						   (length find)))))))))
			  (and (> (length find) l) find))))))))
	 ;; unhide:
	 (if (eq buf obuf) (insert (car lc-stack))))))))

(defun completing-insert-buffer-contents  (&optional buf)
  "Complete on substrings of BUF extending to sexp boundaries.  String is
never complete, so exit with C-c.  Once unique, space means match more.
Interactively, with arg, ask for the buffer, else current buffer."
  (interactive "P")
  (if (and (interactive-p) buf)
      (setq buf (read-buffer "Complete from buffer: ")))
  (setq buf (or buf (current-buffer)))
  (completing-insert 'buffer-completion-internal buf 0 nil "buffer contents"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that depend on the version of Emacs.
;;

(if (not (fboundp 'caar))
    (require 'cl))

(defun lc-redraw-modeline ()
  "Run force-mode-line-update if that function is bound.
Otherwise try to run redraw-modeline.  Otherwise do nothing."
  (if (fboundp 'force-mode-line-update)
      (force-mode-line-update)
    (if (fboundp 'redraw-modeline)
	(redraw-modeline)
      nil)))

(defun lc-character-to-event (char)
  "Convert a character CHAR into an event.  This just returns CHAR
in GNU Emacs 19 or 20.  In XEmacs, it calls character-to-event."
  (if (fboundp 'character-to-event)
      (character-to-event char)
    char))

(defun lc-window-system ()
  "Non-nil if using x windows"
  (if (fboundp 'console-type)
      (eq (console-type) 'x)
    (eq window-system 'x)))

(defun lc-minibuffer-prompt-width ()
  "0 unless using GNU Emacs 21, in which case minibuffer-prompt-width"
  (if (and lc-emacs-21-p
	   (minibuffer-window-active-p (minibuffer-window)))
      (minibuffer-prompt-width)
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More stuff dependent on the version of emacs.  This is all related
;; to displaying completions.
;;

(defvar lc-completion-default-help-string
  '(concat
    (if (device-on-window-system-p)
	(substitute-command-keys
	 "Click \\<lc-completion-list-mode-map>\\[lc-mouse-choose-completion] on a completion to select it.\n") "")
    (substitute-command-keys
     "Type \\<light-mode-map>\\[lc-advertised-switch-to-completions] or \\<light-mode-map>\\[lc-advertised-switch-to-completions] to move to this buffer, for keyboard selection.\n
In this buffer, type \\<lc-completion-list-mode-map>\\[lc-choose-completion] to
select the completion near point.\n\n"))
  "For use with XEmacs only.
Form the evaluate to get a help string for completion lists.
This string is inserted at the beginning of the buffer.
See `display-completion-list'.")

(defun lc-display-completions-internal (all)
  "Run display-completion-list with appropriate modifications,
depending on whether we're using XEmacs or not."
  (if lc-xemacs-p
      (with-output-to-temp-buffer lc-completion-buffer-name
	(display-completion-list
	 (sort all 'string<)
	 :help-string lc-completion-default-help-string))
    (let ((old-hook completion-setup-hook)
	  (old-map completion-list-mode-map))
      (setq completion-setup-hook
	    'lc-completion-setup-function
	    completion-list-mode-map
	    lc-completion-list-mode-map)
      (with-output-to-temp-buffer lc-completion-buffer-name
	(display-completion-list
	 (sort all 'string<)))
      (setq completion-setup-hook old-hook
	    completion-list-mode-map old-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'light)

(provide 'bufcomp)  ;; for backwards compatibility

;;; light.el ends here
