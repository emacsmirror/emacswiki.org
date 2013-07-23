;;; dired-sort-menu.el --- provide menu/dialogue for dired sort options

;; Copyright (C) 2000, 2001 Francis J. Wright

;; Author: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; Maintainer: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; Time-stamp: <26 July 2001>
;; Version: 2001.07.26
;; Package-Requires: ()
;; URL: http://centaur.maths.qmw.ac.uk/Emacs/
;; URL: https://sites.google.com/site/fjwcentaur/emacs/files/dired-sort-menu.el
;; Keywords: dired, sort, menu, dialogue

;; $Id: dired-sort-menu.el,v 1.26 2001-07-26 21:22:48+01 fjw Exp $

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

;;; Commentary:

;; This package, which is intended for use with GNU Emacs 20/21,
;; implements a menu and a dialogue that consist of a set of radio
;; buttons and toggles to control sort order in dired mode.  The menu
;; is added as a submenu to the Immediate menu, and bound to S-mouse-2
;; as a popup menu.  The dialogue can be accessed via the menu or the
;; dired key binding `C-d' or the command `dired-sort-dialogue', which
;; *must* be run in a dired buffer!  The dialogue uses the Emacs
;; widget library, which does not require GUI support but uses it if
;; available.  The dialogue can be used with a character-based
;; display; the main keys to use are <TAB> and <RET> if a mouse is not
;; available or does not work.  With a GUI the dialogue pops up its
;; own frame by default; otherwise it uses part of the dired window.
;; This is controlled by the customizable user option
;; `dired-sort-dialogue-own-frame'.

;; The menu and dialogue support the `ls' sort switches [tSXUucrR].
;; These are all implemented by GNU `ls'; the subset [tSucr] are
;; implemented by the standard Emacs 20 `ls-lisp' emulation and all
;; are implemented by the Emacs 21 `ls-lisp' (see above URL).
;; Changing a main sort option turns off reverse sorting; toggling
;; reverse sorting does not change the main sort option.  Setting any
;; prefix argument before selecting a sort order reverses it
;; (e.g. press `C-u' or `C-number' or `M-number' BEFORE accessing the
;; menu bar).  The menu also supports two Emacs 21 `ls-lisp' switches:
;; `ls-lisp-ignore-case' ignores case in alphanumeric sorts and
;; `ls-lisp-dirs-first' lists all directories first.  (These latter
;; two switches improve compatibility with Microsoft Windows
;; Explorer.)

;; The toggles for reverse sorting, `ls-lisp-ignore-case' and
;; `ls-lisp-dirs-first', are bound respectively to the keys "r", "c"
;; and "b" in the dired map.

;; A `Configuration' submenu allows a dired sort configuration to be
;; saved or restored, the current and saved configurations to be
;; swaped (bound to "T" for "toggle"), or the default to be restored.
;; The saved configuration is saved as a customization, which can also
;; be changed directly.  This submenu also allows
;; `dired-sort-dialogue-own-frame' to be toggled.

;; If a `dired-sort-menu' option causes an error (but not if it is
;; just ignored) then it is automatically disabled via the
;; customization facility.  This is useful because not all `ls' and
;; `ftp' programs support the same sort options.

;; The dialogue allows the setting of arbitrary combinations of dired
;; sort options together, without requiring multiple menu accesses.
;; Each dired buffer has its own dialogue.  This dialogue can be left
;; displayed, in which case it is automatically updated to reflect any
;; changes to its dired buffer that are made by other means.  For
;; tidiness, a dialogue buffer is automatically killed whenever it or
;; its dired buffer becomes invisible.

;; To do:
;;   test `dired-sort-menu-invalid-options' facility;
;;   per-host `dired-sort-menu-invalid-options-remote';
;;   cascade multiple dialogue frames?

;;; Installation:

;; Put this file somewhere where Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it (recommended), and put this in your .emacs:
;;
;; (add-hook 'dired-load-hook
;;           (lambda () (require 'dired-sort-menu)))


;;; History:

;; Code for alternative ways to find files split off into
;; `dired-explore'.

;;; Code:

(require 'dired)
(require 'easymenu)
;; Silence compiler:
(eval-when-compile
  (defvar ls-lisp-ignore-case)		; not in current standard version
  (defvar ls-lisp-dirs-first)		; not in current standard version
  (require 'ange-ftp))

(defun dired-sort-menu-remote-p ()
  "Return the host name for a remote ange-ftp directory or nil if local."
  ;; Use the actual host name later!
  ;; The default extended filename syntax is '/user@host:name', where the
  ;; 'user@' part may be omitted.  This syntax can be customised to a certain
  ;; extent by changing ange-ftp-name-format.  There are limitations.
  (and (string-match "\\`/\\([^@:/]+@\\)?\\([^:/]+\\):" default-directory)
       (match-string 2)))

;;;###autoload
(defgroup dired-sort-menu nil
  "Menu and dialogue to control `dired' sort options."
  :group 'dired)

(defcustom dired-sort-menu-saved-config nil
  "*Alist of saved `dired' sort options.  Defaults to None (nil).
Can be customized directly or by saving from Dired sort menu.  If the
AList option is chosen in a customization buffer then it defaults to
the default options, which can be set via the Dired and Ls-Lisp
customization buffers."
  :type `(choice
	  (const :tag "None" nil)
	  (list :tag "AList"
		:value
		((dired-actual-switches
		  . ,dired-listing-switches)
		 (ls-lisp-ignore-case
		  . ,(let ((v (get 'ls-lisp-ignore-case 'standard-value)))
		       (and v (apply 'eval v))))
		 (ls-lisp-dirs-first
		  . ,(let ((v (get 'ls-lisp-dirs-first 'standard-value)))
		       (and v (apply 'eval v)))))
		(cons :tag "Any Ls"
		      (const :tag "Switches" dired-actual-switches)
		      string)
		(cons :tag "Ls-Lisp Only"
		      (const :tag "Ignore Case" ls-lisp-ignore-case)
		      boolean)
		(cons :tag "Ls-Lisp Only"
		      (const :tag "Dirs First" ls-lisp-dirs-first)
		      boolean)))
  :group 'dired-sort-menu)

(defcustom dired-sort-menu-invalid-options nil
  "*List of sort option strings that are invalid for the local ls program.
The corresponding menu/dialogue items are inactive/unavailable.
This variable is automatically updated when a sort option causes an
error.  It can also be set or reset explicitly, and you should reset
it (usually to nil) if you change the ls program used by `dired'.
Ignored when using `ls-lisp'."
  :type '(repeat string)
  :group 'dired-sort-menu)

(defcustom dired-sort-menu-invalid-options-remote nil
  ;; Add option for one such variable per remote host later or some
  ;; kind of alist.
  "*List of sort option strings that are invalid for the remote ls program.
The corresponding menu/dialogue items are inactive/unavailable.
This variable is automatically updated when a sort option causes an
error.  It can also be set or reset explicitly, and you should reset
it if you change the remote host or ls program used by `dired'.
[Note that `ls-lisp' is never used for remote directory listings.]"
  :type '(repeat string)
  :group 'dired-sort-menu)

(defconst dired-sort-display-graphic-p	; window-system
  (if (fboundp 'display-graphic-p)
      (funcall (symbol-function 'display-graphic-p)) ; Emacs 21
    (memq window-system '(x w32)))	; Emacs 20
  "Non-nil if graphic display is possible.")

(defcustom dired-sort-dialogue-own-frame dired-sort-display-graphic-p
  "*If non-nil then dialogues should pop up in their own frames (if possible)."
  :type 'boolean
  :group 'dired-sort-menu)

(defsubst dired-sort-dialogue-own-frame-really ()
  "Return non-nil if dialogues should and can pop up in their own frames."
  (and dired-sort-dialogue-own-frame dired-sort-display-graphic-p))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ls-lisp-var-p (var)
  "Return non-nil if ls-lisp variable VAR should be used."
  (and (boundp var)
       (boundp 'ls-lisp-use-insert-directory-program)
       (not ls-lisp-use-insert-directory-program)
       (not (dired-sort-menu-remote-p))))

(defsubst dired-sort-menu-help (help-string r)
  "Return HELP-STRING, for reversed sort order if R is non-nil."
  (if r (concat help-string " [reversed]") help-string))

(defun dired-sort-menu-items (&optional r)
  "Return sort-menu item list; reverse sort with optional argument R = \"r\"."
  (list
   (vector
    "Name"
    `(dired-sort-menu-set-switches ,(concat "" r)) :style 'radio
    :selected (if r
		  '(and (not (dired-sort-menu-switch-p "[tSXUuc]"))
			(dired-sort-menu-switch-p "r"))
		'(not (dired-sort-menu-switch-p "[tSXUuc]")))
    :help (dired-sort-menu-help "Sort files by name" r)
    :active t)
   (vector
    "Time Modified"
    `(dired-sort-menu-set-switches ,(concat "t" r)) :style 'radio
    :selected `(dired-sort-menu-switch-p "t" ,r)
    :help (dired-sort-menu-help "Sort files by time last modified" r)
    :active t)
   ;; extended sort options ...
   (vector
    "Size"
    `(dired-sort-menu-set-switches ,(concat "S" r)) :style 'radio
    :selected `(dired-sort-menu-switch-p "S" ,r)
    :help (dired-sort-menu-help "Sort files by size" r)
    :active '(dired-sort-menu-active-p "S"))
   (vector
    "Extension"
    `(dired-sort-menu-set-switches ,(concat "X" r)) :style 'radio
    :selected `(dired-sort-menu-switch-p "X" ,r)
    :help (dired-sort-menu-help "Sort files by extension" r)
    :active '(dired-sort-menu-active-p "X"))
   (vector
    "Unsorted"
    `(dired-sort-menu-set-switches ,(concat "U" r)) :style 'radio
    :selected `(dired-sort-menu-switch-p "U" ,r)
    :help (dired-sort-menu-help "Sort files by physical order" r)
    :active '(dired-sort-menu-active-p "U"))
   (vector
    "Time Created"			; Windows
    `(dired-sort-menu-set-switches ,(concat "c" r)) :style 'radio
    :selected `(dired-sort-menu-switch-p "c" ,r)
    :help (dired-sort-menu-help "Sort files by time created" r)
    :active '(dired-sort-menu-active-p "c")
    :visible (and (eq system-type 'windows-nt)
		  (not (dired-sort-menu-remote-p))))
   (vector
    "Time Changed"			; not Windows
    `(dired-sort-menu-set-switches ,(concat "c" r)) :style 'radio
    :selected `(dired-sort-menu-switch-p "c" ,r)
    :help (dired-sort-menu-help "Sort files by time last changed" r)
    :active '(dired-sort-menu-active-p "c")
    :visible (or (not (eq system-type 'windows-nt))
		 (dired-sort-menu-remote-p)))
   (vector
    "Time Accessed"			; not useful under Windows
    `(dired-sort-menu-set-switches ,(concat "u" r)) :style 'radio
    :selected `(dired-sort-menu-switch-p "u" ,r)
    :help (dired-sort-menu-help "Sort files by time last accessed" r)
    :active '(dired-sort-menu-active-p "u")
    :visible (or (not (eq system-type 'windows-nt))
		 (dired-sort-menu-remote-p)))
   ))

;; Build a menu assigned to `dired-sort-menu' as both a variable and a
;; function, the former to use as a menu-bar sub-menu and the latter
;; as a popup menu:
(easy-menu-define			; (SYMBOL MAPS DOC MENU)
 dired-sort-menu
 nil
 "\"Sort By\" menu for dired mode."
 `("Sort By"
   ,@(dired-sort-menu-items)
   "--"
   ("Reversed Sort By"
    ,@(dired-sort-menu-items "r"))
   "--"
   ["Reverse" dired-sort-menu-toggle-reverse :style toggle
    :selected (dired-sort-menu-switch-p "r")
    :help "Reverse current sort order"
    :active (dired-sort-menu-active-p "r")]
   ["Recursive" dired-sort-menu-toggle-recursive :style toggle
    :selected (dired-sort-menu-switch-p "R")
    :help "Recursively list all subdirectories"
    :active (dired-sort-menu-active-p "R")]
   ["Ignore Case" dired-sort-menu-toggle-ignore-case :style toggle
    :selected ls-lisp-ignore-case :active t
    :help "Ignore case in alphanumeric sorting"
    ;; supported only by (Emacs 21) ls-lisp library and local dired:
    :visible (ls-lisp-var-p 'ls-lisp-ignore-case)]
   ["Dirs First" dired-sort-menu-toggle-dirs-first :style toggle
    :selected ls-lisp-dirs-first :active t
    :help "List subdirectories first [last if reversed]"
    ;; supported only by (Emacs 21) ls-lisp library and local dired:
    :visible (ls-lisp-var-p 'ls-lisp-dirs-first)]
   "--"
   ("Configuration"
    ["Save Current" dired-sort-menu-save-config
     :suffix (dired-sort-menu-current-suffix)
     :help "Save current sort configuration as a customization"
     :active t]
    ["Restore Saved" dired-sort-menu-restore-config
     :suffix (dired-sort-menu-restore-suffix)
     :help "Restore sort configuration saved as a customization"
     :active dired-sort-menu-saved-config]
    ["Swap Current/Saved" dired-sort-menu-swap-config
     :help "Exchange current and saved sort configurations"
     :active dired-sort-menu-saved-config]
    "---"
    ["Restore Default" dired-sort-menu-restore-default
     :suffix (dired-sort-menu-default-suffix)
     :help "Restore default sort configuration from customization"
     :active t]
    ["Customize Saved" (customize-option 'dired-sort-menu-saved-config)
     :help "Customize saved sort configuration"
     :active t]
    "---"
    ["Dialogue Own Frame" (setq dired-sort-dialogue-own-frame
			       (not dired-sort-dialogue-own-frame))
     :style toggle
     :selected dired-sort-dialogue-own-frame
     :help "Sort dialogues use own frame if checked, else dired frame"
     :active t])
   ["Dialogue" dired-sort-dialogue
    :help "Open a dialogue box to control this dired's sort options"
    :active t]
   ))

;; Menu bar "Immediate" menu sub-menu:
(easy-menu-add-item			; (map path item &optional before)
 dired-mode-map '("menu-bar" "immediate")
 dired-sort-menu
 'revert-buffer)

;; Popup menu on "Shift Mouse 2":
(define-key				; (keymap key def)
  dired-mode-map [S-down-mouse-2] 'dired-sort-menu-popup)

;;;###autoload
(defun dired-sort-menu-popup (event)
  "Pop up and run \"Sort By\" menu for dired mode *in EVENT window*."
  (interactive "@e")
  (let ((menu-item (x-popup-menu event dired-sort-menu)))
    ;; menu-item is null if pop-up menu is cancelled
    (and menu-item
	 (command-execute
	  (lookup-key dired-sort-menu (apply 'vector menu-item))))))

;; Add a few key bindings, as suggested by Ed Park <park@sunshine.net>.
;; Mnemonic *r*everse:
(define-key dired-mode-map "r" 'dired-sort-menu-toggle-reverse)
;; Mnemonic *c*asefold:
(define-key dired-mode-map "c" 'dired-sort-menu-toggle-ignore-case)
;; Mnemonic Files at *b*ottom:
(define-key dired-mode-map "b" 'dired-sort-menu-toggle-dirs-first)
;; Mnemonic *T*oggle Current/Saved:
(define-key dired-mode-map "T" 'dired-sort-menu-swap-config)

 

(defun dired-sort-menu-switch-p (switch &optional r)
  "Return true if regexp SWITCH matches (case sensitive) ls switch string.
With optional regexp R this must also match (case sensitive)."
  (let (case-fold-search)
    (setq switch (string-match switch dired-actual-switches))
    (if r
	(and switch (string-match r dired-actual-switches))
      switch
      )))

(defun dired-sort-menu-active-p (switch)
  "Return true if argument SWITCH not known to be invalid for the current ls."
  (if (dired-sort-menu-remote-p)
      (not (member switch dired-sort-menu-invalid-options-remote))
    (or (and (boundp 'ls-lisp-use-insert-directory-program)
	     (not ls-lisp-use-insert-directory-program)) ; ls-lisp
	(not (member switch dired-sort-menu-invalid-options)))))

;;;###autoload
(defun dired-sort-menu-toggle-ignore-case ()
  "Toggle ls-lisp switch `ls-lisp-ignore-case' and update buffer."
  (interactive)
  (setq ls-lisp-ignore-case (not ls-lisp-ignore-case))
  (revert-buffer))

;;;###autoload
(defun dired-sort-menu-toggle-dirs-first ()
  "Toggle ls-lisp switch `ls-lisp-dirs-first' and update buffer."
  (interactive)
  (setq ls-lisp-dirs-first (not ls-lisp-dirs-first))
  (revert-buffer))

(defun dired-sort-menu-set-switches (switch &optional reverse)
  "Set one ls sort switch SWITCH and update buffer.
Clears *all* other sort switches.
Reverse sort order if optional argument REVERSE is non-nil or if
called with any prefix argument \(e.g. \\[universal-argument] or
C-number or M-number BEFORE accessing menu bar)."
  ;; [FJW: I would like to use modifier keys to reverse the sort, but
  ;; that is not possible!]
  ;; First delete ALL ls sort switches:
  (let (case-fold-search)
    (while (string-match "[rtSXUuc]" dired-actual-switches)
      (setq dired-actual-switches
	    (replace-match "" t t dired-actual-switches))))
  (setq dired-actual-switches
	(concat dired-actual-switches switch))
  (if (or reverse current-prefix-arg)
      (setq dired-actual-switches
	    (concat dired-actual-switches "r")))
  (dired-sort-set-modeline)
  (dired-sort-menu-revert-buffer switch))

(defun dired-sort-menu-revert-buffer (switch)
  "Revert buffer carefully, handling invalid sort options gracefully.
If the current sort option SWITCH causes an error then update
`dired-sort-menu-invalid-options' or
`dired-sort-menu-invalid-options-remote' as appropriate."
  (condition-case nil
      (revert-buffer)
    ('file-error
     (if (dired-sort-menu-remote-p)
	 (customize-save-variable
	  'dired-sort-menu-invalid-options-remote
	  (cons switch dired-sort-menu-invalid-options-remote))
       (customize-save-variable
	'dired-sort-menu-invalid-options
	(cons switch dired-sort-menu-invalid-options)))
     ;; Remove all sort switches:
     (dired-sort-menu-set-switches "")
     (error
      "Invalid sort menu option disabled and directory sorted by name"))))

;;;###autoload
(defun dired-sort-menu-toggle-reverse ()
  "Toggle ls -r switch and update buffer.
Does not affect other sort switches."
  (interactive)
  (let (case-fold-search)
    (setq dired-actual-switches
	  (if (string-match "r" dired-actual-switches)
	      (replace-match "" t t dired-actual-switches)
	    (concat dired-actual-switches "r")))
    (dired-sort-set-modeline)
    (dired-sort-menu-revert-buffer "r")))

(defsubst dired-sort-menu-R-check (switches)
  "Additional processing of -R in ls option string SWITCHES.
Calls `dired-sort-R-check' only if defined (by `dired-fix' or Emacs 21).
Saves `dired-subdir-alist' when R is set and restores saved value
minus any directories explicitly deleted when R is cleared.
To be called first in body of `dired-sort-other', etc."
  (and (fboundp 'dired-sort-R-check)
       (funcall (symbol-function 'dired-sort-R-check) switches)))

;;;###autoload
(defun dired-sort-menu-toggle-recursive ()
  "Toggle ls -R switch and update buffer.
Does not affect other sort switches."
  (interactive)
  (let* (case-fold-search
	 (switches
	  (if (string-match "R" dired-actual-switches)
	      (replace-match "" t t dired-actual-switches)
	    (concat dired-actual-switches "R"))))
    (dired-sort-menu-R-check switches)
    (setq dired-actual-switches switches)
    (dired-sort-set-modeline)
    (dired-sort-menu-revert-buffer "R")))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code to save and restore dired sort configurations.  This has a lot
;; of possibilities, e.g. saving multiple configurations, which I
;; could implement if there were any demand.

(defsubst dired-sort-menu-current-suffix-flag (flag label)
  "Construct \"Configuration/Save Current\" menu item suffix for FLAG with LABEL."
  (and (boundp flag)
       (concat "; [" (if (eval flag) "X" " ") "] " label)))

(defun dired-sort-menu-current-suffix ()
  "Construct \"Configuration/Save Current\" menu item suffix."
  (concat
   "(ls " dired-actual-switches
   (dired-sort-menu-current-suffix-flag
    'ls-lisp-ignore-case "Ignore Case")
   (dired-sort-menu-current-suffix-flag
    'ls-lisp-dirs-first "Dirs First")
   ")"
   ))

(defsubst dired-sort-menu-restore-suffix-flag (flag label)
  "Construct \"Configuration/Restore Saved\" menu item suffix for FLAG with LABEL."
  (and (setq flag (assq flag dired-sort-menu-saved-config))
       (concat "; [" (if (cdr flag) "X" " ") "] " label)))

(defun dired-sort-menu-restore-suffix ()
  "Construct \"Configuration/Restore Saved\" menu item suffix."
  (and dired-sort-menu-saved-config
       (concat
	"(ls "
	(cdr (assq 'dired-actual-switches
		   dired-sort-menu-saved-config))
	(dired-sort-menu-restore-suffix-flag
	 'ls-lisp-ignore-case "Ignore Case")
	(dired-sort-menu-restore-suffix-flag
	 'ls-lisp-dirs-first "Dirs First")
	")"
	)))

(defsubst dired-sort-menu-default-suffix-flag (flag label)
  "Construct \"Configuration/Restore Default\" menu item suffix for FLAG with LABEL."
  (and (boundp flag)
       (concat "; [" (if (apply 'eval (get flag 'standard-value)) "X" " ") "] " label)))

(defun dired-sort-menu-default-suffix ()
  "Construct \"Configuration/Restore Default\" menu item suffix."
  (concat
   "(ls " dired-listing-switches
   (dired-sort-menu-default-suffix-flag
    'ls-lisp-ignore-case "Ignore Case")
   (dired-sort-menu-default-suffix-flag
    'ls-lisp-dirs-first "Dirs First")
   ")"
   ))

(defun dired-sort-menu-config-alist ()
  "Return `dired' sort configuration as alist."
  (cons
   (cons 'dired-actual-switches dired-actual-switches)
   (and (boundp 'ls-lisp-use-insert-directory-program)
	(not ls-lisp-use-insert-directory-program)
	(append
	 (and (boundp 'ls-lisp-ignore-case)
	      (list (cons 'ls-lisp-ignore-case ls-lisp-ignore-case)))
	 (and (boundp 'ls-lisp-dirs-first)
	      (list (cons 'ls-lisp-dirs-first ls-lisp-dirs-first)))))))

(defun dired-sort-menu-save-config ()
  "Save current `dired' sort configuration."
  (interactive)
  (customize-save-variable 'dired-sort-menu-saved-config
			   (dired-sort-menu-config-alist)))

(defun dired-sort-menu-restore-config (&optional swap)
  "Restore saved `dired' sort configuration.
If optional (interactive prefix) argument SWAP is non-nil then swap
saved and current configurations."
  (interactive "P")
  (when dired-sort-menu-saved-config
    (setq swap (if swap
		   (dired-sort-menu-config-alist)
		 dired-sort-menu-saved-config))
    (while dired-sort-menu-saved-config
      (set (caar dired-sort-menu-saved-config)
	   (cdar dired-sort-menu-saved-config))
      (setq dired-sort-menu-saved-config
	    (cdr dired-sort-menu-saved-config)))
    (dired-sort-set-modeline)
    (revert-buffer)
    (setq dired-sort-menu-saved-config swap)))

;;;###autoload
(defun dired-sort-menu-swap-config ()
  "Swap saved and current `dired' sort configuration."
  (interactive)
  (dired-sort-menu-restore-config t))

(defun dired-sort-menu-restore-default ()
  "Restore default `dired' sort configuration."
  (interactive)
  (setq dired-actual-switches dired-listing-switches)
  (if (boundp 'ls-lisp-ignore-case)
      (setq ls-lisp-ignore-case
	    (apply 'eval (get 'ls-lisp-ignore-case 'standard-value))))
  (if (boundp 'ls-lisp-dirs-first)
      (setq ls-lisp-dirs-first
	    (apply 'eval (get 'ls-lisp-dirs-first 'standard-value))))
  (dired-sort-set-modeline)
  (revert-buffer))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code to provide a customize-style dialogue-box that allows all sort
;; options to be set at once.  Care must be taken to access
;; `dired-actual-switches' only when the dired buffer is current,
;; because this variable is buffer-local.  There is currently no good
;; way to show that a widget is inactive, so inactive widgets are
;; simply not displayed.

;; Key binding, mnemonic *D*ialogue (d, D and M-d are used):
(define-key dired-mode-map "\C-d" 'dired-sort-dialogue)

(require 'wid-edit)

;; Buffer-local variables set in dired-sort-dialogue:
(make-variable-buffer-local
 (defvar dired-sort-dialogue-dired-buffer))
(make-variable-buffer-local
 (defvar dired-sort-dialogue-radio-widget))
(make-variable-buffer-local
 (defvar dired-sort-dialogue-reverse-widget nil))
(make-variable-buffer-local
 (defvar dired-sort-dialogue-recursive-widget nil))
(make-variable-buffer-local
 (defvar dired-sort-dialogue-ignore-case-widget nil))
(make-variable-buffer-local
 (defvar dired-sort-dialogue-dirs-first-widget nil))

(defun dired-sort-dialogue-choice ()
  "Return string to set Dired sort dialogue radio button choice."
  (let (case-fold-search
	(start (string-match "[tSXUuc]" dired-actual-switches)))
    (if start
	(substring dired-actual-switches start (1+ start))
      "")))

(defsubst dired-sort-dialogue-buffer-p (buffer-name)
  "Return non-nil if BUFFER-NAME corresponds to a dialogue buffer."
  (string-match "\\`\\*<\\(.*\\)> Dired Sort Options\\*\\'" buffer-name))

(defsubst dired-sort-dialogue-buffer-name (&optional buffer)
  "Return name of dialogue buffer corresponding to dired buffer BUFFER.
If BUFFER is nil then current buffer is used."
  (concat "*<" (buffer-name buffer) "> Dired Sort Options*"))

(defconst dired-sort-dialogue-width 23
  "Dired sort dialogue width in characters.")

;;;###autoload
(defun dired-sort-dialogue ()
  "A static dialogue version of the Dired sort menu.
This command *must* be run in the Dired buffer!"
  (interactive)
  (or (eq major-mode 'dired-mode)
      (error "This command may only be run in a Dired buffer"))
  (let
      ;; Must set these variables while still in the dired buffer!
      ((radio (dired-sort-dialogue-choice))
       (reverse (dired-sort-menu-switch-p "r"))
       (recursive (dired-sort-menu-switch-p "R"))
       (dired-buffer (current-buffer))
       ;; Suspend automatic mechanisms:
       window-configuration-change-hook
       kill-buffer-hook)

    ;; Check whether a dialogue buffer for this dired buffer is
    ;; already visible, and if so re-use its window:
    (let ((bufname (dired-sort-dialogue-buffer-name))
	  (bufs (buffer-list)) buf
	  (title (concat "<" (buffer-name dired-buffer) ">")))
      (while (and bufs (not (string= bufname
				     (buffer-name (setq buf (car bufs))))))
	(setq bufs (cdr bufs)))
      (if bufs
	  (progn
	    (if (dired-sort-dialogue-own-frame-really)
		(progn
		  (select-frame (window-frame (get-buffer-window buf t)))
		  (raise-frame))
	      (select-window (get-buffer-window buf t)))
	    (set-window-dedicated-p (selected-window) nil)
	    (kill-buffer buf))
	(if (dired-sort-dialogue-own-frame-really)
	    ;; If room then put dialogue immediately to the right of
	    ;; the dired frame, else at right edge of screen.
	    (let* ((alist (frame-parameters))
		   (top (cdr (assq 'top alist))) ; pixels
		   (left (cdr (assq 'left alist))) ; pixels
		   )
	      ;; Allow form INTEGER or (+ INTEGER):
	      (or (atom left) (setq left (cadr left)))
	      ;; Set left of dialogue frame to avoid falling off right
	      ;; of display:
	      (setq left (+ left (frame-pixel-width)))
	      (setq left (if (> (+ left (* dired-sort-dialogue-width
					   (frame-char-width)))
				(x-display-pixel-width))
			     -10
			   ;; (+ left (* 2 (cdr (assq 'border-width alist))))))
			   (+ left 10)))
	      (select-frame (make-frame
			     `((title . ,title)
			       (top . ,top)
			       (left . ,left)
			       (width . ,dired-sort-dialogue-width)
			       (height . 22)
			       (minibuffer . nil)
			       (vertical-scroll-bars . nil)
			       (horizontal-scroll-bars . nil)
			       (unsplittable . nil)
			       (menu-bar-lines . 0)
			       ))))
	  (split-window			; WINDOW SIZE HORIZONTAL
	   nil (- (window-width) dired-sort-dialogue-width) t)
	  (select-window (next-window))))
      (switch-to-buffer bufname)
      (set-window-dedicated-p (selected-window) t) ; can crash Emacs!
      (kill-all-local-variables)
;       (or buffer-display-table
; 	  (setq buffer-display-table
; 		(or standard-display-table (make-display-table))))
;       (set-display-table-slot buffer-display-table 0 ?_)
      (setq truncate-lines t
	    mode-line-format title))

    (let ((inhibit-read-only t))
      (erase-buffer))
    ;; Must set this only once in the dialogue buffer!
    (setq dired-sort-dialogue-dired-buffer dired-buffer)

    (let ((start (point)))
      (widget-insert "Dired Sort Options")
      (put-text-property start (point) 'face 'bold))
    (widget-insert " for\n<"
		   (buffer-name dired-buffer)
		   ">\n\n(Use any mouse button)\n\n ")
    (setq dired-sort-dialogue-radio-widget
	  (eval `(widget-create
		  'radio-button-choice
		  :indent 1
		  :value radio
		  '(item :tag "Name" "")
		  '(item :tag "Time Modified" "t")
		  ,@(if (dired-sort-menu-active-p "S")
			'('(item :tag "Size" "S")))
		  ,@(if (dired-sort-menu-active-p "X")
			'('(item :tag "Extension" "X")))
		  ,@(if (dired-sort-menu-active-p "U")
			'('(item :tag "Unsorted" "U")))
		  ,@(if (dired-sort-menu-active-p "c")
			`('(item :tag
				 ,(if (or (not (eq system-type 'windows-nt))
					  (dired-sort-menu-remote-p))
				      "Time Changed"
				    "Time Created") "c")))
		  ,@(if (and (dired-sort-menu-active-p "u")
			     (or (not (eq system-type 'windows-nt))
				 (dired-sort-menu-remote-p)))
			'('(item :tag "Time Accessed" "u")))
		  )))
    (widget-insert " _____________________\n\n ")
    (when (dired-sort-menu-active-p "r")
      (setq dired-sort-dialogue-reverse-widget
	    (widget-create 'checkbox
			   :help-echo "Reverse the sort order"
			   reverse))
      (widget-insert " Reverse\n "))
    (when (dired-sort-menu-active-p "R")
      (setq dired-sort-dialogue-recursive-widget
	    (widget-create 'checkbox
			   :help-echo "Recursively list all subdirectories"
			   recursive))
      (widget-insert " Recursive\n "))
    (when (ls-lisp-var-p 'ls-lisp-ignore-case)
      (setq dired-sort-dialogue-ignore-case-widget
	    (widget-create 'checkbox
			   :help-echo "Ignore case when sorting"
			   ls-lisp-ignore-case))
      (widget-insert " Ignore Case\n "))
    (when (ls-lisp-var-p 'ls-lisp-dirs-first)
      (setq dired-sort-dialogue-dirs-first-widget
	    (widget-create 'checkbox
			   :help-echo "Sort directories first"
			   ls-lisp-dirs-first))
      (widget-insert " Dirs First\n "))
    (widget-insert "_____________________\n\n ")
    (widget-create 'push-button
		   :notify 'dired-sort-dialogue-OK
		   :help-echo "Apply the settings and close the window"
		   "OK")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify 'dired-sort-dialogue-close
		   :help-echo "Close the window and ignore the settings"
		   "Cancel")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify 'dired-sort-dialogue-apply
		   :help-echo "Apply the settings without closing the window"
		   "Apply")
    (widget-setup)
    (goto-char (point-min))
;      (use-local-map widget-keymap)
;      (let ((map (make-sparse-keymap)))
;        (suppress-keymap map)
;        (set-keymap-parent map widget-keymap)
;        (define-key map [down-mouse-1] 'widget-button-click)
;        (define-key map [down-mouse-3] 'widget-button-click)
;        (use-local-map map))
    (let ((map widget-keymap))
      ;; (define-key map [t] 'undefined)
      ;; (define-key map [tab] 'widget-forward)
      ;; (define-key map [return] 'widget-button-press)
      (define-key map [down-mouse-1] 'widget-button-click)
      (define-key map [down-mouse-3] 'widget-button-click)
      ;; (define-key map [escape] (lambda () (interactive)
      ;; 			 (dired-sort-dialogue-close)))
      ;; (define-key map "\C-h" 'describe-bindings)
      (use-local-map map)))
  ;; Set up these hooks here to avoid any possibility of causing
  ;; trouble if the dialogue facility is not used:
  (add-hook 'kill-buffer-hook
	    'dired-sort-dialogue-auto-kill-1)
  (add-hook 'window-configuration-change-hook
	    'dired-sort-dialogue-auto-kill-2))

(defun dired-sort-dialogue-apply (&rest ignore)
  "Apply the dired sort dialogue settings (without closing it)."
  (let ((radio-widget dired-sort-dialogue-radio-widget)
	(reverse-widget dired-sort-dialogue-reverse-widget)
	(recursive-widget dired-sort-dialogue-recursive-widget)
	(ignore-case-widget dired-sort-dialogue-ignore-case-widget)
	(dirs-first-widget dired-sort-dialogue-dirs-first-widget)
	window-configuration-change-hook)
    (with-current-buffer dired-sort-dialogue-dired-buffer
      (if dirs-first-widget
	  (setq ls-lisp-dirs-first
		(widget-value dirs-first-widget)))
      (if ignore-case-widget
	  (setq ls-lisp-ignore-case
		(widget-value ignore-case-widget)))
      (if recursive-widget
	  (let* (case-fold-search
		 (switches
		  (if (widget-value recursive-widget)
		      (if (not (string-match "R" dired-actual-switches))
			  (concat dired-actual-switches "R")
			dired-actual-switches)
		    (if (string-match "R" dired-actual-switches)
			(replace-match "" t t dired-actual-switches)
		      dired-actual-switches))))
	    (dired-sort-menu-R-check switches)
	    (setq dired-actual-switches switches)))
      (dired-sort-menu-set-switches
       (widget-value radio-widget)
       (and reverse-widget (widget-value reverse-widget))))))

(defun dired-sort-dialogue-close (&rest ignore)
  "Close the dired sort dialogue (ignoring the settings)."
  (let ((dired-buffer dired-sort-dialogue-dired-buffer)
	window-configuration-change-hook
	kill-buffer-hook)
    (set-window-dedicated-p (selected-window) nil)
    (kill-buffer (current-buffer))
    (if (dired-sort-dialogue-own-frame-really)
	(delete-frame)
      (or (one-window-p t) (delete-window)))
    (select-window (get-buffer-window dired-buffer))))

(defun dired-sort-dialogue-OK (&rest ignore)
  "Apply the dired sort dialogue settings and close it."
  (dired-sort-dialogue-apply)
  (dired-sort-dialogue-close))

 
;; Code to keep dialogue buffers tidy.

(defadvice show-paren-function (around show-paren-function-advice activate)
  "Do not show matching parens in a dired sort dialogue buffer."
  (or (dired-sort-dialogue-buffer-p (buffer-name)) ad-do-it))

(defadvice dired-revert (after dired-revert-advice activate)
  "Redisplay and update any related dired sort dialogue buffer."
  (if (and (eq major-mode 'dired-mode)
	   (get-buffer (dired-sort-dialogue-buffer-name)))
      (if (dired-sort-dialogue-own-frame-really)
	  (let ((frame (selected-frame)))
	    (dired-sort-dialogue)
	    (raise-frame frame))
	(save-selected-window
	  (dired-sort-dialogue)))))

(defun dired-sort-dialogue-auto-kill-1 ()
  "If dialogue buffer is explicitly killed delete its window/frame.
Also, kill any dialogue buffer related to a killed dired mode buffer.
This function is hung on `kill-buffer-hook'."
  (if (dired-sort-dialogue-buffer-p (buffer-name))
      (if (dired-sort-dialogue-own-frame-really)
	  (delete-frame)
	(delete-window))
    (let (buf kill-buffer-hook window-configuration-change-hook)
      (when (and (eq major-mode 'dired-mode)
		 (setq buf
		       (get-buffer
			(dired-sort-dialogue-buffer-name))))
	(or (one-window-p t) (delete-windows-on buf))
	(kill-buffer buf)))))

(defun dired-sort-dialogue-auto-kill-2 ()
  "Kill any dialogue buffer that has ceased to be visible.
But do nothing if it was explicitly killed.  Also, kill any dialogue
buffer whose dired buffer has ceased to be visible.
This function is hung on `window-configuration-change-hook'."
  (or (eq this-command 'kill-buffer)	; both are needed!
      (eq last-command 'kill-buffer)	; both are needed!
      (let ((bufs (buffer-list)) buf bufname
	    kill-buffer-hook window-configuration-change-hook)
	(while bufs
	  (setq buf (car bufs)
		bufs (cdr bufs)
		bufname (buffer-name buf))
	  (if (dired-sort-dialogue-buffer-p bufname)
	      (cond ((null (get-buffer-window buf 'visible))
		     ;; dialogue hidden...
		     (kill-buffer buf))
		    ((null (get-buffer-window
			    (get-buffer (match-string 1 bufname)) 'visible))
		     ;; dired hidden...
		     (or (one-window-p t)
			 (eq this-command 'mouse-delete-other-windows)
			 (eq this-command 'delete-other-windows)
			 (delete-windows-on buf))
		     (kill-buffer buf))))))))

(defadvice handle-delete-frame
  (before handle-delete-frame-advice activate)
  "Kill dialogue buffer before killing its frame."
  (let* ((frame (posn-window (event-start event)))
	 (buf (car (buffer-list frame))))
    (when (dired-sort-dialogue-buffer-p (buffer-name buf))
      (set-window-dedicated-p (selected-window) nil)
      (kill-buffer buf))))

(provide 'dired-sort-menu)

;;; dired-sort-menu.el ends here
