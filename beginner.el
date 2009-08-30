;;; beginner.el --- simplify Emacs

;; Copyright (C) 2007 Jason Spiro

;; Version: 0.1+wikiedits
;; %Id: 2% (This is the number of times this file has been edited on EmacsWiki.)
;; Keywords: help, wp, emulations
;; Human-Keywords: easy, simple, notepad, simplify, novice, beginner
;; Author: Jason Spiro <jasonspiro3@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/emacs/beginner.el
;; Compatibility: GNU Emacs 22.x, and maybe others

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; The purpose of beginner-mode is to make Emacs less imposing, so people will
;; try it instead of gedit, kate, et cetera.  One way to do it is to
;; make it look nicer, but the way I have chosen is to reduce the
;; amount of UI onscreen and make some other changes.

;; Beginner-mode makes Emacs look simpler and
;; cleaner.  It also adds functionality to Emacs that many users
;; appreciate having in a text editor.  (It is added by enabling minor
;; modes.  If you don't want them, just comment out the appropriate
;; lines of this code.)

;; This is only alpha-quality code, and I'm not confident it's useful
;; yet.  If you are a beginner to Emacs and use Windows, instead try
;; the EmacsW32 version of Emacs.

;; I encourage you to edit this file and to submit your changes back
;; to the webpage you got it from.  Or send me a patch.

;; Also, I love feedback, and I would be grateful to hear what you
;; think of beginner-mode.  Even if you hate it, I would like to hear from you.

;;; Installation:

;; Save this file to somewhere on your load-path then add (require
;; 'beginner) to your .emacs file.

;;; Bugs:

;; 1.  When you try to load beginner-mode then color-theme.el, both inside
;; .emacs, you get the Lisp error: (wrong-type-argument keymapp nil).
;; Reason: color-theme calls easy-menu-add-item to add its options to
;; the Tools menu.  But there is no menu.  Workaround: load
;; color-theme before beginner-mode.  Solution idea: Maybe I can leave a
;; phantom Tools menu (and maybe other menus) which exist but which
;; are invisible.  I wonder if that's practical to do.  Notes: Oddly,
;; if you load beginner-mode then manually do M-: (require 'color-theme), it
;; loads fine then creates a "tools" menu with no problem.

;; If you find more bugs, please describe them here or contact the
;; author.

;; beginner-mode was developed and tested on emacs-snapshot-gtk
;; 20060915-1 from Ubuntu Linux 6.10.  As is, I doubt it will work on
;; XEmacs.  If you test it on any other Emacs or XEmacs, I would
;; appreciate it if you could please contact me or edit this file and
;; mention your results.

 

;;; TODOs:

;; Main TODOs for before release:
;; - move code to ~/.emacsen.d
;; NOTE: Release early.  Try to release by mid-March, even if TODOs aren't done.
;; NOTE 2: It is mid-March already.

;; General TODOs:
;; - leave a startup message in scratch that warns users ";; Warning:
;;    This is the scratch area.  Everything here will disappear when
;;    you quit Emacs.  You will not be able to save it."
;; - install and test with Emacs 21
;; - maybe: show tabs; maybe modify tabbar.el to work more obviously
;; - or maybe: open 2nd file -> show in new frame? (ugh)
;; - or maybe: when you open a file, maybe it should first prompt to save the previous
;;    one, hopefully with a nice dialog box, then close it if saved
;;     - Do I really want this?
;; - what does gedit do about the above?  I bet it does tabs.
;; - fix map-ynp.el so it won't ask so hard questions when it shows dialogs
;;    (or, if difficult, ask upstream if they could please do it, then maybe 
;;    backport fix for Emacs 21 beginner-mode users)
;;      - IMO it should just ask Save, Discard, or Cancel.
;; - make this a minor mode?  (make it an easy-mmode or a set of 2 or 3 easy-mmodes)
;; - show modified-or-not (with a **) and line number
;;    in frame title bar
;; - add new bindings for File and Edit menu commands; new bindings
;;    should show up on menu
;; - nice mode-line?  maybe ask mailing list how, and CC color-theme mode maintainers
;; - don't let the user save any files called emacs_untitled.txt; prompt for a new different name.
;;     - if they want to save by keyboard, prompt in minibuffer; if by menu, prompt in a dialog
;; - (maybe) should try not to allow multiple file-editing buffers at once:
;;    when you try to open a 2nd, it should prompt to save the first
;; - simplify save-or-not prompt at quit time when there's just one file open
;; - Edit > Find, Edit > Replace String, Edit > Goto Line, and Help > About should be dialogs
;;     - does Emacs have a facility for defining dialogs that take a line of input?
;;         -> no, I checked both gtkutil.c and xfns.c.  Request one.
;;     - Should I maybe use Perl easygui tools from system(1) for now?
;;         - fine for rapid prototyping
;;         - bad for releasing :-)
;; - should echo area be hidden except when there's a message to show?
;;     - nah, I bet it would be jarring to look at

;;; Stream-of-consciousness thoughts on unresolved issues:

;; - Multiple windows.  Emacs is an application which lets you have
;; many windows open at once.  I'm not sure, but I suspect this can be
;; confusing to newbies.  (But really I'd have to do some usability
;; testing of Emacs on newbies to see.  I wonder if anyone's ever done
;; that before.)  So maybe I want to discourage multiple windows from
;; opening up in Emacs (though I worry that maybe the reason I want to
;; is that I hate Emacs's system of windows always popping up and
;; splitting my frame in two, and if so, this should really be a
;; separate mode, not part of beginner-mode.)  Maybe a good way to do this
;; would be to prevent functions that usually split the screen
;; (e.g. all the C-h <something> commands, compile, etc.) from
;; splitting it, but still allowing the manual split commands to split
;; normally.  Or maybe I just need a Window menu with commands to
;; close windows, and to add good keyboard shortcuts to Alessandro
;; Piras' vimpulse-mode once I get that code released to the web.

;; - The modeline.  My options:

;; (Don't think about this until after I get
;; quit->save/discard dialog simplified.)

;; 0. do nothing (I'm doing this for now)
;; 1. hide the modeline and add a splitter bar (sounds difficult)
;; 2. hide the modeline when there is only 1 window onscreen, show it
;; otherwise
;; 3. disable popups  (Easy?  Not sure.  Useful?  I suspect it'd be
;; appreciated by newbies.)
;; 4. make the modeline appear gray like a status bar (maybe best, as
;; the modeline provides important information, just like the status
;; bar in other apps)
;; 5. combine the modeline and echo area into one line
;; Optional: In addition to 0/1/2/3/4/5, also give the user a Hide
;; Modeline option on the menu (I like this idea)

;; - The menus.  Have I way oversimplified them?

;;; Change Log:

;; Version 0.1: initial release.

;;; Code:

;; Begin beginner {{{

(setq cursor-type '(bar . 2))
(find-file "~/emacs_untitled.txt")

;; Useful modes for any text editor.
(cua-mode t) ;; Useful for Windows and Gnome/KDE users.  Note: Also enables transient-mark-mode.
(setq-default save-place t) ;; IIRC Vim in Debian does place-saving by default
(require 'saveplace) ;; does this ship with Emacs 21?
(auto-compression-mode t) ;; Vim does this by default
(global-font-lock-mode t)
;; Not using recentf-mode b/c it works too differently than in most
;; Gnome apps.  If they have it append to bottom of File menu with &1
;; &2 &3 etc. shortcut keys like AbiWord does, will reconsider.
;(recentf-mode t)
;; Not enabling longlines-mode.  It seems to interact badly with font
;; lock (and with M-x eval-buffer.)  Maybe ok to leave as an option,
;; but bad to leave on by default.  Anyway, even Windows Notepad
;; doesn't have Word Wrap on by default.
;(longlines-mode t)

;; Simplify display

;; this is the same title bar format all Gnome apps seem to use
(setq frame-title-format "%b")

;; Sadly, (setq-default mode-line-format '(:eval (if (string-equal
;; (buffer-name) ".emacs") "yes" nil))) always shows a mode line, even
;; on buffers where the if-statement evaluates to nil.  But I don't
;; want to hide the mode line unless either:

;; 1. I manage to somehow prevent automatic splitting (I'd have to see
;; if it's possible to do without causing bugginess), or:

;; 2. I set a hook or an advice whenever the window is split or
;; unsplit.  If there's only one editing window, hide all modelines.
;; If there are mode, show modelines on all windows.

(set-fringe-mode 0)
(setq default-frame-alist
      (append default-frame-alist 
;	      '((minibuffer . nil))    ;; no b/c causes ugly minibuffer window to appear
	      '((vertical-scroll-bars . right))))
;; Hide toolbar for now.  It has kill-buffer and printing buttons I don't want to show.
(tool-bar-mode -1)
;; don't show tiny scroll bar in echo area
(set-window-scroll-bars (minibuffer-window) nil)
;; Don't show annoying help text in File > Open that normally shows up
;; for all users of GTK+ less than 2.10.  Note: This setting requires
;; an Oct. 2007 or later CVS Emacs to work.
(setq x-gtk-file-dialog-help-text nil)
;; TODO: also apply this to the minibuffer window; IIRC its name is " *Minibuf-1*"
;; TODO: Delete scratch buffer:
;;  (if (get-buffer "*scratch*")...

;; UI stuff

(setq inhibit-startup-message t)
(setq scroll-conservatively 10) ;; enable inefficient Windows Notepad-style scrolling

;; IMO a distracting message that closed-source IDEs don't show, so hide from new users
(defun startup-echo-area-message ()
  (message ""))

 

;; 1 new function to be called from a menu item

;; unneeded now that I'm not pretending to be a standalone text editor called Epad
;(defun beginner-mode-about ()
;  (interactive)
;  (message (concat "This is beginner-mode, the simple text editor based on Emacs" emacs-version ".  Emacs is Free software; press C-h C-h for more information.")))

(defun beginner-mode-about ()
  (interactive)
  (message (concat "This is " (version) ".  It is Free software and comes with NO WARRANTY; press the F1 key twice for more information.")))

;; 1 new menu item

;(defun beginner-mode-manual ()
;  (interactive)
;  (message "beginner-mode has no manual.  However, since it is Emacs-based, the Emacs manual applies."))

;; unneeded now that I'm not pretending to be a standalone text editor called Epad
;(define-key menu-bar-help-menu [beginner-mode-manual]
;  '(menu-item "beginner-mode has no manual" beginner-mode-manual
;	      :help "Display beginner-mode instruction manual"))

(define-key menu-bar-help-menu [about]
  '(menu-item "About Emacs" beginner-mode-about
	      :help "Display version number"))

;; 4 standard Emacs menu items with new names

(define-key menu-bar-i-search-menu [isearch-forward]
  '(menu-item "Find" isearch-forward
	      :help "Search forward for a string as you type it"))

(define-key menu-bar-file-menu [new-file]
  '(menu-item "New..." find-file
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Specify a new file's name, to edit the file"))

(define-key menu-bar-file-menu [open-file]
  '(menu-item "Open..." menu-find-file-existing
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Read an existing file into an Emacs buffer"))

(define-key menu-bar-file-menu [exit-emacs]
  '(menu-item "Quit" save-buffers-kill-emacs
	      :help "Save unsaved buffers, then exit"))

;; beginner-mode's new, slim menu bar.  (It's far simpler than that of Emacs.)

(define-key global-map [menu-bar] (list 'keymap
					menu-bar-file-menu
					menu-bar-edit-menu
					menu-bar-help-menu))

(setq menu-bar-help-menu (list 'keymap 
;			       (assoc 'beginner-mode-manual menu-bar-help-menu)
			       (assoc 'emacs-manual menu-bar-help-menu)
			       (assoc 'about menu-bar-help-menu)))

(define-key global-map [menu-bar help-menu] (cons "Help" menu-bar-help-menu))

(setq menu-bar-edit-menu (list 'keymap 
			       (assoc 'undo menu-bar-edit-menu)
			       (assoc 'cut menu-bar-edit-menu)
			       (assoc 'copy menu-bar-edit-menu)
			       (assoc 'paste menu-bar-edit-menu)
			       (assoc 'clear menu-bar-edit-menu)
			       (assoc 'mark-whole-buffer menu-bar-edit-menu)
			       (assoc 'isearch-forward menu-bar-i-search-menu)
;			       (query-replace 'menu-item "Replace String..." 'query-replace (nil) :enable (not buffer-read-only) :help "Replace all occurrences of one string of characters with a different string")
;			       (assoc 'go-to-line menu-bar-edit-menu)
			       ))

(define-key global-map [menu-bar edit] (cons "Edit" menu-bar-edit-menu))

(setq menu-bar-file-menu (list 'keymap 
			       (assoc 'new-file menu-bar-file-menu)
			       (assoc 'open-file menu-bar-file-menu)
			       (assoc 'save-buffer menu-bar-file-menu)
;			       (assoc 'write-file menu-bar-file-menu)
;			       (assoc 'print-buffer menu-bar-file-menu)
			       (assoc 'exit-emacs menu-bar-file-menu)))

(define-key global-map [menu-bar file] (cons "File" menu-bar-file-menu))

;; End of menu code

;(define-minor-mode xterm-mouse-mode
;  "Toggle XTerm mouse mode.
;With prefix arg, turn XTerm mouse mode on iff arg is positive.
;
;Turn it on to use Emacs mouse commands, and off to use xterm mouse commands.
;This works in terminal emulators compatible with xterm.  It only
;works for simple uses of the mouse.  Basically, only non-modified
;single clicks are supported.  When turned on, the normal xterm
;mouse functionality for such clicks is still available by holding
;down the SHIFT key while pressing the mouse button."
;  :global t :group 'mouse
;  (if xterm-mouse-mode
;      ;; Turn it on
;      (unless window-system
;	(setq mouse-position-function #'xterm-mouse-position-function)
;	(turn-on-xterm-mouse-tracking))
;    ;; Turn it off
;    (turn-off-xterm-mouse-tracking 'force)
;    (setq mouse-position-function nil)))

(provide 'beginner)

;; }}} End beginner
