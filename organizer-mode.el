;;; organizer-mode.el --- major mode for editing organizer list files

;;; Commentary:

;; organizer-mode.el is free software

;; The following code is meant to provide functions to allow use as an
;; organizer.

;;  This code borrows heavily and shamelessly from todo-mode.el.  Many
;;  of the functions are directly from that code with only minor
;;  alteration.  The following is information regarding that package.

;; =======================
;; todo-mode.el --- major mode for editing todo list files
;; Copyright (C) 1997, 1999, 2001 Free Software Foundation, Inc.

;; Author: Oliver Seidel <os10000@seidel-space.de>
;;   [Not clear the above works, July 2000]
;; Created: 2 Aug 1997
;; Version: Id: todo-mode.el,v 1.47.4.1 2001/11/13 04:12:45 rms Exp
;; Keywords: calendar, todo
;; =========================

;; Quick Installation:
;; The easiest install method is to simply put the
;; organizer-mode.el file into you home directory, create a directory
;; to put organizer files in and put the following into your .emacs
;; file:

;; (load-file "~/organizer-mode.el")

;; Quick Start/Tutorial

;; To get an idea of what this mode is designed to do and how to set
;; things up, do the following steps:

;; 1) M-x organizer-show and type in the name of the new organizer
;; file, "~/Organizer/software_development" without the quotes.  Type
;; in "Notes" at the prompt for a new category.

;; 2) Type "I" to insert an organizer entry and type "Need commentary
;; for organizer.el /* organizer_program */" at the prompt.  The word
;; inside the C-quotes (/* */) is the name of the organizer we are
;; going to create in 5, below.

;; 3)  Type Q to quit and save this organizer file.

;; 4) M-x orgainzer-show and type in the name of the new organizer
;; file, "~/Organizer/organizer_program" without the quotes.  Type in
;; "Files" at the prompt for a new category.

;; 5) Type "I" to insert an organizer entry and type "~/organizer-mode.el".

;; 6) Go to the "Organizer" menu and choose "Diary" from the "Create
;; linked entry to this page" submenu.

;; 7) Insert the diary entry "9:00am See John /* organizer_program */" into
;; the diary for the current date and "3:00pm See Jim /* organizer_program */"
;; for tomorrow.  Save and kill the buffer.

;; 8) Type Q to quit and save this organizer file.

;; 9) Go to the "Organizer" menu and choose "Todo" from the "Create
;; linked entry to this page" submenu. Insert the entry "Work on
;; organizer-mode /* organizer_program */" into your todo list.

;; 10) If you use bbdb to store your contacts, create an entry for
;; "Thomas Shannon" with my email "tshannon@rush.edu" and
;; "/* software_development */ /* organizer_program */" somewhere in the
;; notes field.  Save the database with M-x bbdb-save-db

;; 12) Start the calendar and press "O" to execute the
;; 'organizer-view-other-diary-files' function.  At the type in
;; "~/Organizer/software_development" at the prompt.  You should see
;; something like this:

;; Wednesday, December 4, 2002
;; ===========================
;; --- Notes
;; Need commentary for organizer.el /* organizer_program */
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; -- BBDB
;; Thomas Shannon
;;             net: tshannon@rush.net
;;           notes: /* software_development */ /* organizer_program */
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; The date will obviously be your current date and if you don't use
;; bbdb, my entry above won't be there.

;; Note that my bbdb entry, with the comment "/* software_development
;; */" in the notes field, shows up in the "software_development"
;; page.  The entry is, therefore, "linked" to this page and the
;; information is available for when you decide to work on software
;; development.  You can also edit the bbdb entry by putting the
;; cursor on my name (Shannon), going to the "organizer" menu and
;; choosing "BBDB search string" from the "Commands on string at
;; cursor" submenu.

;; 13) Put the cursor on the word "organizer_program" anywhere on the
;; page.  Go to the "Organizer" menu and choose "Go to another
;; organizer" from the "Commands on string at cursor" submenu.  You
;; should now see something like this:

;; Wednesday, December 4, 2002
;; ===========================
;; --- Files
;; ~/organizer-mode.el
;; ---------------------------------------------------------------------------
;; -- [software_development]
;; Need commentary for organizer.el /* organizer_program */
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; -- BBDB
;; Thomas Shannon
;;             net: tshannon@rush.net
;;           notes: /* software_development */ /* organizer_program */  /* junk */
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; 9:00am See John /* organizer_program */

;; Thursday, December 5, 2002
;; ==========================
;; 3:00pm See Jim /* organizer_program */

;; Note that all of the entries in the diary, the todo file, the bbdb
;; database and the software_development organizer file with the
;; comment "/* organizer_program */" also show up in the
;; "organizer_program" organizer page.  These entries are, therefore,
;; linked to this page and the information is available for when you
;; decide to do things related to the organizer program.

;; 14) Put your cursor on "~/organizer-mode.el" and choose "Dired
;; directory or file" from the "Commands on string at cursor" submenu.
;; Hit enter to accept the file name.  You should see this file.

;; Detailed Comments:

;; These comments are out of date.  Please see the info file for organzier-mode:

;; http://home.attbi.com/~shannon1679/organizer-mode.info

;; I decided to put this mode together because I was intensely
;; dissatisified with Personal Information Managers for Linux as the
;; programs now stand.  It's my opinion that you should be able to use
;; you computer like you would use an organizer that you carry around.
;; In other words, if you want to work on a project, you look at the
;; proper page in your organizer, which provides all of the
;; information needed to start working.  Its my opinion that a proper
;; computer organizer should allow you to start with the proper "page"
;; and do anything from there.

;; I thought emacs was the best place to start if you're going to do
;; this because its possible to do nearly everything with it to begin
;; with.  It has a pretty good calendar and todo-mode which are
;; essential.  It also allows you to read news (gnus) and mail (vm)
;; and has a pretty good contact manager/addressbook (bbdb).

;; To create a new organizer file, simply use the "organizer-show"
;; command and type in the name of the new file when prompted.  The
;; default directory is "~/Organizer" and this directory will be
;; created for you.  Insert entries using the same commands as those
;; for the todo-mode but the commands use "organizer" instead of
;; "todo" as the first word.  For instance, use
;; "organizer-insert-item-here" to insert new items into the file in
;; the same way tht you use "todo-insert-item-here" to insert new
;; items into a todo file.

;; The organizer files in this package are really just todo files that
;; are organized in one or more directories.  Usually the
;; organizer-directory is the root of this tree.  Files can be
;; displayed with the command 'organizer-show'.  Pages associated with
;; the files are displayed with the
;; 'organizer-view-other-diary-entries' command.  'organizer-show' is
;; also used to create new files and new entries in existing files
;; ('organizer-insert-item' or 'organizer-insert-item-here').

;; As stated above, an entry into an organizer file is nothing more
;; than a todo entry.  Indeed, all of the todo-mode functions
;; (with "organizer" substituted for "todo") are there and the
;; shortcut keys are the same.  For instance, I might have the
;; following entries in my "development" organizer file:

;;  Brian Barry
;;  ~/mail/organizer_mail
;;  comp.emacs
;;  ~/code/organizer-mode.el
;;  ~/code
;;  My_programs
;;  http://www.emacswiki.org/cgi-bin/wiki.pl?SiteMap
;;  ~/junk.jpg

;; These entries allow you to do the following:

;;  Access Brian Barry in bbdb with organizer-bbdb-at-point.
;;  Access organizer_mail with organizer-vm-at-point.
;;  Access comp.emacs with organizer-gnus-at-point.
;;  Call up organizer-mode.el or the directory with code
;;  with find-file-at-point.
;;  Access the related organizer page, My_programs, with
;;  organizer-at-point.
;;  Access the URL with w3m-browse-url, browse-url-netscape or some other variation.
;;  Access junk.jpg with its associated dired-guess-shell-alist-user
;;  command (xv in my case) using organizer-shell-command-at-point

;; You can, therefore, do nearly anything associated with the project
;; simply by going to the organizer page and using the proper command.
;; Your work, therefore, becomes task based.  These commands can (and
;; probably should) be bound to shortcut keys.  They are all also
;; available from the "Organizer" menu.  At some point I'll get around
;; to customizing them so that you can input your own commands and use
;; your own mail/news/addressbooks.  For now, the package will be best
;; for emacs people like me who practically live with the program
;; running and do nearly everything with it.

;; In addition, there are a few additional features which are
;; implemented with the 'organizer-view-other-diary-entries'.  This
;; command generates an "organizer page" from an organizer file.  It
;; is probably the best one to use when you don't actually need to
;; create a new file or add entries to an existing one.  You start by
;; calling the calendar and putting the cursor on the relevant date
;; (usually the current one).  When you call the function, the
;; computer first displays the contents of your organizer file.  It then
;; searches through your diary, the todo-file and your bbdb-file for
;; entries which contain the name of the organizer file with C-type
;; delimeters (/* */) and also displays them.  For instance, my
;; "development" organizer page will not only display the information
;; above (which is in ~/my_organizer_directory/development"), but will
;; also display the todo entry "finish organizer-mode.el, /*
;; development */" and the appointment "June 14, 2002 9:00am send
;; Brain Jones the file, /* development */" entry from my diary file
;; on June 14.  It will also display Brian Jones' BBDB entry if /*
;; development */ is somewhere in the entry (usually the notes field).

;; In addition, by putting the proper information in the right places,
;; all of you work becomes linked.  For instance, in the notes field
;; of Brain's BBDB entry, I might put "/* development */" as above.
;; Now with organizer-at-point, I can call up the proper page from his
;; bbdb entry.  Or I could put "~/mail/Brian" which will allow me to
;; access the related email box with "vm-at-point".  The same goes
;; with todo entries and calendar entries.  If used correctly,
;; everything can be associated to make access to information from
;; anywhere go smoothly.

;;  Have fun.

;; Shortcut keys:
;; O    calendar-mode   organizer-view-other-diary-entries

;; All shortcut keys once you are in organizer-mode are the same as
;; those in todo-mode.

;; Todo:
;; 1) Speedbar mode
;; 2) info file

;; Known bugs

;; Every once in a while in 'organizer-mode' the error, "Can't find
;; category --- $" or just "Arithmetic error" appears.  The work
;; around is to simply execute the 'organizer-forward-category'
;; command (the "+" key) and this will fix it.

;;; History:

;; Author: Thomas R. Shannon <tshannon@rush.edu>
;; Homepage: http://home.attbi.com/~shannon1679/organizer-mode.html
;; Created: 12 Jun, 2002
;; CVS Version $Id: organizer-mode.el,v 1.21 2003/03/18 18:04:47 tshanno Exp $
;; Release version: 1.9
;; Keywords: calendar, todo, address book, mail, news, file manager, organizer

;; Corrections: Xavier Maillard <xma@gnu.org>
;; Last modifications: 29 Mar, 2009
;;
;; -----------------------------------------------------------------------------

;;; Code:

;; Identify whether we are running xemacs or not
(defvar organizer-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Is the current version xemacs?"
  )


;; Needed packages
(require 'calendar)
(require 'diary-lib)
(require 'dired)
(require 'ffap)
(if (fboundp 'todo-show)
    (require 'todo-mode))
(if (fboundp 'vm)
    (require 'vm))
(if (fboundp 'bbdb)
    (require 'bbdb))
(if (fboundp 'gnus)
    (require 'gnus))
(if organizer-running-xemacs
    (require 'dired-shell))

;; For xemacs compatibility
(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))
(unless (fboundp 'line-end-position)
  (defalias 'line-end-position 'point-at-eol))

(unless (fboundp 'dired-do-toggle)
  (progn
    (defun organizer-xemacs-dired-toggle-marks ()
      "Toggle marks in dired mode"
      (dired-change-marks ?* nil))
    (defalias 'dired-do-toggle 'organizer-xemacs-dired-toggle-marks)))


;; There is an already alternate fn. in fsf-emacs which does this.  In
;; addition, the fn below is actually defined in "bbdb-snarf" so this
;; is only relevant if running xemacs but not bbdb.
(if organizer-running-xemacs
    (defun organizer-delete-and-return-region (begin end)
      (prog1
	  (buffer-substring begin end)
	(delete-region begin end))))


;; User-configurable variables:

(defcustom organizer-directory "~/Organizer/"
  "Directory where organizer pages can be found.  Must have the trailing forward slash."
  :type 'string
  :group 'organizer)
(defgroup organizer nil
  "Maintain a list of organizer items."
  :link '(emacs-commentary-link "organizer-mode")
  :version "21.2"
  :group 'calendar)

(defcustom organizer-mail-folder-directory "~/mail/"
  "Directory where mailboxes can be found.  Must have the trailing forward slash."
  :type 'string
  :group 'organizer)

(defcustom organizer-prefix     "&%%(equal (calendar-current-date) date)"
  "*ORGANIZER mode prefix for entries.  This is useful in conjunction with
`calendar' and `diary' to include your organizer list file as part of
your organizer page.  With the value \"*/*\" the organizer page
displays each entry every day and it may also be marked on every day
of the calendar.  Using \"&%%(equal
(calendar-current-date) date)\" instead will only show and mark
organizer entries for today, but may slow down processing of the organizer
file somewhat."

  :type 'string
  :group 'organizer)
(defvar organizer-prefix-prefix     "&%%"
  "*ORAGANIZER mode pre-prefix for entries.  All entries must start with this.")
(defcustom organizer-file    "~/.organizer-do"
  "* Initial ORGANIZER mode list file."
  :type 'file
  :group 'organizer)
(defcustom organizer-diary-file-temp    "/tmp/organizer-diary-delete-me"
  "*Temporary variable used by ORGANIZER mode when generating organizer pages.  The program should clean up after itself but you can set it to whatever you want just in case.  Make sure you have permission to write the file."
  :type 'file
  :group 'organizer)
(defcustom organizer-file-done  "~/.organizer-done"
  ;;  "*ORGANIZER mode archive file.  This varable is currently defunct
  ;; though I may bring it back if there is a desire to archive old
  ;; entries."
  "*Ignore this.  I may need it soem day but not now."
  :type 'file
  :group 'organizer)
(defcustom organizer-external-mailbox-command  "xterm -e pine -i -f"
  "External mailbox command."
  :type 'string
  :group 'organizer)
(defcustom organizer-external-mail-command  "xterm -e pine"
  "External mail command to send email."
  :type 'string
  :group 'organizer)
;; (defcustom organizer-external-news-command  "xterm -e pine"
;;   "External mail command."
;;   :type 'string
;;   :group 'organizer)
(defcustom organizer-mode-hook  nil
  "*ORGANIZER mode hooks."
  :type 'hook
  :group 'organizer)
(defcustom organizer-minor-mode-hook  nil
  "*ORGANIZER mode hooks."
  :type 'hook
  :group 'organizer)
(defcustom organizer-page-mode-hook  '(organizer-speedbar)
  "*ORGANIZER mode hooks."
  :type 'hook
  :group 'organizer)
(defcustom organizer-edit-mode-hook nil
  "*ORGANIZER Edit mode hooks."
  :type 'hook
  :group 'organizer)
(defcustom organizer-list-diary-entries-hook '(include-other-diary-files sort-diary-entries)
  "*ORGANIZER mode hooks for listing the entries on the page.  Don't remove the first two ('include-other-diary-files' and 'sort-diary-entries').  They're important."
  :type 'hook
  :group 'organizer)
(defcustom organizer-diary-display-hook 'fancy-diary-display
  "*ORGANIZER mode hooks for page display.  Don't remove the first hook (fancy-diary-display) unless you know what you're doing."
  :type 'hook
  :group 'organizer)
(defcustom organizer-insert-threshold 0
  "*ORGANIZER mode insertion accuracy.

If you have 8 items in your ORGANIZER list, then you may get asked 4
questions by the binary insertion algorithm.  However, you may not
really have a need for such accurate priorities amongst your ORGANIZER
items.  If you now think about the binary insertion halfing the size
of the window each time, then the threshhold is the window size at
which it will stop.  If you set the threshhold to zero, the upper and
lower bound will coincide at the end of the loop and you will insert
your item just before that point.  If you set the threshhold to,
e.g. 8, it will stop as soon as the window size drops below that
amount and will insert the item in the approximate centre of that
window."
  :type 'integer
  :group 'organizer)
(defvar organizer-edit-buffer " *ORGANIZER Edit*"
  "ORGANIZER Edit buffer name.")
(defcustom organizer-file-top "~/.organizer-top"
  "*ORGANIZER mode top priorities file.

Not in ORGANIZER format, but diary compatible.
Automatically generated when `organizer-save-top-priorities' is non-nil."
  :type 'string
  :group 'organizer)

(defcustom organizer-print-function 'ps-print-buffer-with-faces
  "*Function to print the current buffer."
  :type 'symbol
  :group 'organizer)
(defcustom organizer-show-priorities 1
  "*Default number of priorities to show by \\[organizer-top-priorities].
0 means show all entries."
  :type 'integer
  :group 'organizer)
(defcustom organizer-print-priorities 0
  "*Default number of priorities to print by \\[organizer-print].
0 means print all entries."
  :type 'integer
  :group 'organizer)
(defcustom organizer-remove-separator t
  "*Non-nil to remove category separators in\
\\[organizer-top-priorities] and \\[organizer-print]."
  :type 'boolean
  :group 'organizer)
(defcustom organizer-save-top-priorities-too nil
  "*Non-nil makes `organizer-save' automatically save top-priorities in `organizer-file-top'."
  :type 'boolean
  :group 'organizer)

;; Thanks for the ISO time stamp format go to Karl Eichwalder <ke@suse.de>
;; My format string for the appt.el package is "%3b %2d, %y, %02I:%02M%p".
;;
(defcustom organizer-time-string-format
  "%:y/%02m/%02d %02H:%02M"
  "*ORGANIZER mode time string format for done entries.
For details see the variable `time-stamp-format'."
  :type 'string
  :group 'organizer)

(defcustom organizer-entry-prefix-function 'organizer-entry-timestamp-initials
  "*Function producing text to insert at start of organizer entry."
  :type 'symbol
  :group 'organizer)
(defcustom organizer-entry-timestamp-initals-option nil
  "*Set to t if you want the time stamp to appear in the minibuffer when inserting new entries.  The default is nil."
  :type 'boolean
  :group 'organizer)
(defcustom organizer-initials (or (getenv "INITIALS") (user-login-name))
  "*Initials of organizer item author."
  :type 'string
  :group 'organizer)

(autoload 'time-stamp-string "time-stamp")

(defun organizer-entry-timestamp-initials ()
  "Prepend timestamp and your initials to the head of a ORGANIZER entry."
  (let ((time-stamp-format organizer-time-string-format))
    (if organizer-entry-timestamp-initals-option
	(concat (time-stamp-string) " " organizer-initials ": ")
      "")))

(defcustom organizer-display-todo-done-by-date t
  "*Set to t if you want entries in the .todo-done file to be displayed on the date of completion (according to the automatically added timestamp when filing the item)."
  :type 'boolean
  :group 'organizer)

;; -----------------------------------------------------------------------------

;; Set up some helpful context ...

(defcustom organizer-startup-list '()
  "List of organizer pages to open upon (X)Emacs startup."
  :type 'symbol
  :group 'organizer)
(defvar organizer-categories nil
  "ORGANIZER categories.")

(defvar organizer-cats nil
  "Old variable for holding the ORGANIZER categories.
Use `organizer-categories' instead.")

(defvar organizer-previous-line 0
  "Previous line asked about.")

(defvar organizer-previous-answer 0
  "Previous answer got.")

(defcustom organizer-number-of-entries 30
  "Numer of days to collect diary entries to put into the organizer page."
  :type 'symbol
  :group 'organizer)

(add-hook 'calendar-load-hook (define-key calendar-mode-map "O" 'organizer-view-other-diary-entries))

(defvar organizer-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'organizer-forward-category)
    (define-key map "-" 'organizer-backward-category)
    (define-key map "\C-lb" 'organizer-insert-linked-bbdb-item)
    (define-key map "\C-ld" 'organizer-insert-linked-diary-entry)
    (define-key map "\C-lf" 'organizer-insert-linked-dired-entry)
    (define-key map "\C-lo" 'organizer-insert-linked-organizer-item)
    (define-key map "\C-lt" 'organizer-insert-linked-todo-item)
    (define-key map "\C-lk" 'organizer-kill-ring-save-link-mark)
    (define-key map "\C-le" 'organizer-eshell-command-on-region)
    (define-key map "d" 'organizer-file-item) ;done/delete
    (define-key map "e" 'organizer-edit-item)
    (define-key map "E" 'organizer-edit-multiline)
    (define-key map "f" 'organizer-file-item)
    (define-key map "g" 'organizer-generate-page-for-this-file)
    (define-key map "i" 'organizer-insert-item)
    (define-key map "I" 'organizer-insert-item-here)
    (define-key map "j" 'organizer-jump-to-category)
    (define-key map "k" 'organizer-delete-item)
    (define-key map "l" 'organizer-lower-item)
    (define-key map "m" 'organizer-move-entry-to-another-category)
    (define-key map "n" 'organizer-forward-item)
    (define-key map "p" 'organizer-backward-item)
    (define-key map "P" 'organizer-print)
    (define-key map "q" 'organizer-quit)
    (define-key map "r" 'organizer-raise-item)
    (define-key map "s" 'organizer-save)
    (define-key map "S" 'organizer-save-top-priorities)
    (define-key map "t" 'organizer-top-priorities)
    (define-key map "w" 'organizer-indent-item)
    map)
  "ORGANIZER mode keymap.")

(defvar organizer-minor-mode-map
  (let ((map (make-keymap)))
    ;;    (suppress-keymap map t)
    (define-key map "\C-lb" 'organizer-insert-linked-bbdb-item)
    (define-key map "\C-ld" 'organizer-insert-linked-diary-entry)
    (define-key map "\C-lf" 'organizer-insert-linked-dired-entry)
    (define-key map "\C-lo" 'organizer-insert-linked-organizer-item)
    (define-key map "\C-lt" 'organizer-insert-linked-todo-item)
    (define-key map "\C-lk" 'organizer-kill-ring-save-link-mark)
    (define-key map "\C-le" 'organizer-eshell-command-on-region)
    map)
  "ORGANIZER-MINOR mode keymap.")

(defvar organizer-page-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "\C-lb" 'organizer-insert-linked-bbdb-item)
    (define-key map "\C-ld" 'organizer-insert-linked-diary-entry)
    (define-key map "\C-lf" 'organizer-insert-linked-dired-entry)
    (define-key map "\C-lo" 'organizer-insert-linked-organizer-item)
    (define-key map "\C-lt" 'organizer-insert-linked-todo-item)
    (define-key map "\C-lk" 'organizer-kill-ring-save-link-mark)
    (define-key map "\C-le" 'organizer-eshell-command-on-region)
    (define-key map "d" 'diary)
    (define-key map "e" 'organizer-edit-this-page)
    (define-key map "n" 'organizer-next-page)
    (define-key map "p" 'organizer-previous-page)
    (define-key map "r" 'organizer-reload-this-page)
    (define-key map "q" 'bury-buffer)
    map)
  "ORGANIZER-PAGE mode keymap.")

(defvar organizer-category-number 0 "ORGANIZER category number.")

(defvar organizer-tmp-buffer-name " *organizer tmp*")

(defvar organizer-category-sep (make-string 75 ?-)
  "Category separator.")

(defvar organizer-category-beg " --- "
  "Category start separator to be prepended onto category name.")

(defvar organizer-category-end "--- End"
  "Separator after a category.")

(defvar organizer-header "-*- mode: organizer; "
  "Header of organizer files.")

(defcustom organizer-enable-speedbar nil
  "Start speedbar for easy listing and calling organizer page.  In very preliminary stages of development.  Probably shouldn't use this yet."
  :type 'symbol
  :group 'organizer)
;; -----------------------------------------------------------------------------

(defun organizer-category-select ()
  "Make ORGANIZER mode display the current category correctly."
  (let ((name (nth organizer-category-number organizer-categories)))
    (setq mode-line-buffer-identification
	  ;;          (concat "Category: " name))
          (concat "Category: " (format "%18s" name)))
    (widen)
    (goto-char (point-min))
    (if (not (search-forward-regexp
	      (concat "^"
		      (regexp-quote (concat organizer-prefix organizer-category-beg name))
		      "$") nil t))
	(todo-forward-category))
    (let ((begin (1+ (line-end-position))))
      (search-forward-regexp (concat "^" organizer-category-end))
      (narrow-to-region begin (line-beginning-position))
      (goto-char (point-min)))))
(defalias 'organizer-cat-slct 'organizer-category-select)

(defun organizer-forward-category ()
  "Go forward to ORGANIZER list of next category."
  (interactive)
  ;;   (setq organizer-categories nil)
  ;;   (setq organizer-cats nil)
  ;;   (setq organizer-category-number nil)
  ;; ;;   (find-file buffer-file-name)
  ;;   (setq organizer-category-number 0)
  (setq organizer-category-number
        (mod (1+ organizer-category-number) (length organizer-categories)))
  (organizer-category-select))
(defalias 'organizer-cmd-forw 'organizer-forward-category)

(defun organizer-backward-category ()
  "Go back to ORGANIZER list of previous category."
  (interactive)
  (setq organizer-category-number
        (mod (1- organizer-category-number) (length organizer-categories)))
  (organizer-category-select))
(defalias 'organizer-cmd-back 'organizer-backward-category)

(defun organizer-backward-item ()
  "Select previous entry of ORGANIZER list."
  (interactive)
  ;;  (search-backward-regexp (concat "^" (regexp-quote organizer-prefix)) nil t)
  (search-backward-regexp (concat "^" (regexp-quote organizer-prefix-prefix)) nil t)
  (message ""))
(defalias 'organizer-cmd-prev 'organizer-backward-item)

(defun organizer-forward-item (&optional count)
  "Select COUNT-th next entry of ORGANIZER list."
  (interactive "P")
  (if (listp count) (setq count (car count)))
  (end-of-line)
  ;;  (search-forward-regexp (concat "^" (regexp-quote organizer-prefix))
  ;;                         nil 'goto-end count)
  (search-forward-regexp (concat "^" (regexp-quote organizer-prefix-prefix))
                         nil 'goto-end count)
  (beginning-of-line)
  (message ""))
(defalias 'organizer-cmd-next 'organizer-forward-item)

(defun organizer-save ()
  "Save the ORGANIZER list."
  (interactive)
  (save-excursion
    (save-restriction
      (save-buffer)
      (if organizer-save-top-priorities-too (organizer-save-top-priorities))
      )))
(defalias 'organizer-cmd-save 'organizer-save)

(defun organizer-quit ()
  "Done with ORGANIZER list for now."
  (interactive)
  (widen)
  (organizer-save)
  (message "")
  (bury-buffer))
;;  (kill-this-buffer))
(defalias 'organizer-cmd-done 'organizer-quit)

(defun organizer-edit-item ()
  "Edit current ORGANIZER list entry."
  (interactive)
  (let ((item (organizer-item-string)))
    (if (organizer-string-multiline-p item)
        (organizer-edit-multiline)
      (let ((new (read-from-minibuffer "Edit: " item)))
        (organizer-remove-item)
        (insert new "\n")
        (organizer-backward-item)
        (message "")))))
(defalias 'organizer-cmd-edit 'organizer-edit-item)

(defun organizer-insert-whitespace ()
  "Puts a space at the beginning of all lines when in ORGANIZER Edit mode"
  (goto-char (point-max))
  (beginning-of-line)
  (while (not (equal (point) (organizer-item-start)))
    (progn
      (if (looking-at "^[^\t ]")
	  (insert " "))
      (beginning-of-line)
      (previous-line 1))))

(defun organizer-indent-item ()
  "Make sure that each line begins with white space so that the item
will show up on an organizer-page."
  (interactive)
  (if (string= mode-name "ORGANIZER")
      ;; organizer-edit-multiline calls this function again after calling
      ;; ORGANIZER Edit mode.  Just kill buffer after whitespace is inserted
      ;; to get back to organizer mode."
      (progn
	(organizer-edit-multiline)
	(kill-buffer (current-buffer))
	(minibuffer-message ""))
    (if (string= mode-name "ORGANIZER Edit")
	(organizer-insert-whitespace)
      (error "You must be in organizer mode to do this."))))

(defun organizer-edit-multiline ()
  "Set up a buffer for editing a multiline ORGANIZER list entry."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name organizer-edit-buffer))(organizer buffer-file-name))
    (switch-to-buffer
     (make-indirect-buffer
      ;;      (file-name-nondirectory organizer-file-do) buffer-name))
      ;;       (setq organizer (read-file-name "Organizer page: " organizer-directory))
      (file-name-nondirectory organizer) buffer-name))
    (message "To exit, simply kill this buffer and return to list.")
    (organizer-edit-mode)
    (narrow-to-region (organizer-item-start) (organizer-item-end))
    (organizer-indent-item)))

;;;###autoload
(organizer-read-file-name)
(defun organizer-read-file-name ()
  "Read an organizer-mode filename at prompt"
  (let ((organizer-file ))
    (setq organizer-file (read-file-name  (concat "Organizer file: (default "
                             (file-name-nondirectory organizer-file-last)
                             ") ")
                     organizer-directory
                     organizer-file-last))
    (setq organizer-file (expand-file-name organizer-file))
    (setq organizer-file-last organizer-file)
  organizer-file))

;;;###autoload
(defun organizer-add-category (category &optional organizer)
  "Add new category CAT to the ORGANIZER list."
  (interactive "sCategory: ")
  (let ((organizer (organizer-read-file-name)))
    (save-window-excursion
      (if (not (string-equal mode-name "ORGANIZER"))
          (organizer-show organizer))
    (setq organizer-categories (cons cat organizer-categories))
    (setq organizer buffer-file-name)
    (widen)
    (goto-char (point-min))
    ;; If this is a new file, there's no previous mode line to kill
    (if (not (equal (point-min) (point-max)))
	(progn
	  (beginning-of-line)
	  (let ((junk-point (point)))
	    (end-of-line)
	    (let ((junk-point-two (point)))
	      (delete-region junk-point junk-point-two)))))
    (insert "-*- mode: organizer; ")
    (insert (format "organizer-categories: %S; -*-" organizer-categories))
    ;; If this is a new file, insert line otherwise advance to the next line
    (if (equal (point) (point-max))
	(newline)
      (forward-char 1))
    (insert (format "%s%s%s\n%s\n%s %s\n"
                    organizer-prefix organizer-category-beg cat
                    organizer-category-end
                    organizer-prefix organizer-category-sep)))
  (organizer-quit)
  (organizer-show organizer)
  (organizer-jump-to-category cat organizer)
  (organizer-category-select)
  0))

;; (defun organizer-add-category-no-prompt (cat organizer)
;;   "Add new category CAT to the ORGANIZER list."
;;   (interactive "sCategory: ")
;;   (save-window-excursion
;;     (setq organizer-categories (cons cat organizer-categories))
;; ;;    (setq organizer (read-file-name "Organizer page: " organizer-directory))
;; ;;     (setq organizer buffer-file-name)
;;     (find-file organizer)
;;     (widen)
;;     (goto-char (point-min))
;;     (let ((posn (search-forward "-*- mode: organizer; " 17 t)))
;;       (if (not (null posn)) (goto-char posn))
;;       (if (equal posn nil)
;;           (progn
;;             (insert "-*- mode: organizer; \n")
;;             (forward-char -1))
;;         (kill-line)))
;;     (insert (format "organizer-categories: %S; -*-" organizer-categories))
;;     (forward-char 1)
;;     (insert (format "%s%s%s\n%s\n%s %s\n"
;;                     organizer-prefix organizer-category-beg cat
;;                     organizer-category-end
;;                     organizer-prefix organizer-category-sep)))
;;   0)

;;;###autoload
(defun organizer-add-item-non-interactively (organizer new-item category)
  "Insert NEW-ITEM in ORGANIZER list as a new entry in CATEGORY."
  (save-excursion
    (organizer-show organizer))
  (save-excursion
    (if (string= nil category)
        (setq category (nth organizer-category-number organizer-categories)))
    (let ((cat-exists (member category organizer-categories)))
      (setq organizer-category-number
            (if cat-exists
                (- (length organizer-categories) (length cat-exists))
              (organizer-add-category category))))
    (organizer-show organizer)
    (setq organizer-previous-line 0)
    (let ((top 1))
      ;; goto-line doesn't have the desired behavior in a narrowed buffer
      (goto-char (point-min))
      (forward-line (1- top)))
    (insert new-item "\n")
    (organizer-backward-item)
    (organizer-save)
    (message "")))

;;;###autoload
(defun organizer-insert-item (arg &optional o-buff-str organizer-file)
  "Insert new ORGANIZER list entry.
With a prefix argument solicit the category, otherwise use the current
category."
  (interactive "P")
  (save-excursion
    ;;     (if (not (string-equal mode-name "ORGANIZER")) (organizer-show organizer-file))
    (if (not (string-equal mode-name "ORGANIZER")) (organizer-show organizer-file))
    (let* ((new-item (concat organizer-prefix " "
			     (if o-buff-str
				 (read-from-minibuffer
				  (concat "New ORGANIZER entry: " o-buff-str)
				  (if organizer-entry-prefix-function
				      (funcall organizer-entry-prefix-function)))
			       (read-from-minibuffer
				"New ORGANIZER entry: "
				(if organizer-entry-prefix-function
				    (funcall organizer-entry-prefix-function))))))
	   (categories organizer-categories)
	   (history (cons 'categories (1+ organizer-category-number)))
	   (current-category (nth organizer-category-number organizer-categories))
	   (category
	    (if arg
		current-category
	      (completing-read (concat "Category [" current-category "]: ")
			       (organizer-category-alist) nil nil nil
			       history current-category))))
      (message (concat new-item " " category))
      (organizer-add-item-non-interactively organizer-file new-item category)
      (setq organzier-file nil))))
(defalias 'organizer-cmd-inst 'organizer-insert-item)



(defun organizer-insert-item-here ()
  "Insert new ORGANIZER list entry under the cursor."
  (interactive "")

  (let ((organizer-file (organizer-read-file-name organizer-file)))
    (save-excursion
    (if (not (string-equal mode-name "ORGANIZER")) (organizer-show))
    (let* ((new-item (concat organizer-prefix " "
			     (read-from-minibuffer
			      "New ORGANIZER entry: "
			      (if organizer-entry-prefix-function
				  (funcall organizer-entry-prefix-function))))))
      (insert (concat new-item "\n")))))
   )

(defun organizer-more-important-p (line)
  "Ask whether entry is more important than the one at LINE."
  (if (not (equal organizer-previous-line line))
      (progn
        (setq organizer-previous-line line)
        (goto-char (point-min))
        (forward-line (1- organizer-previous-line))
        (let ((item (organizer-item-string-start)))
          (setq organizer-previous-answer
                (y-or-n-p (concat "More important than '" item "'? "))))))
  organizer-previous-answer)
(defalias 'organizer-ask-p 'organizer-more-important-p)

(defun organizer-delete-item ()
  "Delete current ORGANIZER list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let* ((organizer-entry (organizer-item-string-start))
             (organizer-answer (y-or-n-p (concat "Permanently remove '"
						 organizer-entry "'? "))))
        (if organizer-answer
            (progn
              (organizer-remove-item)
              (organizer-backward-item)))
        (message ""))
    (error "No ORGANIZER list entry to delete")))
(defalias 'organizer-cmd-kill 'organizer-delete-item)

(defun organizer-raise-item ()
  "Raise priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point)) 0)
      (let ((item (organizer-item-string)))
        (organizer-remove-item)
        (organizer-backward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No ORGANIZER list entry to raise")))
(defalias 'organizer-cmd-rais 'organizer-raise-item)

(defun organizer-lower-item ()
  "Lower priority of current entry."
  (interactive)
  (if (> (count-lines (point) (point-max)) 1)
      ;; Assume there is a final newline
      (let ((item (organizer-item-string)))
        (organizer-remove-item)
        (organizer-forward-item)
        (save-excursion
          (insert item "\n"))
        (message ""))
    (error "No ORGANIZER list entry to lower")))
(defalias 'organizer-cmd-lowr 'organizer-lower-item)

(defun organizer-previous-page ()
  "Go to the last visited organizer page"
  (interactive)
  (list-buffers)
  (other-window 1)
  (setq junk_buffer (current-buffer))
  (delete-other-windows)
  (goto-char (point-max))
  (search-backward "ORGANIZER-PAGE" nil t)
  (Buffer-menu-this-window)
  (kill-buffer junk_buffer))

(defun organizer-next-page ()
  "Go to the next organizer page"
  (interactive)
  (list-buffers)
  (other-window 1)
  (setq junk_buffer (current-buffer))
  (delete-other-windows)
  (goto-char (point-min))
  (setq indic 0)
  (setq ind 0)
  (while (equal ind 0)
    (if (search-forward "ORGANIZER-PAGE" nil t) (setq ind 0) (setq ind 1))
    (setq indic (+ indic 1)))
  (goto-char (point-max))
  (while (> indic 3)
    (search-backward "ORGANIZER-PAGE" nil t)
    (Buffer-menu-this-window)
    (set-buffer "*Buffer List*")
    (list-buffers)
    (goto-char (point-max))
    (setq indic (- indic 1)))
  (search-backward "ORGANIZER-PAGE" nil t)
  (Buffer-menu-this-window)
  (delete-other-windows)
  (kill-buffer junk_buffer))

(defun organizer-file-item (&optional comment)
  "File the current ORGANIZER list entry away, annotated with an optional COMMENT."
  ;;   (interactive "sComment: ")
  (interactive)
  (or (> (count-lines (point-min) (point-max)) 0)
      (error "No ORGANIZER list entry to file away"))
  (let ((time-stamp-format organizer-time-string-format))
    ;;     (if (and comment (> (length comment) 0))
    ;; 	(progn
    ;; 	  (goto-char (organizer-item-end))
    ;; 	  (insert
    ;; 	   (if (save-excursion (beginning-of-line)
    ;; ;;			       (looking-at (regexp-quote organizer-prefix)))
    ;; 			       (looking-at (regexp-quote organizer-prefix-prefix)))
    ;; 	       " "
    ;; 	     "\n\t")
    ;; 	   "(" comment ")")))
    (goto-char (organizer-item-end))
    ;;     (insert " [" (nth organizer-category-number organizer-categories) "]")
    (goto-char (organizer-item-start))
    (let ((temp-point (point)))
      ;;      (if (looking-at (regexp-quote organizer-prefix))
      ;;       (if (looking-at (regexp-quote organizer-prefix-prefix))
      ;; 	  (replace-match (time-stamp-string))
      ;; 	;; Standard prefix -> timestamp
      ;; 	;; Else prefix non-standard item start with timestamp
      ;; 	(insert (time-stamp-string)))
      ;;       (setq organizer (read-file-name "Organizer page: " organizer-directory))
      (setq organizer buffer-file-name)
      ;;     (append-to-file temp-point (1+ (organizer-item-end)) organizer)
      (delete-region temp-point (1+ (organizer-item-end))))
    (organizer-backward-item)
    (message "")))

;; -----------------------------------------------------------------------------

;; Utility functions:


;;;###autoload
(defun organizer-top-priorities (&optional nof-priorities category-pr-page)
  "List top priorities for each category.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to \'organizer-show-priorities\'.

If CATEGORY-PR-PAGE is non-nil, a page separator \'^L\' is inserted
between each category."

  (interactive "P")
  (or nof-priorities (setq nof-priorities organizer-show-priorities))
  (if (listp nof-priorities)            ;universal argument
      (setq nof-priorities (car nof-priorities)))
  (let ((organizer-print-buffer-name organizer-tmp-buffer-name)
        ;;(organizer-print-category-number 0)
        (organizer-category-break (if category-pr-page " " ""))
        (cat-end
         (concat
          (if organizer-remove-separator
              (concat organizer-category-end "\n"
                      (regexp-quote organizer-prefix) " " organizer-category-sep "\n")
            (concat organizer-category-end "\n"))))
        beg end)
    (organizer-show)
    (save-excursion
      (save-restriction
        (widen)
        (copy-to-buffer organizer-print-buffer-name (point-min) (point-max))
        (set-buffer organizer-print-buffer-name)
        (goto-char (point-min))
        (when (re-search-forward (regexp-quote organizer-header) nil t)
	  (beginning-of-line 1)
	  (delete-region (point) (line-end-position)))
        (while (re-search-forward       ;Find category start
                (regexp-quote (concat organizer-prefix organizer-category-beg))
                nil t)
          (setq beg (+ (line-end-position) 1)) ;Start of first entry.
          (re-search-forward cat-end nil t)
          (setq end (match-beginning 0))
          (replace-match organizer-category-break)
          (narrow-to-region beg end) ;In case we have too few entries.
          (goto-char (point-min))
          (if (= 0 nof-priorities)      ;Traverse entries.
              (goto-char end)		;All entries
            (organizer-forward-item nof-priorities))
          (setq beg (point))
          (delete-region beg end)
          (widen))
        (and (looking-at " ") (replace-match "")) ;Remove trailing form-feed.
        (goto-char (point-min))         ;Due to display buffer
        ))
    ;; Could have used switch-to-buffer as it has a norecord argument,
    ;; which is nice when we are called from e.g. organizer-print.
    ;; Else we could have used pop-to-buffer.
    (display-buffer organizer-print-buffer-name)
    (message "Type C-x 1 to remove %s window.  M-C-v to scroll the help."
             organizer-print-buffer-name)))

(defun organizer-save-top-priorities (&optional nof-priorities)
  "Save top priorities for each category in `organizer-file-top'.

Number of entries for each category is given by NOF-PRIORITIES which
defaults to `organizer-show-priorities'."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (save-restriction
        (organizer-top-priorities nof-priorities)
        (set-buffer organizer-tmp-buffer-name)
        (write-file organizer-file-top)
        (kill-this-buffer)))))

;;;###autoload
(defun organizer-print (&optional category-pr-page)
  "Print organizer summary using `organizer-print-function'.
If CATEGORY-PR-PAGE is non-nil, a page separator `^L' is inserted
between each category.

Number of entries for each category is given by `organizer-print-priorities'."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (save-restriction
	(organizer-top-priorities organizer-print-priorities
				  category-pr-page)
	(set-buffer organizer-tmp-buffer-name)
	(and (funcall organizer-print-function)
	     (kill-this-buffer))
	(message "Organizer printing done.")))))

(defun organizer-jump-to-category (&optional category organizer)
  "Jump to a category.  Default is previous category."
  (interactive)
  (let* ((categories organizer-categories)
         (history (cons 'categories (1+ organizer-category-number)))
	 (default (nth organizer-category-number organizer-categories)))
    (if (not category)
	(setq category (completing-read
			(concat "Category [" default "]: ")
			(organizer-category-alist) nil nil nil history default)))
    (if (string= "" category)
	(setq category (nth organizer-category-number organizer-categories)))
    (setq organizer-category-number
	  (if (member category organizer-categories)
	      (- (length organizer-categories)
                 (length (member category organizer-categories)))
            (organizer-add-category category)))
    (organizer-show organizer)))

(defun organizer-line-string ()
  "Return current line in buffer as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun organizer-item-string-start ()
  "Return the start of this ORGANIZER list entry as a string."
  ;; Suitable for putting in the minibuffer when asking the user
  (let ((item (organizer-item-string)))
    (if (> (length item) 60)
        (setq item (concat (substring item 0 56) "...")))
    item))

(defun organizer-item-start ()
  "Return point at start of current ORGANIZER list item."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at (regexp-quote organizer-prefix-prefix)))
        (search-backward-regexp
         (concat "^" (regexp-quote organizer-prefix-prefix)) nil t))
    (point)))

(defun organizer-item-end ()
  "Return point at end of current ORGANIZER list item."
  (save-excursion
    (end-of-line)
    (search-forward-regexp
     (concat "^" (regexp-quote organizer-prefix-prefix)) nil 'goto-end)
    (1- (line-beginning-position))))

(defun organizer-remove-item ()
  "Delete the current entry from the ORGANIZER list."
  (delete-region (organizer-item-start) (1+ (organizer-item-end))))

(defun organizer-item-string ()
  "Return current ORGANIZER list entry as a string."
  (buffer-substring (organizer-item-start) (organizer-item-end)))

(defun organizer-string-count-lines (string)
  "Return the number of lines STRING spans."
  (length (split-string string "\n")))

(defun organizer-string-multiline-p (string)
  "Return non-nil if STRING spans several lines."
  (> (organizer-string-count-lines string) 1))

(defun organizer-category-alist ()
  "Generate an alist for use in `completing-read' from `organizer-categories'."
  (mapcar #'list organizer-categories))

;; -----------------------------------------------------------------------------

(easy-menu-define organizer-menu organizer-mode-map "Organizer Menu"
  '("Organizer"
    ("Commands on string at cursor"
     ["BBDB search string" organizer-bbdb-at-point t]
     ["Browse to bookmark" organizer-bookmark-at-point t]
     ["Browse to URL" browse-url-at-point t]
     ["Dired directory or file" organizer-dired-at-point t]
     ["Edit organizer file"  organizer-edit-at-point t]
     ["Find file" find-file-at-point t]
     ["Generate/goto organizer page" organizer-at-point t]
     ("Gnus articles"
      ["Gnus to group" organizer-gnus-at-point t]
      ["Gnus to article number" organizer-gnus-article-number-at-point t]
      )
     ["Open mailbox with external program" organizer-external-mailbox-at-point t]
     ["Rlogin into host" organizer-rlogin-at-point t]
     ["Rsh to host" organizer-rsh-at-point t]
     ["Run shell command on file" organizer-shell-command-at-point t]
     ["Run shell command" organizer-eshell-command-at-point t]
     ["Send mail to address" goto-address-at-point t]
     ["Send mail to address with external program" organizer-external-mail-at-point t]
     ["Telnet to host" organizer-telnet-at-point t]
     ("VM messages"
      ["VM mailbox" organizer-vm-at-point t]
      ["VM to labeled messages" organizer-vm-label-at-point t]
      ["VM to message ID" organizer-vm-messageID-at-point t]
      )
     )
    ["Run shell command in region" organizer-eshell-command-on-region t]
    ("Create linked entry to the page for this file"
     ["BBDB entry" organizer-insert-linked-bbdb-item  t]
     ["Diary" organizer-insert-linked-diary-entry  t]
     ["File or directory" organizer-insert-linked-dired-entry  t]
     ["Other organizer" organizer-insert-linked-organizer-item t]
     ["Todo" organizer-insert-linked-todo-item t]
     )
    ("Category manipulation and movement"
     ["Next category"        organizer-forward-category t]
     ["Previous category"    organizer-backward-category t]
     ["Jump to category"     organizer-jump-to-category t]
     ["Print categories"     organizer-print t]
     )
    ("Item manipulation and movement"
     ["Edit item"            organizer-edit-item t]
     ["File item"            organizer-file-item t]
     ["Indent item"          organizer-indent-item t]
     ["Insert new item"      organizer-insert-item t]
     ["Insert item here"     organizer-insert-item-here t]
     ["Kill item"            organizer-delete-item t]
     "---"
     ["Lower item priority"  organizer-lower-item t]
     ["Raise item priority"  organizer-raise-item t]
     ["Move item to another category" organizer-move-entry-to-another-category t]
     "---"
     ["Next item"            organizer-forward-item t]
     ["Previous item"        organizer-backward-item t]
     )
    ["Kill ring save link-mark for this file" organizer-kill-ring-save-link-mark t]
    "---"
    ["Generate page for this file" organizer-generate-page-for-this-file t]
    ["Next open organizer page" organizer-next-page t]
    ["Previous open organizer page" organizer-previous-page t]
    "---"
    ["Edit another organizer file" organizer-show t]
    ["Edit todo list"       todo-show t]
    ["Fancy diary entries"      diary t]
    "---"
    ["Save"                 organizer-save t]
    ["Quit"                 organizer-quit t]
    ))

(easy-menu-define organizer-minor-menu organizer-minor-mode-map "Organizer-Minor Menu"
  ;; (easy-menu-define organizer-minor-menu nil "Organizer-Minor Menu"
  '("Organizer"
    ("Commands on string at cursor"
     ["BBDB search string" organizer-bbdb-at-point t]
     ["Browse to bookmark" organizer-bookmark-at-point t]
     ["Browse to URL" browse-url-at-point t]
     ["Dired directory or file" organizer-dired-at-point t]
     ["Edit organizer file"  organizer-edit-at-point t]
     ["Find file" find-file-at-point t]
     ["Generate/goto organizer page" organizer-at-point t]
     ("Gnus articles"
      ["Gnus to group" organizer-gnus-at-point t]
      ["Gnus to article number" organizer-gnus-article-number-at-point t]
      )
     ["Open mailbox with external program" organizer-external-mailbox-at-point t]
     ["Rlogin into host" organizer-rlogin-at-point t]
     ["Rsh to host" organizer-rsh-at-point t]
     ["Run shell command on file" organizer-shell-command-at-point t]
     ["Run shell command" organizer-eshell-command-at-point t]
     ["Send mail to address" goto-address-at-point t]
     ["Send mail to address with external program" organizer-external-mail-at-point t]
     ["Telnet to host" organizer-telnet-at-point t]
     ("VM messages"
      ["VM mailbox" organizer-vm-at-point t]
      ["VM to labeled messages" organizer-vm-label-at-point t]
      ["VM to message-ID" organizer-vm-messageID-at-point t]
      )
     )
    ["Run shell command in region" organizer-eshell-command-on-region t]
    ("Create linked entry to an organizer page"
     ["BBDB entry" organizer-insert-linked-bbdb-item  t]
     ["Diary" organizer-insert-linked-diary-entry  t]
     ["File or directory" organizer-insert-linked-dired-entry  t]
     ["File or directory under cursor (while in Dired only)" organizer-insert-linked-dired-entry-under-cursor t]
     ["Other organizer" organizer-insert-linked-organizer-item t]
     ["Todo" organizer-insert-linked-todo-item t]
     ("Gnus to organizer file (while in gnus only)"
      ["Current group" organizer-gnus-insert-linked-organizer-entry-group t]
      ["Current article" organizer-gnus-insert-linked-organizer-entry-current-article t]
      ["Current article number" organizer-gnus-insert-linked-organizer-entry-current-article-number t]
      )
     ("VM to organizer file (while in VM only)"
      ["Current mail folder" organizer-vm-insert-linked-organizer-entry-mail-folder t]
      ["Current message" organizer-vm-insert-linked-organizer-entry-current-message t]
      ["Current message ID" organizer-vm-insert-linked-organizer-entry-current-messageID t]
      ["Label" organizer-vm-insert-linked-organizer-entry-label t]
      )
     )
    ("Kill ring save"
     ["Link-mark for an organizer file" organizer-kill-ring-save-link-mark t]
     ("Gnus (Gnus only)"
      ["Current group" organizer-gnus-kill-ring-save-group t]
      ["Current article" organizer-gnus-kill-ring-save-current-article t]
      ["Current article number" organizer-gnus-kill-ring-save-current-article-number t]
      )
     ("VM (VM only)"
      ["Mail folder" organizer-vm-kill-ring-save-mail-folder t]
      ["Current message" organizer-vm-kill-ring-save-current-message t]
      ["Current message ID" organizer-vm-kill-ring-save-current-messageID t]
      ["Label" organizer-vm-kill-ring-save-label t]
      )
     )
    ["Next open organizer page" organizer-next-page t]
    ["Previous open organizer page" organizer-previous-page t]
    "---"
    ["Edit an organizer file" organizer-show t]
    ["Edit todo list"       todo-show t]
    ["Fancy diary entries"      diary t]
    ))

(easy-menu-define organizer-page-menu organizer-page-mode-map "Organizer-Page Menu"
  '("Organizer"
    ("Commands on string at cursor"
     ["BBDB search string" organizer-bbdb-at-point t]
     ["Browse to bookmark" organizer-bookmark-at-point t]
     ["Browse to URL" browse-url-at-point t]
     ["Dired directory or file" organizer-dired-at-point t]
     ["Edit organizer file"  organizer-edit-at-point t]
     ["Find file" find-file-at-point t]
     ["Generate/goto organizer page" organizer-at-point t]
     ("Gnus articles"
      ["Gnus to group" organizer-gnus-at-point t]
      ["Gnus to article number" organizer-gnus-article-number-at-point t]
      )
     ["Open mailbox with external program" organizer-external-mailbox-at-point t]
     ["Rlogin into host" organizer-rlogin-at-point t]
     ["Rsh to host" organizer-rsh-at-point t]
     ["Run shell command on file" organizer-shell-command-at-point t]
     ["Run shell command" organizer-eshell-command-at-point t]
     ["Send mail to address" goto-address-at-point t]
     ["Send mail to address with external program" organizer-external-mail-at-point t]
     ["Telnet to host" organizer-telnet-at-point t]
     ("VM messages"
      ["VM mailbox" organizer-vm-at-point t]
      ["VM to labeled messages" organizer-vm-label-at-point t]
      ["VM to message ID" organizer-vm-messageID-at-point t]
      )
     )
    ["Run shell command in region" organizer-eshell-command-on-region t]
    ("Create linked entry to this page"
     ["BBDB entry" organizer-insert-linked-bbdb-item  t]
     ["Diary" organizer-insert-linked-diary-entry  t]
     ["File or directory" organizer-insert-linked-dired-entry  t]
     ["Other organizer" organizer-insert-linked-organizer-item t]
     ["Todo" organizer-insert-linked-todo-item t]
     )
    ("Page movement and manipulation"
     ["Edit this page's organizer file" organizer-edit-this-page t]
     ["Reload this organizer page" organizer-reload-this-page t]
     "---"
     ["Next open organizer page" organizer-next-page t]
     ["Previous open organizer page" organizer-previous-page t]
     )
    ["Kill ring save link-mark for this page" organizer-kill-ring-save-link-mark t]
    "---"
    ["Edit another organizer file"       organizer-show t]
    ["Edit todo list"       todo-show t]
    ["Fancy diary entries"      diary t]
    "---"
    ["Quit" bury-buffer t]
    ))

;; As calendar reads .organizer-do before organizer-mode is loaded.
;;;###autoload
(defun organizer-mode ()
  "Major mode for editing ORGANIZER lists.

\\{organizer-mode-map}"
  (interactive)
  (setq major-mode 'organizer-mode)
  (setq mode-name "ORGANIZER")
  (use-local-map organizer-mode-map)
  (easy-menu-add organizer-menu)
  (run-hooks 'organizer-mode-hook))

;; (defun organizer-minor-mode ()
;;   "Minor mode for accessing commands at cursor as with ORGANIZER lists."
;; ;;\\{organizer-minor-mode-map}"
;;   (interactive)
;;   (setq minor-mode 'organizer-minor-mode)
;; ;;   (setq mode-name "ORGANIZER-MINOR")
;;   (use-local-map organizer-minor-mode-map)
;;   (easy-menu-add organizer-minor-menu)
;;   (run-hooks 'organizer-mode-hook)
;; )

(if organizer-running-xemacs
    (progn
      (define-minor-mode organizer-minor-mode
	"Minor mode which provides commands at cursor in other major modes."
	nil " Organizer" organizer-minor-mode-map
	;; nil " Organizer" nil
	)
      (add-hook 'organizer-minor-mode-hook
		'(lambda () (easy-menu-add organizer-minor-menu))))
  (define-minor-mode organizer-minor-mode
    "Minor mode which provides commands at cursor in other major modes."
    nil " Organizer" organizer-minor-mode-map
    ;; nil " Organizer" nil
    (easy-menu-add organizer-minor-menu)))

(defun organizer-page-mode ()
  "Major mode for editing ORGANIZER lists.

\\{organizer-page-mode-map}"
  (interactive)
  (setq major-mode 'organizer-page-mode)
  (setq mode-name "ORGANIZER-PAGE")
  (use-local-map organizer-page-mode-map)
  (easy-menu-add organizer-page-menu)
  (run-hooks 'organizer-page-mode-hook)
  )

(eval-when-compile
  (defvar date)
  (defvar entry))

;; Read about this function in the setup instructions above!
;;;###autoload
(defun organizer-cp ()
  "Make a diary entry appear only in the current date's diary."
  (if (equal (calendar-current-date) date)
      entry))

(define-derived-mode organizer-edit-mode text-mode "ORGANIZER Edit"
  "Major mode for editing items in the ORGANIZER list.

\\{organizer-edit-mode-map}")

(defvar organizer-file-last "none"
  "Latest organizer-mode file opened.")

;;;###autoload
(defun organizer-show (&optional organizer-file)
  "Show ORGANIZER list."
  (interactive)
  (unless organizer-file  (setq organizer-file (organizer-read-file-name)))

  (if (not (file-exists-p organizer-directory))
      (organizer-first-run))
  (if (find-buffer-visiting organizer-file)
      (switch-to-buffer (find-buffer-visiting organizer-file))
    (if (file-exists-p organizer-file)
	    (find-file organizer-file)
	(if (y-or-n-p (concat organizer-file " not found.  Create it?"))
	    (organizer-initial-setup organizer-file)
	  (minibuffer-message (concat "File " organizer-file " not found.")))))
  (if (null organizer-categories)
      (if (null organizer-cats)
          (error "Error in %s: No categories in list `organizer-categories'"
                 organizer-file)
        (goto-char (point-min))
        (and (search-forward "organizer-cats:" nil t)
             (replace-match "organizer-categories:"))
        (make-local-variable 'organizer-categories)
        (setq organizer-categories organizer-cats)))
  (beginning-of-line)
  (organizer-category-select))

;; (defun organizer-show (&optional organizer-file)
;;   "Show ORGANIZER list."
;; ;;  (interactive)
;; ;;  (setq organizer-file (read-file-name "Organizer page: " organizer-directory))
;;   (if (file-exists-p organizer-file)
;;       (find-file organizer-file)
;; ;;    (set organizer-categories nil)
;; ;;    (set organizer-cats nil)
;;     (organizer-initial-setup organizer-file))
;;   (if (null organizer-categories)
;;       (if (null organizer-cats)
;;           (error "Error in %s: No categories in list `organizer-categories'"
;;                  organizer-file)
;;         (goto-char (point-min))
;;         (and (search-forward "organizer-cats:" nil t)
;;              (replace-match "organizer-categories:"))
;;         (make-local-variable 'organizer-categories)
;;         (setq organizer-categories organizer-cats)))
;;   (beginning-of-line)
;;   (organizer-category-select))

(defun organizer-initial-setup (organizer)
  "Set up things to work properly in ORGANIZER mode."
  ;;  (setq organizer (read-file-name "Organizer page: " organizer-directory))
  (find-file organizer)
  ;;  (switch-to-buffer (file-name-nondirectory organizer))
  (erase-buffer)
  (organizer-mode)
  ;;  (organizer-add-category-no-prompt (concat "End of " (file-name-nondirectory organizer)) organizer))
  (if (not (file-exists-p organizer))
      (progn
	(setq cat (read-string "New category: "))
	(organizer-add-category cat organizer))))


;; (defvar organizer-prefix "%%(equal (calendar-current-date) date) "
;; "Default prefix for noew organizer entries with trailing space.")

;; (defvar organizer-prefix-prefix "%%"
;; "Default prefix for noew organizer entries with trailing space.")

;; (defun organizer-yank-to-organizer ()
;;   "Yank kill buffer to the end of a diary/organizer file, save and close.  Only
;; works on one entry at a time since the prefix if only put on the first
;; line."
;;   (interactive)
;;   (setq organizer-file (read-file-name "Organizer file: " organizer-directory))
;;   (setq prefx (read-string "Prefix with space: " organizer-prefix))
;;   (save-excursion
;;     (find-file organizer-file)
;;     (goto-char (point-max))
;;     (newline)
;;     (insert-string prefx)
;;     (yank)
;;     (goto-char (point-max))
;;     (save-buffer)
;;     (kill-this-buffer)))

;; (defun organizer-dired-yank-to-organizer ()
;;   "Yank full filename and path from dired at prompt to the end of a
;; diary/organizer file, save and close."
;;   (interactive)
;;   (setq organizer (read-file-name "Organizer file: " organizer-directory))
;;   (setq prefx (read-string "Prefix with space: " organizer-prefix))
;;   (dired-copy-filename-as-kill 0)
;;   (save-excursion
;;     (find-file organizer)
;;     (goto-char (point-max))
;;     (newline)
;;     (insert-string prefx)
;;     (yank)
;;     (goto-char (point-max))
;;     (save-buffer)
;;     (kill-this-buffer)))

(defun organizer-bbdb-at-point ()
  "Start BBDB, searching for name at point."
  (interactive)
  (if (fboundp 'bbdb)
      (progn
	;;	  (require 'ffap)
	(let ((str (ffap-string-at-point)))
	  (bbdb str nil)))
    (error "bbdb isn't loaded.")))

(defun organizer-shell-command-at-point ()
  "Perform shell command upon the full file name at the cursor in any buffer."
  (interactive)
  ;;  (require 'ffap)
  (let ((str (ffap-string-at-point))
	(shcom "shell command on file "))
    (organizer-dired-at-point)
    (let ((filestr "junk_string")
	  (junk-buffer (current-buffer)))
    (if (not organizer-running-xemacs)
	(setq filestr (concat (dired-guess-shell-command shcom (list str)) " " str))
      (setq filestr (concat (dired-read-shell-command shcom 1 (list str)) " " str)))
      (shell-command  (concat filestr " &"))
      (kill-buffer junk-buffer))))

(defun organizer-eshell-command-at-point ()
  "Perform shell command at the point."
  (interactive)
  ;;  (require 'ffap)
  (let ((str (ffap-string-at-point)))
    (shell-command str)))

(defun organizer-eshell-command-on-region (start end)
  "Perform shell command contained within the region."
  (interactive
   (list (region-beginning) (region-end)))
  (copy-to-register 'o start end)
  (switch-to-buffer "junk-buffer")
  (insert-string (concat (insert-register 'o t) "\n"))
  (next-line -1)
  (end-of-line)
  (if (not organizer-running-xemacs)
      (setq organizer-esh-comm (delete-and-extract-region (point-min) (point)))
    (setq organizer-esh-comm (organizer-delete-and-return-region (point-min) (point))))
  (kill-buffer (current-buffer))
  (shell-command organizer-esh-comm))

(defun organizer-at-point ()
  "Open a organizer with the name at the cursor."
  (interactive)
  ;;    (require 'ffap)
  (let ((str (ffap-string-at-point)))
    (let ((filestr (concat organizer-directory str)))
      ;; 	(setq ind 0)
      ;; 	(organizer-look-for-organizer-file-in-subdirectories ind filestr str)
      (calendar)
      (setq organizer filestr)
      (organizer-view-other-diary-entries organizer str)))
  (other-window -1))

(defun organizer-edit-at-point ()
  "Open a organizer with the name at the cursor for editing."
  (interactive)
  ;;    (require 'ffap)
  (let ((str (ffap-string-at-point)))
    (let ((filestr (concat organizer-directory str)))
      (organizer-show filestr))))

(defun organizer-edit-this-page ()
  "Open organizer file for the current organizer page."
  (interactive)
  (let ((str (substring (buffer-name) 1 -1)))
    ;;       (message (concat "str=" str))
    (let ((filestr (concat organizer-directory str)))
      ;; 	(message (concat "filestr=" filestr))
      (if (not (file-exists-p filestr))
	  (progn
	    (find-file organizer-directory)
	    (organizer-list-subdirectories)
	    (goto-char (point-min))
	    (if (re-search-forward (concat " " str "\n") nil t)
		(progn
		  (next-line -1)
		  (setq filestr_junk (dired-get-filename t))
		  (setq filestr (concat default-directory filestr_junk))
		  (kill-this-buffer))
	      (error (concat "The organizer file, " str ", doesn't exist.")))))
      ;;      (let ((filestr (concat organizer-directory str)))
      ;; 	(message (concat "filestr=" filestr))
      (organizer-show filestr))))

(defun organizer-reload-this-page ()
  "Reload this organizer page so changes can take effect."
  (interactive)
  (let ((str (substring (buffer-name) 1 -1)))
    (let ((filestr (concat organizer-directory str)))
      (if (not (file-exists-p filestr))
	  (progn
	    (find-file organizer-directory)
	    (organizer-list-subdirectories)
	    (goto-char (point-min))
	    (if (re-search-forward (concat " " str "\n") nil t)
		(progn
		  (next-line -1)
		  (setq filestr_junk (dired-get-filename t))
		  (setq filestr (concat default-directory filestr_junk))
		  (kill-this-buffer))
	      (error (concat "The organizer file, " str ", doesn't exist.")))))
      ;;      (let ((filestr (concat organizer-directory str)))
      (calendar)
      (organizer-view-other-diary-entries filestr))))

(defun organizer-generate-page-for-this-file ()
  "Generate and show the organizer page for this file."
  (interactive)
  (save-buffer)
  (let ((str (buffer-name)))
    (let ((filestr (concat organizer-directory str)))
      (if (not (file-exists-p filestr))
	  (progn
	    (find-file organizer-directory)
	    (organizer-list-subdirectories)
	    (goto-char (point-min))
	    (if (re-search-forward (concat " " str "\n") nil t)
		(progn
		  (next-line -1)
		  (setq filestr_junk (dired-get-filename t))
		  (setq filestr (concat default-directory filestr_junk))
		  (kill-this-buffer))
	      (error (concat "The organizer file, " str ", doesn't exist.")))))
      ;;      (let ((filestr (concat organizer-directory str)))
      (calendar)
      (organizer-view-other-diary-entries filestr))))

(defun organizer-find-file ()
  "Open the full file name and path at the cursor."
  (interactive)
  ;;    (require 'ffap)
  (let ((filestr (ffap-string-at-point)))
    (find-file filestr)))

(defun organizer-open-organizer ()
  "Open a organizer file for editing.  Basically this is here in case
a file doesn't load and you want to look at it to know why.  More for
diagnostic work."
  (interactive)
  (setq organizer (read-file-name "Organizer file: " organizer-directory))
  (find-file organizer))

(defun organizer-view-other-diary-entries (&optional organizer str)
  "Generate an organizer page."
  (interactive)
  (setq file-exists_ind 0)
  (if (not organizer)
      (setq organizer (read-file-name "Organizer page: " organizer-directory)))
  (if (and (not (file-exists-p organizer)) (not str))
      ;;      (progn
      ;; This means that the user wants to generate a page with marked entries but there's no file.  We'll just go without one in this case."
      (setq file-exists_ind 1)
    ;; 	(organizer-show organizer)
    ;;     (organizer-quit))
    )
  (if (and (not (file-exists-p organizer)) str)
      ;; Either there's no file or its in a subdirectory.
      (progn
	;;	  (organizer-look-for-organizer-file-in-subdirectories file-exists_ind organizer str)
	(find-file organizer-directory)
	(organizer-list-subdirectories)
	(goto-char (point-min))
	(if (re-search-forward (concat " " str "\n") nil t)
	    ;;  Ah, ha.  It was in a subdirectory
	    (progn
	      (next-line -1)
	      (setq filestr_junk (dired-get-filename t))
	      (setq organizer (concat default-directory filestr_junk))
	      (kill-this-buffer))
	  ;; This means that the user wants to generate a page with marked entries but there's no file.  We'll just go without one in this case."
	  (progn
	    (setq file-exists_ind 1)
	    (setq filestr_junk organizer)
	    (setq organzier (concat organizer-directory filestr_junk))
	    ;; 	    (organizer-show organizer)
	    ;; 	    (organizer-quit)
	    ))))
  (if (not (get-buffer calendar-buffer))
      (progn
	(calendar)
	(shrink-window-if-larger-than-buffer)
	(calendar-goto-today))
    (progn
      (set-buffer calendar-buffer)
      (shrink-window-if-larger-than-buffer)))
  (organizer-hidden-view-other-diary-entries organizer-number-of-entries diary-file organizer)
  (other-window 1)
  (switch-to-buffer fancy-diary-buffer)
  (rename-buffer (concat "*" o-buff "*"))
  (calendar-set-mode-line (concat "*" o-buff "*"))
  (organizer-page-mode)
  (setq default-directory organizer-directory)
  ;; Get rid of temporary organizer file that didn't exist before command invocation.
  ;;   (if (equal file-exists_ind 1)
  ;; 	(delete-file organizer))
  )
;;(message (concat "The organizer file, " organizer ", doesn't exist.")


(defun organizer-echo-at-point ()
  "Get string from buffer and echo.  For testing.  See `ffap'."
  (interactive)
  ;;  (require 'ffap)
  (let ((str (ffap-string-at-point)))
    (message "The string at the cursor is \"%s\"." str)))

;; (defun todo-show-alternate-file ()
;;   "Show an alternate TODO list."
;;   (interactive)
;;   (setq todo-file-alt (read-file-name "FTodo file: " organizer-directory))
;;   (if (file-exists-p todo-file-alt)
;;       (find-file todo-file-alt)
;;     (todo-initial-setup))
;;   (if (null todo-categories)
;;       (if (null todo-cats)
;;           (error "Error in %s: No categories in list `todo-categories'"
;;                  todo-file-alt)
;;         (goto-char (point-min))
;;         (and (search-forward "todo-cats:" nil t)
;;              (replace-match "todo-categories:"))
;;         (make-local-variable 'todo-categories)
;;         (setq todo-categories todo-cats)))
;;   (beginning-of-line)
;;   (todo-category-select))

;; VM integration

(defun organizer-vm-at-point ()
  "Start vm and open folder name at point.  Requires full path to
mailbox in buffer unless in dired."
  (interactive)
  ;;    (require 'ffap)
  (let ((filestr (ffap-string-at-point "vm-mode")))
    (vm filestr)))

(defun organizer-vm-label-at-point ()
  "Start vm and open folder name at point before \"~>\" delimiter.  Requires full path to
mailbox. Show only those messages with the label after the \"~>\" delimiter.
The mailbox name can't contain this delimiter character."
  (interactive)
  ;;   (require 'ffap)
  (let ((str (ffap-string-at-point)))
    (switch-to-buffer "junk-buffer")
    (insert str)
    (beginning-of-buffer)
    (if (not (search-forward "~>"))
	(progn
	  (kill-buffer (current-buffer))
	  (error "You need a mark with the format mailbox~>label")))
    (backward-delete-char-untabify 2)
    (let ((junk-mailbox-variable
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point))
	     (organizer-delete-and-return-region (point-min) (point)))))
      ;;       (forward-char)
      (let ((junk-label-variable
	     (if (not organizer-running-xemacs)
		 (delete-and-extract-region (point) (point-max))
	       (organizer-delete-and-return-region (point) (point-max)))))
	(kill-buffer (current-buffer))
	(vm junk-mailbox-variable)
	(vm-create-virtual-folder 'label junk-label-variable nil nil)
	;;	 (message "here")
	))))

(defun organizer-vm-messageID-at-point ()
  "Start vm and open folder name at point before \">\" delimiter.  Requires full path to
mailbox. Show only those message with message ID after the \">\" delimiter.
The mailbox name can't contain this delimiter character."
  (interactive)
  ;;   (require 'ffap)
  (let ((str (ffap-string-at-point)))
    (switch-to-buffer "junk-buffer")
    (insert str)
    (beginning-of-buffer)
    (if (not (search-forward "~>"))
	(progn
	  (kill-buffer (current-buffer))
	  (error "You need a mark with the format mailbox~>message_ID")))
    (backward-delete-char-untabify 2)
    (let ((junk-mailbox-variable
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point))
	     (organizer-delete-and-return-region (point-min) (point)))))
      ;;       (forward-char)
      (let ((junk-label-variable
	     (if (not organizer-running-xemacs)
		 (delete-and-extract-region (point) (point-max))
	       (organizer-delete-and-return-region (point) (point-max)))))
	(kill-buffer (current-buffer))
	(vm junk-mailbox-variable)
	(vm-create-virtual-folder 'header junk-label-variable nil nil)
	;;	 (message "here")
	))))

(defun organizer-vm-kill-ring-save-mail-folder ()
  "Save the folder name (full path) of the current message to the
kill-ring.  Handy for insertion into todo or diary entries."
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (let ((organizer-temp-mailbox-file (buffer-file-name)))
      (switch-to-buffer "junk-buffer")
      (insert organizer-temp-mailbox-file))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer "junk-buffer")))

(defun organizer-vm-kill-ring-save-current-messageID ()
  "Save the mark with the folder name and message-ID of the current
message to the kill-ring.  Handy for insertion into todo or diary
entries.  For instance, a specific message in your INBOX folder will
have the following saved in the kill-ring:

~/mail/INBOX~>message-id-of-message"
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (vm-expose-hidden-headers)
    (other-window 2)
    (goto-char (point-min))
    (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	(progn
	  (vm-expose-hidden-headers)
	  (goto-char (point-min))
	  (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	      (error "Couldn't find message ID.  Try selecting message in the Summary buffer."))))
    (forward-char)
    (let ((temp-point (point)))
      (end-of-line)
      (backward-char)
      (copy-to-register 'o temp-point (point)))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (let ((organizer-temp-mailbox-file (buffer-file-name)))
      (switch-to-buffer "junk-buffer")
      (insert (concat organizer-temp-mailbox-file "~>" (insert-register 'o))))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer "junk-buffer")))

(defun organizer-vm-kill-ring-save-current-message ()
  "Save the mark with the folder name and message-ID of the current
message and the message itself to the kill-ring.  Inserts white space
before each line.  Handy for insertion into todo or diary entries.
For instance, a specific message in your INBOX folder will have the
following saved in the kill-ring:

~/mail/INBOX~>message-id-of-message
The message headers and body here."
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (vm-expose-hidden-headers)
    (other-window 2)
    (goto-char (point-min))
    (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	(progn
	  (vm-expose-hidden-headers)
	  (goto-char (point-min))
	  (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	      (error "Couldn't find message ID.  Try selecting message in the Summary buffer."))))
    (forward-char)
    (let ((temp-point (point)))
      (end-of-line)
      (backward-char)
      (copy-to-register 'o temp-point (point)))
    (vm-expose-hidden-headers)
    (copy-to-register 'p (point-min) (point-max))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (let ((organizer-temp-mailbox-file (buffer-file-name)))
      (switch-to-buffer "junk-buffer")
      (insert (concat organizer-temp-mailbox-file "~>" (insert-register 'o)))
      (end-of-line)
      (insert (concat "\n" (insert-register 'p))))
    (goto-char (point-max))
    (beginning-of-line)
    (while (not (equal (point) (point-min)))
      (progn
	(insert " ")
	(previous-line 1)
	(beginning-of-line)))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer "junk-buffer")))

(defun organizer-vm-kill-ring-save-label ()
  "Save the mark with the folder name and message-ID of the current
message to the kill-ring.  Handy for insertion into todo or diary
entries.  For instance, a specific message in your INBOX folder will
have the following saved in the kill-ring:

~/mail/INBOX~>label"
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (let ((organizer-temp-label (read-string "Label: ")))
      (vm-follow-summary-cursor)
      (vm-select-folder-buffer)
      (let ((organizer-temp-mailbox-file (buffer-file-name)))
	(switch-to-buffer "junk-buffer")
	(insert (concat organizer-temp-mailbox-file "~>" organizer-temp-label))))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer "junk-buffer")))

(defun organizer-vm-insert-linked-organizer-entry-mail-folder ()
  "Insert an entry into an organizer file with the folder name (full
path) of the current message.  The program will call the
organizer-file and prompt for an entry with the folder name already
inserted into the minibuffer."
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (let ((organizer-temp-mailbox-file (buffer-file-name)))
      (switch-to-buffer "junk-buffer")
      (insert organizer-temp-mailbox-file))
    (let ((organizer-temp-string
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point-max))
	     (organizer-delete-and-return-region (point-min) (point-max))))
	  (organizer (read-file-name "Organizer file: " organizer-directory)))
      (organizer-show organizer)
      (organizer-page-insert-organizer-item nil organizer-temp-string organizer)
      (kill-buffer "junk-buffer"))))

(defun organizer-vm-insert-linked-organizer-entry-current-messageID ()
  "Insert an entry into an organizer file with the mark for the folder
name and message-ID of the current message.  The program will call the
organizer-file and prompt for an entry with the mark already inserted
into the minibuffer.  For instance, a specific message in your INBOX
folder will have the following in the minibuffer:

~/mail/INBOX~>message-id-of-message"
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (vm-expose-hidden-headers)
    (other-window 2)
    (goto-char (point-min))
    (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	(progn
	  (vm-expose-hidden-headers)
	  (goto-char (point-min))
	  (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	      (error "Couldn't find message ID.  Try selecting message in the Summary buffer."))))
    (forward-char)
    (let ((temp-point (point)))
      (end-of-line)
      (backward-char)
      (copy-to-register 'o temp-point (point)))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (let ((organizer-temp-mailbox-file (buffer-file-name)))
      (switch-to-buffer "junk-buffer")
      (insert (concat organizer-temp-mailbox-file "~>" (insert-register 'o))))
    (let ((organizer-temp-string
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point-max))
	     (organizer-delete-and-return-region (point-min) (point-max))))
	  (organizer (read-file-name "Organizer file: " organizer-directory)))
      (organizer-show organizer)
      (organizer-page-insert-organizer-item nil organizer-temp-string organizer)
      (kill-buffer "junk-buffer"))))

(defun organizer-vm-insert-linked-organizer-entry-current-message ()
  "Insert an entry into an organizer file with the mark for the folder
name and message-ID along the current message.  The program will call
the organizer-file and insert an entry with the mark already inserted.
For instance, a specific message in your INBOX folder will have the
following in the entry:

~/mail/INBOX~>message-id-of-message
Headers and body of the message"
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (vm-expose-hidden-headers)
    (other-window 2)
    (goto-char (point-min))
    (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	(progn
	  (vm-expose-hidden-headers)
	  (goto-char (point-min))
	  (if (not (re-search-forward "[Mm]essage-[Ii][Dd]: <" nil t))
	      (error "Couldn't find message ID.  Try selecting message in the Summary buffer."))))
    (forward-char)
    (let ((temp-point (point)))
      (end-of-line)
      (backward-char)
      (copy-to-register 'o temp-point (point)))
    (vm-expose-hidden-headers)
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (let ((organizer-temp-mailbox-file (buffer-file-name)))
      (copy-to-register 'p (point-min) (point-max))
      (switch-to-buffer "junk-buffer")
      (insert (concat organizer-temp-mailbox-file "~>" (insert-register 'o))))
    (end-of-line)
    (insert (concat "\n" (insert-register 'p)))
    (goto-char (point-max))
    (beginning-of-line)
    (while (not (equal (point) (point-min)))
      (progn
	(insert " ")
	(previous-line 1)
	(beginning-of-line)))
    (let ((organizer-temp-string
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point-max))
	     (organizer-delete-and-return-region (point-min) (point-max))))
	  (organizer (read-file-name "Organizer file: " organizer-directory)))
      (organizer-show organizer)
      (organizer-page-insert-organizer-item nil organizer-temp-string organizer t)
      ;;    (exit-minibuffer)
      (kill-buffer "junk-buffer"))))

(defun organizer-vm-insert-linked-organizer-entry-label ()
  "Insert an entry into an organizer file with the mark for the folder
name and label of the current message.  The program will call the
organizer-file and prompt for an entry with the mark already inserted
into the minibuffer.  For instance, a specific message in your INBOX
folder will have the following in the minibuffer:

~/mail/INBOX~>label"
  (interactive)
  (save-excursion
    (if (not (string-equal mode-name "VM Summary"))
	(if (or (string-equal mode-name "VM Presentation") (string-equal mode-name "VM"))
	    (vm-summarize)
	  (error "You must be in a VM buffer to execute this command")))
    (let ((organizer-temp-label (read-string "Label: ")))
      (vm-follow-summary-cursor)
      (vm-select-folder-buffer)
      (let ((organizer-temp-mailbox-file (buffer-file-name)))
	(switch-to-buffer "junk-buffer")
	(insert (concat organizer-temp-mailbox-file "~>" organizer-temp-label))))
    (let ((organizer-temp-string
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point-max))
	     (organizer-delete-and-return-region (point-min) (point-max))))
	  (organizer (read-file-name "Organizer file: " organizer-directory)))
      (organizer-show organizer)
      (organizer-page-insert-organizer-item nil organizer-temp-string organizer)
      (kill-buffer "junk-buffer"))))

;; Gnus integration

(defun organizer-gnus-at-point ()
  "Start gnus and open folder name at cursor on page.  Requires that
gnus be set up before hand."
  (interactive)
  ;;    (require 'ffap)
  (let ((groupstr (ffap-string-at-point)))
    (gnus)
    (gnus-fetch-group groupstr)))

(defun organizer-gnus-article-number-at-point ()
  "Start gnus and open article in folder indicated by mark."
  (interactive)
  ;;   (require 'ffap)
  (let ((str (ffap-string-at-point)))
    (switch-to-buffer "junk-buffer")
    (insert str)
    (beginning-of-buffer)
    (if (not (search-forward "~>"))
	(progn
	  (kill-buffer (current-buffer))
	  (error "You need a mark with the format group~>article_number")))
    (backward-delete-char-untabify 2)
    (let ((junk-group-variable
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point))
	     (organizer-delete-and-return-region (point-min) (point)))))
      ;;       (forward-char)
      (let ((junk-message-id-variable
	     (if (not organizer-running-xemacs)
		 (delete-and-extract-region (point) (point-max))
	       (organizer-delete-and-return-region (point) (point-max)))))
	(kill-buffer (current-buffer))
	(gnus)
	(gnus-group-list-all-groups 10)
	(goto-char (point-min))
	(search-forward junk-group-variable)
	(gnus-group-select-group 10000)
	(gnus-summary-goto-article junk-message-id-variable)
	;;	 (message "here")
	))))

(defun organizer-gnus-insert-linked-organizer-entry-group ()
  "Insert an entry into an organizer file with the folder name of the
current message.  The program will call the organizer file and prompt
for an entry with the folder name already inserted into the
minibuffer."
  (interactive)
  (if (not (string-equal mode-name "Summary"))
      (if (string-equal mode-name "Article")
	  (gnus-article-show-summary)
	(error "You must be in a gnus buffer with a selected message to execute this command")))
  (let ((organizer-temp-gnus-current-group (car gnus-article-current)))
    (switch-to-buffer "junk-buffer")
    (insert organizer-temp-gnus-current-group))
  (let ((organizer-temp-string
	 (if (not organizer-running-xemacs)
	     (delete-and-extract-region (point-min) (point-max))
	   (organizer-delete-and-return-region (point-min) (point-max))))
	(organizer (read-file-name "Organizer file: " organizer-directory)))
    (organizer-show organizer)
    (organizer-page-insert-organizer-item nil organizer-temp-string organizer)
    (kill-buffer "junk-buffer")))

(defun organizer-gnus-insert-linked-organizer-entry-current-article-number ()
  "Insert an entry into an organizer file with the mark for the folder
name and article numer of the current message.  The program will call the
organizer file and prompt for an entry with the mark already inserted
into the minibuffer.  For instance, a specific message in comp.os.linux.misc
will have the following in the minibuffer:

comp.os.linux.misc~>article_number"
  (interactive)
  (if (not (string-equal mode-name "Summary"))
      (if (string-equal mode-name "Article")
	  (gnus-article-show-summary)
	(error "You must be in a gnus buffer with a selected message to execute this command")))
    (gnus-summary-toggle-header 1)
    (other-window 1)
    (goto-char (point-min))
    (if (not (re-search-forward "Xref: " nil t))
	(if (not (re-search-forward "[Aa]rticle-[Nn]umber: " nil t))
	    (error "Couldn't find article number.  Try selecting message in the Summary buffer.")))
    (end-of-line)
    (let ((temp-point (point)))
      (search-backward ":")
      (forward-char)
      (copy-to-register 'o temp-point (point)))
  (let ((organizer-temp-gnus-current-group (car gnus-article-current)))
    (switch-to-buffer "junk-buffer")
    (message "here 1")
    (insert (concat organizer-temp-gnus-current-group "~>" (insert-register 'o))))
  (message "here 2")
    (let ((organizer-temp-string
	   (if (not organizer-running-xemacs)
	       (delete-and-extract-region (point-min) (point-max))
	     (organizer-delete-and-return-region (point-min) (point-max))))
	  (organizer (read-file-name "Organizer file: " organizer-directory)))
      (organizer-show organizer)
      (organizer-page-insert-organizer-item nil organizer-temp-string organizer)
      (kill-buffer "junk-buffer")))

(defun organizer-gnus-insert-linked-organizer-entry-current-article ()
  "Insert an entry into an organizer file with the mark for the folder
name and article numer with the current message.  The program will call the
organizer file and insert an entry with the mark and message already inserted
into the minibuffer.  For instance, a specific message in comp.os.linux.misc
will have the following in the entry:

comp.os.linux.misc~>article_number
Headers and body of the article here"
  (interactive)
  (if (not (string-equal mode-name "Summary"))
      (if (string-equal mode-name "Article")
	  (gnus-article-show-summary)
	(error "You must be in a gnus buffer with a selected message to execute this command")))
  (let ((organizer-temp-gnus-current-group (car gnus-article-current)))
    (gnus-summary-toggle-header 1)
    (other-window 1)
    (goto-char (point-min))
    (if (not (re-search-forward "Xref: " nil t))
	(if (not (re-search-forward "[Aa]rticle-[Nn]umber: " nil t))
	    (error "Couldn't find article number.  Try selecting message in the Summary buffer.")))
    (end-of-line)
    (let ((temp-point (point)))
      (search-backward ":")
      (forward-char)
      (copy-to-register 'o temp-point (point))
      (gnus-summary-toggle-header -1)
      (copy-to-register 'p (point-min) (point-max)))
    (switch-to-buffer "junk-buffer")
    (insert (concat organizer-temp-gnus-current-group "~>" (insert-register 'o)))
    (end-of-line)
    (insert (concat "\n" (insert-register 'p))))
  (goto-char (point-max))
  (beginning-of-line)
  (while (not (equal (point) (point-min)))
    (progn
      (insert " ")
      (previous-line 1)
      (beginning-of-line)))
  (let ((organizer-temp-string
	 (if (not organizer-running-xemacs)
	     (delete-and-extract-region (point-min) (point-max))
	   (organizer-delete-and-return-region (point-min) (point-max))))
	(organizer (read-file-name "Organizer file: " organizer-directory)))
    (organizer-show organizer)
    (organizer-page-insert-organizer-item nil organizer-temp-string organizer t)
    ;;  (exit-minibuffer)
    (kill-buffer "junk-buffer")))

(defun organizer-gnus-kill-ring-save-group ()
  "Save the folder name of the current message to the kill-ring.  Handy
for insertion into todo or diary entries."
  (interactive)
  (if (not (string-equal mode-name "Summary"))
      (if (string-equal mode-name "Article")
	  (gnus-article-show-summary)
	(error "You must be in a gnus buffer with a selected message to execute this command")))
  (let ((organizer-temp-gnus-current-group (car gnus-article-current)))
    (switch-to-buffer "junk-buffer")
    (insert organizer-temp-gnus-current-group))
  (kill-ring-save (point-min) (point-max))
  (kill-buffer "junk-buffer"))

(defun organizer-gnus-kill-ring-save-current-article-number ()

  "Save the mark with the folder name and article numer of the current
message to the kill-ring.  Inserts white space before each line.
Handy for insertion into todo or diary entries.  For instance, a
specific message comp.os.linux.misc will have the following saved in
the kill-ring:

comp.os.linux.misc~>article_numer"
  (interactive)
  (if (not (string-equal mode-name "Summary"))
      (if (string-equal mode-name "Article")
	  (gnus-article-show-summary)
	(error "You must be in a gnus buffer with a selected message to execute this command")))
  (let ((organizer-temp-gnus-current-group (car gnus-article-current)))
    (gnus-summary-toggle-header 1)
    (other-window 1)
    (goto-char (point-min))
    (if (not (re-search-forward "Xref: " nil t))
	(if (not (re-search-forward "[Aa]rticle-[Nn]umber: " nil t))
	    (error "Couldn't find article number.  Try selecting message in the Summary buffer.")))
    (end-of-line)
    (let ((temp-point (point)))
      (search-backward ":")
      (forward-char)
      (copy-to-register 'o temp-point (point))
      (switch-to-buffer "junk-buffer")
      (insert (concat organizer-temp-gnus-current-group "~>" (insert-register 'o))))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer "junk-buffer")))

(defun organizer-gnus-kill-ring-save-current-article ()
  "Save the mark with the folder name and article numer of the current
message to the kill-ring.  Handy for insertion into todo or diary
entries.  For instance, a specific message comp.os.linux.misc will
have the following saved in the kill-ring:

comp.os.linux.misc~>article_number
Headers and body of message"
  (interactive)
  (if (not (string-equal mode-name "Summary"))
      (if (string-equal mode-name "Article")
	  (gnus-article-show-summary)
	(error "You must be in a gnus buffer with a selected message to execute this command")))
  (let ((organizer-temp-gnus-current-group (car gnus-article-current)))
    (gnus-summary-toggle-header 1)
    (other-window 1)
    (goto-char (point-min))
    (if (not (re-search-forward "Xref: " nil t))
		(if (not (re-search-forward "[Aa]rticle-[Nn]umber: " nil t))
		    (error "Couldn't find article number.  Try selecting message in the Summary buffer.")))
    (end-of-line)
    (let ((temp-point (point)))
      (search-backward ":")
      (forward-char)
      (copy-to-register 'o temp-point (point))
      (gnus-summary-toggle-header 1)
      (copy-to-register 'p (point-min) (point-max))
      (switch-to-buffer "junk-buffer")
      (insert (concat organizer-temp-gnus-current-group "~>" (insert-register 'o)))
      (end-of-line)
      (insert (concat "\n" (insert-register 'p))))
    (goto-char (point-max))
    (beginning-of-line)
    (while (not (equal (point) (point-min)))
      (progn
	(insert " ")
	(previous-line 1)
	(beginning-of-line)))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer "junk-buffer")))

(defun organizer-external-mailbox-at-point ()
  "Start a customizable external mail command ('organizer-external-mailbox-command') and open folder name at the cursor.  Requires full path to
mailbox in buffer."
  (interactive)
  ;;    (require 'ffap)
  (let ((filestr (ffap-string-at-point)))
    (shell-command (concat organizer-external-mailbox-command " " filestr))))

(defun organizer-external-mail-at-point ()
  "Start a customizable external mail command ('organizer-external-mail-command') and send mail to the address at the cursor."
  (interactive)
  ;;    (require 'ffap)
  (let ((filestr (ffap-string-at-point)))
    (shell-command (concat organizer-external-mail-command " " filestr))))

(defun organizer-insert-diary-entry (arg)
  "Insert a organizer entry for the date indicated by point.  Prefix arg
will make the entry nonmarking.  Based upon insert-diary-entry but
allows you to chhose the organizer file."
  (interactive "P")
  (setq organizer (read-file-name "Organizer page: " organizer-directory))
  (make-diary-entry (calendar-date-string (calendar-cursor-to-date t) t t)
                    arg organizer))

(defun organizer-telnet-at-point ()
  "Telnet to the server under the cursor."
  (interactive)
  ;;    (require 'ffap)
  (let ((serverstr (ffap-string-at-point)))
    (telnet serverstr)))

(defun organizer-rlogin-at-point ()
  "Rlogin to the server under the cursor"
  (interactive)
  ;;    (require 'ffap)
  (let ((serverstr (ffap-string-at-point)))
    (rlogin serverstr)))

(defun organizer-rsh-at-point ()
  "Rsh to the server under the cursor."
  (interactive)
  ;;    (require 'ffap)
  (let ((serverstr (ffap-string-at-point)))
    (rlogin serverstr)))

(defun organizer-view-diary-entries (arg)
  "Prepare and display a buffer with organizer entries.  Searches the
organizer file and the file named by the 'diary-file' variable for
entries that match ARG days starting with the date indicated by the
cursor position in the displayed three-month calendar."
  (interactive "p")
  (let ((d-file (substitute-in-file-name diary-file)) (o-file (substitute-in-file-name organizer-file)))
    ;;     (if ;; (and
    ;; 	 (and d-file (file-exists-p d-file))
    ;; (and o-file (file-exists-p o-file)))
    ;;         (if ;; (and
    ;; 	     (file-readable-p d-file)
    ;; 	    ;;(file-readable-p o-file))
    (organizer-list-diary-entries (calendar-cursor-to-date t) arg)
    ;;           (error "Diary or organizer file is not readable!"))
    ;;       (error (concat "You don't have a diary or organizer file! " o-file))
    ))
;; )

(defun organizer-hidden-view-other-diary-entries (arg d-file o-file)
  "Prepare and display buffer of diary/organizer entries.  Prompts for
a organizer file name and searches that file and the 'diary-file' or
an alternate diary for entries that match ARG days starting with the
date indicated by the cursor position in the displayed three-month
calendar."
  (interactive
   ;; Make sure any todo file buffer is saved
   (if (fboundp 'todo-show)
       (if (file-exists-p todo-file-do)
	   (progn
	     (todo-show)
	     (set-buffer (find-buffer-visiting todo-file-do))
	     (save-buffer)
	     (todo-quit))))
   (list (cond ((null current-prefix-arg) 1)
               ((listp current-prefix-arg) (car current-prefix-arg))
               (t current-prefix-arg))
	 (read-file-name "Enter diary file name: " default-directory nil t)))
  (let ((diary-file d-file) (organizer-file o-file))
    (organizer-view-diary-entries arg)))

(defun organizer-startup-organizer-files ()
  "Start up 'organizer-startup-list' organizer files upon start of (X)Emacs"
  (interactive)
  (setq organizer-file nil)
  (calendar)
  (setq organizer-startup-temp organizer-startup-list)
  (setq organizer-file (car organizer-startup-temp))
  (while organizer-file
    (setq organizer-startup-temp (cdr organizer-startup-temp))
    (let ((filestr (concat organizer-directory organizer-file)))
      (organizer-view-other-diary-entries filestr organizer-file))
    (setq organizer-file (car organizer-startup-temp)))
  ;; (setq organizer-startup-list organizer-startup-temp)
  (other-window -1)
  (shrink-window-if-larger-than-buffer)
  (setq organizer-file nil))

(defun organizer-list-subdirectories ()
  "List 'organizer-directory' and subdirectories in buffer"
  (interactive)
  (goto-char (point-min))
  (re-search-forward dired-re-dir nil t)
  (dired-move-to-filename)		; user may type `i' or `f'
  (re-search-forward dired-re-dir nil t)
  (dired-move-to-filename)		; user may type `i' or `f'
  (setq ind 1)
  (setq oldpoint (point))
  (re-search-forward dired-re-dir nil t)
  (dired-move-to-filename)		; user may type `i' or `f'
  (setq newpoint (point))
  (if (equal newpoint oldpoint)
      (setq ind 0))
  (while (equal ind 1)
    (dired-insert-subdir (dired-get-filename))
    (goto-char newpoint)
    (setq oldpoint (point))
    (re-search-forward dired-re-dir nil t)
    (dired-move-to-filename)		; user may type `i' or `f'
    (setq newpoint (point))
    (if (equal newpoint oldpoint)
	(setq ind 0))))

(defun organizer-mark-files ()
  "Mark files in dired buffer excluding backup files and directories"
  (interactive)
  (dired-mark-directories nil)
  (dired-mark-files-regexp ".*~")
  (dired-do-toggle)
  )

(defun organizer-search-organizer-string (f-buff t-buff o-buff)
  "Search files for organizer filename"
  ;; indicates o-buff string re-search failure
  (interactive)
  (setq ind 1)
  (setq ind_two 1)
  (if (search-forward (concat "/* " o-buff " */") nil t)
      (progn
	(setq ind 1)
	(setq ind_two 1))
    (progn
      (setq ind 0)
      (setq ind_two 0)))
  (while (equal ind 1)
    (if (re-search-backward "^[^\n\t ]" nil t) (setq ind 1) (setq ind 0))
    (let ((beg (point)))
      (if (search-forward (concat "/* " o-buff " */") nil t) (setq ind 1) (setq ind 0))
      (re-search-forward "^[^\n\t ]" nil t)
      (copy-to-register 'o beg (point))
      (set-buffer t-buff)
      (insert-string (concat (insert-register 'o t) "\n"))
      (set-buffer f-buff)
      (if (search-forward (concat "/* " o-buff " */") nil t) (setq ind 1) (setq ind 0))))
  (if (equal ind_two 1)
      (progn
	(set-buffer t-buff)
	(insert-string (concat organizer-prefix organizer-category-sep "\n"))
	(set-buffer f-buff))))

(defun organizer-search-todo-done-for-organizer-string (f-buff t-buff o-buff)
  "Search 'todo-file-done' for organizer filename"
  ;; indicates o-buff string re-search failure
  (interactive)
  (setq ind 1)
  (setq ind_two 0)
  (if (search-forward (concat "/* " o-buff " */") nil t)
      (progn
	(setq ind 1)
	(setq ind_two 1))
    (progn
      (setq ind 0)
      (setq ind_two 0)))
  (if (equal ind 1)
      (progn
	(set-buffer t-buff)
	(if (not organizer-display-todo-done-by-date)
	    (insert-string (concat organizer-prefix " ----Todo Done\n")))
	(set-buffer f-buff)))
  (while (equal ind 1)
    (if (re-search-backward "^[^\t ]" nil t) (setq ind 1) (setq ind 0))
    (let ((beg (point)))
      (if (search-forward (concat "/* " o-buff " */") nil t) (setq ind 1) (setq ind 0))
      (re-search-forward "^[^\t ]" nil t)
      (copy-to-register 'o beg (point))
      (set-buffer t-buff)
      (if (not organizer-display-todo-done-by-date)
	  (insert-string (concat organizer-prefix " ")))
      (insert-string (concat (insert-register 'o t) "\n"))
      (set-buffer f-buff)
      (if (search-forward (concat "/* " o-buff " */") nil t) (setq ind 1) (setq ind 0))))
  (if (equal ind_two 1)
      (progn
	(set-buffer t-buff)
	(if (not organizer-display-todo-done-by-date)
	    (insert-string (concat organizer-prefix organizer-category-sep "\n")))
	(set-buffer f-buff))))

(defun organizer-search-organizer-for-organizer-string (f-buff t-buff o-buff)
  "Search organizer files for organizer filename"
  ;; indicates o-buff string re-search failure
  (interactive)
  (setq ind 1)
  (setq i 1)
  (if (search-forward (concat "/* " o-buff " */") nil t)
      (progn
	(setq ind 1)
	(setq i 1))
    (progn
      (setq ind 0)
      (setq i 0)))
  (if (equal i 1)
      (progn
	(set-buffer t-buff)
	(insert-string (concat organizer-prefix "--- [" f-buff "]\n"))
	(set-buffer f-buff)))
  (while (equal ind 1)
    (if (re-search-backward "^[^\n\t ]" nil t) (setq ind 1) (setq ind 0))
    (let ((beg (point)))
      (if (search-forward (concat "/* " o-buff " */") nil t) (setq ind 1) (setq ind 0))
      (re-search-forward "^[^\n\t ]" nil t)
      ;;     (end-of-line)
      (copy-to-register 'o beg (point))
      (set-buffer t-buff)
      (insert-string (concat (insert-register 'o t) "\n"))
      ;;    (yank)
      ;;    (insert-string (concat " (" f-buff ")\n"))
      ;;    (insert-string "\n")
      (set-buffer f-buff)
      (if (search-forward (concat "/* " o-buff " */") nil t) (setq ind 1) (setq ind 0))))
  (if (equal i 1)
      (progn
	(set-buffer t-buff)
	(insert-string (concat organizer-prefix organizer-category-sep "\n"))
	(set-buffer f-buff))))

(defun organizer-search-bbdb-organizer-string (bbdb-buff t-buff o-buff)
  "Search 'bbdb-file' for organizer filename"
  ;; indicates o-buff string re-search failure
  (interactive)
  (if (fboundp 'bbdb)
      (progn
	(setq ind 1)
	(setq i 1)
	(if (search-forward "\[" nil t) (setq ind 1) (setq ind 0))
	(backward-char)
	(setq beg (point))
	(if (search-forward (concat "/* " o-buff " */") nil t)
	    (progn
	      (setq ind 1)
	      (setq i 1))
	  (progn
	    (setq ind 0)
	    (setq i 0)))
	(if (equal i 1)
	    (progn
	      (set-buffer t-buff)
	      (insert-string (concat organizer-prefix "--- BBDB\n"))
	      (set-buffer bbdb-buff)))
	(while (equal ind 1)
	  (beginning-of-line)
	  (copy-to-register 'o beg (point))
	  (delete-region beg (point))
	  ;;	(kill-region beg (point))
	  (end-of-line)
	  (insert-string "\n")
	  (setq beg (point))
	  (if (search-forward (concat "/* " o-buff " */") nil t) (setq ind 1) (setq ind 0)))
	(copy-to-register 'o beg (point-max))
	(delete-region beg (point-max))
	;;       (kill-region beg (point-max))
	(backward-delete-char-untabify 1)
	(save-buffer)
	(bbdb ".*" nil)
	(set-buffer bbdb-buffer-name)
	(goto-char (point-min))
	(setq beg (point))
	(setq ind_two 1)
	(if (re-search-forward "^[^\t ]" nil t) (setq ind_two 1) (setq ind_two 0))
	(goto-char (point-min))
	(setq ind 1)
	(if (re-search-forward "^[^\t ]" nil t) (setq ind 1) (setq ind 0))
	(if (re-search-forward "^[^\t ]" nil t) (setq ind 1) (setq ind 0))
	(while (equal ind 1)
	  (backward-char)
	  (copy-to-register 'o beg (point))
	  ;;	(kill-ring-save beg (point))
	  (setq beg (point))
	  (set-buffer t-buff)
	  (if organizer-running-xemacs
	      (insert-string " ")
	    (insert-string (concat organizer-prefix " ")))
	  ;;	(yank)
	  (if organizer-running-xemacs
	      (insert-string (concat (insert-register 'o t) " "))
	    (insert-string (concat (insert-register 'o t) "\n")))
	  (goto-char (point-max))
	  ;;	(insert-string "\n")
	  (set-buffer bbdb-buffer-name)
	  (if (re-search-forward "^[^\t ]" nil t) (setq ind 1) (setq ind 0))
	  (if (re-search-forward "^[^\t ]" nil t) (setq ind 1) (setq ind 0)))
	(if (equal ind_two 1)
	    (progn
	      (backward-char)
	      (copy-to-register 'o beg (point-max))
	      ;; 	    (kill-ring-save beg (point-max))
	      (set-buffer t-buff)
	      (if organizer-running-xemacs
		  (insert-string " ")
		(insert-string (concat organizer-prefix " ")))
	      ;;	    (yank)
	      (if organizer-running-xemacs
		  (insert-string (concat (insert-register 'o t) " "))
		(insert-string (concat (insert-register 'o t) "\n")))
	      (goto-char (point-max))
	      (set-buffer bbdb-buffer-name)))
	(if (equal i 1)
	    (progn
	      (set-buffer t-buff)
	      (goto-char (point-max))
	      (if organizer-running-xemacs
		  (insert-string (concat organizer-category-sep "\n"))
		(insert-string (concat  organizer-prefix organizer-category-sep "\n")))
	      (goto-char (point-max))
	      (set-buffer bbdb-buffer-name)))
	(bbdb-bury-buffer)
	)))

(defun organizer-search-bbdb-files (t-buff o-buff)
  "Search bbdb files for commented filename string of current organizer."
  (interactive)
  (if (boundp 'bbdb-file)
      (progn
	(bbdb-save-db)
	(find-file bbdb-file)
	(save-buffer)
	(kill-this-buffer)
	(find-file bbdb-file)
	(setq organizer-bbdb-filename-temp bbdb-file)
	(write-file "/tmp/bbdb-temp")
	(setq bbdb-file "/tmp/bbdb-temp")
	(goto-char (point-min))
	;; indicates whether any entries were found
	(setq indic 1)
	(if (search-forward (concat "/* " o-buff " */") nil t) (setq indic 1) (setq indic 0))
	(goto-char (point-min))
	(setq bbdb-buff (buffer-name))
	(organizer-search-bbdb-organizer-string bbdb-buff t-buff o-buff)
	(set-buffer bbdb-buff)
	(save-buffer bbdb-buff)
	(kill-this-buffer)
	(if (file-exists-p "/tmp/bbdb-temp")
	    (delete-file "/tmp/bbdb-temp"))
	(setq bbdb-file organizer-bbdb-filename-temp)
	;; Insert line if any entries are found
	(if (equal indic 1)
	    (set-buffer t-buff))
	(if (equal indic 1)
	    (if organizer-running-xemacs
		(insert-string (concat organizer-category-sep "\n"))
	      (insert-string (concat organizer-prefix organizer-category-sep "\n")))))))

(defun organizer-search-organizer-files (t-buff o-buff)
  "Search other organizer files for commented filename string of current organizer."
  (interactive)
  (find-file organizer-directory)
  (organizer-list-subdirectories)
  (organizer-mark-files)
  (goto-char (point-min))
  ;; indicates when re-search fails
  (setq indicat 1)
  ;; indicates whether any entries were found
  (setq indic 1)
  (let ((org-buff (buffer-name)))
    (setq oldpoint (point))
    (re-search-forward dired-re-mark nil t)
    (dired-move-to-filename)
    (setq newpoint (point))
    (if (equal newpoint oldpoint)
	;; organizer directory re-search failure
	(progn
	  (setq indicat 0)
	  ;; no entries found
	  (setq indic 0)))
    (while (equal indicat 1)
      (find-file (dired-get-filename))
      (setq f-buff (buffer-name))
      (organizer-search-organizer-for-organizer-string f-buff t-buff o-buff)
      (kill-buffer f-buff)
      (switch-to-buffer org-buff)
      (setq oldpoint (point))
      (re-search-forward dired-re-mark nil t)
      (dired-move-to-filename)
      (setq newpoint (point))
      ;; organizer directory re-search failure
      (if (equal newpoint oldpoint)
	  (progn
	    (setq indicat 0)
	    ;; 	  (kill-this-buffer)
	    )))
    (kill-buffer org-buff))
  ;; Insert line if any entries are found
  (if (equal indic 1)
      (set-buffer t-buff))
  (if (equal indic 1)
      (insert-string (concat organizer-prefix organizer-category-sep "\n"))))

(defun organizer-list-diary-entries (date number)
  "Create and display a buffer containing the relevant lines in `diary-file'.
The arguments are DATE and NUMBER; the entries selected are those
for NUMBER days starting with date DATE.  The other entries are hidden
using selective display.

Returns a list of all relevant diary/organizer entries found, if any, in order by date.
The list entries have the form ((month day year) string specifier) where
\(month day year) is the date of the entry, string is the entry text, and
specifier is the applicability.  If the variable `diary-list-include-blanks'
is t, this list includes a dummy diary entry consisting of the empty string)
for a date with no diary entries.

After the list is prepared, the hooks `nongregorian-diary-listing-hook',
`organizer-list-diary-entries-hook', `organizer-diary-display-hook', and `diary-hook' are run.
These hooks have the following distinct roles:

    `nongregorian-diary-listing-hook' can cull dates from the diary
        and each included file.  Usually used for Hebrew or Islamic
        diary entries in files.  Applied to *each* file.

    `organizer-list-diary-entries-hook' adds or manipulates diary entries from
        external sources.  Used, for example, to include diary entries
        from other files or to sort the diary entries.  Invoked *once* only,
        before the display hook is run.

    `organizer-diary-display-hook' does the actual display of information.  If this is
        nil, `simple-diary-display' will be used.  Use `add-hook' to set this to
        `fancy-diary-display', if desired.  If you want no diary display, use
        `add-hook' to set this to ignore.

    `diary-hook' is run last.  This can be used for an appointment
        notification function."
  (organizer-srchfn)
  (if (< 0 number)
      (let* ((original-date date) ;; save for possible use in the hooks
             (old-diary-syntax-table)
             (diary-entries-list)
             (date-string (calendar-date-string date))
             (o-file (substitute-in-file-name organizer-file))
	     (diary-file organizer-diary-file-temp)
             (d-file (substitute-in-file-name diary-file)))
        (message "Preparing diary...")
        (save-excursion
	  (find-file d-file)
	  (let ((diary-buffer (find-buffer-visiting d-file)))
	    (find-file o-file)
	    (setq o-buff (buffer-name))
	    (append-to-buffer diary-buffer (point-min) (point-max))
	    (goto-char (point-min))
	    (kill-buffer o-buff)
	    (set-buffer diary-buffer)
	    (or (verify-visited-file-modtime diary-buffer)
		(revert-buffer t t)))
          (setq selective-display t)
          (setq selective-display-ellipses nil)
          (setq old-diary-syntax-table (syntax-table))
          (set-syntax-table diary-syntax-table)
          (unwind-protect
	      (let ((buffer-read-only nil)
		    (diary-modified (buffer-modified-p))
		    (mark (regexp-quote diary-nonmarking-symbol)))
		;; First and last characters must be ^M or \n for
		;; selective display to work properly
		(goto-char (1- (point-max)))
		(if (not (looking-at "\^M\\|\n"))
		    (progn
		      (goto-char (point-max))
		      (insert-string "\^M")))
		(goto-char (point-min))
		(if (not (looking-at "\^M\\|\n"))
		    (insert-string "\^M"))
		(subst-char-in-region (point-min) (point-max) ?\n ?\^M t)
		(calendar-for-loop i from 1 to number do
				   (let ((d diary-date-forms)
					 (month (extract-calendar-month date))
					 (day (extract-calendar-day date))
					 (year (extract-calendar-year date))
					 (entry-found (list-sexp-diary-entries date)))
				     (while d
				       (let*
					   ((date-form (if (equal (car (car d)) 'backup)
							   (cdr (car d))
							 (car d)))
					    (backup (equal (car (car d)) 'backup))
					    (dayname
					     (concat
					      (calendar-day-name date) "\\|"
					      (substring (calendar-day-name date) 0 3) ".?"))
					    (monthname
					     (concat
					      "\\*\\|"
					      (calendar-month-name month) "\\|"
					      (substring (calendar-month-name month) 0 3) ".?"))
					    (month (concat "\\*\\|0*" (int-to-string month)))
					    (day (concat "\\*\\|0*" (int-to-string day)))
					    (year
					     (concat
					      "\\*\\|0*" (int-to-string year)
					      (if abbreviated-calendar-year
						  (concat "\\|" (format "%02d" (% year 100)))
						"")))
					    (regexp
					     (concat
					      "\\(\\`\\|\^M\\|\n\\)" mark "?\\("
					      (mapconcat 'eval date-form "\\)\\(")
					      "\\)"))
					    (case-fold-search t))
					 (goto-char (point-min))
					 (while (re-search-forward regexp nil t)
					   (if backup (re-search-backward "\\<" nil t))
					   (if (and (or (char-equal (preceding-char) ?\^M)
							(char-equal (preceding-char) ?\n))
						    (not (looking-at " \\|\^I")))
					       ;;  Diary entry that consists only of date.
					       (backward-char 1)
					     ;; Found a nonempty diary entry--make it visible and
					     ;; add it to the list.
					     (setq entry-found t)
					     (let ((entry-start (point))
						   (date-start))
					       (re-search-backward "\^M\\|\n\\|\\`")
					       (setq date-start (point))
					       (re-search-forward "\^M\\|\n" nil t 2)
					       (while (looking-at " \\|\^I")
						 (re-search-forward "\^M\\|\n" nil t))
					       (backward-char 1)
					       (subst-char-in-region date-start
								     (point) ?\^M ?\n t)
					       (add-to-diary-list
						date
						(buffer-substring-no-properties
						 entry-start (point))
						(buffer-substring-no-properties
						 (1+ date-start) (1- entry-start)))))))
				       (setq d (cdr d)))
				     (or entry-found
					 (not diary-list-include-blanks)
					 (setq diary-entries-list
					       (append diary-entries-list
						       (list (list date "" "")))))
				     (setq date
					   (calendar-gregorian-from-absolute
					    (1+ (calendar-absolute-from-gregorian date))))
				     (setq entry-found nil)))
		(set-buffer-modified-p diary-modified))
	    (set-syntax-table old-diary-syntax-table))
	  (goto-char (point-min))
	  (run-hooks 'nongregorian-diary-listing-hook
		     'organizer-list-diary-entries-hook)
	  (if organizer-diary-display-hook
	      (run-hooks 'organizer-diary-display-hook)
	    (simple-diary-display))
	  (run-hooks 'organizer-diary-hook)
	  diary-entries-list
	  (set-buffer (find-buffer-visiting d-file))
	  (save-buffer)
	  (kill-this-buffer)
 	  (if (file-exists-p organizer-diary-file-temp)
 	      (delete-file organizer-diary-file-temp))

	  ;;	  (if (file-exists-p (concat organizer-diary-file-temp "~"))
	  ;;	      (delete-file (concat organizer-diary-file-temp "~")))
	  (switch-to-buffer calendar-buffer)
	  (other-window 2)
	  ))
    ))

(defun organizer-srchfn ()
  "Used internally by 'organizer-mode.el' to search diary, todo files
and, eventually, others for entries relevant to the current organizer
file and inserts them into a temporary buffer.  Relevant files are
defines as those with contain the name of the current organizer file."
  (setq d-file (substitute-in-file-name diary-file))
  (find-file d-file)
  (let ((d-buff (buffer-name)))
    (setq t-file organizer-diary-file-temp)
    (if (fboundp 'todo-show)
	(progn
	  (if (file-exists-p todo-file-do)
	      ;; This is just to make sur the todo file is saved before going on.
	      (progn
		(todo-show)
		(set-buffer (find-buffer-visiting todo-file-do))
		(save-buffer)
		(todo-quit)
		(setq to-file todo-file-do)
		(find-file to-file)
		(copy-to-register 'o (point-min) (point-max))
		;;		(kill-ring-save (point-min) (point-max))
		(kill-this-buffer)
		(switch-to-buffer "todo-temp")
		(insert-string (concat (insert-register 'o t) "\n"))
		;;		(yank)
		(goto-char (point-max))
		(insert-string "\njunk string\n")
		(beginning-of-buffer)
		(setq to-buff (buffer-name))))
	  (if (file-exists-p todo-file-done)
	      (progn
		(setq todn-file todo-file-done)
		(find-file todn-file)
		(copy-to-register 'o (point-min) (point-max))
		;;		(kill-ring-save (point-min) (point-max))
		(kill-this-buffer)
		(switch-to-buffer "todn-temp")
		(insert-string (concat (insert-register 'o t) "\n"))
		;;		(yank)
		(goto-char (point-max))
		(insert-string "\njunk string\n")
		(beginning-of-buffer)
		(message "todn1")
		(setq todn-buff (buffer-name))))))
    (find-file organizer-file)
    (setq o-buff (file-name-nondirectory organizer-file))
    (kill-buffer o-buff)
    (switch-to-buffer (concat "*" o-buff "*"))
    (kill-buffer (concat "*" o-buff "*"))
    (find-file t-file)
    (let ((t-buff (buffer-name)))
      (organizer-search-organizer-files t-buff o-buff)
      (organizer-search-bbdb-files t-buff o-buff)
      (set-buffer d-buff)
      (organizer-search-organizer-string d-buff t-buff o-buff)
      (if (fboundp 'todo-show)
	  (progn
	    (if (file-exists-p todo-file-done)
		(progn
		  (switch-to-buffer todn-buff)
		  (organizer-search-todo-done-for-organizer-string todn-buff t-buff o-buff)))
	    (if (file-exists-p todo-file-do)
		(progn
		  (set-buffer to-buff)
		  (organizer-search-organizer-string to-buff t-buff o-buff)))))
      (set-buffer t-buff)
      (save-buffer t-buff)
      ;;        (kill-buffer t-buff)
      ;;        (kill-buffer d-buff)
      (kill-this-buffer)
      (set-buffer d-buff)
      (kill-this-buffer)
      (if (fboundp 'todo-show)
	  (progn
	    (if (file-exists-p todo-file-do)
		(progn
		  (set-buffer to-buff)
		  (kill-this-buffer)))
	    (if (file-exists-p todo-file-done)
		(progn
		  (message "todn4")
		  (set-buffer todn-buff)
		  (kill-this-buffer)))))
      )))

(defun organizer-speedbar-item-load ()
  "Load the item under the cursor or mouse if it is a Lisp file."
  (interactive)
  (let ((f (speedbar-line-file)))
    (other-frame 1)
    (if (file-exists-p f)
	(progn
	  (organizer-view-other-diary-entries f)
	  (other-window -1)
	  (shrink-window-if-larger-than-buffer)
	  (other-window 1))
      (error "Not a loadable file"))))

(defun organizer-speedbar ()
  "Load start the speedbar in the 'organizer-directory'.  Experimental"
  (interactive)
  (if organizer-enable-speedbar
      (progn
	(setq default-directory organizer-directory)
	(speedbar 1))))

(defun organizer-kill-ring-save-link-mark ()
  "Copies the proper mark into the kill-ring so that the user
can easily insert it into any new entry for linking."
  (interactive)
  (setq okrslm_ind 0)
  (if (not (or (equal major-mode 'organizer-mode) (equal major-mode 'organizer-page-mode)))
      (progn
	(setq okrslm_ind 1)
	(organizer-show)))
  (let ((str (buffer-name)))
    (if (equal major-mode 'organizer-page-mode)
	(setq str (substring (buffer-name) 1 -1)))
    (let ((o-buff-str (concat " /* " str " */")))
      (switch-to-buffer "junk_buffer")
      (insert-string o-buff-str)
      (setq end_point (point))
      (beginning-of-line)
      (kill-ring-save (point) end_point)
      (kill-this-buffer)))
  (if (= okrslm_ind 1)
      (kill-this-buffer)))

(defun organizer-insert-linked-todo-item ()
  "Insert a todo item linked to the current page or file.  The
link-mark is inserted automatically into the minibuffer"
  ;;  Decided I didn't like this behavior.
  ;; "The
  ;; function copies the proper mark into the kill-ring so that the user
  ;; can easily insert it into the new entry."
  (interactive)
  (if (fboundp 'todo-show)
      (if (file-exists-p todo-file-do)
	  (progn
	    (if (equal major-mode 'organizer-mode)
		(setq str (buffer-name))
	      (if (equal major-mode 'organizer-page-mode)
		  (setq str (substring (buffer-name) 1 -1))
		(progn
		  (organizer-show)
		  (setq str (buffer-name)))))
	    (let ((o-buff-str (concat " /* " str " */")))
	      ;;  Decided I didn't like this behavior.
	      ;; 		(switch-to-buffer "junk_buffer")
	      ;; 		(insert-string o-buff-str)
	      ;; 		(setq end_point (point))
	      ;; 		(beginning-of-line)
	      ;; 		(kill-ring-save (point) end_point)
	      ;; 		(kill-this-buffer)
	      (organizer-todo-insert-item nil o-buff-str))
	    (todo-save))
	(error (concat "You don't have a " todo-file-do " file")))
    (error "You don't have todo-mode loaded")))

(defun organizer-todo-insert-item (arg o-buff-str)
  "Insert new TODO list entry.  With a prefix argument solicit the
category, otherwise use the current category. This function is
specifically designed to insert entries from a todo page with the
link-mark for the page inserted into the entry.  The link-mark is
inserted automatically into the minibuffer."
;;  (interactive "P")
  (if (fboundp 'todo-show)
      (save-excursion
	(if (not (string-equal mode-name "TODO")) (todo-show))
	(let* ((new-item (concat todo-prefix " "
				 (read-from-minibuffer "New TODO entry: "
						       (concat
							(if todo-entry-prefix-function
							    (funcall todo-entry-prefix-function))
							o-buff-str))))
	       (categories todo-categories)
	       (history (cons 'categories (1+ todo-category-number)))
	       (current-category (nth todo-category-number todo-categories))
	       (category
		(if arg
		    current-category
		  (completing-read (concat "Category [" current-category "]: ")
				   (todo-category-alist) nil nil nil
				   history current-category))))
	  (todo-add-item-non-interactively new-item category)))))

(defun organizer-page-insert-organizer-item (arg o-buff-str organizer &optional insert-item)
  "Insert new ORGANIZER list entry.  With a prefix argument solicit
the category, otherwise use the current category. This function is
specifically designed to insert entries from an organizer page to
another organizer file with the link-mark for the page inserted into
the entry.  The link-mark is inserted automatically into the
minibuffer. If insert-item is non-nil, go ahead and insert item without prompting."
;;  (interactive "P")
  (save-excursion
    (if (not (string-equal mode-name "ORGANIZER")) (organizer-show organizer))
    (let* ((new-item " ")
	   (categories organizer-categories)
	   (history (cons 'categories (1+ organizer-category-number)))
	   (current-category (nth organizer-category-number organizer-categories))
	   (category
	    (if arg
		current-category
	      (completing-read (concat "Category [" current-category "]: ")
			       (organizer-category-alist) nil nil nil
			       history current-category))))
      (if (not (equal insert-item nil))
	  (setq new-item (concat organizer-prefix " " o-buff-str))
	(setq new-item (concat organizer-prefix " "
			       (read-from-minibuffer "New ORGANIZER entry: "
						     (concat
						      (if organizer-entry-prefix-function
							  (funcall organizer-entry-prefix-function))
						      o-buff-str)))))
      (organizer-add-item-non-interactively organizer new-item category))))

(defun organizer-insert-linked-organizer-item ()
  "Insert an organizer item linked to the current page or file.  The
link-mark is inserted automatically into the minibuffer."

  ;;  Decided I didn't like this behavior:
  ;;   The function
  ;; also copies the proper mark into the kill-ring so that the user can
  ;; easily insert it into other new entries.

  (interactive)
  (let ((str (buffer-name)))
    (if (equal major-mode 'organizer-page-mode)
	(setq str (substring (buffer-name) 1 -1))
      (if (equal major-mode 'organizer-mode)
	  (setq str (buffer-name))
	(progn
	  (organizer-show)
	  (setq str (buffer-name)))))
    ;;	  (error "You need to be looking at an organizer file or page to do this.")))
    (let ((o-buff-str (concat " /* " str " */")))
      ;;  Decided I didn't like this behavior.
      ;;       (switch-to-buffer "junk_buffer")
      ;;       (insert-string o-buff-str)
      ;;       (setq end_point (point))
      ;;       (beginning-of-line)
      ;;       (kill-ring-save (point) end_point)
      ;;       (kill-this-buffer)
      (setq organizer (read-file-name "Organizer file: " organizer-directory))
      (organizer-show str)
      (organizer-quit)
      (organizer-page-insert-organizer-item nil o-buff-str organizer))))

(defun organizer-insert-linked-diary-entry ()
  "Insert a diary entry linked to the current page or file."
  (interactive)
  (let ((str (buffer-name)))
    (if (equal major-mode 'organizer-page-mode)
	(setq str (substring (buffer-name) 1 -1))
      (if (equal major-mode 'organizer-mode)
	  (setq str (buffer-name))
	(progn
	  (organizer-show)
	  (setq str (buffer-name)))))
    ;;	  (error "You need to be looking at an organizer file or page to do this.")))
    (let ((o-buff-str (concat " /* " str " */")))
      (if (not (get-buffer calendar-buffer))
	  (progn
	    (calendar)
	    (shrink-window-if-larger-than-buffer))
	(progn
	  (set-buffer calendar-buffer)
	  (shrink-window-if-larger-than-buffer)))
      (insert-diary-entry nil)
      (insert-string o-buff-str))))

(defun organizer-insert-linked-mailbox-entry ()
  "Insert a dired entry linked to the current organizer file or its page."
  (interactive)
  (let ((str (buffer-name)))
    (if (equal major-mode 'organizer-page-mode)
	(setq str (substring (buffer-name) 1 -1))
      (if (equal major-mode 'organizer-mode)
	  (setq str (buffer-name))
	(progn
	  (organizer-show)
	  (setq str (buffer-name)))))
    ;;	  (error "You need to be looking at an organizer file or page to do this.")))
    (let ((o-buff-str (concat " /* " str " */")))
      (organizer-insert-mailbox-item nil o-buff-str str)))
  )

(defun organizer-insert-mailbox-item (arg o-buff-str organizer)
  "Insert new ORGANIZER list entry.  With a prefix argument solicit
the category, otherwise use the current category. This function is
specifically designed to insert entries from an organizer page to
another organizer file with the link-mark for the page inserted into
the entry.  The link-mark is inserted automatically into the
minibuffer."
;;  (interactive "P")
  (setq new-item (concat organizer-prefix " " (read-file-name "Mailbox directory or file: " organizer-mail-folder-directory)))
  (save-excursion
    (if (not (string-equal mode-name "ORGANIZER")) (organizer-show organizer))
    (let* ((categories organizer-categories)
	   (history (cons 'categories (1+ organizer-category-number)))
	   (current-category (nth organizer-category-number organizer-categories))
	   (category
	    (if arg
		current-category
	      (completing-read (concat "Category [" current-category "]: ")
			       (organizer-category-alist) nil nil nil
			       history current-category))))
      (organizer-add-item-non-interactively organizer new-item category))))

(defun organizer-insert-linked-dired-entry ()
  "Insert a dired entry linked to the current organizer file or its page."
  (interactive)
  (let ((str (buffer-name)))
    (if (equal major-mode 'organizer-page-mode)
	(setq str (substring (buffer-name) 1 -1))
      (if (equal major-mode 'organizer-mode)
	  (setq str (buffer-name))
	(progn
	  (organizer-show)
	  (setq str (buffer-name)))))
    ;;	  (error "You need to be looking at an organizer file or page to do this.")))
    (let ((o-buff-str (concat " /* " str " */")))
      (organizer-insert-dired-item nil o-buff-str str nil)))
  )

(defun organizer-insert-linked-dired-entry-under-cursor ()
  "Link dired file or directory under the cursor to an organizer file
or its page.  Use from dired only."
  (interactive)
  (let ((organizer-junk-string (car (dired-get-marked-files))))
    (organizer-show)
    (let ((str (buffer-name)))
      ;;	  (error "You need to be looking at an organizer file or page to do this.")))
      (let ((o-buff-str (concat " /* " str " */")))
	(organizer-insert-dired-item nil o-buff-str str organizer-junk-string)))))

(defun organizer-insert-dired-item (arg o-buff-str organizer new-item-no-prefix)
  "Insert new ORGANIZER list entry.  With a prefix argument solicit
the category, otherwise use the current category. This function is
specifically designed to insert entries from an organizer page to
another organizer file with the link-mark for the page inserted into
the entry.  The link-mark is inserted automatically into the
minibuffer."
;;  (interactive "P")
  (if (not (equal new-item-no-prefix nil))
      (if (not (stringp new-item-no-prefix))
	  (setq new-item (concat organizer-prefix " " (read-file-name "Directory or file: " "~/")))
	(setq new-item (concat organizer-prefix " " new-item-no-prefix)))
    (setq new-item (concat organizer-prefix " " (read-file-name "Directory or file: " "~/"))))
  (save-excursion
    (if (string-equal mode-name "ORGANIZER-PAGE") (organizer-edit-this-page)
      (if (not (string-equal mode-name "ORGANIZER")) (organizer-show)))
    (let* ((organizer buffer-file-name)
	   (categories organizer-categories)
	   (history (cons 'categories (1+ organizer-category-number)))
	   (current-category (nth organizer-category-number organizer-categories))
	   (category
	    (if arg
		current-category
	      (completing-read (concat "Category [" current-category "]: ")
			       (organizer-category-alist) nil nil nil
			       history current-category))))
      (organizer-save)
      (message new-item)
      (organizer-add-item-non-interactively organizer new-item category))))

(defun organizer-insert-linked-bbdb-item ()
  "Insert an link mark into the notes field of bbdb item."
  (interactive)
  (if (and (fboundp 'bbdb) (file-exists-p bbdb-file))
      (let ((str (buffer-name)))
	(if (equal major-mode 'organizer-mode)
	    (setq str (buffer-name))
	  (if (equal major-mode 'organizer-page-mode)
	      (setq str (substring (buffer-name) 1 -1))
	    (progn
	      (organizer-show)
	      (setq str (buffer-name)))))
	(let ((o-buff-str (concat " /* " str " */")))
	  ;; 	  (switch-to-buffer "junk_buffer")
	  ;; 	  (insert-string o-buff-str)
	  ;; 	  (setq end_point (point))
	  ;; 	  (beginning-of-line)
	  ;; 	  (kill-ring-save (point) end_point)
	  ;; 	  (kill-this-buffer)
	  (setq organizer-bbdb-name (read-from-minibuffer "Search records matching regexp: "))
	  (bbdb organizer-bbdb-name nil)
	  (other-window 1)
	  (organizer-bbdb-record-edit-notes (bbdb-current-record) o-buff-str)
	  (bbdb organizer-bbdb-name nil)))
    (error "bbdb is either not loaded or I can't find the database file.")))
;;      (organizer-bbdb-record-edit-notes))))

(defun organizer-bbdb-record-edit-notes (bbdb-record o-buff-str &optional regrind)
  ;;  (interactive (list (bbdb-current-record t) t))
  (interactive)
  (if (and (fboundp 'bbdb) (file-exists-p bbdb-file))
      (progn
	(let ((notes (bbdb-read-string "Notes: " (concat (bbdb-record-notes bbdb-record) " " o-buff-str))))
	  (bbdb-record-set-notes bbdb-record (if (string= "" notes) nil notes)))
	(if regrind
	    (save-excursion
	      (set-buffer bbdb-buffer-name)
	      (bbdb-redisplay-one-record bbdb-record))))
    (error "bbdb is either not loaded or I can't find the data fille for it."))
  nil)

(defun organizer-first-run ()
  "Run this when program hasn't been setup, yet."
  (dired-create-directory organizer-directory)
  )

(defun organizer-move-entry-to-another-category ()
  "Move entry from old category to new category.  If new category doesn't exist, create it."
  (interactive)
  (setq organizer-real-file buffer-file-truename)
  ;; Now write the file to a temp name so we can safely edit it.
  (write-file "/tmp/delete-me")
  (beginning-of-line)
  (setq old-entry-begin (point))
  (re-search-forward "^[^\t ]" nil t)
  (if (not (re-search-forward "^[^\t ]" nil t))
      (goto-char (point-max))
    (backward-char))
  (backward-char)
  (copy-to-register 'o old-entry-begin (point) t)
  (delete-char 1)
  (setq organizer buffer-file-truename)
  (save-buffer)
  (kill-buffer (current-buffer))
  (find-file organizer)
  (goto-char (point-min))
  (setq new-category (read-from-minibuffer "Category to move to: "))
  (if (not (re-search-forward (concat "--- " new-category "\n") nil t))
      (progn
	(organizer-add-category new-category)
	(save-buffer)
	(kill-buffer (current-buffer))
	(find-file organizer)))
  (goto-char (point-min))
  (re-search-forward (concat "--- " new-category "\n") nil t)
  (next-line -1)
  (end-of-line)
  (insert-string (concat "\n" (insert-register 'o)))
  (beginning-of-line)
  (next-line 1)
  ;; (backward-delete-char-untabify 1)
  (save-buffer)
  (kill-buffer (current-buffer))
  (organizer-show organizer)
  (organizer-jump-to-category new-category organizer)
  (write-file organizer-real-file nil)
  (if (file-exists-p "/tmp/delete-me")
      (delete-file "/tmp/delete-me"))
  )

;; Had to create a fn sinc 'dired-at-point' apparently doesn't exist in xemacs
(defun organizer-dired-at-point ()
  "List file or directory at cursor in dired"
  (interactive)
  (let ((str (ffap-string-at-point)))
    (dired str)))

(defun organizer-bookmark-at-point ()
  "List file or directory at cursor in dired"
  (interactive)
  (let ((str (ffap-string-at-point)))
    (bookmark-jump str)))


(defadvice view-diary-entries (after fancy-diary-display-advice activate)
  (switch-to-buffer fancy-diary-buffer)
  (organizer-minor-mode)
  (switch-to-buffer calendar-buffer))

(provide 'organizer-mode)
;;; organizer-mode.el ends here
