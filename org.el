;;; org.el --- Carsten's outline mode for keeping track of everything.
;; Copyright (c) 2003 Carsten Dominik
;;
;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Keywords: outlines, hypermedia, calendar
;; Version: 2.3
;;
;; This file is not part of GNU Emacs
;;
;; This program is free software you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Org-mode is a mode for keeping notes and doing project planning
;; with a fast and effective plain-text system.
;;
;; Org-mode develops organizational tasks around a NOTES file which
;; contains information about projects as plain text.  Org-mode is
;; implemented on top of outline-mode, which is ideal to keep the
;; content of large files well structured.  It supports ToDo items,
;; deadlines and time stamps, which magically appear in the diary
;; listing of the Emacs calendar.  Tables are easily created with a
;; built-in table editor.  Plain text URL-like links connect to
;; websites, emails (VM), Usenet messages (GNUS), BBDB entries, and
;; any files related to the project.  For printing and sharing of
;; notes, an org-mode file (or a part of it) can be exported as a
;; well-structured ASCII or HTML file.
;;
;; Installation
;; ------------
;; Org-mode requires Emacs 21 or later or XEmacs 21 or later.
;; Byte-compile org.el and put it on your load path.  Then copy the
;; following lines into .emacs.  The fourth line defines a *global*
;; key for the command `org-store-link' - please choose a suitable key
;; yourself.
;;
;;     (autoload 'org-mode "org" "Org mode" t)
;;     (autoload 'org-diary "org" "Diary entries from Org mode")
;;     (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;     (define-key global-map "\C-cl" 'org-store-link)
;;
;; This will put all files with extension ".org" into org-mode.  As an
;; alternative, make the first line of a file look like this:
;;
;;     MY PROJECTS    -*- mode: org; -*-
;;
;; which will select org-mode for this buffer no matter what the file's
;; name is.  In order to facilitate global cycling (see below), the
;; first line of a file should not be a headline.
;;
;; Outlines
;; --------
;; Org-mode is implemented on top of outline-mode.  Outlines allow to
;; organize a document in a hierarchical structure.  Overview over this
;; structure is achieved by folding (hiding) large parts of the document
;; to show only the general document structure and the parts currently
;; being worked on.  Org-mode greatly simplifies the use of outlines by
;; compressing the entire show/hide functionality into a single command
;; `org-cycle', which is bound to the TAB key.
;;
;; Headlines
;; ---------
;; The Headlines in org-mode start with one or more stars, for example
;;
;; * Top level headline
;; ** Second level
;; *** 3rd level
;;     some text
;; *** 3rd level
;;     more text
;; * Another top level headline
;;
;; Visibility cycling
;; ------------------
;; Outlines make it possible to hide parts of the text in the buffer.
;; Org-mode uses a single command bound to the TAB key to change the
;; visibility in the buffer.
;;
;; TAB         Rotate current subtree between the states
;;               ,-> FOLDED -> CHILDREN -> SUBTREE --.
;;               '-----------------------------------'
;;             At the beginning of the buffer (or when called with C-u),
;;             this does the same as the command S-TAB below.
;;
;; S-TAB       Rotate the entire buffer between the states
;;               ,-> OVERVIEW -> CONTENTS -> SHOW ALL --.
;;               '--------------------------------------'
;;             Note that inside tables, S-TAB jumps to the previous field.
;;
;; C-c C-a     Show all
;;
;; Motion
;; ------
;; The following commands jump to other headlines in the buffer
;;
;; C-c C-n     next heading
;; C-c C-p     previous heading
;; C-c C-f     next heading same level
;; C-c C-b     previous heading same level
;; C-c C-u     backward to higher level heading
;;
;; Structure editing
;; -----------------
;; M-RET       Insert new heading with same level as current
;; M-left      Promote current heading by one level
;; M-right     Demote current level by one level
;; M-S-left    Promote the current subtree by one level
;; M-S-right   Demote the current subtree by one level
;; M-S-up      Move subtree up (swap with previous heading of same level)
;; M-S-down    Move subtree down (swap with next heading of same level)
;;
;; When there is an active region (transient-mark-mode), promotion and
;; demotion work on all headlines in the region.  To select a region
;; of headlines, it is best to place both point and mark at the
;; beginning of a line, one at the beginning of the first headline,
;; and the other at the line just after the last headline to change.
;; Note that when the cursor is inside a table (see below), the
;; Meta-Cursor keys have different functionality.
;;
;; TODO items
;; ----------
;; Any headline can become a TODO item by starting it with the word TODO.
;;
;; C-c C-t     Rotate the TODO state of the current item between
;;              ,-> (unmarked) -> TODO -> DONE --.
;;              '--------------------------------'
;; C-c C-v     View TODO items.  Folds the entire buffer, but shows all
;;             TODO items and the headings hierarchy above them.
;;             With prefix arg, show also the DONE entries.
;; C-c /       Occur.  Prompts for a regexp and shows a compact tree with
;;             all matches, in a similar way as the TODO tree.
;;
;; Time stamps
;; -----------
;; A time stamp is a specification of a date (possibly with time) in a
;; special format like <2003-09-16 Tue> or <2003-09-16 Tue 09:39>.  A
;; time _range_ consists of two time stamps connected by "--".  For
;; org-mode to recognize time stamps, they need to be in the specific
;; format created by the commands listed below.  Several of these
;; commands prompt for a date and/or a time.  The function reading
;; your input is very forgiving and will replace anything you choose
;; not to specify with the current date and time.  See also the
;; documentation string of the function `org-read-date'.
;;
;; C-c .         Prompt for a date and insert a corresponding time stamp.
;;               When the cursor is at a previously used time stamp, it
;;               is updated to NOW.  When this command is used twice
;;               in succession, a time range is inserted.
;; C-u C-c .     Like `C-c .', but use the alternative format which
;;               contains date and time.
;; S-left/right  Change date at cursor by one day.
;; S-up/down     Change the item under the cursor in a timestamp.  The
;;               cursor can be on a year, month, day, hour or minute.
;;
;; Time stamps are used to assign dates to items.  For example, if you
;; write down a note about a future meeting, you can mark this note
;; with a date range specifying when this meeting will take place.  Or
;; you can specify when you finished a certain TODO item.  You can
;; also mark an item to have a deadline (see below).  In order to get
;; a time-sorted overview of the items in a certain org file or a
;; region of it, use the following command:
;;
;; C-c C-r     Produce a time-sorted view of time-stamped items in
;;             the current file (or the current region if active).
;;             By default, only future dates are shown.  Use a C-u
;;             prefix to include any past dates.  Middle-click on
;;             any displayed line to jump back to the corresponding
;;             location in the org file.
;;
;; Deadlines
;; ---------
;; A DEADLINE identifies things which have to be finished by a certain
;; date.  Unlike the TODO keyword, DEADLINE does not have to be in a
;; headline - it can appear anywhere in the entry.
;;
;; C-c C-d     Prompt for a date and insert the DEADLINE keyword
;;             along with a date stamp.
;;             If there is already a DEADLINE in the current line,
;;             toggle it on (UPCASE) and off (downcase).
;; C-c C-w     Show all deadlines which are past-due, or which will
;;             become due within `org-deadline-warning-days'.  With
;;             C-u prefix, show all deadlines in the file.  With a
;;             numeric prefix, check that many days.  For example,
;;             C-1 C-c C-w shows all deadlines due tomorrow.
;;
;; Calendar and Diary Integration
;; ------------------------------
;; Org-mode allows to easily jump between an org file and the Emacs
;; calendar.
;;
;; C-c >       Access the Emacs calendar for the current date.  If
;;             there is a timestamp in the current line, goto the
;;             corresponding date instead. C-c C-o does the same when
;;             the cursor is on a date (see LINKS below).
;; C-c <       Insert a time stamp corresponding to the cursor date in
;;             the Calendar.
;;
;; Items marked by timestamps and deadlines can be integrated into the
;; diary display of the calendar.  For this to work you need to
;; autoload the function `org-diary' as shown above under INSTALLATION.
;; You also need to use fancy-diary-display by setting in .emacs:
;;
;;     (add-hook 'diary-display-hook 'fancy-diary-display)
;;
;; Then you need to include lines into your ~/diary file pointing to
;; the org files you'd like to check for diary entries, e.g.
;; 
;;     &%%(org-diary) ~/path/to/some/org-file.org
;;     &%%(org-diary) ~/path/to/another/org-file.org
;;
;; If you then launch the calendar and press "d" to display todays
;; diary, the headlines of entries containing a timestamp, date range
;; or deadline referring to the selected date will be listed.  In the
;; diary for the *current date* (i.e. "today" only) also entries
;; containing a DEADLINE due within `org-deadline-warning-days' days
;; will be shown.  You can middle-click on the lines in the diary
;; buffer (does not work under XEmacs) to jump to the corresponding
;; location in the org file.  See also the documentation of the
;; function `org-diary'.
;;
;; To look at the diary entries coming from a single org file (or a
;; part of it, the active region), use the command `C-c C-r'.
;;
;; Links
;; -----
;; Org-mode supports links to files, websites, Usenet and Email
;; messages and BBDB database entries.  Links are just plain-text
;; URL-like locators.  The following list shows examples for each link
;; type.
;;
;;   http://zon.astro.uva.nl/~dominik            ; on the web
;;   file:/home/dominik/images/jupiter.jpg       ; file, absolute path
;;   file:papers/last.pdf                        ; file, relative path
;;   news:comp.emacs                             ; Usenet link
;;   mailto:adent@galaxy.net                     ; Mail link
;;   vm:folder                                   ; VM folder link
;;   vm:folder#id                                ; VM message link
;;   vm://myself@some.where.org/folder#id        ; VM on remote machine
;;   gnus:group                                  ; GNUS group link
;;   gnus:group#id                               ; GNUS article link
;;   bbdb:Richard Stallman                       ; BBDB link
;;   shell:ls *.org                              ; A shell command
;;
;; A link may contain space characters and is terminated by the end of
;; the line.  Therefore, there can be only one link per line.
;;
;; C-c l       Store a link to the current location.  This is a
;;             *global* command which can be used in any buffer to
;;             create a link.  The link will be stored for later
;;             insertion into an org-mode buffer (see below).  For VM,
;;             GNUS and BBDB buffers, the link will point to the
;;             current article/entry.  For W3 and W3M buffer, the link
;;             goes to the current URL.  For any other files, the link
;;             will just point to the file.  The key binding `C-c l'
;;             is only a suggestion - see above under INSTALLATION.
;;
;; C-c C-l     Insert a link.  This prompts for a link to be inserted
;;             into the buffer.  You can just type a link, using one
;;             of the link type prefixes mentioned in the examples
;;             above.  Using completion, all links stored during the
;;             current session can be accessed.  When called with
;;             prefix arg, you can use file name completion to enter a
;;             file link.  Note that you don't have to use this
;;             command to insert a link.  Links in org-mode are plain
;;             text, and you can type or paste them straight into the
;;             buffer.
;;
;; C-c C-o     Open link at point.  This will launch a web browser for
;;             URLs (using `browse-url-at-point'), run vm/gnus/bbdb
;;             for the corresponding links, execute the command in a
;;             shell link, visit text files with Emacs and select a
;;             suitable application for non-text files.
;;             Classification of files is based on file extension
;;             only.  See option `org-file-apps'.  If there is no link
;;             at point, the current subtree will be searched for one.
;;             If you want to override the default application in
;;             order to visit the file with Emacs, use a C-u prefix.
;;             IMPORTANT: Be careful not to use any dangerous commands
;;             in a shell link.
;;
;; mouse-2     On links, mouse-2 will open the link just like
;;             `C-c C-o' would.
;; mouse-3     Like mouse-2, but force file links to be opened with Emacs.
;;
;; Tables
;; ------
;; Org-mode helps creating tables.  Any line starting with "|" is
;; considered part of a table.  "|" is also the column separator.  A
;; table might look like this:
;;
;;   | Name  | Phone | Age |
;;   |-------+-------+-----|
;;   | Peter |  1234 |  17 |
;;   | Anna  |  4321 |  25 |
;;
;; A table is re-aligned automatically each time you press TAB or RET
;; inside the table.  The indentation of the table is set by the first
;; line.  TAB also moves to the next field (RET to the next row) and
;; creates new table rows at the end of the table or before horizontal
;; lines.  Any line starting with "|-" is considered as a horizontal
;; separator line and will be expanded on the next re-align to span
;; the whole table width.  So to create the above table, you would
;; only type
;;
;;   |Name|Phone|Age
;;   |-
;;
;; and then press TAB to align the table and fill in fields.
;;
;; C-c C-c       Re-align the table without moving the cursor.
;; TAB           Re-align the table, move to the next field.  Creates
;;               a new row if necessary.
;; S-TAB         Move to previous field.
;; RET           Re-align the table and move down to next row.  Creates
;;               a new row if necessary.  At beginning or end of line,
;;               RET still does NEWLINE.
;; S-RET         Copy from first non-empty field above current field.
;;
;; M-left/right  Move the current column left/right
;; M-up/down     Move the current row up/down
;;
;; M-S-left      Kill the current column.
;; M-S-right     Insert a new column to the left of the cursor position.
;; M-S-up        Kill the current row or horizontal line.
;; M-S-down      Insert a new row above (with arg: below) the current row.
;; C-c -         Insert a horizontal line below current row.
;;               With prefix arg, line is created above the current. 
;; C-c |         Toggle the visibility of vertical lines in tables.
;;               The lines are still there, only made invisible with a
;;               text property.  Any "|" added by hand will become
;;               invisible on the next align.  Typographically it is
;;               good style to have no vertical lines in tables.
;; C-c +         Sum the numbers in the current column, or in the
;;               rectangle defined by the active region.
;;
;; If you don't like the automatic table editor because it gets into
;; your way in lines which you would like to start with "|", you can
;; turn it off with
;;                   (set org-enable-table-editor nil)
;; The only table command which then still works is `C-c C-c' to do
;; a manual re-align.
;;
;; More complex tables (with line wrapping, column- and row-spanning,
;; and alignment) can be created using the Emacs table package
;; (http://sourceforge.net/projects/table).  When TAB is pressed in a
;; line which is part of such a table, org-mode will call
;; `table-recognize-table' and move the cursor into the table.  Note
;; that inside a table, the keymap of org-mode is overruled by the
;; table.el keymap.  In order to execute org-related commands, leave
;; the table. 
;;
;; Exporting
;; ---------
;; For printing and sharing of notes, an org-mode document can be
;; exported as an ASCII file, or as HTML.  In the exported version,
;; the first 3 outline levels will become headlines, defining a
;; general document structure.  Additional levels will be exported as
;; itemize lists.  If you want that transition to occur at a different
;; level, specify a prefix argument.  For example,
;;             M-1 M-x org-export-as-html
;; creates only top level headlines and does the rest as items.
;;
;; C-c C-x a    Export as ASCII file.  If there is an active region,
;;              only the region will be exported.  For an org file
;;              `myfile.org', the ASCII file will be `myfile.txt'.
;;              The file will be overwritten without warning.
;; C-c C-x h    Export as HTML file `myfile.html'.
;; C-c C-x C-h  Export as HTML file and open it with a browser.
;; C-c C-x t    Insert template with export options, see below.
;; C-c :        Toggle fixed-width for line or region, see below.
;;
;; If you want to include HTML tags which should be interpreted as
;; such, quote them with a "@" like in @<b>bold text@</b>.  Plain <
;; and > are always transformed to &lt; and &gt; in HTML export.
;; 
;; The HTML exporter applies several useful conversions.
;;   - Stars indicate *emphasized* words
;;   - TeX-like constructs are interpreted as simple math:
;;      - 10^22 and J_n are super- and subscripts
;;      - \alpha indicates a Greek letter, \to an arrow.
;;     You can quote ^ and _ with a backslash: \_ and \^
;;   - Lines starting with ":" are typeset in a fixed-width font, to
;;     allow quoting of computer code etc.
;;   - Lines starting with "|" are typeset as a table.  Hiding
;;     vertical lines with `C-c |' has no effect in HTML, but does
;;     work in ASCII export.
;; If these conversions conflict with your habits of typing ASCII
;; text, they can all be turned off with corresponding variables.
;;
;; The exporter recognizes special lines in the buffer which provide
;; additional information.  These lines should be put before the first
;; headline in the file.
;;
;;   +TITLE:     the title to be shown (default is the buffer name)
;;   +AUTHOR:    the author (default taken from `user-full-name')
;;   +EMAIL:     his/her email address (default from `user-mail-address')
;;   +LANGUAGE:  language for HTML, e.g. `en' (`org-export-default-language')
;;   +TEXT:      Some descriptive text to be inserted at the beginning.
;;   +TEXT:      Several lines may be given.
;;   +OPTIONS:   H:2  num:t  toc:t  \n:nil  @:t  ::t  |:t  ^:t  *:nil  TeX:t
;;
;; The OPTIONS line is a compact form to specify export settings.
;; Here you can set the number of headline levels for export (H:), and
;; turn on/off section-numbers (num:), table of contents (toc:),
;; linebreak-preservation (\n:), quoted html tags (@:), fixed-width
;; sections (::), tables (|:), emphasized text (*:) and TeX macros
;; (TeX:).
;;
;; Comment lines
;; -------------
;; Lines starting with "#" in column zero are treated as comments and
;; will never be exported.  Also entire subtrees starting with the
;; word COMMENT will never be exported.  Finally, any text before the
;; first headline will not be exported either.
;;
;; C-c ;        Toggle the COMMENT keyword at the beginning of an entry.
;;
;; Tips and Tricks
;; ---------------
;; - Paste URLs into org-mode whenever this seems useful.  For
;;   example, if you are writing notes about a paper which is
;;   available on the web, put the corresponding URL there and a
;;   direct look at the paper is only a mouse click away.  If you have
;;   a local copy of the paper, use a file:path link.
;;
;; - If you plan to use ASCII or HTML export, make sure things you want
;;   to be exported as item lists are level 4 at least, even if that does
;;   mean there is a level jump.  For example
;;
;;     * Todays top priorities
;;     **** TODO write a letter to xyz
;;     **** TODO Finish the paper
;;     **** Pick up kids at the school
;;
;; - Alternatively, if you need a specific value for the heading/item
;;   transition in a particular file, use the +OPTIONS line to
;;   configure the H switch.
;;
;;   +OPTIONS:   H:2; ...
;;
;; - Deadlines should normally be part of a TODO entry, so that they
;;   will show up both in the deadline list and in the TODO list.  For
;;   example
;;
;;     *** TODO write article about the Earth for the Guide
;;         The editor in charge is bbdb:Ford Prefect
;;         DEADLINE <2004-02-29 Sun>
;;
;; - To insert an empty table template, just type "|-" and use TAB.
;;
;; - In a table, to add a new column at the end, just type some text
;;   anywhere after the final "|".  Upon the next re-align, a new
;;   column will be created.
;;
;; - In tables, TAB creates new rows before horizontal separator
;;   lines.  If  the cursor is at "Age" in the following table,
;;
;;        | Name  | Phone | Age |
;;        |-------+-------+-----|
;;        |       |       |     |
;;
;;   the next TAB would create a second header line.  If you want
;;   instead to go to the first empty field below the lines, press
;;   <down> (to get on the separator line) and then TAB.
;;
;;
;; Customization
;; -------------
;; There is large number of options through which org-mode can be
;; configured.  For a good overview, use `M-x org-customize'.
;;
;; Acknowledgments
;; ---------------
;; - Thanks to Matthias Rempe (Oelde) for a patch introducing
;;   Windows NT/2000 support, for other useful suggestions and
;;   patches, for extensive beta-testing and relentless quality control.
;; - The idea to link to VM/BBDB/GNUS was adopted from Tom Shannon's
;;   organizer-mode.el.
;; - Thanks to Kevin Rogers for code to access VM files on remote hosts.
;; - Thanks to Juergen Vollmer for code generating the table of contents
;;   in HTML output, and other export improvements.
;;
;; Bugs
;; ----
;; - When the application called by `C-c C-l' to open a file link
;;   fails (for example because the application does not exits or
;;   refuses to open the file), it does so silently.  No error message
;;   is displayed.
;; - Under XEmacs, it is not possible to jump back from a diary entry to
;;   the org file.  Apparently, the text properties are lost when the
;;   fancy diary buffer is created.  However, from the timeline buffer,
;;   things do work correctly.
;; - Linux should also have a default viewer application, using
;;   mailcap.  Maybe we can use GNUS or VM mime code?  Or dired's
;;   guessing commands?  Any hints are appreciated.
;; - Org-mode has to go to some length (~4% of the code) to be
;;   compatible with both the old and the new outline mode.  It would
;;   be really useful if someone could port the new outline mode
;;   (which uses overlays with invisible properties instead of
;;   selective display) from GNU Emacs to XEmacs.
;;==========================================================================
;;
;; Changes:
;; -------
;; Version 2.3
;;    - A time-sorted view on all time stamps can be created with C-c C-r.
;;    - Timestamps and Deadlines can be shown in the Emacs diary.
;;    - Date ranges introduced.
;;    - Time-string formats are no longer configurable.
;;    - Vertical lines in tables can be made invisible with `C-c |'.
;;    - New "link" type to execute shell commands, like "shell:ls *.org"
;;    - Upon export, "myfile.org" becomes "myfile.html" or "myfile.txt",
;;      instead of "myfile.org.html" or "myfile.org.txt".
;;    - When the cursor is in the white space at the beginning of a line,
;;      TAB removes the whitespace before indenting again.
;;
;; Version 2.0
;;    - Windows (NT/2000) support.
;;    - Works with both Emacs and XEmacs.
;;    - Fully automatic table editor.
;;    - New link types into GNUS, VM and BBDB.
;;    - Other link system changes
;;      - Time stamps are treated as links to the calendar.
;;      - Easy creation of links with global command `org-store-link'.
;;      - Insertion of links with `C-c C-l' works differently now.
;;      - Space characters allowed as part of a link.
;;      - Options in `org-file-apps' extended.  The command may now be
;;        symbol 'emacs', or a lisp form. 
;;    Please re-read the manual section about links.
;;    - Timestamp changes
;;      - `org-deadline' now prompts for a date.
;;      - A line can now contain several timestamps.  Updating of a
;;        timestamp only happens if the cursor is at the timestamp.
;;      - Changed the time-stamp-format to ISO, to make sure it will
;;        always work (non-English month names had caused problems
;;        with `parse-time-string'.).  Changing the time stamp format
;;        is not recommended.
;;    - Picture mode enhancements have been removed from org.el
;;
;; Version 1.4
;;    - Some option name changes, not backward compatible.
;;    - ASCII exporter upgrade: Table of contents.
;;    - HTML exporter upgrade: fixed-width regions, better
;;      sub/superscripts, many TeX symbols supported.
;;    - Calendar support.
;;
;; Version 1.3
;;    - HTML exporter upgrade, in particular table of contents
;;
;; Version 1.0
;;    - Initial release

;;; Code:

(require 'outline)
(require 'time-date)
(require 'easymenu)

;;; Customization variables

(defvar org-version "2.3")
(defun org-version (arg)
  (interactive "P")
  (message "Org-mode version %s" org-version))
(defconst org-xemacs-p (featurep 'xemacs))

(defgroup org nil
  "Carsten's outline mode for keeping track of everything."
  :tag "Org"
  :group 'outlines
  :group 'hypermedia
  :group 'calendar)

(defcustom org-startup-folded t
  "Non-nil means, entering ORG mode will switch to OVERVIEW."
  :group 'org
  :type 'boolean)

(defcustom org-startup-truncated t
  "Non-nil means, entering ORG mode will set `truncate-lines'.
This is useful since some lines containing links can be very long and
uninteresting.  Also tables look terrible when wrapped."
  :group 'org
  :type 'boolean)

(defcustom org-startup-with-deadline-check t
  "Non-nil means, entering ORG mode will run the deadline check.
This means, if you start editing an org file, you will get an
immediate reminder of any due deadlines."
  :group 'org
  :type 'boolean)

(defcustom org-deadline-warning-days 30
  "No. of days before expiration during which a deadline becomes active.
This variable governs the display in the org file."
  :group 'org
  :type 'number)

(defcustom org-adapt-indentation t
  "Non-nil means, adapt indentation when promoting and demoting.
When this is set and the *entire* text in an entry is indented, the
indentation is increased by one space in a demotion command, and
decreased by one in a promotion command.  If any line in the entry
body starts at column 0, indentation is not changed at all."
  :group 'org
  :type 'boolean)

(defcustom org-cycle-emulate-tab t
  "Where should `org-cycle' emulate TAB.
nil    Never
white  Only in completely white lines
t      Everywhere except in headlines"
  :group 'org
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Everywhere except in headlines" t)
		 ))

(defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
  "Formats for `format-time-string' which are used for time stamps.
It is not recommended to change this constant.")

(defgroup org-link nil
  "Options concerning links in org-mode."
  :tag "Org Link"
  :group 'org)

(defcustom org-allow-space-in-links t
  "Non-nil means, file names in links may contain space characters."
  :group 'org-link
  :type 'boolean)

(defcustom org-keep-stored-link-after-insertion nil
  "Non-nil means, keep link in list for entire session.

The command `org-store-link' adds a link pointing to the current
location to an internal list. These links accumulate during a session.
The command `org-insert-link' can be used to insert links into any
org-mode file (offering completion for all stored links).  When this
option is nil, every link which has been inserted once using `C-c C-l'
will be removed from the list, to make completing the unused links
more efficient."
  :group 'org-link
  :type 'boolean)

(defcustom org-link-frame-setup
  '((vm . vm-visit-folder-other-frame)
    (gnus . gnus-other-frame)
    (file . find-file-other-window))
  "Setup the frame configuration for following links.
When following a link with Emacs, it may often be useful to display
this link in another window or frame.  This variable can be used to
set this up for the different types of links.
For VM, use any of
    vm-visit-folder
    vm-visit-folder-other-frame
For GNUS, use any of
    gnus
    gnus-other-frame
For FILE, use any of
    find-file
    find-file-other-window
    find-file-other-frame
For the calendar, use the variable `calendar-setup'.
For BBDB, it is currently only possible to display the matches in
another window."
  :group 'org-link
  :type '(list
          (cons (const vm) 
                (choice
                 (const vm-visit-folder)
                 (const vm-visit-folder-other-window)
                 (const vm-visit-folder-other-frame)))
          (cons (const gnus)
                (choice
                 (const gnus)
                 (const gnus-other-frame)))
          (cons (const file)
                (choice
                 (const find-file)
                 (const find-file-other-window)
                 (const find-file-other-frame)))))  

(defcustom org-usenet-links-prefer-google nil
  "Non-nil means, `org-store-link' will create web links to google groups.
When nil, GNUS will be used for such links."
  :group 'org-link
  :type 'boolean)

(defconst org-file-apps-defaults-linux
  '((t        . emacs)
    ("jpg"    . "xv %s")
    ("gif"    . "xv %s")
    ("ppm"    . "xv %s")
    ("pgm"    . "xv %s")
    ("pbm"    . "xv %s")
    ("tif"    . "xv %s")
    ("png"    . "xv %s")
    ("ps"     . "gv %s")
    ("ps.gz"  . "gv %s")
    ("eps"    . "gv %s")
    ("eps.gz" . "gv %s")
    ("dvi"    . "xdvi %s")
    ("mpeg"   . "plaympeg %s")
    ("mp3"    . "plaympeg %s")
    ("fig"    . "xfig %s")
    ("pdf"    . "acroread %s")
    ("doc"    . "soffice %s")
    ("ppt"    . "soffice %s")
    ("pps"    . "soffice %s")
    ("html"   . "netscape -remote openURL(%s,new-window)")
    ("htm"    . "netscape -remote openURL(%s,new-window)")
    ("xs"     . "soffice %s"))
  "Default file applications on a UNIX/LINUX system.
See `org-file-apps'.")

(defconst org-file-apps-defaults-macosx
  '((t        . "open %s")
    ("ps"     . "gv %s")
    ("ps.gz"  . "gv %s")
    ("eps"    . "gv %s")
    ("eps.gz" . "gv %s")
    ("dvi"    . "xdvi %s")
    ("fig"    . "xfig %s"))
  "Default file applications on a MacOS X system. 
The system \"open\" is known as a default, but we use X11 applications
for some files for which the OS does not have a good default.
See `org-file-apps'.")

(defconst org-file-apps-defaults-windowsnt
  '((t        . (w32-shell-execute "open" file)))
  "Default file applications on a Windows NT system.
The system \"open\" is used for most files.
See `org-file-apps'.")

(defcustom org-file-apps
  '(
    ("txt" . emacs)
    ("tex" . emacs)
    ("ltx" . emacs)
    ("org" . emacs)
    ("el"  . emacs)
    )
  "External applications for opening `file:path' items in a document.
Org-mode uses system defaults for different file types, but
you can use this variable to set the application for a given file
extension.  The entries in
this list are cons cells with a file extension and the corresponding
command.  Possible values for the command are:
 `emacs'     The file will be visited by the current Emacs process.   
 `default'   Use the default application for this file type.
 string      A command to be executed by a shell. %s will be replaced
             by the path to the file.
 sexp        A lisp form which will be evaluated.  The file path will
             be available in the lisp variable `file'.
For more examples, see the system specific constants
`org-file-apps-defaults-macosx'
`org-file-apps-defaults-windowsnt'
`org-file-apps-defaults-linux'."
  :group 'org-link
  :type '(repeat
          (cons (string :tag "Extension") 
                (choice :value ""
                 (const :tag "Visit with Emacs" 'emacs)
                 (const :tag "Use system default" 'default)
                 (string :tag "Command")
                 (sexp :tag "Lisp form")))))

(defgroup org-table nil
  "Options concerning tables in org-mode."
  :tag "Org Table"
  :group 'org)

(defcustom org-enable-table-editor t
  "Non-nil means, lines starting with \"|\" are handled by the table editor.
When nil, such lines will be treated like ordinary lines."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-default-size "5x2"
  "The default size for newly created tables, Columns x Rows."
  :group 'org-table
  :type 'string)

(defcustom org-table-automatic-realign t
  "Non-nil means, automatically re-align table when pressing TAB or RETURN.
When nil, aligning is only done with \\[org-table-align], or after column
removal/insertion."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-spaces-around-separators '(1 . 1)
  "The number of spaces to be placed before and after separators."
  :group 'org-table
  :type '(cons (number :tag "Before \"|\"") (number :tag " After \"|\"")))

(defcustom org-table-spaces-around-invisible-separators '(1 . 2)
  "The number of spaces to be placed before and after separators.
This option applies when the column separators have been made invisible."
  :group 'org-table
  :type '(cons (number :tag "Before \"|\"") (number :tag " After \"|\"")))

(defcustom org-table-number-regexp "^[<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*$"
  "Regular expression for recognizing numbers in table columns.
If a table column contains mostly numbers, it will be aligned to the
right.  If not, it will be aligned to the left.

The default value of this option is a regular expression which allows
anything which looks remotely like a number as used in scientific
context.  For example, all of the following will be considered a
number:
    12    12.2    2.4e-08    2x10^12    4.034+-0.02    2.7(10)  >3.5

Other options offered by the customize interface are more restrictive."
  :group 'org-table
  :type '(choice
          (const :tag "Positive Integers" 
                 "^[0-9]+$")
          (const :tag "Integers"
                 "^[-+]?[0-9]+$")
          (const :tag "Floating Point Numbers" 
                 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.[0-9]*\\)$")
          (const :tag "Floating Point Number or Integer"
                 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)$")
          (const :tag "Exponential, Floating point, Integer"
                 "^[-+]?[0-9.]+\\([eEdD][-+0-9]+\\)?$")
          (const :tag "Very General Number-Like" 
                 "^[<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*$")
          (string :tag "Regexp:")))

(defcustom org-table-number-fraction 0.5
  "Fraction of numbers in a column required to make the column align right.
In a column all non-white fields are considered.  If at least this
fraction of fields is matched by `org-table-number-fraction',
alignment to the right border applies." 
  :group 'org-table
  :type 'number)

(defcustom org-export-highlight-first-table-line t
  "Non-nil means, highlight the first table line.
In HTML export, this means use <th> instead of <td>.
In tables created with table.el, this applies to the first table line.
In org-mode tables, all lines before the first horizontal separator
line will be formatted with <th> tags."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-tab-recognizes-table.el t
  "Non-nil means, TAB will automatically notice a table.el table.
When it sees such a table, it moves point into it and - if necessary -
calls `table-recognize-table'."
  :group 'org-table
  :type 'boolean)

(defcustom org-export-prefer-native-exporter-for-tables nil
  "Non-nil means, always export tables created with table.el natively.
Natively means, use the HTML code generator in table.el.
When nil, org-mode's own HTML generator is used when possible (i.e. if
the table does not use row- or column-spanning).  This has the
advantage, that the automatic HTML conversions for math symbols and
sub/superscripts can be applied.  Org-mode's HTML generator is also
much faster."
  :group 'org-table
  :type 'boolean)

(defcustom org-enable-fixed-width-editor t
  "Non-nil means, lines starting with \":\" are treated as fixed-width.
This currently only means, they are never auto-wrapped.
When nil, such lines will be treated like ordinary lines."
  :group 'org-table
  :type 'boolean)

(defgroup org-export nil
  "Options for exporting org-listings."
  :tag "Org Export"
  :group 'org)

(defcustom org-export-language-setup
  '(("en"  "Author"          "Date"  "Table of Contents")
    ("da"  "Ophavsmand"      "Dato"  "Indhold")
    ("de"  "Autor"           "Datum" "Inhaltsverzeichnis")
    ("es"  "Autor"           "Fecha" "\xccndice")
    ("fr"  "Auteur"          "Date"  "Table des Mati\xe8res") 
    ("it"  "Autore"          "Data"  "Indice")    
    ("nl"  "Auteur"          "Datum" "Inhoudsopgave")    
    ("nn"  "Forfattar"       "Dato"  "Innhold")  ;; nn = Norsk (nynorsk)
    ("sv"  "F\xf6rfattarens" "Datum" "Inneh\xe5ll"))
  "Terms used in export text, translated to different languages.
Use the variable `org-export-default-language' to set the language,
or use the +OPTION lines for a per-file setting."
  :group 'org-export
  :type '(repeat
          (list
           (string :tag "HTML language tag")
           (string :tag "Author")
           (string :tag "Date")
           (string :tag "Table of Contents"))))

(defcustom org-export-default-language "en"
  "The default language of HTML export, as a string.
This should have an association in `org-export-language-setup'"
  :group 'org-export
  :type 'string)

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.
Inferior levels will produce itemize lists when exported.
Note that a numeric prefix argument to an exporter function overrides
this setting.

This option can also be set with the +OPTIONS line, e.g. \"H:2\"."
  :group 'org-export
  :type 'number)

(defcustom org-export-with-section-numbers t
  "Non-nil means, add section numbers to headlines when exporting.

This option can also be set with the +OPTIONS line, e.g. \"num:t\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-toc t
  "Non-nil means, create a table of contents in exported files.
The TOC contains headlines with levels up to`org-export-headline-levels'.

Headlines which contain any TODO items will be marked with \"(*)\" in
ASCII export, and with red color in HTML output.

In HTML output, the TOC will be clickable.

This option can also be set with the +OPTIONS line, e.g. \"toc:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-preserve-breaks nil
  "Non-nil means, preserve all line breaks when exporting.
Normally, in HTML output paragraphs will be reformatted.  In ASCII
export, line breaks will always be preserved, regardless of this variable.

This option can also be set with the +OPTIONS line, e.g. \"\\n:t\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-expand t
  "Non-nil means, for HTML export, treat @<...> as HTML tag.
When nil, these tags will be exported as plain text and therefore
not be interpreted by a browser.

This option can also be set with the +OPTIONS line, e.g. \"@:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-fixed-width t
  "Non-nil means, lines starting with \":\" will be in fixed width font.
This can be used to have preformatted text, fragments of code etc.  For
example
  : ;; Some Lisp examples
  : (while (defc cnt)
  :   (ding))
will be looking just like this in also HTML.  In ASCII export, this option
has no effect.

This option can also be set with the +OPTIONS line, e.g. \"::nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-tables t
  "Non-nil means, lines starting with \"|\" define a table
For example:

  | Name        | Address  | Birthday  |
  |-------------+----------+-----------+
  | Arthur Dent | England  | 29.2.2100 |

In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"|:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-table-tag 
  "<table border=1 cellspacing=0 cellpadding=6>"
  "The HTML tag used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export
  :type 'string)

(defcustom org-export-with-emphasize t
  "Non-nil means, interprete *word* as emphasized text.
If the export target supports emphasizing text, the word will be
typeset in italic or bold.  Works only for single words.
In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"*:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-match-sexp-depth 3
  "Number of stacked braces for sub/superscript matching.
This has to be set before loading org.el to be effective."
  :group 'org-export
  :type 'integer)

;; FIXME: Should () parens be removed as well in sub/superscripts?
(defcustom org-export-with-sub-superscripts t
  "Non-nil means, interprete \"_\" and \"^\" for export.
When this option is turned on, you can use TeX-like syntax for sub- and
superscripts.  Several characters after \"_\" or \"^\" will be
considered as a single item - so grouping with {} is normally not
needed.  For example, the following things will be parsed as single
sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
                         terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose the
sub/superscript.
In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"^:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-with-TeX-macros t
  "Non-nil means, interprete simple TeX-like macros when exporting.
For example, HTML export converts \\alpha to &alpha; and \\AA to &Aring;.
No only real TeX macros will work here, but the standard HTML entities
for math can be used as macro names as well.  For a list of supported
names in HTML export, see the constant `org-html-entities'.
In ASCII export, this option has no effect.

This option can also be set with the +OPTIONS line, e.g. \"TeX:nil\"."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-with-timestamp nil
  "Non-nil means,  write `org-export-html-html-helper-timestamp'
into the exported html text.  Otherwise, the buffer will just be saved
to a file."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-html-helper-timestamp
  "<br><br><hr><p><!-- hhmts start --> <!-- hhmts end -->\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode."
  :group 'org-export
  :type 'string)
 
(defcustom org-export-ascii-show-new-buffer t
  "Non-nil means, popup buffer containing the exported ASCII text.
Otherwise the buffer will just be saved to a file and stay hidden."
  :group 'org-export
  :type 'boolean)

(defcustom org-export-html-show-new-buffer nil
  "Non-nil means,  popup buffer containing the exported html text.
Otherwise, the buffer will just be saved to a file and stay hidden."
  :group 'org-export
  :type 'boolean)

;; Tell the compiler about dynamically scoped variables
;; FIXME: I don't know if this produces unwanted side effects.
(eval-when-compile
  (defvar zmacs-regions)
  (defvar org-transient-mark-mode)
  (defvar org-cursor-color)
  (defvar org-time-was-given)
  (defvar org-ts-what)
  (defvar timecnt)
  (defvar levels-open)
  (defvar title)
  (defvar author)
  (defvar email)
  (defvar text)
  (defvar entry)
  (defvar date)
  (defvar language)
  (defvar options))

;;; Define the mode

(defvar org-mode-map (copy-keymap outline-mode-map)
  "Keymap for org-mode.")

(defvar org-struct-menu)
(defvar org-org-menu)

;;;###autoload
(defun org-mode ()
  "Carsten's special outline mode for keeping notes about everything."
  (interactive)
  (outline-mode)
  (setq major-mode 'org-mode)
  (setq mode-name "Org")
  (use-local-map org-mode-map)
  (easy-menu-add org-struct-menu)
  (easy-menu-add org-org-menu)
  (setq outline-regexp "\\*+")
  (org-patch-outline-font-lock-regexp)
  (if org-startup-truncated (setq truncate-lines t))
  ;; Hack the old menu stuff out of the map
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'org-unfontify-region)
  ;; Activate before-change-function
  (add-hook 'before-change-functions 'org-before-change-function nil
            'local)
  ;; Inhibit auto-fill for headers, tables and fixed-width lines.
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       (concat "\\*"
               (if (or org-enable-table-editor org-enable-fixed-width-editor)
                   (concat
                    "\\|[ \t]*["
                    (if org-enable-table-editor "|" "")
                   (if org-enable-fixed-width-editor ":"  "")
                   "]"))))
  (run-hooks 'org-mode-hook)
  (if org-startup-with-deadline-check
      (call-interactively 'org-check-deadlines)
    (if org-startup-folded (org-cycle t))))

;;; Font-Lock stuff

(defvar org-mouse-map (make-sparse-keymap))
(if org-xemacs-p
    (progn
      (define-key org-mouse-map [button2] 'org-open-at-mouse)
      (define-key org-mouse-map [button3] 'org-find-file-at-mouse))
  (define-key org-mouse-map [mouse-2] 'org-open-at-mouse)
  (define-key org-mouse-map [mouse-3] 'org-find-file-at-mouse))

(require 'font-lock)

(defconst org-link-regexp
  (if org-allow-space-in-links
      "\\(https?\\|ftp\\|mailto\\|file\\|news\\|bbdb\\|vm\\|gnus\\|shell\\):\\([^\t\n\r]+[^ \t\n\r]\\)"
    "\\(https?\\|ftp\\|mailto\\|file\\|news\\|bbdb\\|vm\\|gnus\\|shell\\):\\([^ \t\n\r]+\\)"
    )
  "Regular expression for matching links.")
(defconst org-ts-lengths 
  (cons (length (format-time-string (car org-time-stamp-formats)))
        (length (format-time-string (cdr org-time-stamp-formats))))
  "This holds the lengths of the two different time formats.")
(defconst org-ts-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^\r\n>]*\\)>"
  "Regular expression for fast time stamp matching.")
(defconst org-ts-regexp1 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\([^0-9>\r\n]*\\)\\(\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.")
(defconst org-ts-regexp2 (concat "<" org-ts-regexp1 ">")
  "Regular expression matching time stamps, with groups.")
(defconst org-tr-regexp (concat org-ts-regexp "--?-?" org-ts-regexp)
  "Regular expression matching a time stamp range.")
(defconst org-tsr-regexp (concat org-ts-regexp "\\(--?-?"
                                 org-ts-regexp "\\)?")
  "Regular expression matching a time stamp or time stamp range.")

(defun org-activate-links (limit)
  "Run through the buffer and add overlays to links."
  (if (re-search-forward org-link-regexp limit t)
      (progn
        (add-text-properties (match-beginning 0) (match-end 0)
                             (list 'mouse-face 'highlight
                                   'keymap org-mouse-map))
        t)))

(defun org-activate-dates (limit)
  "Run through the buffer and add overlays to links."
  (if (re-search-forward org-tsr-regexp limit t)
      (progn
        (add-text-properties (match-beginning 0) (match-end 0)
                             (list 'mouse-face 'highlight
                                   'keymap org-mouse-map))
        t)))


(defconst org-font-lock-extra-keywords
  '((org-activate-links (0 font-lock-keyword-face))
    (org-activate-dates (0 font-lock-keyword-face))
    ("\\<TODO\\>" (0 font-lock-warning-face t))
    ("\\<DEADLINE\\>" (0 font-lock-warning-face t))
    ("\\<FIXME\\>" (0 font-lock-warning-face t))
    ("^\\*+[ \t]*\\<\\(COMMENT\\)\\>\\(.*\\)" (1 font-lock-warning-face t)
     (2 font-lock-comment-face t))
    ("^#.*" (0 font-lock-comment-face t))
    ("^[*]+ +\\<\\(DONE\\)\\>\\(.*\\)"
     (1 font-lock-type-face t) (2 font-lock-string-face t)))
  "Extra font lock keywords for org-mode.")

(defun org-font-lock-add-keywords (mode keywords append)
  "Add font lock keywords."
  (if org-xemacs-p
      (progn
        (add-hook 'font-lock-mode-hook
                  `(lambda ()
                     (if (eq major-mode 'org-mode)
                         (setq font-lock-keywords 
                               ;; FIXME:  We always do append here.  Should 
                               ;;         depend upon the `append' value.
                               (append (quote ,keywords) font-lock-keywords))))))
    (font-lock-add-keywords mode keywords append)))

(org-font-lock-add-keywords 'org-mode
                           org-font-lock-extra-keywords
                           'append)

(if (or org-enable-table-editor org-export-with-tables)
    (org-font-lock-add-keywords 'org-mode
                                '(("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
                                   (1 font-lock-function-name-face t)))
                                'append))
(if (or org-enable-fixed-width-editor org-export-with-fixed-width)
    (org-font-lock-add-keywords 'org-mode
                                '(("^[ \t]*\\(:.*\\)"
                                   (1 font-lock-function-name-face t)))
                                'append))

(defun org-unfontify-region (beg end &optional maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (let* ((modified (buffer-modified-p))
         (buffer-undo-list t)
         (inhibit-read-only t) (inhibit-point-motion-hooks t)
         (inhibit-modification-hooks t)
         deactivate-mark buffer-file-name buffer-file-truename)
    (remove-text-properties beg end '(mouse-face nil keymap nil))))

;;; Visibility cycling

(defvar org-cycle-global-status nil)
(defvar org-cycle-subtree-status nil)
(defun org-cycle (&optional arg)
  "Visibility cycling for outline(-minor)-mode.

- When this function is called with a prefix argument, rotate the entire
  buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
               this state, you can move to one of the children and
               zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is the the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg."
  (interactive "P")

  (if (and (bobp) (not (looking-at outline-regexp)))
      ; special case:  use global cycling
      (setq arg t))

  (cond

   ((org-at-table-p 'any)
    ;; Enter the table or move to the next field in the table
    (or (org-table-recognize-table.el)
        (org-table-next-field)))

   (arg ;; Global cycling

    (cond
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (message "CONTENTS...")
      (save-excursion
	;; Visit all headings and show their offspring
	(goto-char (point-max))
	(catch 'exit
	  (while (and (progn (condition-case nil
				 (outline-previous-visible-heading 1)
			       (error (goto-char (point-min))))
			     t)
		      (looking-at outline-regexp))
	    (show-branches)
	    (if (bobp) (throw 'exit nil))))
	(message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents))
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'contents))
      ;; We just showed the table of contents - now show everything
      (show-all)
      (message "SHOW ALL")
      (setq org-cycle-global-status 'all))
     (t
      ;; Default action: go to overview
      (hide-sublevels 1)
      (message "OVERVIEW")
      (setq org-cycle-global-status 'overview))))

   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    ;; At a heading: rotate between three different views
    (outline-back-to-heading)
    (let ((goal-column 0) beg eoh eol eos nxh)
      ;; First, some boundaries
      (save-excursion
	(outline-back-to-heading)  (setq beg (point))
	(save-excursion
	  (beginning-of-line 2)
	  (while (and (not (eobp))   ;; this is like `next-line'
		      (get-char-property (1- (point)) 'invisible))
	    (beginning-of-line 2)) (setq eol (point)))
	(outline-end-of-heading)   (setq eoh (point))
	(outline-end-of-subtree)   (setq eos (point))
        (outline-next-heading)     (setq nxh (point)))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
	;; Nothing is hidden behind this heading
        (message "EMPTY ENTRY")
        (setq org-cycle-subtree-status nil))
       ((>= eol eos)
	;; Entire subtree is hidden in one line: open it
	(show-entry)
	(show-children)
	(message "CHILDREN")
	(setq org-cycle-subtree-status 'children))
       ((and (eq last-command this-command)
	     (eq org-cycle-subtree-status 'children))
	;; We just showed the children, now show everything.
	(show-subtree)
	(message "SUBTREE")
	(setq org-cycle-subtree-status 'subtree))
       (t
	;; Default action: hide the subtree.
	(hide-subtree)
	(message "FOLDED")
	(setq org-cycle-subtree-status 'folded)))))

   ;; TAB emulation
   (buffer-read-only (outline-back-to-heading))
   ((if (and (eq org-cycle-emulate-tab 'white)
	     (save-excursion (beginning-of-line 1) (looking-at "[ \t]+$")))
	t
      (eq org-cycle-emulate-tab t))
    (if (and (looking-at "[ \n\r\t]")
             (string-match "^[ \t]*$" (buffer-substring
                                       (point-at-bol) (point))))
        (progn
          (beginning-of-line 1)
          (and (looking-at "[ \t]+") (replace-match ""))))
    (indent-relative))

   (t (save-excursion
        (outline-back-to-heading)
        (org-cycle)))))

;;; Promotion, Demotion, Inserting new headlines

(defvar org-ignore-region nil
  "To temporary disable the active region.")

(defun org-insert-heading ()
  "Insert a new heading with same depth at point."
  (interactive)
  (let* ((head (save-excursion
		 (condition-case nil
		     (outline-back-to-heading)
		   (error (outline-next-heading)))
		 (prog1 (match-string 0)
		   (funcall outline-level)))))
    (unless (bolp) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (unless (equal (char-before) ?\ )
      (insert " "))
    (run-hooks 'org-insert-heading-hook)))

(defun org-promote-subtree (&optional arg)
  "Promote the entire subtree.
See also `org-promote'."
  (interactive "P")
  (org-map-tree 'org-promote))

(defun org-demote-subtree (&optional arg)
  "Demote the entire subtree.  See `org-demote'.
See also `org-promote'."
  (interactive "P")
  (org-map-tree 'org-demote))

(defun org-do-promote (&optional arg)
  "Promote the current heading higher up the tree.
If the region is active in transient-mark-mode, promote all headings
in the region."
  (interactive "P")
  (if (org-region-active-p)
      (org-map-region 'org-promote (region-beginning) (region-end))
    (org-promote)
    (org-fix-position-after-promote)))

(defun org-do-demote (&optional arg)
  "Demote the current heading lower down the tree.
If the region is active in transient-mark-mode, demote all headings
in the region."
  (interactive "P")
  (if (org-region-active-p)
      (org-map-region 'org-demote (region-beginning) (region-end))
    (org-demote)
    (org-fix-position-after-promote)))

(defun org-fix-position-after-promote ()
  "Make sure that after pro/demotion cursor position is right."
  (and (equal (char-after) ?\ )
       (equal (char-before) ?*)
       (forward-char 1)))

(defun org-promote ()
  "Promote the current heading higher up the tree.
If the region is active in transient-mark-mode, promote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
         (up-head (make-string (1- level) ?*)))
    (if (= level 1) (error "Cannot promote to level 0. UNDO to recover."))
    (replace-match up-head nil t)
    (if org-adapt-indentation
        (org-fixup-indentation "^ " "" "^ ?\\S-"))))

(defun org-demote ()
  "Demote the current heading lower down the tree.
If the region is active in transient-mark-mode, demote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
         (down-head (make-string (1+ level) ?*)))
    (replace-match down-head nil t)
    (if org-adapt-indentation
        (org-fixup-indentation "^ " "  " "^\\S-"))))

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading)
  (let ((level (outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
                    (outline-next-heading)
                    (> (funcall outline-level) level))
                  (not (eobp)))
        (funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let ((org-ignore-region t))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      ;;      (if (fboundp 'deactivate-mark) (deactivate-mark))
      ;;    (if (fboundp 'zmacs-deactivate-region) (zmacs-deactivate-region))
      (if (and (re-search-forward (concat "^" outline-regexp) nil t)
               (< (point) end))
          (funcall fun))
      (while (and (progn
                    (outline-next-heading)
                    (< (point) end))
                  (not (eobp)))
        (funcall fun)))))

(defun org-fixup-indentation (from to prohibit)
  "Change the indentation in the current entry by re-replacing FROM with TO.
However, if the regexp PROHIBIT matches at all, don't do anything.
This is being used to change indentation along with the length of the
heading marker.  But if there are any lines which are not indented, nothing
is changed at all."
  (save-excursion
    (let ((end (save-excursion (outline-next-heading)
                               (point-marker))))
      (unless (save-excursion (re-search-forward prohibit end t))
        (while (re-search-forward from end t)
          (replace-match to)
          (beginning-of-line 2)))
      (move-marker end nil))))

;;; Vertical tree motion

(defun org-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (org-move-subtree-down (- (prefix-numeric-value arg))))

(defun org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (let ((movfunc (if (> arg 0) 'outline-get-next-sibling
		   'outline-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	beg end txt folded)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (outline-end-of-subtree))
    (if (equal (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (and (funcall movfunc) (looking-at outline-regexp))
	  (progn (goto-char beg)
		 (error "Cannot move past superior level or buffer limit")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (outline-end-of-subtree)
	       (if (equal (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (delete-region beg end)
    (insert txt)
    (goto-char ins-point)
    (if folded (hide-subtree))
    (move-marker ins-point nil)))

;;; Comments, TODO and DEADLINE

(defun org-toggle-comment ()
  "Change the COMMENT state of an entry."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (looking-at (concat outline-regexp "\\( +\\<COMMENT\\>\\)"))
        (replace-match "" t t nil 1)
      (if (looking-at outline-regexp)
          (progn
            (goto-char (match-end 0))
            (insert " COMMENT"))))))

(defun org-todo ()
  "Change the TODO state of an item.
When the item starts with TODO, it is changed to DONE.  When it starts with
DONE, the DONE is removed.  And when neither TODO nor DONE are present, add
TODO at the beginning of the heading.  So this function rotates through the
three different TODO states."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (cond
     ((looking-at (concat outline-regexp " +TODO")) (org-mark-done))
     ((looking-at (concat outline-regexp " +DONE")) (org-unmark-todo))
     (t (org-mark-todo)))))

(defun org-mark-todo ()
  "Make the current headline start with TODO."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (looking-at outline-regexp)
        (save-excursion
          (goto-char (match-end 0))
          (if (looking-at " +\\(TODO\\|DONE\\)\\>")
              (replace-match " TODO" t t nil)
            (insert (concat " TODO")))))))

(defun org-mark-done ()
  "Make the current headline start with DONE."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (looking-at outline-regexp)
        (save-excursion
          (goto-char (match-end 0))
          (if (looking-at " +\\(TODO\\|DONE\\)\\>")
              (replace-match " DONE" t t nil)
            (insert (concat " DONE")))))))

(defun org-unmark-todo ()
  "Remove TODO or DONE marks from the current headline."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (looking-at outline-regexp)
        (save-excursion
          (goto-char (match-end 0))
          (if (looking-at " +\\(TODO\\|DONE\\)\\>")
              (replace-match "" t t nil))))))

(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match."
  (interactive "P")
  (let ((case-fold-search nil)
        (kwd-re (if arg "\\<\\(TODO\\|DONE\\)\\>" "\\<TODO\\>")))
    (message "%d TODO entries found"
             (org-occur (concat "^" outline-regexp " +" kwd-re )))))

(defun org-deadline ()
  "Insert the DEADLINE: string to make a deadline.
A timestamp is also inserted - use \\[org-timestamp-up] and \\[org-timestamp-down]
to modify it to the correct date."
  (interactive)
  (let ((case-fold-search nil))
    (cond
     ((save-excursion
        (beginning-of-line 1)
        (looking-at ".*\\(\\<DEADLINE\\>\\)"))
      (replace-match "deadline" t t nil 1))
     ((save-excursion
        (beginning-of-line 1)
        (looking-at ".*\\(\\<deadline\\>\\)"))
      (replace-match "DEADLINE" t t nil 1))
     (t
      (insert 
       "DEADLINE: "
       (format-time-string (car org-time-stamp-formats) 
                           (org-read-date nil 'to-time)))
      (message (substitute-command-keys
                "Use \\[org-timestamp-up-day] and \\[org-timestamp-down-day] to change the date."))))))

(defun org-read-date (&optional with-time to-time)
  "Read a date.  This function is very forgiving and swallows a lot.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month ,year,
hour and minute.  For example, 
  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  etc.
The function understands only English month and weekday abbreviations,
but this can be configured with the variables `parse-time-months' and
`parse-time-weekdays'. 

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to also
insert a time.  Note that when WITH-TIME is not set, you can still
enter a time, and this function will inform the calling routine about
this change.  The calling routine may then choose to change the format
used to insert the time stamp into the buffer to include the time."
  (let* ((timestr (format-time-string
                   (if with-time "%Y-%m-%d %H:%M" "%Y-%m-%d") (current-time)))
         (prompt (format "YYYY-MM-DD [%s]: " timestr))
         (ans (read-string prompt "" nil timestr))
         second minute hour day month year tl)
    (if (string-match
         "^ *\\(\\([0-9]+\\)-\\)?\\([0-1]?[0-9]\\)-\\([0-3]?[0-9]\\)\\([^-0-9]\\|$\\)" ans)
        (progn
          (setq year (if (match-end 2)
                         (string-to-number (match-string 2 ans))
                       (string-to-number (format-time-string "%Y")))
                month (string-to-number (match-string 3 ans))
                day (string-to-number (match-string 4 ans)))
          (if (< year 100) (setq year (+ 2000 year)))
          (setq ans (replace-match (format "%04d-%02d-%02d" year month day) t t ans))))
    (setq tl (parse-time-string ans)
          year (or (nth 5 tl) (string-to-number (format-time-string "%Y")))
          month (or (nth 4 tl) (string-to-number (format-time-string "%m")))
          day (or (nth 3 tl) (string-to-number (format-time-string "%d")))
          hour (or (nth 2 tl) (string-to-number (format-time-string "%H")))
          minute (or (nth 1 tl) (string-to-number (format-time-string "%M")))
          second (or (nth 0 tl) 0))
    (if (and (boundp 'org-time-was-given)
             (nth 2 tl))
        (setq org-time-was-given t))
    (if (< year 100) (setq year (+ 2000 year)))
    (if to-time
        (encode-time second minute hour day month year)
      (if (or (nth 1 tl) (nth 2 tl))
          (format "%04d-%02d-%02d %02d:%02d" year month day hour minute)
        (format "%04d-%02d-%02d" year month day)))))

(defun org-check-deadlines (ndays)
  "Check if there are any deadlines due or past due.
A deadline is considered due if it happens within `org-deadline-warning-days'
days from todays date.  The prefix arg NDAYS can be used to test that many
days.  If the prefix are is a raw C-u prefix, all deadlines are shown."
  (interactive "P")
  (let* ((org-warn-days
          (cond
           ((equal ndays '(4)) 100000)
           (ndays (prefix-numeric-value ndays))
           (t org-deadline-warning-days)))
         (case-fold-search nil)
         (regexp "\\<DEADLINE\\>: *<\\([^>]+\\)>")
         (callback
          (lambda ()
            (let ((d1 (time-to-days (current-time)))
                  (d2 (time-to-days
                       (org-time-string-to-time (match-string 1)))))
              (< (- d2 d1) org-warn-days)))))
    (message "%d deadlines past-due or due within %d days"
             (org-occur regexp callback)
             org-warn-days)))

(defun org-occur (regexp &optional callback)
  "Make a compact tree which shows all matches of REGEXP.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.  It will also show the heading after the match,
to make sure editing the matching entry is easy.
if CALLBACK is non-nil, it is a function which is called to confirm
that the match should indeed be shown."
  (interactive "sRegexp: ")
  (setq regexp (org-check-occur-regexp regexp))
  (let ((cnt 0))
    (save-excursion
      (goto-char (point-min))
      (hide-sublevels 1)
      (while (re-search-forward regexp nil t)
        (when (or (not callback)
                  (funcall callback))
          (setq cnt (1+ cnt))
          (if (org-on-heading-p t)
              (org-flag-heading nil)    ; only show the heading
            (org-show-hidden-entry))    ; show entire entry
          (save-excursion
            (and (outline-next-heading)
                 (org-flag-heading nil)))  ; show the next heading
          (save-excursion                  ; show all higher headings
            (while (condition-case nil
                       (progn (org-up-heading-all 1) t)
                     (error nil))
              (org-flag-heading nil))))))
    (if (interactive-p)
        (message "%d match(es) for regexp %s" cnt regexp))
    cnt))

;;; Timestamps

(defun org-time-stamp (arg)
  "Prompt for a date/time and insert a time stamp.
If the user specifies a time like HH:MM, or if this command is called
with a prefix argument, the time stamp will contain date and time.
Otherwise, only the date will be included.  All parts of a date not
specified by the user will be filled in from the current date/time.
So if you press just return without typing anything, the time stamp
will represent the current date/time."
  (interactive "P")
  (let ((fmt (if arg (cdr org-time-stamp-formats)
               (car org-time-stamp-formats)))
        (org-time-was-given nil)
        time)
    (cond
     ((and (org-at-timestamp-p)
           (eq last-command this-command))
      (insert "--")
      (setq time (let ((this-command this-command))
                  (org-read-date arg 'totime)))
      (if org-time-was-given (setq fmt (cdr org-time-stamp-formats)))
      (insert (format-time-string fmt time)))               
     ((org-at-timestamp-p)
      (replace-match "")
      (insert (format-time-string fmt))
      (message "Timestamp updated"))
     (t
      (setq time (let ((this-command this-command))
                  (org-read-date arg 'totime)))
      (if org-time-was-given (setq fmt (cdr org-time-stamp-formats)))
      (insert (format-time-string fmt time))))))

(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P")
  (save-excursion
    (unless (org-at-date-range-p)
      (goto-char (point-at-bol))
      (re-search-forward org-tr-regexp (point-at-eol) t))
    (if (not (org-at-date-range-p))
        (error "Not at a time-stamp range, and none found in current line.")))
  (let* ((ts1 (match-string 1))
         (ts2 (match-string 2))
         (match-end (match-end 0))
         (l2 (cdr org-ts-lengths))
         (time1 (org-time-string-to-time ts1))
         (time2 (org-time-string-to-time ts2))
         (t1 (time-to-seconds time1))
         (t2 (time-to-seconds time2))
         (diff (abs (- t2 t1)))
         (negative (< (- t2 t1) 0))
         (ys (floor (* 365 24 60 60)))
         (ds (* 24 60 60))
         (hs (* 60 60))
         (fy "%dy %dd %02d:%02d")
         (fd "%dd %02d:%02d")
         (fh "%02d:%02d")
         y d h m align)
    (setq y (floor (/ diff ys))  diff (mod diff ys)
          d (floor (/ diff ds))  diff (mod diff ds)
          h (floor (/ diff hs))  diff (mod diff hs)
          m (floor (/ diff 60)))
    (if to-buffer
        (progn
          (goto-char match-end)
          (when (and (org-at-table-p) (looking-at " *|"))
            (setq align t)
            (goto-char (match-end 0)))
          (if (looking-at 
               "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
              (replace-match ""))
          (if negative (insert " -"))
          (if (> y 0) (insert " " (format fy y d h m))
            (if (> d 0) (insert " " (format fd d h m))
              (insert " " (format fh h m))))
          (if align (org-table-align))
          (message "Time difference inserted"))
      (message (org-make-tdiff-string y d h m)))))

(defun org-make-tdiff-string (y d h m)
  (let ((fmt "")
        (l nil))
    (if (> y 0) (setq fmt (concat fmt "%d year" (if (> y 1) "s" "") " ")
                      l (push y l)))
    (if (> d 0) (setq fmt (concat fmt "%d day"  (if (> d 1) "s" "") " ")
                      l (push d l)))
    (if (> h 0) (setq fmt (concat fmt "%d hour" (if (> h 1) "s" "") " ")
                      l (push h l)))
    (if (> m 0) (setq fmt (concat fmt "%d minute" (if (> m 1) "s" "") " ")
                      l (push m l)))
    (apply 'format fmt (nreverse l))))

(defun org-time-string-to-time (s)
  (apply 'encode-time (org-parse-time-string s)))

(defun org-parse-time-string (s)
  "Parse the standard org-mode time string.
This should be a lot faster than the normal parse-time-string."
  (if (string-match org-ts-regexp1 s)
      (list 0
            (string-to-number (or (match-string 8 s) "0"))
            (string-to-number (or (match-string 7 s) "0"))
            (string-to-number (match-string 4 s))
            (string-to-number (match-string 3 s))
            (string-to-number (match-string 2 s))
            nil nil nil)
    (make-list 9 0)))

(defun org-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month or
the day, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (prefix-numeric-value arg)))

(defun org-timestamp-down (&optional arg)
  "Decrease the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month or
the day, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (- (prefix-numeric-value arg))))

(defun org-timestamp-up-day (&optional arg)
  "Increase the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (org-timestamp-change (prefix-numeric-value arg) 'day))

(defun org-timestamp-down-day (&optional arg)
  "Increase the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (org-timestamp-change (- (prefix-numeric-value arg)) 'day))

(defsubst org-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defun org-at-timestamp-p ()
  "Determine if the the cursor is in or at a timestamp."
  (interactive)
  (let* ((tsr org-ts-regexp2)
         (pos (point))
         (ans (or (looking-at tsr)
                  (save-excursion
                    (skip-chars-backward "^<\n\r\t")
                    (if (> (point) 1) (backward-char 1))
                    (and (looking-at tsr)
                         (> (- (match-end 0) pos) -1))))))
    (and (boundp 'org-ts-what)
         (setq org-ts-what
              (cond
               ((org-pos-in-match-range pos 2)      'year)
               ((org-pos-in-match-range pos 3)      'month)
               ((org-pos-in-match-range pos 7)      'hour)
               ((org-pos-in-match-range pos 8)      'minute)
               ((or (org-pos-in-match-range pos 4)
                    (org-pos-in-match-range pos 5)) 'day)
               (t 'day))))
    ans))

(defun org-timestamp-change (n &optional what)
  "Change the date in the time stamp at point.
The date will be changed by N times WHAT.  WHAT can be `day', `month', 
`year', `minute', `second'.  If WHAT is not given, the cursor position
in the timestamp determines what will be changed."
  (let ((fmt (car org-time-stamp-formats))
        org-ts-what
        (pos (point))
        ts time time0)
    (if (not (org-at-timestamp-p))
        (error "Not at a timestamp"))
    (setq org-ts-what (or what org-ts-what))
    (setq fmt (if (<= (abs (- (cdr org-ts-lengths)
                              (- (match-end 0) (match-beginning 0))))
                      1)
                  (cdr org-time-stamp-formats)
                (car org-time-stamp-formats)))
    (setq ts (match-string 0))
    (replace-match "")
    (setq time0 (org-parse-time-string ts))
    (setq time
          (apply 'encode-time
                 (append (list (or (car time0) 0))
                         (list (+ (if (eq org-ts-what 'minute) n 0) (nth 1 time0)))
                         (list (+ (if (eq org-ts-what 'hour) n 0)   (nth 2 time0)))
                         (list (+ (if (eq org-ts-what 'day) n 0)    (nth 3 time0)))
                         (list (+ (if (eq org-ts-what 'month) n 0)  (nth 4 time0)))
                         (list (+ (if (eq org-ts-what 'year) n 0)   (nth 5 time0)))
                         (nthcdr 6 time0))))
    (if (eq what 'calendar)
        (let ((cal-date 
               (save-excursion
                 (save-match-data
                   (set-buffer "*Calendar*")
                   (calendar-cursor-to-date)))))
          (setcar (nthcdr 4 time0) (nth 0 cal-date)) ; month
          (setcar (nthcdr 3 time0) (nth 1 cal-date)) ; day
          (setcar (nthcdr 5 time0) (nth 2 cal-date)) ; year
          (setcar time0 (or (car time0) 0))
          (setcar (nthcdr 1 time0) (or (nth 1 time0) 0))
          (setcar (nthcdr 2 time0) (or (nth 1 time0) 0))
          (setq time (apply 'encode-time time0))))
;    (replace-match "" nil nil nil 0)
    (insert (format-time-string fmt time))
    (goto-char pos)))


(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used force the current date."
  (interactive "P")
  (let ((tsr org-ts-regexp) diff)
    (if (or (org-at-timestamp-p)
            (save-excursion
              (beginning-of-line 1)
              (looking-at (concat ".*" tsr))))
        (let ((d1 (time-to-days (current-time)))
              (d2 (time-to-days
                   (org-time-string-to-time (match-string 1)))))
          (setq diff (- d2 d1))))
    (calendar)
    (calendar-goto-today)
    (if (and diff (not arg)) (calendar-forward-day diff))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive)
  (org-timestamp-change 0 'calendar))

;;; Diary Integration
(defvar org-diary-keymap (make-sparse-keymap)
  "Local keymap for diary entries from org-mode.")
(define-key org-diary-keymap [(return)] 'org-diary-goto)
(if org-xemacs-p
    (define-key org-diary-keymap [(button2)] 'org-diary-goto-mouse)
  (define-key org-diary-keymap [(mouse-2)] 'org-diary-goto-mouse))

(defvar org-diary-markers nil
  "List of all currently active markers created by org-diary")
(defvar org-diary-last-marker-time (time-to-seconds (current-time))
  "Creation time of the last diary marker.")

(defun org-diary-new-marker (pos)
  "Return a new diary marker.
org-mode keeps a list of these markers and resets them when they are
no longer in use."
  (let ((m (copy-marker pos)))
    (setq org-diary-last-marker-time (time-to-seconds (current-time)))
    (push m org-diary-markers)
    m))

(defun org-diary-maybe-reset-markers ()
  "Reset markers created by org-diary.  But only if they are old enough."
  (if (> (- (time-to-seconds (current-time))
            org-diary-last-marker-time)
         5)
      (while org-diary-markers
        (move-marker (pop org-diary-markers) nil))))

(defun org-diary-view (&optional include-past)
  "Show a time-sorted view of the entries in the current org file.
Only entries with a time stamp of some sort will be listed.  Only
dates in the future will be shown - use a prefix argument to include
past dates as well.
If the buffer contains an active region, only check the region for
dates."
  (interactive "P")
  (require 'calendar)
  (let* ((entry (buffer-file-name))
         (date (calendar-current-date))
         (win (selected-window))
         (pos (point))
         ;; FIXME: Should we go back to headline?
         (beg (if (org-region-active-p) (region-beginning) (point-min)))
         (end (if (org-region-active-p) (region-end) (point-max)))
         (day-numbers (org-get-all-dates beg end 'no-ranges))
         (today (time-to-days (current-time)))
         (org-diary-view t)
         (past t)
         s e rtn d)
    (if (not include-past)
        ;; Remove past dates from the list of dates.
        (setq day-numbers (delq nil (mapcar (lambda(x) 
                                              (if (>= x today) x nil))
                                            day-numbers))))
    (switch-to-buffer-other-window (get-buffer-create "*Org Timeline*"))
    (erase-buffer)
    (fundamental-mode)
    (while (setq d (pop day-numbers))
      (if (and (>= d today)
               include-past
               past)
          (progn
            (setq past nil)
            (insert (make-string 79 ?-) "\n")))
      (setq date (calendar-gregorian-from-absolute d))
      (setq s (point))
      (setq rtn (org-diary))
      (if rtn
          (progn
            (insert (calendar-day-name date) " "
                    (number-to-string (extract-calendar-day date)) " "
                    (calendar-month-name (extract-calendar-month date)) " "
                    (number-to-string (extract-calendar-year date)) "\n")
            (put-text-property s (1- (point)) 'face
                               'font-lock-keyword-face)
            (insert rtn "\n"))))
    (goto-char (point-min))
    (select-window win)
    (goto-char pos)))

(defun org-get-all-dates (beg end &optional no-ranges)
  "Return a list of all relevant day numbers from BEG to END buffer positions.
If NO-RANGES is non-nil, include only the start and end dates of a range,
not every single day in the range."
  (let (dates date day day1 day2 ts1 ts2)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-ts-regexp end t)
        (setq day (time-to-days (org-time-string-to-time
                                 (substring (match-string 1) 0 10))))
        (or (memq day dates) (push day dates)))
      (unless no-ranges
        (goto-char beg)
        (while (re-search-forward org-tr-regexp end t)
          (setq ts1 (substring (match-string 1) 0 10)
                ts2 (substring (match-string 2) 0 10)
                day1 (time-to-days (org-time-string-to-time ts1))
                day2 (time-to-days (org-time-string-to-time ts2)))
          (while (< (setq day1 (1+ day1)) day2)
            (or (memq day1 dates) (push day1 dates)))))
      (sort dates '<))))

;;;###autoload
(defun org-diary (&rest args)
  "Returns diary information from an org-file.
This function can be used in an \"sexp\" diary entry in the Emacs calendar.
It accesses an org file and extracts information from that file to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  The following arguments are allowed:

   :timestamp    List the headlines of items containing a date stamp or
                 date range matching the selected date.  Deadlines will
                 also be listed, on the expiration day.

   :deadline     List any deadlines past due, or due within
                 `org-deadline-warning-days'.  The listing occurs only
                 in the diary for *today*, not at any other date.

   :todo         List all TODO items from the org-file.  This may be a
                 long list - so this is not turned on by default.
                 Like deadlines, these entires only show up in the
                 diary for *today*, not at any other date.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.

If you don't give any arguments (as in the example above), the default
arguments (:timestamp :deadline) are used.  So the example above may
also be written as

   &%%(org-diary :timestamp :deadline) ~/path/to/some/orgfile.org
"
  (org-diary-maybe-reset-markers)
  (setq args (or args '(:timestamp :deadline)))
  (let* ((file entry)   ; ENTRY bound by calendar, contains file path
         (buffer (if (file-exists-p file)
                     (find-file-noselect file)
                   (error "No such file %s" file)))
         (diary-view-p (boundp 'org-diary-view))
         arg results rtn)
    (if (not buffer)
        ;; If file does not exist, make sure an error message ends up in diary
        (format "ORG-DIARY-ERROR: No such org-file %s" file)
      (save-excursion
        (set-buffer buffer)
        (let ((case-fold-search nil))
          (save-excursion
            (save-restriction
              (if diary-view-p
                  (if (org-region-active-p)
                      ;; Respect a region to restrict search
                      (narrow-to-region (region-beginning) (region-end)))
                ;; If we work for the calendar, get rid of any restriction
                (widen))
              (while (setq arg (pop args))
                (cond
                 ((and (eq arg :todo)
                       (equal date (calendar-current-date)))
                  (setq rtn (org-diary-get-todos))
                  (if rtn (push rtn results)))
                 ((eq arg :timestamp)
                  (setq rtn (org-diary-get-blocks))
                  (if rtn (push rtn results))
                  (setq rtn (org-diary-get-timestamps))
                  (if rtn (push rtn results)))
                 ((and (eq arg :deadline)
                       (equal date (calendar-current-date)))
                  (setq rtn (org-diary-get-deadlines))
                  (if rtn (push rtn results))))))))))
    (setq results (delq nil results))
    (if results
        ;; If we have any results, return them.  otherwise, return nil
        (if diary-view-p
            (mapconcat 'identity (nreverse results) "\n")
          ;; When working with the Emacs calendar, we need a note
          ;; saying where the data came from.
          (format "%s" "Diary entries from file %s\n%s")
          file (mapconcat 'identity (nreverse results) "\n")))))

(defsubst org-at-date-range-p ()
  "It the cursor inside a date range?"
  (interactive)
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
        (skip-chars-backward "^<\r\n")
        (skip-chars-backward "<")
        (and (looking-at org-tr-regexp)
             (>= (match-end 0) pos)
             (throw 'exit t))
        (skip-chars-backward "^<\r\n")
        (skip-chars-backward "<")
        (and (looking-at org-tr-regexp)
             (>= (match-end 0) pos)
             (throw 'exit t)))
      nil)))

(defun org-diary-get-todos ()
  "Return the TODO information for diary display."
  (let* ((props (list 'face nil
                      'mouse-face 'highlight
                      'keymap org-diary-keymap
                      'help-echo "Use mouse-2 or RET to go to org file."))
         (regexp "[\n\r]\\*+ *\\(TODO\\>[^\n\r]*\\)")
         marker deadlinep
         ee txt)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (goto-char (match-beginning 1))
      (setq marker (org-diary-new-marker (point))
            txt (concat "  " (match-string 1)))
      (add-text-properties
       0 (length txt) (append (list 'org-marker marker) props)
       txt)
      (push txt ee))
    (setq txt (mapconcat 'identity (nreverse ee) "\n"))
    (if ee txt nil)))

(defconst org-diary-no-heading-message
  "No heading for this item in buffer or region")

(defun org-diary-get-timestamps ()
  "Return the date stamp information for diary display."
  (let* ((props (list 'face nil
                      'mouse-face 'highlight
                      'keymap org-diary-keymap
                      'help-echo "Use mouse-2 or RET to go to org file."))
         (regexp (regexp-quote
                  (substring 
                   (format-time-string
                    (car org-time-stamp-formats)
                    (apply 'encode-time  ; DATE bound by calendar
                           (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
                   0 11)))
         marker deadlinep
         ee txt)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (if (not (save-match-data (org-at-date-range-p)))
          (progn
            (setq marker (org-diary-new-marker (point))
                  deadlinep (string-match "\\<DEADLINE:"
                                          (buffer-substring 
                                           (max (point-min) 
                                                (- (match-beginning 0) 12))
                                           (match-beginning 0))))
            (save-excursion
              (if (re-search-backward "\\(^\\|\r\\)\\*+" nil t)
                  (progn
                    (goto-char (match-end 1))
                    (looking-at "\\*+[ \t]*\\([^\r\n]+\\)")
                    (setq txt (format "  %s%s"
                                      (if deadlinep "Deadline: " "")
                                      (match-string 1))))
                (setq txt org-diary-no-heading-message))
              (add-text-properties
               0 (length txt) (append (list 'org-marker marker) props)
               txt)
              (if deadlinep
                  (add-text-properties 
                   0 (length txt)
                   '(face font-lock-warning-face) txt))
              (push txt ee))
            (outline-next-heading))))
    (setq txt (mapconcat 'identity (nreverse ee) "\n"))
    (if ee txt nil)))

(defun org-diary-get-deadlines ()
  "Return the deadline information for diary display."
  (let* ((wdays org-deadline-warning-days)
         (props (list 'face nil
                      'mouse-face 'highlight
                      'keymap org-diary-keymap
                      'help-echo "Use mouse-2 or RET to go to org file."))
         (regexp "\\<DEADLINE: *<\\([^>]+\\)>")
         (todayp (equal date (calendar-current-date))) ; DATE bound by calendar
         (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
         d2 diff marker
         ee txt)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq d2 (time-to-days
                (org-time-string-to-time (match-string 1)))
            diff (- d2 d1))
      ;; When to show a deadline in the calendar:
      ;; If the expiration is within wdays warning time.
      ;; Past-due deadlines are only shown on the current date
      (if (and (< diff wdays) todayp)
          (save-excursion
            (setq marker (org-diary-new-marker (point)))
            (if (re-search-backward "\\(^\\|\r\\)\\*+" nil t)
                (progn
                  (goto-char (match-end 1))
                  (looking-at "\\*+[ \t]*\\([^\r\n]+\\)")
                  (setq txt (format "  In %3d days: %s" diff
                                    (match-string 1))))
              (setq txt org-diary-no-heading-message))
            (add-text-properties
             0 (length txt) (append (list 'org-marker marker) props)
             txt)
            (if (<= diff 0)
                (add-text-properties 
                 0 (length txt)
                 '(face font-lock-warning-face) txt)
              (if (<= diff 5)
                  (add-text-properties 
                   0 (length txt)
                   '(face font-lock-function-name-face) txt)))
            (push (cons diff txt) ee))))
    ;; Sort the entries by expiration date.
    (setq ee (sort ee (lambda (a b) (< (car a) (car b)))))
    (setq txt (mapconcat 'cdr ee "\n"))
    (if ee txt nil)))

(defun org-diary-get-blocks ()
  "Return the date-range information for diary display."
  (let* ((props (list 'face nil
                      'mouse-face 'highlight
                      'keymap org-diary-keymap
                      'help-echo "Use mouse-2 or RET to go to org file."))
         (regexp org-tr-regexp)
         (d0 (calendar-absolute-from-gregorian date))
         marker ee txt d1 d2 s1 s2)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq s1 (match-string 1)
            s2 (match-string 2)
            d1 (time-to-days (org-time-string-to-time s1))
            d2 (time-to-days (org-time-string-to-time s2)))
      (if (and (> (- d0 d1) -1) (> (- d2 d0) -1))
          ;; Only allow days between the limits, because the normal
          ;; date stamps will catch the limits.
          (save-excursion
            (setq marker (org-diary-new-marker (point)))
            (if (re-search-backward "\\(^\\|\r\\)\\*+" nil t)
                (progn
                  (goto-char (match-end 1))
                  (looking-at "\\*+[ \t]*\\([^\r\n]+\\)")
                  (setq txt (format "  (%d/%d): %s"
                                    (1+ (- d0 d1)) (1+ (- d2 d1))
                                    (match-string 1))))
              (setq txt org-diary-no-heading-message))
            (add-text-properties
             0 (length txt) (append (list 'org-marker marker) props)
             txt)
            (push txt ee)))
      (outline-next-heading))
    ;; Sort the entries by expiration date.
    (setq txt (mapconcat 'identity (nreverse ee) "\n"))
    (if ee txt nil)))

(defun org-diary-goto ()
  "Go to the org-mode file which contains the deadline at point."
  (interactive)
  (let* ((marker (get-text-property (point) 'org-marker))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (goto-char pos)
    (org-show-hidden-entry)
    (save-excursion
      (and (outline-next-heading)
           (org-flag-heading nil)))))  ; show the next heading

(defun org-diary-goto-mouse (ev)
  "Go to the org-mode file which contains the deadline at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-diary-goto))

;;; Link Stuff


(defun org-find-file-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (org-open-at-point 'in-emacs))

(defun org-open-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (org-open-at-point))

(defun org-open-at-point (&optional in-emacs)
  "Open link at or after point.
If there is no link at point, this function will search forward up to
the end of the current subtree.
Normally, files will be opened by an appropriate application.  If the
optional argument IN-EMACS is non-nil, Emacs will visit the file."
  (interactive "P")
  (if (org-at-timestamp-p)
      (org-goto-calendar)
    (let (type path (pos (point)))
      (save-excursion
        (skip-chars-backward
         (if org-allow-space-in-links "^\t\n\r" "^ \t\n\r"))
        (if (re-search-forward
             org-link-regexp
             (save-excursion
               (condition-case nil
                   (progn (outline-end-of-subtree) (max pos (point)))
                 (error (end-of-line 1) (point))))
             t)
            (setq type (match-string 1)
                  path (match-string 2)))
        (unless path
          (error "No link found."))
        ;; Remove any trailing spaces in path
        (if (string-match " +\\'" path)
            (setq path (replace-match "" t t path)))
        
        (cond
         
         ((string= type "file")
          (org-open-file path in-emacs))
         
         ((string= type "news")
          (org-follow-gnus-link path))
         
         ((string= type "bbdb")
          (org-follow-bbdb-link path))
         
         ((string= type "gnus")
          (let (group article)
            (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
                (error "Error in GNUS link"))
            (setq group (match-string 1 path)
                  article (match-string 3 path))
            (org-follow-gnus-link group article)))
         
         ((string= type "vm")
          (let (folder article)
            (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
                (error "Error in VM link"))
            (setq folder (match-string 1 path)
                  article (match-string 3 path))
            (org-follow-vm-link folder article)))
         ((string= type "shell")
          (let ((cmd path))
            (if (yes-or-no-p (format "Execute \"%s\" in the shell? " cmd))
                (shell-command cmd)
              (error "Abort"))))
         (t
          (browse-url-at-point)))))))

(defun org-follow-bbdb-link (name)
  "Follow a BBDB link to NAME."
  (require 'bbdb)
  ;; First try an exact match
  (bbdb-name (concat "\\`" name "\\'") nil)
  (if (= 0 (buffer-size (get-buffer "*BBDB*")))
      ;; No exact match - try partial match
      (bbdb-name name nil)))

(defun org-follow-gnus-link (&optional group article)
  "Follow a gnus link to GROUP and ARTICLE."
  (require 'gnus)
  (funcall (cdr (assq 'gnus org-link-frame-setup)))
  (if group (gnus-fetch-group group))
  (if article (gnus-summary-goto-article article nil 'force)))

(defun org-follow-vm-link (&optional folder article)
  "Follow a gnus link to FOLDER and ARTICLE."
  (require 'vm)
  (if (string-match "^//\\([a-zA-Z]+@\\)?\\([^:]+\\):\\(.*\\)" folder)
      ;; ange-ftp or efs or tramp access
      (let ((user (or (match-string 1 folder) (user-login-name)))
            (host (match-string 2 folder))
            (file (match-string 3 folder)))
        (cond
         ((featurep 'tramp)
          ;; use tramp to access the file
          (if org-xemacs-p
              (setq folder (format "[%s@%s]%s" user host file))
            (setq folder (format "/%s@%s:%s" user host file))))
         (t
          ;; use ange-ftp or efs
          (if org-xemacs-p (require 'efs) (require 'ange-ftp))
          (setq folder (format "/%s@%s:%s" user host file))))))
  (when folder 
    (funcall (cdr (assq 'vm org-link-frame-setup)) folder)
    (sit-for 0.1)
    (when article
      (vm-select-folder-buffer)
      (widen)
      (let ((case-fold-search t))
        (goto-char (point-min))
        (if (not (re-search-forward 
                  (concat "^" "message-id: *" (regexp-quote article))))
            (error "Could not find the specified message in this folder"))
        (vm-isearch-update)
        (vm-isearch-narrow)
        (vm-beginning-of-message)
        (vm-summarize)))))

(defun org-open-file (path &optional in-emacs)
  "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.
If no application is found, Emacs simply visits the file.
With optional argument IN-EMACS, Emacs will visit the file.
If the file does not exist, an error is thrown."
  (let* ((file (convert-standard-filename (org-expand-file-name path)))
         (dfile (downcase file))
         ext cmd apps)
    (if (not (file-exists-p file))
        (error "No such file: %s" file))
    (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\.gz\\)$" dfile)
        (setq ext (match-string 1 dfile))
      (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\)$" dfile)
          (setq ext (match-string 1 dfile))))
    (setq apps (append org-file-apps (org-default-apps)))
    (if in-emacs
        (setq cmd 'emacs)
      (setq cmd (or (cdr (assoc ext apps))
                    (cdr (assoc t apps)))))
    (cond
     ((and (stringp cmd) (not (string-match "^\\s-*$" cmd)))
      (setq cmd (format cmd file))
      (save-window-excursion
        (shell-command (concat cmd " & &"))))
     ((or (stringp cmd)
          (eq cmd 'emacs))
      (funcall (cdr (assq 'file org-link-frame-setup)) file))
     ((consp cmd)
      (eval cmd))
     (t (funcall (cdr (assq 'file org-link-frame-setup)) file)))))

(defun org-default-apps ()
  "Return the default applications for this operating system."
  (cond
   ((eq system-type 'darwin)
    org-file-apps-defaults-macosx)
   ((eq system-type 'windows-nt)
    org-file-apps-defaults-windowsnt)
   ((eq system-type 'linux)
    org-file-apps-defaults-linux)
   (t org-file-apps-defaults-linux)))

(defun org-expand-file-name (path)
  "Replace special path abbreviations and expand the file name."
  (expand-file-name path))


(defvar org-insert-link-history nil
  "Minibuffer history for links inserted with `org-insert-link'.")

(defvar org-stored-links nil
  "Contains the links stored with `org-store-link'.")

(defun org-store-link (arg)
  "\\<org-mode-map>Store an org-link to the current location.
This link can later be inserted into an org-buffer with
\\[org-insert-link]."
  (interactive "P")
  (let (link cpltxt)
    (cond

     ((eq major-mode 'bbdb-mode)
      (setq link (concat "bbdb:" 
                         (bbdb-record-name (bbdb-current-record)))))

     ((eq major-mode 'calendar-mode)
      (let ((cd (calendar-cursor-to-date)))
        (setq link
              (format-time-string 
               (car org-time-stamp-formats)
               (apply 'encode-time
                      (list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
                            nil nil nil))))))

     ((or (eq major-mode 'vm-summary-mode)
          (eq major-mode 'vm-presentation-mode))
      (and (eq major-mode 'vm-presentation-mode) (vm-summarize))
      (vm-follow-summary-cursor)
      (save-excursion
       (vm-select-folder-buffer)
       (let* ((message (car vm-message-pointer))
              (folder (buffer-file-name))
              (subject (vm-su-subject message))
              (author (vm-su-full-name message))
              (address (vm-su-from message))
              (message-id (vm-su-message-id message)))
         (setq folder (abbreviate-file-name folder))
         (if (string-match (concat "^" (regexp-quote vm-folder-directory))
                           folder)
             (setq folder (replace-match "" t t folder)))
         (setq cpltxt (concat author " on: " subject))
         (setq link (concat cpltxt "\n  " "vm:" folder 
                            "#" message-id)))))

     ((eq major-mode 'gnus-group-mode)
      (if (or arg org-usenet-links-prefer-google)
          (setq link (format "http://groups.google.com/groups?group=%s"
                             gnus-group-name))
        (setq link (concat "gnus:" (gnus-group-name)))))
     ((or (eq major-mode 'gnus-summary-mode)
          (eq major-mode 'gnus-article-mode))
      (gnus-article-show-summary)
      (gnus-summary-beginning-of-article)
      (let* ((group (car gnus-article-current))
             (article (cdr gnus-article-current))
             (header (gnus-summary-article-header article))
             (author (mail-header-from header))
             (message-id (mail-header-id header))
             (date (mail-header-date header))
             (subject (gnus-summary-subject-string)))
        (setq cpltxt (concat author " on: " subject))
        (if (or arg org-usenet-links-prefer-google)
            (setq link 
                  (concat
                   cpltxt "\n  "
                   (format "http://groups.google.com/groups?as_umsgid=%s"
                           (org-fixup-message-id-for-http message-id))))
          (setq link (concat cpltxt "\n" "gnus:" group 
                             "#" (number-to-string article))))))
     ((eq major-mode 'w3-mode)
      (setq link (url-view-url t)))
     ((eq major-mode 'w3m-mode)
      (setq link w3m-current-url))

     (t
      ;; Just link to this file here.
      (setq link (concat "file:"
                         (abbreviate-file-name (buffer-file-name))))))

    (setq org-stored-links
          (cons (cons (or cpltxt link) link) org-stored-links))
    (message "Stored: %s" (or cpltxt link))))

(defun org-get-header (header)
  "Find a header field in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t) s)
      (cond
       ((eq header 'from)
        (if (re-search-forward "^From:\\s-+\\(.*\\)" nil t)
            (setq s (match-string 1)))
	(while (string-match "\"" s)
	  (setq s (replace-match "" t t s)))
	(if (string-match "[<(].*" s)
	    (setq s (replace-match "" t t s))))
       ((eq header 'message-id)
        (if (re-search-forward "^message-id:\\s-+\\(.*\\)" nil t)
            (setq s (match-string 1))))
       ((eq header 'subject)
        (if (re-search-forward "^subject:\\s-+\\(.*\\)" nil t)
            (setq s (match-string 1)))))
      (if (string-match "\\`[ \t\]+" s) (setq s (replace-match "" t t s)))
      (if (string-match "[ \t\]+\\'" s) (setq s (replace-match "" t t s)))
      s)))
       

(defun org-fixup-message-id-for-http (s)
  "Replace special characters in a message id, so that it can be used
in an http query."
  (while (string-match "<" s)
    (setq s (replace-match "%3C" t t s)))
  (while (string-match ">" s)
    (setq s (replace-match "%3E" t t s)))
  (while (string-match "@" s)
    (setq s (replace-match "%40" t t s)))
  s)

(defun org-insert-link (&optional complete-file)
  "Insert a link. At the prompt, enter the link.

Completion can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press RET at the prompt), the link defaults to the most recently
stored link.

With a C-u prefix, prompts for a file to link to.  The file name can be
selected using completion.  The path to the file will be relative to
the current directory if the file is in the current directory or a
subdirectory.  Otherwise, the link will be the absolute path as
completed in the minibuffer (i.e. normally ~/path/to/file).

With two C-u prefixes, enforce an absolute path even if the file
is in the current directory or below."
  (interactive "P")
  (let ((link (if complete-file
                  (read-file-name "File: ")
                (completing-read
                 "Link: " org-stored-links nil nil nil
                 org-insert-link-history
                 (or (car (car org-stored-links))))))
	linktxt matched)
    (if (or (not link) (equal link ""))
      (error "No links available"))
    (if complete-file
        (let ((pwd (file-name-as-directory (expand-file-name "."))))
          (cond
           ((equal complete-file '(16))
            (insert "file:" (abbreviate-file-name (expand-file-name link))))
           ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
                          (expand-file-name link))
            (insert "file:" (match-string 1 (expand-file-name link))))
           (t (insert "file:" link))))
      (setq linktxt (cdr (assoc link org-stored-links)))
      (if (not org-keep-stored-link-after-insertion)
          (setq org-stored-links (delq (assoc link org-stored-links)
                                       org-stored-links)))
      (let ((lines (org-split-string (or linktxt link) "\n")))
        (insert (car lines))
        (setq matched (string-match org-link-regexp (car lines)))
        (setq lines (cdr lines))
        (while lines
          (insert "\n")
	  (if (save-excursion
		(beginning-of-line 0)
		(looking-at "[ \t]+\\S-"))
	      (indent-relative))
          (setq matched (or matched 
                            (string-match org-link-regexp (car lines))))
          (insert (car lines))
          (setq lines (cdr lines))))
      (unless matched
        (error "Please add link type: http(s),ftp,mailto,file,news,bbdb,vm,gnus,or shell")))))
          

;;; Tables

;; Careful:  Here we are talking about two different kind of tables.
;; Most of the code is for the tables created with the org-mode table editor.
;; Sometimes, we talk about tables created and edited with the table.el 
;; Emacs package.  We call the former org-type tables, and the latter
;; table.el-type tables.

;; We use a before-change function to check if a table might need
;; an update.
(defvar org-table-may-need-update t
  "Indicates of a table might need an update.
This variable is set by `org-before-change-function'. `org-table-align'
sets it back to nil.")

(defun org-before-change-function (beg end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))


(defconst org-table-line-regexp "^[ \t]*|"
  "Detects an org-type table line.")
(defconst org-table-dataline-regexp "^[ \t]*|[^-]"
  "Detects an org-type table line.")
(defconst org-table-hline-regexp "^[ \t]*|-"
  "Detects an org-type table hline.")
(defconst org-table1-hline-regexp "^[ \t]*\\+-[-+]"
  "Detects a table-type table hline.")
(defconst org-table-any-line-regexp "^[ \t]*\\(|\\|\\+-[-+]\\)"
  "Detects an org-type or table-type table")
(defconst org-table-border-regexp "^[ \t]*[^| \t]"
  "Searching from within a table (any type) this finds the first line
outside the table.")
(defconst org-table-any-border-regexp "^[ \t]*[^|+ \t]"
  "Searching from within a table (any type) this finds the first line
outside the table.")

(defun org-table-create-with-table.el ()
  "Use the table.el package to insert a new table."
  (interactive)
  (require 'table)
  (call-interactively 'table-insert))

(defun org-table-create (&optional size)
  "Query for a size and insert a table skeleton.
SIZE is a string Columns x Rows like for example \"3x2\"."
  (interactive "P")
  (unless size
    (setq size (read-string 
                (concat "Table size Columns x Rows [e.g. "
                        org-table-default-size "]: ")
                "" nil org-table-default-size)))
  
  (let* ((pos (point))
         (indent (make-string (current-column) ?\ ))
         (split (org-split-string size " *x *"))
         (rows (string-to-number (nth 1 split)))
         (columns (string-to-number (car split)))
         (line (concat (apply 'concat indent "|" (make-list columns "  |"))
                       "\n")))
    (if (string-match "^[ \t]*$" (buffer-substring-no-properties 
                                  (point-at-bol) (point)))
        (beginning-of-line 1)
      (newline))
    (mapcar (lambda (x) (insert line)) (make-list rows t))
    (goto-char pos)
    (if (> rows 1)
        ;; Insert a hline after the first row.
        (progn
          (end-of-line 1)
          (insert "\n|-")
          (goto-char pos)))
    (org-table-align)))

(defun org-table-align (&optional arg)
  "Align the table at point by aligning all vertical bars."
  (interactive "P")
  (let* (
         ;; Limits of table
         (beg (org-table-begin))
         (end (org-table-end))
         ;; Current cursor position
         (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
         (colpos (org-table-current-column))
         (winstart (window-start))
         text lines (new "") lengths l typenums ty fields maxfields i
         column
         (indent "") cnt frac
         rfmt hfmt
         (spaces (if (org-in-invisibility-spec-p '(org-table))
                     org-table-spaces-around-invisible-separators
                   org-table-spaces-around-separators))
         (sp1 (car spaces))
         (sp2 (cdr spaces))
         (rfmt1 (concat 
                 (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
         (hfmt1 (concat 
                 (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
         emptystrings)
    (untabify beg end)
    ;; (message "Aligning table...")
    ;; Get the rows
    (setq lines (org-split-string 
                 (buffer-substring-no-properties beg end) "\n"))
    ;; Store the indentation of the first line
    (if (string-match "^ *" (car lines))
        (setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    ;; Mark the hlines
    (setq lines (mapcar (lambda (l) (if (string-match "^ *|-" l) nil l))
                        lines))
    ;; Get the data fields
    (setq fields (mapcar
                  (lambda (l)
                      (org-split-string l " *| *"))
                  (delq nil (copy-sequence lines))))
    ;; How many fields in the longest line?
    (condition-case nil
        (setq maxfields (apply 'max (mapcar 'length fields)))
      (error
       (kill-region beg end)
       (org-table-create org-table-default-size)
       (error "Empty table - created default table")))
    ;; A list of empty string to fill any short rows on output
    (setq emptystrings (make-list maxfields ""))
    ;; Get the maximum length of a field and the most common datatype
    ;; for each column
    (setq i -1)
    (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
      (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
      ;; maximum length
      (push (apply 'max 1 (mapcar 'length column)) lengths)
      ;; compute the fraction stepwise, ignoring empty fields
      (setq cnt 0 frac 0.0)
      (mapcar
       (lambda (x)
         (if (equal x "") 
             nil
           (setq frac ( / (+ (* frac cnt)
                             (if (string-match org-table-number-regexp x) 1 0))
                          (setq cnt (1+ cnt))))))
       column)
      (push (>= frac org-table-number-fraction) typenums))
    (setq lengths (nreverse lengths)
          typenums (nreverse typenums))
    ;; Compute the formats needed for output of the table
    (setq rfmt (concat indent "|") hfmt (concat indent "|"))
    (while (setq l (pop lengths))
      (setq ty (if (pop typenums) "" "-")) ; number types flushright
      (setq rfmt (concat rfmt (format rfmt1 ty l))
            hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
    (setq rfmt (concat rfmt "\n")
          hfmt (concat (substring hfmt 0 -1) "|\n"))
    ;; Produce the new table
    (while lines
      (setq l (pop lines))
      (if l
          (setq new (concat new (apply 'format rfmt 
                                       (append (pop fields) emptystrings))))
        (setq new (concat new hfmt))))
    ;; Replace the old one
    (delete-region beg end)
    (move-marker end nil)
    (insert new)
    ;; Try to move to the old location (approximately)
    (goto-line linepos)
    (set-window-start (selected-window) winstart 'noforce)
    (org-table-goto-column colpos)
    (setq org-table-may-need-update nil)
    ;; (message "Aligning table...done")
    (if (org-in-invisibility-spec-p '(org-table))
        (org-table-add-invisible-to-vertical-lines))
    ))


(defun org-table-begin (&optional table-type)
  "Find the beginning of the table and return its position.
With argument TABLE-TYPE, go to the beginning of a table.el-type table."
  (save-excursion
    (if (not (re-search-backward
              (if table-type org-table-any-border-regexp
                org-table-border-regexp)
              nil t))
        (error "Can't find beginning of table")
      (goto-char (match-beginning 0))
      (beginning-of-line 2)
      (point))))

(defun org-table-end (&optional table-type)
  "Find the end of the table and return its position.
With argument TABLE-TYPE, go to the end of a table.el-type table."
  (save-excursion
    (if (not (re-search-forward 
              (if table-type org-table-any-border-regexp 
                org-table-border-regexp)
              nil t))
        (goto-char (point-max))
      (goto-char (match-beginning 0)))
    (point-marker)))

(defun org-table-next-field (&optional arg)
  "Go to the next field in the current table.
Before doing so, re-align the table if necessary."
  (interactive "P")
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (if (org-at-table-hline-p)
      (end-of-line 1))
  (condition-case nil
      (progn
        (re-search-forward "|" (org-table-end))
        (if (looking-at "[ \t]*$")
            (re-search-forward "|" (org-table-end)))
        (if (looking-at "-")
            (progn 
              (beginning-of-line 0)
              (org-table-insert-row 'below))
          (if (looking-at " ") (forward-char 1))))
    (error
     (org-table-insert-row 'below))))

(defun org-table-previous-field (&optional arg)
  "Go to the previous field in the table.
Before doing so, re-align the table if necessary."
  (interactive "P")
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (if (org-at-table-hline-p)
      (end-of-line 1))
  (re-search-backward "|" (org-table-begin))
  (re-search-backward "|" (org-table-begin))
  (while (looking-at "|\\(-\\|[ \t]*$\\)")
    (re-search-backward "|" (org-table-begin)))
  (if (looking-at "| ?")
      (goto-char (match-end 0))))

(defun org-table-next-row (&optional arg)
  "Go to the next row (same column) in the current table.
Before doing so, re-align the table if necessary."
  (interactive "P")
  (if (or (looking-at "[ \t]*$")
          (save-excursion (skip-chars-backward " \t") (bolp)))
      (newline)
    (if (and org-table-automatic-realign
             org-table-may-need-update)
        (org-table-align))
    (let ((col (org-table-current-column)))
      (beginning-of-line 2)
      (if (or (not (org-at-table-p))
              (org-at-table-hline-p))
          (progn
            (beginning-of-line 0)
            (org-table-insert-row 'below)))
      (org-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (if (looking-at " ") (forward-char 1)))))

(defun org-table-copy-from-above (n)
  "Copy into the current column the nearest non-empty field from above.
With prefix argument N, take the Nth non-empty field."
  (interactive "p")
  (let ((colpos (org-table-current-column))
        (beg (org-table-begin))
        txt)
    (org-table-check-inside-data-field)
    (if (save-excursion
          (setq txt
                (catch 'exit
                  (while (progn (beginning-of-line 1)
                                (re-search-backward org-table-dataline-regexp
                                                    beg t))
                    (org-table-goto-column colpos t)
                    (if (and (looking-at
                              "|[ \t]*\\([^| \t][^|]*[^| \t]\\)[ \t]*|")
                             (= (setq n (1- n)) 0))
                        (throw 'exit (match-string 1)))))))
        (progn
          (insert txt)
          (org-table-align))
      (error "No non-empty field found"))))

(defun org-table-check-inside-data-field ()
  "Is point inside a table data field?
I.e. not on a hline or before the first or after the last column?"
  (if (or (not (org-at-table-p))
          (= (org-table-current-column) 0)
          (org-at-table-hline-p)
          (looking-at "[ \t]*$"))
      (error "Not in table data field")))

(defun org-table-current-column ()
  "Find out which column we are in."
  (save-excursion
    (let ((cnt 0) (pos (point)))
      (beginning-of-line 1)
      (while (search-forward "|" pos t)
        (setq cnt (1+ cnt)))
      cnt)))

(defun org-table-goto-column (n &optional on-delim)
  "Move the cursor to the Nth column in the current table line.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field."
  (let ((pos (point-at-eol)))
    (beginning-of-line 1)
    (when (> n 0)
      (while (and (search-forward "|" pos t)
                  (> (setq n (1- n)) 0)))
      (if on-delim
          (backward-char 1)
        (if (looking-at " ") (forward-char 1))))))
  
(defun org-at-table-p (&optional table-type)
  "Return t if the cursor is inside an org-type table."
  (if org-enable-table-editor
      (save-excursion
        (beginning-of-line 1)
        (looking-at (if table-type org-table-any-line-regexp 
                      org-table-line-regexp)))
    nil))

(defun org-table-recognize-table.el ()
  "If there is a table.el table nearby, recognize it and move into it."
  (if org-table-tab-recognizes-table.el
      (if (org-at-table.el-p)
          (progn
            (beginning-of-line 1)
            (if (looking-at org-table-dataline-regexp)
                nil
              (if (looking-at org-table1-hline-regexp)
                  (progn
                    (beginning-of-line 2)
                    (if (looking-at org-table-any-border-regexp)
                        (beginning-of-line -1)))))
            (if (re-search-forward "|" (org-table-end t) t)
                (progn
                  (require 'table)
                  (if (table--at-cell-p (point))
                      t
                    (message "recognizing table.el table...")
                    (table-recognize-table)
                    (message "recognizing table.el table...done")))
              (error "This should not happen..."))
            t)
        nil)
    nil))

(defun org-at-table.el-p ()
  "Return t if the cursor is inside a table.el-type table."
  (save-excursion
    (if (org-at-table-p 'any)
        (progn
          (goto-char (org-table-begin 'any))
          (looking-at org-table1-hline-regexp))
      nil)))

(defun org-at-table-hline-p ()
  "Return t if the cursor is inside a hline in a table."
  (if org-enable-table-editor
      (save-excursion
        (beginning-of-line 1)
        (looking-at org-table-hline-regexp))
    nil))

(defun org-table-insert-column (&optional arg)
  "Insert a new column into the table."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (let* ((col (max 1 (org-table-current-column)))
         (beg (org-table-begin))
         (end (org-table-end))
         ;; Current cursor position
         (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
         (colpos col))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
          nil
        (org-table-goto-column col t)
        (insert "|   "))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos))
  (org-table-align))
      
(defun org-table-find-dataline ()
  "Find a dataline in the current table, which is needed for column commands."
  (if (and (org-at-table-p)
           (not (org-at-table-hline-p)))
      t
    (let ((col (current-column))
          (end (org-table-end)))
      (move-to-column col)
      (while (and (< (point) end) 
                  (or (not (= (current-column) col))
                      (org-at-table-hline-p)))
        (beginning-of-line 2)
        (move-to-column col))
      (if (and (org-at-table-p)
               (not (org-at-table-hline-p)))
          t
        (error 
         "Please position cursor in a data line for column operations")))))

(defun org-table-delete-column (&optional arg)
  "Insert a new column into the table."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
         (beg (org-table-begin))
         (end (org-table-end))
         ;; Current cursor position
         (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
         (colpos col))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
          nil
        (org-table-goto-column col t)
        (and (looking-at "|[^|\n]+|")
             (replace-match "|")))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos))
  (org-table-align))

(defun org-table-move-column (&optional left)
  "Move the current column to the right.  With arg LEFT, move to the left."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
         (col1 (if left (1- col) col))
         (beg (org-table-begin))
         (end (org-table-end))
         ;; Current cursor position
         (linepos (+ (if (bolp) 1 0) (count-lines (point-min) (point))))
         (colpos (if left (1- col) (1+ col))))
    (if (and left (= col 1))
        (error "Cannot move column further left"))
    (if (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
        (error "Cannot move column further right"))
    (goto-char beg)
    (while (< (point) end)
      (if (org-at-table-hline-p)
          nil
        (org-table-goto-column col1 t)
        (and (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
             (replace-match "|\\2|\\1|")))
      (beginning-of-line 2))
    (move-marker end nil)
    (goto-line linepos)
    (org-table-goto-column colpos))
  (org-table-align))

(defun org-table-move-row (&optional up)
  "Move the current table line down. With arg UP, move it up."
  (interactive "P")
  (let ((col (current-column))
        (pos (point))
        (tonew (if up 0 2))
        txt)
    (beginning-of-line tonew)
    (if (not (org-at-table-p))
        (progn
          (goto-char pos)
          (error "Cannot move row further.")))
    (goto-char pos)
    (beginning-of-line 1)
    (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (point-at-eol))))
    (delete-region (point) (1+ (point-at-eol)))
    (beginning-of-line tonew)
    (insert txt)
    (beginning-of-line 0)
    (move-to-column col)))
    
(defun org-table-insert-row (&optional arg)
  "Insert a new row above the current line into the table.
With prefix ARG, insert below the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (if (string-match "^[ \t]*|-" line)
        (setq line (mapcar (lambda (x) (if (member x '(?| ?+)) ?| ?\ )) line))
      (setq line (mapcar (lambda (x) (if (equal x ?|) ?| ?\ )) line)))
    (beginning-of-line (if arg 2 1))
    (apply 'insert line)
    (insert "\n")
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (org-table-align)))

(defun org-table-insert-hline (&optional arg)
  "Insert a horizontal-line below the current line into the table.
With prefix ARG, insert above the current line."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (col (current-column))
        start)
    (if (string-match "^[ \t]*|-" line)
        (setq line
              (mapcar (lambda (x) (if (member x '(?| ?+))
                                      (prog1 (if start ?+ ?|) (setq start t))
                                    (if start ?- ?\ )))
                      line))
      (setq line
            (mapcar (lambda (x) (if (equal x ?|)
                                    (prog1 (if start ?+ ?|) (setq start t))
                                    (if start ?- ?\ )))
                    line)))
    (beginning-of-line (if arg 1 2))
    (apply 'insert line)
    (insert "\n")
    (beginning-of-line 0)
    (move-to-column col)))
      
(defun org-table-kill-row (&optional arg)
  "Delete the current row or horizontal line from the table."
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (let ((col (current-column)))
    (kill-region (point-at-bol) (min (1+ (point-at-eol)) (point-max)))
    (if (not (org-at-table-p)) (beginning-of-line 0))
    (move-to-column col)))

;; FIXME: I think I can make this more efficient
(defun org-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
No empty strings are returned if there are matches at the beginning
and end of sting."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun org-table-add-invisible-to-vertical-lines ()
  "Add an `invisible' property to vertical lines of current table."
  (interactive)
  (let* ((beg (org-table-begin))
         (end (org-table-end))
         (end1))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq end1 (point-at-eol))
        (if (looking-at org-table-dataline-regexp)
            (while (re-search-forward "|" end1 t)
              (add-text-properties (1- (point)) (point)
                                   '(invisible org-table)))
          (while (re-search-forward "[+|]" end1 t)
            (add-text-properties (1- (point)) (point)
                                 '(invisible org-table))))
        (beginning-of-line 2)))))

(defun org-table-toggle-vline-visibility (&optional arg)
  "Toggle the visibility of table vertical lines.
The effect is immediate and on all tables in the file.
With prefix ARG, make lines invisible when ARG if positive, make lines
visible when ARG is not positive"
  (interactive "P")
  (let ((action (cond
                 ((and arg (> (prefix-numeric-value arg) 0)) 'on)
                 ((and arg (< (prefix-numeric-value arg) 1)) 'off)
                 (t (if (org-in-invisibility-spec-p '(org-table))
                        'off
                      'on)))))
    (if (eq action 'off)
        (progn
          (org-remove-from-invisibility-spec '(org-table))
          (org-table-map-tables 'org-table-align)
          (message "Vertical table lines visible")
          (if (org-at-table-p)
              (org-table-align)))
      (org-add-to-invisibility-spec '(org-table))
      (org-table-map-tables 'org-table-align)
      (message "Vertical table lines invisible"))
    (redraw-frame (selected-frame))))

(defun org-table-map-tables (function)
  "Apply FUNCTION to the start of all tables in the  buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward org-table-any-line-regexp nil t)
        (message "Mapping tables: %d%%" (/ (* 100.0 (point)) (buffer-size)))
        (beginning-of-line 1)
        (if (looking-at org-table-line-regexp)
            (save-excursion (funcall function)))
        (re-search-forward org-table-any-border-regexp nil 1)))))

(defun org-table-sum ()
  "Sum numbers in region of current table column.
The result will be displayed in the echo area, and will be available
as kill to be inserted with \\[yank].

If there is an active region, it is interpreted as a rectangle and all
numbers in that rectangle will be summed.  If there is no active
region and point is located in a table column, sum all numbers in that
column.

If at least on number looks like a time HH:MM or HH:MM:SS, all other
numbers are assumed to be times as well (in decimal hours) and the
numbers are added as such."
  (interactive)
  (save-excursion
    (let (beg end col (timecnt 0) diff h m s)
      (if (org-region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq col (org-table-current-column))
        (goto-char (org-table-begin))
        (org-table-goto-column col)
        (skip-chars-backward "^|")
        (setq beg (point))
        (goto-char (org-table-end))
        (beginning-of-line 0)
        (org-table-goto-column col)
        (skip-chars-forward "^|")
        (setq end (point)))
      (let* ((l1 (progn (goto-char beg) 
                        (+ (if (bolp) 1 0) (count-lines (point-min) (point)))))
             (l2 (progn (goto-char end)
                        (+ (if (bolp) 1 0) (count-lines (point-min) (point)))))          
             (items (if (= l1 l2)
                        (split-string (buffer-substring beg end))
                      (split-string 
                       (mapconcat 'identity (extract-rectangle beg end) " "))))
             (numbers (delq nil (mapcar 'org-table-get-number-for-summing items)))
             (res (apply '+ numbers))
             (sres (if (= timecnt 0)
                       (format "%g" res)
                     (setq diff (* 3600 res)
                           h (floor (/ diff 3600)) diff (mod diff 3600)
                           m (floor (/ diff 60)) diff (mod diff 60)
                           s diff)
                     (format "%d:%02d:%02d" h m s))))
        (kill-new sres)
        (message (substitute-command-keys
                  (format "Sum of %d items: %-20s     (\\[yank] will insert result into buffer)"
                          (length numbers) sres)))))))

(defun org-table-get-number-for-summing (s)
  (let (n)
    (if (string-match "^ *|? *" s)
        (setq s (replace-match "" nil nil s)))
    (if (string-match " *|? *$" s)
        (setq s (replace-match "" nil nil s)))
    (setq n (string-to-number s))
    (cond
     ((and (string-match "0" s)
           (string-match "\\`[-+ \t0.edED]+\\'" s)) 0)
     ((string-match "\\`[ \t]+\\'" s)         nil)
     ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\'" s)
      (let ((h (string-to-number (or (match-string 1 s) "0")))
            (m (string-to-number (or (match-string 2 s) "0")))
            (s (string-to-number (or (match-string 4 s) "0"))))
        (if (boundp 'timecnt) (setq timecnt (1+ timecnt)))
        (* 1.0 (+ h (/ m 60.0) (/ s 3600.0)))))
     ((equal n 0)                             nil)
     (t n))))

;;; Exporting

(defconst org-level-max 20)

(defun org-export-find-first-heading-line (list)
  "Remove all lines from LIST which are before  the first headline."
  (let ((orig-list list)
        (re (concat "^" outline-regexp)))
    (while (and list
                (not (string-match re (car list))))
      (pop list))
    (or list orig-list)))

(defun org-skip-comments (lines)
  "Skip lines starting with \"#\" and subtrees starting with COMMENT."
  (let ((re1 "^\\(\\*+\\)[ \t]+COMMENT")
        (re2 "^\\(\\*+\\)[ \t\n\r]")
        rtn line level)
    (while (setq line (pop lines))
      (cond
       ((and (string-match re1 line)
             (setq level (- (match-end 1) (match-beginning 1))))
        ;; Beginning of a COMMENT subtree.  Skip it.
        (while (and (setq line (pop lines))
                    (or (not (string-match re2 line))
                        (> (- (match-end 1) (match-beginning 1)) level))))
        (setq lines (cons line lines)))
       ((string-match "^#" line)
        ;; an ordinary comment line
        )
       (t (setq rtn (cons line rtn)))))
    (nreverse rtn)))

;; ASCII

(defconst org-ascii-underline '(?\$ ?\# ?^ ?\~ ?\= ?\-)
  "Characters for underlining headings in ASCII export.")

(defconst org-html-entities 
  '(("nbsp")
    ("iexcl")
    ("cent")
    ("pound")
    ("curren")
    ("yen")
    ("brvbar")
    ("sect")
    ("uml")
    ("copy")
    ("ordf")
    ("laquo")
    ("not")
    ("shy")
    ("reg")
    ("macr")
    ("deg")
    ("plusmn")
    ("sup2")
    ("sup3")
    ("acute")
    ("micro")
    ("para")
    ("middot")
    ("odot"."o")
    ("star"."*")
    ("cedil")
    ("sup1")
    ("ordm")
    ("raquo")
    ("frac14")
    ("frac12")
    ("frac34")
    ("iquest")
    ("Agrave")
    ("Aacute")
    ("Acirc")
    ("Atilde")
    ("Auml")
    ("Aring") ("AA"."&Aring;")
    ("AElig")
    ("Ccedil")
    ("Egrave")
    ("Eacute")
    ("Ecirc")
    ("Euml")
    ("Igrave")
    ("Iacute")
    ("Icirc")
    ("Iuml")
    ("ETH")
    ("Ntilde")
    ("Ograve")
    ("Oacute")
    ("Ocirc")
    ("Otilde")
    ("Ouml")
    ("times")
    ("Oslash")
    ("Ugrave")
    ("Uacute")
    ("Ucirc")
    ("Uuml")
    ("Yacute")
    ("THORN")
    ("szlig")
    ("agrave")
    ("aacute")
    ("acirc")
    ("atilde")
    ("auml")
    ("aring")
    ("aelig")
    ("ccedil")
    ("egrave")
    ("eacute")
    ("ecirc")
    ("euml")
    ("igrave")
    ("iacute")
    ("icirc")
    ("iuml")
    ("eth")
    ("ntilde")
    ("ograve")
    ("oacute")
    ("ocirc")
    ("otilde")
    ("ouml")
    ("divide")
    ("oslash")
    ("ugrave")
    ("uacute")
    ("ucirc")
    ("uuml")
    ("yacute")
    ("thorn")
    ("yuml")
    ("fnof")
    ("Alpha")
    ("Beta")
    ("Gamma")
    ("Delta")
    ("Epsilon")
    ("Zeta")
    ("Eta")
    ("Theta")
    ("Iota")
    ("Kappa")
    ("Lambda")
    ("Mu")
    ("Nu")
    ("Xi")
    ("Omicron")
    ("Pi")
    ("Rho")
    ("Sigma")
    ("Tau")
    ("Upsilon")
    ("Phi")
    ("Chi")
    ("Psi")
    ("Omega")
    ("alpha")
    ("beta")
    ("gamma")
    ("delta")
    ("epsilon")
    ("varepsilon"."&epsilon;")
    ("zeta")
    ("eta")
    ("theta")
    ("iota")
    ("kappa")
    ("lambda")
    ("mu")
    ("nu")
    ("xi")
    ("omicron")
    ("pi")
    ("rho")
    ("sigmaf") ("varsigma"."&sigmaf;")
    ("sigma")
    ("tau")
    ("upsilon")
    ("phi")
    ("chi")
    ("psi")
    ("omega")
    ("thetasym") ("vartheta"."&thetasym;")
    ("upsih")
    ("piv")
    ("bull") ("bullet"."&bull;")
    ("hellip") ("dots"."&hellip;")
    ("prime")
    ("Prime")
    ("oline")
    ("frasl")
    ("weierp")
    ("image")
    ("real")
    ("trade")
    ("alefsym")
    ("larr") ("leftarrow"."&larr;") ("gets"."&larr;")
    ("uarr") ("uparrow"."&uarr;")
    ("rarr") ("to"."&rarr;") ("rightarrow"."&rarr;")
    ("darr")("downarrow"."&darr;")
    ("harr") ("leftrightarrow"."&harr;")
    ("crarr") ("hookleftarrow"."&crarr;") ; FIXME: has a round hook, not quite CR
    ("lArr") ("Leftarrow"."&lArr;")
    ("uArr") ("Uparrow"."&uArr;")
    ("rArr") ("Rightarrow"."&rArr;")
    ("dArr") ("Downarrow"."&dArr;")
    ("hArr") ("Leftrightarrow"."&hArr;")
    ("forall")
    ("part") ("partial"."&part;")
    ("exist") ("exists"."&exist;")
    ("empty") ("emptyset"."&empty;")
    ("nabla")
    ("isin") ("in"."&isin;")
    ("notin")
    ("ni")
    ("prod")
    ("sum")
    ("minus")
    ("lowast") ("ast"."&lowast;")
    ("radic")
    ("prop") ("proptp"."&prop;")
    ("infin") ("infty"."&infin;")
    ("ang") ("angle"."&ang;")
    ("and") ("vee"."&and;")
    ("or") ("wedge"."&or;")
    ("cap")
    ("cup")
    ("int")
    ("there4")
    ("sim") 
    ("cong") ("simeq"."&cong;")
    ("asymp")("approx"."&asymp;")
    ("ne") ("neq"."&ne;")
    ("equiv")
    ("le")
    ("ge")
    ("sub") ("subset"."&sub;")
    ("sup") ("supset"."&sup;")
    ("nsub")
    ("sube")
    ("supe")
    ("oplus")
    ("otimes")
    ("perp")
    ("sdot") ("cdot"."&sdot;")
    ("lceil")
    ("rceil")
    ("lfloor")
    ("rfloor")
    ("lang")
    ("rang")
    ("loz") ("Diamond"."&loz;")
    ("spades") ("spadesuit"."&spades;")
    ("clubs") ("clubsuit"."&clubs;")
    ("hearts") ("diamondsuit"."&hearts;")
    ("diams") ("diamondsuit"."&diams;")
    ("quot")
    ("amp")
    ("lt")
    ("gt")
    ("OElig")
    ("oelig")
    ("Scaron")
    ("scaron")
    ("Yuml")
    ("circ")
    ("tilde")
    ("ensp")
    ("emsp")
    ("thinsp")
    ("zwnj")
    ("zwj")
    ("lrm")
    ("rlm")
    ("ndash")
    ("mdash")
    ("lsquo")
    ("rsquo")
    ("sbquo")
    ("ldquo")
    ("rdquo")
    ("bdquo")
    ("dagger")
    ("Dagger")
    ("permil")
    ("lsaquo")
    ("rsaquo")
    ("euro")

    ("arccos"."arccos")
    ("arcsin"."arcsin")
    ("arctan"."arctan")
    ("arg"."arg")
    ("cos"."cos")
    ("cosh"."cosh")
    ("cot"."cot")
    ("coth"."coth")
    ("csc"."csc")
    ("deg"."deg")
    ("det"."det")
    ("dim"."dim")
    ("exp"."exp")
    ("gcd"."gcd")
    ("hom"."hom")
    ("inf"."inf")
    ("ker"."ker")
    ("lg"."lg")
    ("lim"."lim")
    ("liminf"."liminf")
    ("limsup"."limsup")
    ("ln"."ln")
    ("log"."log")
    ("max"."max")
    ("min"."min")
    ("Pr"."Pr")
    ("sec"."sec")
    ("sin"."sin")
    ("sinh"."sinh")
    ("sup"."sup")
    ("tan"."tan")
    ("tanh"."tanh")
    )
  "Entities for TeX->HTML translation.
Entries can be like (\"ent\"), in which case \"\\ent\" will be translated to
\"&ent;\".  An entry can also be a dotted pair like (\"ent\".\"&other;\").
In that case, \"\\ent\" will be translated to \"&other;\".
The list contains HTML entities for Latin-1, Greek and other symbols.
It is supplemented by a number of commonly used TeX macros with appropriate
translations.")

(defun org-export-as-ascii (arg)
  "Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3."
  (interactive "P")
  (let* ((region
          (buffer-substring
           (if (org-region-active-p) (region-beginning) (point-min))
           (if (org-region-active-p) (region-end) (point-max))))
         (lines (org-export-find-first-heading-line
                 (org-skip-comments (org-split-string region "[\r\n]"))))
         (level 0) line txt
         (umax nil)
         (case-fold-search nil)
         (filename (concat (file-name-sans-extension (buffer-file-name))
                           ".txt"))
         (buffer (find-file-noselect filename))
         (levels-open (make-vector org-level-max nil))
	 (date  (format-time-string "%Y/%m/%d" (current-time)))
	 (time  (format-time-string "%X" (current-time)))
         (author      user-full-name)
	 (title       (buffer-name))
         (options     nil)
	 (email       user-mail-address)
         (language    org-export-default-language)
	 (text        nil)
         (last-level  1)
         (todo nil)
         (lang-words nil))

    (org-init-section-numbers)

    (find-file-noselect filename)

    ;; Search for the export key lines
    (org-parse-key-lines)

    (setq lang-words (or (assoc language org-export-language-setup)
                         (assoc "en" org-export-language-setup)))
    (if org-export-ascii-show-new-buffer
	(switch-to-buffer-other-window buffer)
      (set-buffer buffer))
    (erase-buffer)
    (fundamental-mode)
    (if options (org-parse-export-options options))
    (setq umax (if arg (prefix-numeric-value arg)
                 org-export-headline-levels))

    ;; File header
    (if title (org-insert-centered title ?=))
    (insert "\n")
    (if (or author email)
        (insert (concat (nth 1 lang-words) ": " (or author "")
                        (if email (concat " <" email ">") "")
                        "\n")))
    (if (and date time) 
        (insert (concat (nth 2 lang-words) ": " date " " time "\n")))
    (if text (insert (concat (org-html-expand-for-ascii text) "\n\n")))

    (insert "\n\n")

    (if org-export-with-toc
        (progn
          (insert (nth 3 lang-words) "\n" 
                  (make-string (length (nth 3 lang-words)) ?=) "\n")
          (mapcar '(lambda (line)
		     (if (string-match "^\\(\\*+\\)[ \t]*\\(TODO\\|DONE\\)? *\\(.*\\)" line)
			 ;; This is a headline
			 (progn
			   (setq level (- (match-end 1) (match-beginning 1))
				 txt (match-string 3 line)
                                 todo 
                                 (or (and (match-beginning 2)
                                          (equal (match-string 2 line) "TODO"))
                                     (and (= level umax)
                                          (org-search-todo-below line lines level))))
                           (setq txt (org-html-expand-for-ascii txt))

                           (if org-export-with-section-numbers
                               (setq txt (concat (org-section-number level)
                                                 " " txt)))
			   (if (<= level umax)
			       (progn
				 (insert
                                  (make-string (* (1- level) 4) ?\ )
				  (format (if todo "%s (*)\n" "%s\n") txt))
				 (setq last-level level))
			     ))))
		  lines)))

    (org-init-section-numbers)
    (while (setq line (pop lines))
      ;; Remove the quoted HTML tags.
      (setq line (org-html-expand-for-ascii line))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]*\\(.*\\)" line)
        ;; a Headline
        (setq level (- (match-end 1) (match-beginning 1))
              txt (match-string 2 line))
        (org-ascii-level-start level txt umax))
       (t (insert line "\n"))))
    (normal-mode)
    (save-buffer)
    (goto-char (point-min))))

(defun org-search-todo-below (line lines level)
  "Search the subtree below LINE for any TODO entries."
  (let ((rest (cdr (memq line lines)))
        (re "^\\(\\*+\\)[ \t]*\\(TODO\\|DONE\\)?")
        line lv todo)
    (catch 'exit
      (while (setq line (pop rest))
        (if (string-match re line)
            (progn
              (setq lv (- (match-end 1) (match-beginning 1))
                    todo (and (match-beginning 2)
                              (equal (match-string 2 line) "TODO")))
              (if (<= lv level) (throw 'exit nil))
              (if todo (throw 'exit t))))))))

;; FIXME: Try to handle <b> and <i> as faces via text properties.
(defun org-html-expand-for-ascii (line)
  "Handle quoted HTML for ASCII export."
  (if org-export-html-expand
      (while (string-match "@<[^<>\n]*>" line)
        ;; We just remove the tags for now.
        (setq line (replace-match "" nil nil line))))
  line)

(defun org-insert-centered (s &optional underline)
  "Insert the string S centered and underline it with character UNDERLINE."
  (let ((ind (max (/ (- 80 (length s)) 2) 0)))
    (insert (make-string ind ?\ ) s "\n")
    (if underline
        (insert (make-string ind ?\ )
                (make-string (length s) underline)
                "\n"))))

(defun org-ascii-level-start (level title umax)
  "Insert a new level in ASCII export."
  (let (char)
    (if (> level umax)
        (insert (make-string (* 2 (- level umax 1)) ?\ ) "* " title "\n")
      (if (or (not (equal (char-before) ?\n))
              (not (equal (char-before (1- (point))) ?\n)))
          (insert "\n"))
      (setq char (nth (- umax level) (reverse org-ascii-underline)))
      (if org-export-with-section-numbers
          (setq title (concat (org-section-number level) " " title)))
      (insert title "\n" (make-string (length title) char) "\n"))))

;; HTML

(defun org-insert-export-options-template ()
  "Insert into the buffer a template with information for exporting."
  (interactive)
  (if (not (bolp)) (newline))
  (insert (format
"+TITLE:     %s
+AUTHOR:    %s
+EMAIL:     %s
+LANGUAGE:  %s
+TEXT:      Some descriptive text to be emitted.  Several lines may 
+TEXT:      be given. Precede HTML tags with \"@\", like @<b>here@</b>.
+OPTIONS:   H:%d num:%s toc:%s \\n:%s @:%s ::%s |:%s ^:%s *:%s TeX:%s
" (buffer-name) (user-full-name) user-mail-address org-export-default-language
org-export-headline-levels
org-export-with-section-numbers
org-export-with-toc
org-export-preserve-breaks
org-export-html-expand
org-export-with-fixed-width
org-export-with-tables
org-export-with-sub-superscripts
org-export-with-emphasize
org-export-with-TeX-macros)))

(defun org-toggle-fixed-width-section (arg)
  "Toggle the fixed-width indicator at the beginning of lines in the region.
If there is no active region, only acts on the current line.
If the first non-white  character in the first line of the region is a
vertical bar \"|\", then the command removes the bar from all lines in
the region.  If the first character is not a bar, the command adds a
bar to all lines, in the column given by the beginning of the region.

If there is a numerical prefix ARG, create ARG new lines starting with \"|\"."
  (interactive "P")
  (let* ((cc 0)
         (regionp (org-region-active-p))
         (beg (if regionp (region-beginning) (point)))
         (end (if regionp (region-end)))
         (nlines (or arg (if (and beg end) (count-lines beg end) 1)))
         (re "[ \t]*\\(:\\)")
         off)
    (save-excursion
      (goto-char beg)
      (setq cc (current-column))
      (beginning-of-line 1)
      (setq off (looking-at re))
      (while (> nlines 0)
        (setq nlines (1- nlines))
        (beginning-of-line 1)
        (cond
         (arg
          (move-to-column cc t)
          (insert ":\n")
          (forward-line -1))
         ((and off (looking-at re))
          (replace-match "" t t nil 1))
         ((not off) (move-to-column cc t) (insert ":")))
        (forward-line 1)))))
          
(defun org-export-as-html-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-html arg 'hidden)
  (org-open-file (buffer-file-name)))

(defun org-export-as-html-batch ()
  "Call org-export-as-html, may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as-html org-export-headline-levels 'hidden)
)

(defun org-export-as-html (arg &optional hidden)
  "Export the outline as a pretty HTML file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (let* ((region-p (org-region-active-p))
         (region
          (buffer-substring
           (if region-p (region-beginning) (point-min))
           (if region-p (region-end) (point-max))))
         (all_lines 
          (org-skip-comments (org-split-string region "[\r\n]")))
         (lines (org-export-find-first-heading-line all_lines))
         (level 0) (line "") (origline "") txt todo
	 (last-level 1)
         (umax nil)
         (filename (concat (file-name-sans-extension (buffer-file-name))
                           ".html"))
         (buffer (find-file-noselect filename))
         (levels-open (make-vector org-level-max nil))
	 (date  (format-time-string "%Y/%m/%d" (current-time)))
	 (time  (format-time-string "%X" (current-time)))
         (author      user-full-name)
	 (title       (buffer-name))
         (options     nil)
	 (email       user-mail-address)
         (language    org-export-default-language)
	 (text        nil)
         (lang-words  nil)
	 (head-count  0) cnt
         table-open type
         table-buffer table-orig-buffer
	 )
    (message "Exporting...")

    (org-init-section-numbers)

    ;; Search for the export key lines
    (org-parse-key-lines)
    (setq lang-words (or (assoc language org-export-language-setup)
                         (assoc "en" org-export-language-setup)))

    ;; Switch to the output buffer
    (if (or hidden (not org-export-html-show-new-buffer))
        (set-buffer buffer)
      (switch-to-buffer-other-window buffer))
    (erase-buffer)
    (fundamental-mode)
    (let ((case-fold-search nil))
      (if options (org-parse-export-options options))
      (setq umax (if arg (prefix-numeric-value arg)
                   org-export-headline-levels))
      
      ;; File header
      (insert (format
               "<html lang=\"%s\"><head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html\">
<meta name=generator content=\"org-mode\">
<meta name=generated content=\"%s %s\">
<meta name=author content=\"%s\">
</head><body>
"
         language (org-html-expand title) date time author))
      (if title     (insert (concat "<H1 align=\"center\">"
                                    (org-html-expand title) "</H1>\n")))
      (if author    (insert (concat (nth 1 lang-words) ": " author "\n")))
      (if email	  (insert (concat "<a href=\"mailto:" email "\">&lt;" email "&gt;</a>\n")))
      (if (or author email) (insert "<br>\n"))
      (if (and date time) (insert (concat (nth 2 lang-words) ": "
                                          date " " time "<br>\n")))
      (if text      (insert (concat "<p>\n" (org-html-expand text))))
      (if org-export-with-toc
          (progn
            (insert (format "<H2>%s</H2>\n" (nth 3 lang-words)))
            (insert "<ul>\n")
            (mapcar '(lambda (line)
                       (if (string-match "^\\(\\*+\\)[ \t]*\\(TODO\\|DONE\\)?\\(.*\\)" line)
                           ;; This is a headline
                           (progn
                             (setq level (- (match-end 1) (match-beginning 1))
                                   txt (org-html-expand (match-string 3 line))
                                   todo
                                   (or (and (match-beginning 2)
                                            (equal (match-string 2 line) "TODO"))
                                       (and (= level umax)
                                            (org-search-todo-below line lines level))))
                             (if org-export-with-section-numbers
                                 (setq txt (concat (org-section-number level)
                                                   " " txt)))
                             (if (<= level umax)
                                 (progn
                                   (setq head-count (+ head-count 1))
                                   ;;(if (> level last-level) (insert "<ul>\n"))
                                   (if (> level last-level)
                                       (progn
                                         (setq cnt (- level last-level))
                                         (while (>= (setq cnt (1- cnt)) 0)
                                           (insert "<ul>"))
                                         (insert "\n")))
                                   (if (< level last-level)
                                       (progn
                                         (setq cnt (- last-level level))
                                         (while (>= (setq cnt (1- cnt)) 0)
                                           (insert "</ul>"))
                                         (insert "\n")))
                                   (insert
                                    (format
                                     (if todo
                                         "<li><a href=\"#sec-%d\"><span style='color:red'>%s</span></a></li>\n"
                                       "<li><a href=\"#sec-%d\">%s</a></li>\n")
                                     head-count txt))
                                   (setq last-level level))
                               ))))
                    lines)
            (while (> last-level 0)
              (setq last-level (1- last-level))
              (insert "</ul>\n"))
            ))
      (setq head-count 0)
      (org-init-section-numbers)
      
      (while (setq line (pop lines) origline line)
        ;; replace "<" and ">" by "&lt;" and "&gt;"
        ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
        (setq line (org-html-expand line))
        
        ;; Verbatim lines
        (if (and org-export-with-fixed-width
                 (string-match "^[ \t]*:\\(.*\\)" line))
            (progn
              (let ((l (match-string 1 line)))
                (while (string-match " " l)
                  (setq l (replace-match "&nbsp;" t t l)))
                (insert "\n<span style='font-family:Courier'>"
                        l "</span>"
                        (if (and lines 
                                 (not (string-match "^[ \t]+\\(:.*\\)" (car lines))))
                            "<br>\n" "\n"))))

          (when (string-match org-link-regexp line)
            (setq type (match-string 1 line))
            (cond 
             ((member type '("http" "https" "ftp" "mailto" "news"))
              ;; standard URL
              (setq line (replace-match "<a href=\"\\1:\\2\">&lt;\\1:\\2&gt;</a>"
                                        nil nil line)))
             ((string= type "file")
              ;; FILE link
              (setq line (replace-match
                          "<a href=\"\\2\">\\1:\\2</a>"
                          nil nil line)))
             ((member type '("bbdb" "vm" "gnus" "shell"))
              (setq line (replace-match
                          "<i>&lt;\\1:\\2&gt;</i>" nil nil line)))))
          
          ;; TODO items
          (if (string-match "^\\(\\*+\\)[ \t]*\\(TODO\\)\\>" line)
              (setq line (replace-match "<span style='color:red'>TODO</span>"
                                        nil nil line 2)))
          ;; DONE items
          (if (string-match "^\\(\\*+\\)[ \t]*\\(DONE\\)\\>" line)
              (setq line (replace-match
                          "<span style='color:green'>DONE</span>"
                          nil nil line 2)))
          ;; DEADLINES
          (if (string-match "\\<DEADLINE\\>.*" line)
              (progn
              (if (save-match-data
                    (string-match "<a href"
                                  (substring line 0 (match-beginning 0))))
                  nil  ; Don't do the replacement - it is inside a link
                (setq line (replace-match "<span style='color:red'>\\&</span>"
                                          nil nil line)))))
          
          (cond
           ((string-match "^\\(\\*+\\)[ \t]*\\(.*\\)" line)
            ;; This is a headline
            (setq level (- (match-end 1) (match-beginning 1))
                  txt (match-string 2 line))
            (if (<= level umax) (setq head-count (+ head-count 1)))
            (org-html-level-start level txt umax
                                  (and org-export-with-toc (<= level umax))
                                  head-count))

           ((and org-export-with-tables
                 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
            (if (not table-open)
                ;; New table starts
                (setq table-open t table-buffer nil table-orig-buffer nil))
            ;; Accumulate lines
            (setq table-buffer (cons line table-buffer)
                  table-orig-buffer (cons origline table-orig-buffer))
            (when (or (not lines)
                      (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" (car lines))))
              (setq table-open nil
                    table-buffer (nreverse table-buffer)
                    table-orig-buffer (nreverse table-orig-buffer))
              (insert (org-format-table-html table-buffer table-orig-buffer))))
           (t
            ;; Normal lines
            ;; Lines starting with "-", and empty lines make new paragraph.
            (if (string-match "^ *-\\|^[ \t]*$" line) (insert "<p>"))
            (insert line (if org-export-preserve-breaks "<br>\n" "\n"))))
          ))
      (if org-export-html-with-timestamp
          (insert org-export-html-html-helper-timestamp))
      (insert "</body>\n</html>\n")
      (normal-mode)
      (save-buffer)
      (goto-char (point-min)))))

(defun org-format-table-html (lines olines)
  "Find out which HTML converter to use and return the HTML code."
  (if (string-match "^[ \t]*|" (car lines))
      ;; A normal org table
      (org-format-org-table-html lines)
    ;; Table made by table.el - test for spanning
    (let* ((hlines (delq nil (mapcar 
                              (lambda (x)
                                (if (string-match "^[ \t]*\\+-" x) x
                                  nil))
                              lines)))
           (first (car hlines))
           (ll (and (string-match "\\S-+" first)
                    (match-string 0 first)))
           (re (concat "^[ \t]*" (regexp-quote ll)))
           (spanning (delq nil (mapcar (lambda (x) (not (string-match re x)))
                                       hlines))))
      (if (and (not spanning)
               (not org-export-prefer-native-exporter-for-tables))
          ;; We can use my own converter with HTML conversions
          (org-format-table-table-html lines)
        ;; Need to use the code generator in table.el, with the original text.
        (org-format-table-table-html-using-table-generate-source olines)))))

(defun org-format-org-table-html (lines)
  "Format a table into html."
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (let ((head (and org-export-highlight-first-table-line
                   (delq nil (mapcar 
                              (lambda (x) (string-match "^[ \t]*|-" x))
                              lines))))
        lastline line fields html empty)
    (setq html (concat org-export-html-table-tag "\n"))
    (while (setq lastline line
                 line (pop lines))
      (setq empty "&nbsp")
      (catch 'next-line
        (if (string-match "^[ \t]*|-" line)
            (if lastline
                ;; A hline: simulate an empty table row instead.
                (setq line (org-fake-empty-table-line lastline)
                      head nil
                      empty "")
              ;; Ignore this line
              (throw 'next-line t)))
        ;; Break the line into fields
        (setq fields (org-split-string line "[ \t]*|[ \t]*"))
        (setq html (concat
                    html
                    "<tr>"
                    (mapconcat (lambda (x)
                                 (if (equal x "") (setq x empty))
                                 (if head
                                     (concat "<th>" x "</th>")
                                   (concat "<td valign=\"top\">" x "</td>")))
                               fields "")
                    "</tr>\n"))))
    (setq html (concat html "</table>\n"))
    html))

(defun org-fake-empty-table-line (line)
  "Replace everything except \"|\" with spaces."
  (let ((i (length line))
	(newstr (copy-sequence line)))
    (while (> i 0)
      (setq i (1- i))
      (if (not (eq (aref newstr i) ?|))
	  (aset newstr i ?\ )))
    newstr))

(defun org-format-table-table-html (lines)
  "Format a table generated by table.el into html.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer 
             (head org-export-highlight-first-table-line)
             fields html empty)
    (setq html (concat org-export-html-table-tag "\n"))
    (while (setq line (pop lines))
      (setq empty "&nbsp")
      (catch 'next-line
        (if (string-match "^[ \t]*\\+-" line)
            (progn
              (if field-buffer
                  (progn
                    (setq html (concat
                                html
                                "<tr>"
                                (mapconcat
                                 (lambda (x)
                                   (if (equal x "") (setq x empty))
                                   (if head
                                       (concat "<th valign=\"top\">" x "</th>\n")
                                     (concat "<td valign=\"top\">" x "</td>\n")))
                                 field-buffer "\n")                             
                                "</tr>\n"))
                    (setq head nil)
                    (setq field-buffer nil)))
              ;; Ignore this line
              (throw 'next-line t)))
        ;; Break the line into fields and store the fields
        (setq fields (org-split-string line "[ \t]*|[ \t]*"))
        (if field-buffer
            (setq field-buffer (mapcar
                                (lambda (x)
                                  (concat x "<br>" (pop fields)))
                                field-buffer))
          (setq field-buffer fields))))
    (setq html (concat html "</table>\n"))
    html))

(defun org-format-table-table-html-using-table-generate-source (lines)
  "Format a table into html, using `table-generate-source' from table.el.
This has the advantage that cell- or row-spanning is allowed.
But it has the disadvantage, that org-mode's HTML conversions cannot be used."
  (require 'table)
  (save-excursion
    (set-buffer (get-buffer-create " org-tmp1 "))
    (erase-buffer)
    (insert (mapconcat 'identity lines "\n"))
    (goto-char (point-min))
    (if (not (re-search-forward "|[^+]" nil t))
        (error "Error processing table."))
    (table-recognize-table)
    (save-excursion
      (set-buffer (get-buffer-create " org-tmp2 "))
      (erase-buffer))
    (table-generate-source 'html " org-tmp2 ")
    (set-buffer " org-tmp2 ")
    (buffer-substring (point-min) (point-max))))

(defun org-html-expand (string)
  "Prepare STRING for HTML export. Applies all active conversions."
  ;; First check if there is a link in the line - if yes, apply conversions
  ;; only before the start of the link.
  (let* ((m (string-match org-link-regexp string))
         (s (if m (substring string 0 m) string))
         (r (if m (substring string m) "")))
    ;; convert < to &lt; and > to &gt;
    (while (string-match "<" s)
      (setq s (replace-match "&lt;" nil nil s)))
    (while (string-match ">" s)
      (setq s (replace-match "&gt;" nil nil s)))
    (if org-export-html-expand
        (while (string-match "@&lt;\\([^&]*\\)&gt;" s)
          (setq s (replace-match "<\\1>" nil nil s))))
    (if org-export-with-sub-superscripts
        (setq s (org-export-html-convert-sub-super s)))
    (if org-export-with-emphasize
        (setq s (org-export-html-convert-emphasize s)))
    (if org-export-with-TeX-macros
        (let ((start 0) wd ass)
          (while (setq start (string-match "\\\\\\([a-zA-Z]+\\)" s start))
            (setq wd (match-string 1 s))
            (if (setq ass (assoc wd org-html-entities))
                (setq s (replace-match (or (cdr ass)
                                           (concat "&" (car ass) ";"))
                                       t t s))
              (setq start (+ start (length wd)))))))
    (concat s r)))

(defun org-create-multibrace-regexp (left right n)
  "Create a regular expression which will match a balanced sexp.
Opening delimiter is LEFT, and closing delimiter is RIGHT, both given
as single character strings.
The regexp returned will match the entire expression including the
delimiters.  It will also define a single group which contains the
match except for the outermost delimiters. The maximum depth of
stacked delimiters is N. Escaping delimiters is not possible."
  (let* ((nothing (concat "[^" "\\" left "\\" right "]*?"))
         (or "\\|")
         (re nothing)
         (next (concat "\\(?:" nothing left nothing right "\\)+" nothing)))
    (while (> n 1)
      (setq n (1- n)
            re (concat re or next)
            next (concat "\\(?:" nothing left next right "\\)+" nothing)))
    (concat left "\\(" re "\\)" right)))

(defvar org-match-substring-regexp
  (concat 
   "\\([^\\]\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
   "\\|"
   "\\([-+]?[^-+*!@#$%^_ \t\r\n,:\"?<>~;./{}=()]+\\)\\)")
  "The regular expression matching a sub- or superscript.")
  
;; FIXME: Make T_A^* work
(defun org-export-html-convert-sub-super (string)
  "Convert sub- and superscripts in STRING to HTML."
  (let (key c)
    (while (string-match org-match-substring-regexp string)
      (setq key (if (string= (match-string 2 string) "_") "sub" "sup"))
      (setq c (or (match-string 8 string)
                  (match-string 6 string)
                  (match-string 5 string)))
      (setq string (replace-match
                    (concat (match-string 1 string)
                            "<" key ">" c "</" key ">")
                    t t string)))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string))))
  string)

(defun org-export-html-convert-emphasize (string)
  (while (string-match
          "\\(\\s-\\|^\\)\\(\\*\\([a-zA-Z]+\\)\\*\\)\\([^a-zA-Z*]\\|$\\)"
          string)
    (setq string (replace-match
                  (concat "<i>" (match-string 3 string) "</i>")
                  t t string 2)))
  string)

(defun org-parse-key-lines ()
  "Find the special key lines with the information for exporters."
  (save-excursion
    (goto-char 0)
    (let ((re (concat "^[ \t]*\\+[ \t]*\\("
                         "TITLE\\|AUTHOR\\|EMAIL\\|TEXT\\|OPTIONS\\|LANGUAGE"
                         "\\):[ \t]*\\(.*\\)"))
          key)
      (while (re-search-forward re nil t)
        (setq key (match-string 1))
        (cond ((string-equal key "TITLE") 
               (setq title (match-string 2)))
              ((string-equal key "AUTHOR") 
               (setq author (match-string 2)))
              ((string-equal key "EMAIL")
               (setq email (match-string 2)))
              ((string-equal key "LANGUAGE")
               (setq language (match-string 2)))
              ((string-equal key "TEXT") 
               (setq text (concat text "\n" (match-string 2))))
              ((string-equal key "OPTIONS")
               (setq options (match-string 2))))))))
  
(defun org-parse-export-options (s)
  "Parse the export options line."
  (let ((op '(("H"     . org-export-headline-levels)
              ("num"   . org-export-with-section-numbers)
              ("toc"   . org-export-with-toc)
              ("\\n"   . org-export-preserve-breaks)
              ("@"     . org-export-html-expand)
              (":"     . org-export-with-fixed-width)
              ("|"     . org-export-with-tables)
              ("^"     . org-export-with-sub-superscripts)
              ("*"     . org-export-with-emphasize)
              ("TeX"   . org-export-with-TeX-macros)))
        o)
    (while (setq o (pop op))
      (if (string-match (concat (regexp-quote (car o)) ":\\([^ \t\n\r;,.]*\\)") s)
          (set (make-local-variable (cdr o))
               (car (read-from-string (match-string 1 s))))))))

(defun org-html-level-start (level title umax with-toc head-count)
  "Insert a new level in HTML export."
  (let ((l (1+ (max level umax))))
    (while (<= l org-level-max)
      (if (aref levels-open (1- l))
          (progn
            (org-html-level-close l)
            (aset levels-open (1- l) nil)))
      (setq l (1+ l)))
    (if (> level umax)
        (progn
          (if (aref levels-open (1- level))
              (insert "<li>" title "<p>\n")
            (aset levels-open (1- level) t)
            (insert "<ul><li>" title "<p>\n")))
      (if org-export-with-section-numbers
          (setq title (concat (org-section-number level) " " title)))
      (setq level (+ level 1))
      (if with-toc
	  (insert (format "\n<H%d><a name=\"sec-%d\">%s</a></H%d>\n"
			  level head-count title level))
	(insert (format "\n<H%d>%s</H%d>\n" level title level))))))

(defun org-html-level-close (level)
  "Terminate one level in HTML export."
  (insert "</ul>"))


;; Variable holding the vector with section numbers
(defvar org-section-numbers (make-vector org-level-max 0))

(defun org-init-section-numbers ()
  "Initialize the vector for the section numbers."
  (let* ((level  -1)
         (numbers (nreverse (org-split-string "" "\\.")))
         (depth (1- (length org-section-numbers)))
         (i depth) number-string)
    (while (>= i 0)
      (if (> i level)
          (aset org-section-numbers i 0)
        (setq number-string (or (car numbers) "0"))
        (if (string-match "\\`[A-Z]\\'" number-string)
            (aset org-section-numbers i
                  (- (string-to-char number-string) ?A -1))
            (aset org-section-numbers i (string-to-int number-string)))
        (pop numbers))
      (setq i (1- i)))))

(defun org-section-number (&optional level)
  "Return a string with the current section number.
When LEVEL is non-nil, increase section numbers on that level."
  (let* ((depth (1- (length org-section-numbers))) idx n (string ""))
    (when level
      (when (> level -1)
        (aset org-section-numbers
              level (1+ (aref org-section-numbers level))))
      (setq idx (1+ level))
      (while (<= idx depth)
        (if (not (= idx 1))
            (aset org-section-numbers idx 0))
        (setq idx (1+ idx))))
    (setq idx 0)
    (while (<= idx depth)
      (setq n (aref org-section-numbers idx))
      (setq string (concat string (if (not (string= string "")) "." "")
                           (int-to-string n)))
      (setq idx (1+ idx)))
    (save-match-data
      (if (string-match "\\`\\([@0]\\.\\)+" string)
          (setq string (replace-match "" nil nil string)))
      (if (string-match "\\(\\.0\\)+\\'" string)
          (setq string (replace-match "" nil nil string))))
    string))


;;; Key bindings

;; - Bindings in org-mode map are currently
;;     abcdefghijklmnopqrstuvwxyz         the alphabet
;;     abcd fg    l nop rstuvwxyz         necessary bindings
;;         e           q                  reasonably useful from outline-mode
;;             i k                        expendable from outline-mode
;;            h j  m                      free

(define-key org-mode-map [(tab)] 'org-cycle)
(define-key org-mode-map [(meta shift left)] 'org-shiftmetaleft)
(define-key org-mode-map [(meta left)] 'org-metaleft)
(define-key org-mode-map [(meta shift right)] 'org-shiftmetaright)
(define-key org-mode-map [(meta shift up)] 'org-shiftmetaup)
(define-key org-mode-map [(meta shift down)] 'org-shiftmetadown)
(define-key org-mode-map [(meta right)] 'org-metaright)
(define-key org-mode-map [(meta up)] 'org-metaup)
(define-key org-mode-map [(meta down)] 'org-metadown)
(define-key org-mode-map "\C-c\C-t" 'org-todo)
(define-key org-mode-map "\C-c\C-d" 'org-deadline)
(define-key org-mode-map "\C-c;"    'org-toggle-comment)
(define-key org-mode-map "\C-c\C-v" 'org-show-todo-tree)
(define-key org-mode-map "\C-c\C-w" 'org-check-deadlines)
(define-key org-mode-map "\C-c\C-s" 'org-occur)   ; alternative binding
(define-key org-mode-map "\C-c/"    'org-occur)   ; Minor-mode reserved
(define-key org-mode-map "\C-c\C-m" 'org-insert-heading)
(define-key org-mode-map "\M-\C-m"  'org-insert-heading)
(define-key org-mode-map "\C-c\C-l" 'org-insert-link)
(define-key org-mode-map "\C-c\C-o" 'org-open-at-point)
(define-key org-mode-map "\C-c\C-z" 'org-time-stamp)  ; Alternative binding
(define-key org-mode-map "\C-c."    'org-time-stamp)  ; Minor-mode reserved
(define-key org-mode-map "\C-c\C-y" 'org-evaluate-time-range)
(define-key org-mode-map "\C-c>"    'org-goto-calendar)
(define-key org-mode-map "\C-c<"    'org-date-from-calendar)
(define-key org-mode-map "\C-c\C-r"       'org-diary-view)
(define-key org-mode-map [(shift up)]     'org-timestamp-up)
(define-key org-mode-map [(shift down)]   'org-timestamp-down)
(define-key org-mode-map [(shift left)]   'org-timestamp-down-day)
(define-key org-mode-map [(shift right)]  'org-timestamp-up-day)
(define-key org-mode-map "\C-c-"          'org-table-insert-hline)
(define-key org-mode-map [(shift tab)]    'org-shifttab)
(define-key org-mode-map "\C-c\C-c"       'org-ctrl-c-ctrl-c)
(define-key org-mode-map [(return)]       'org-return)
(define-key org-mode-map [(shift return)] 'org-table-copy-from-above)
(define-key org-mode-map [(control up)]   'org-move-line-up)
(define-key org-mode-map [(control down)] 'org-move-line-down)
(define-key org-mode-map "\C-c+"          'org-table-sum)
(define-key org-mode-map "\C-c|"          'org-table-toggle-vline-visibility)
(define-key org-mode-map "\C-c="          'org-table-create)
(define-key org-mode-map "\C-c#"          'org-table-create-with-table.el)
(define-key org-mode-map "\C-c\C-xa"      'org-export-as-ascii)
(define-key org-mode-map "\C-c\C-x\C-a"   'org-export-as-ascii)
(define-key org-mode-map "\C-c\C-xt"      'org-insert-export-options-template)
(define-key org-mode-map "\C-c:"          'org-toggle-fixed-width-section)
(define-key org-mode-map "\C-c\C-xh"      'org-export-as-html)
(define-key org-mode-map "\C-c\C-x\C-h"   'org-export-as-html-and-open)

(defun org-shiftcursor-error ()
  "Throw an error because Shift-Cursor command was applied in wrong context."
  (error "This command is only active in tables and on headlines."))

(defun org-shifttab ()
  "Call `(org-cycle t)' or `org-table-previous-field'."
  (interactive)
  (cond
   ((org-at-table-p) (org-table-previous-field))
   (t (org-cycle '(4)))))

(defun org-shiftmetaleft (&optional arg)
  "Call `org-promote-subtree' or `org-table-delete-column'."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-delete-column arg))
   ((org-on-heading-p) (org-promote-subtree arg))
   (t (org-shiftcursor-error))))
(defun org-shiftmetaright (&optional arg)
  "Call `org-demote-subtree' or `org-table-insert-column'."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-insert-column arg))
   ((org-on-heading-p) (org-demote-subtree arg))
   (t (org-shiftcursor-error))))
(defun org-shiftmetaup (&optional arg)
  "Call `org-move-subtree-up' or `org-table-kill-row'."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-kill-row arg))
   ((org-on-heading-p) (org-move-subtree-up arg))
   (t (org-shiftcursor-error))))
(defun org-shiftmetadown (&optional arg)
  "Call `org-move-subtree-down' or `org-table-insert-row'."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-insert-row arg))
   ((org-on-heading-p) (org-move-subtree-down arg))
   (t (org-shiftcursor-error))))

(defun org-metaleft (&optional arg)
  "Call `org-do-promote' or `org-table-move-column' to left."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-move-column 'left))
   ((or (org-on-heading-p) (org-region-active-p)) (org-do-promote arg))
   (t (org-shiftcursor-error))))
(defun org-metaright (&optional arg)
  "Call `org-do-demote' or `org-table-move-column' to right."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-move-column nil))
   ((or (org-on-heading-p) (org-region-active-p)) (org-do-demote arg))
   (t (org-shiftcursor-error))))
(defun org-metaup (&optional arg)
  "Call `org-move-subtree-up' or `org-table-move-row' up."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-table-move-row 'up))
   ((org-on-heading-p) (org-move-subtree-up arg))
   (t (org-shiftcursor-error))))
(defun org-metadown (&optional arg)
  "Call `org-move-subtree-down' or `org-table-move-row' down."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-move-row nil))
   ((org-on-heading-p) (org-move-subtree-down arg))
   (t (org-shiftcursor-error))))

(defun org-ctrl-c-ctrl-c (&optional arg)
  "Call realign table, or recognize a table.el table.
When the cursor is inside a table created by the table.el package,
activate that table.  Otherwise, if the cursor is at a normal table
created with org.el, re-align that table.  This command works even if
the automatic table editor has been turned off."
  (interactive "P")
  (let  ((org-enable-table-editor t))
    (cond 
     ((org-at-table.el-p)
      (require 'table)
      (beginning-of-line 1)
      (re-search-forward "|" (save-excursion (end-of-line 2) (point)))
      (table-recognize-table))
     ((org-at-table-p)
      (org-table-align))
     (t (error "This command is only active in tables")))))

(defun org-return (&optional arg)
  "Call `org-table-next-row' or `newline'."
  (interactive "P")
  (cond 
   ((org-at-table-p) (org-table-next-row))
   (t (newline))))


;;; Menu entries

;; First, remove the outline menus.
(if org-xemacs-p
    (add-hook 'org-mode-hook
              (lambda ()
                (delete-menu-item '("Headings"))
                (delete-menu-item '("Show"))
                (delete-menu-item '("Hide"))
                (set-menubar-dirty-flag)))
  (setq org-mode-map (delq (assoc 'menu-bar (cdr org-mode-map))
                             org-mode-map)))

;; Define the org-mode menus
(easy-menu-define org-org-menu org-mode-map "Org menu"
  '("Org"
    ["TODO/DONE/-" org-todo t]
    ["Show TODO Tree" org-show-todo-tree t]
    ["Occur" org-occur t]
    "--"
    ["Store link (global)" org-store-link t]
    ["Insert Link" org-insert-link t]
    ["Follow Link" org-open-at-point t]
    ;; ["BBDB" org-bbdb-name t]
    "--"
    ["Timestamp" org-time-stamp t]
    ("Change Date"
     ["1 day later" org-timestamp-up-day t]
     ["1 day earlier" org-timestamp-down-day t]
     ["1 ... later" org-timestamp-up t]
     ["1 ... earlier" org-timestamp-down t])
    ["Compute Time Range" org-evaluate-time-range t]
    "--"
    ["Deadline" org-deadline t]
    ["Check Deadlines" org-check-deadlines t]
    ["Time-Sorted View" org-diary-view t]
    "--"
    ["Goto Calendar" org-goto-calendar t]
    ["Date from Calendar" org-date-from-calendar t]
    "--"
    ["Show Version" org-version t]
    ["Show Documentation" org-show-commentary t]
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Build Full Customize Menu" org-create-customize-menu 
      (fboundp 'customize-menu-create)])
    ))
;;         ("Documentation"
;;          ["Info" org-info t]
;;          ["Commentary" org-show-commentary t]

(easy-menu-define org-struct-menu org-mode-map "Org menu"
  '("Struct"
    ["Cycle" org-cycle (or (bobp) (outline-on-heading-p))]
    ["Show All" show-all t]
    "--"
    ["New Heading" org-insert-heading t]
    ("Motion"
     ["Up" outline-up-heading t]
     ["Next" outline-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t])
    ("Edit Structure"
     ["Move subtree up" org-shiftmetaup (not (org-at-table-p))]
     ["Move subtree down" org-shiftmetadown (not (org-at-table-p))]
     ["Promote Heading" org-metaleft (not (org-at-table-p))]
     ["Promote Subtree" org-shiftmetaleft (not (org-at-table-p))]
     ["Demote Heading"  org-metaright (not (org-at-table-p))]
     ["Demote Subtree"  org-shiftmetaright (not (org-at-table-p))])
    "--"
    ("Table"
     ["Align" org-ctrl-c-ctrl-c (org-at-table-p)]
     ["Next field" org-cycle (org-at-table-p)]
     ["Previous Field" org-shifttab (org-at-table-p)]
     ["Next row" org-return (org-at-table-p)]
     ["Copy field from above" org-table-copy-from-above (org-at-table-p)]
     "--"
     ["Move column left" org-metaleft (org-at-table-p)]
     ["Move column right" org-metaright (org-at-table-p)]
     ["Move row up" org-metaup (org-at-table-p)]
     ["Move row down" org-metadown (org-at-table-p)]
     "--"
     ["Delete row" org-shiftmetaup (org-at-table-p)]
     ["Insert row" org-shiftmetadown (org-at-table-p)]
     ["Insert hline" org-table-insert-hline (org-at-table-p)]
     "--"
     ["Delete column" org-shiftmetaleft (org-at-table-p)]
     ["Insert column" org-shiftmetaright (org-at-table-p)]
     "--"
     ["Sum column/rectangle" org-table-sum (or (org-at-table-p) (org-region-active-p))]
     "--"
     ["Invisible Vlines" org-table-toggle-vline-visibility
      :style toggle :selected (org-in-invisibility-spec-p '(org-table))]
     "--"
     ["Create" org-table-create (and (not (org-at-table-p))
                                     org-enable-table-editor)]
     ["Create (table.el)" org-table-create-with-table.el 
      (not (org-at-table-p 'any))])
    "--"
    ("Export"
     ["ASCII" org-export-as-ascii t]
     ["HTML"  org-export-as-html t]
     ["HTML, and open" org-export-as-html-and-open t]
     "--"
     ["Option template" org-insert-export-options-template t]
     ["Toggle fixed width" org-toggle-fixed-width-section t])
    ))

;;; Documentation

(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (customize-browse 'org))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org mode, insert it into the menu."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (progn
	(easy-menu-change 
	 '("Org") "Customize"
	 `(["Browse Org group" org-customize t]
	   "--"
	   ,(customize-menu-create 'org)
	   ["Set" Custom-set t]
	   ["Save" Custom-save t]
	   ["Reset to Current" Custom-reset-current t]
	   ["Reset to Saved" Custom-reset-saved t]
	   ["Reset to Standard Settings" Custom-reset-standard t]))
	(message "\"Org\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

(defun org-show-commentary ()
  "Use the finder to view the file documentation from `org.el'."
  (interactive)
  (require 'finder)
  (finder-commentary "org.el"))

;;; Miscellaneous stuff

(defun org-move-line-down (arg)
  "Move the current line up."
  (interactive "p")
  (let ((col (current-column))
	beg end pos)
    (beginning-of-line 1) (setq beg (point))
    (beginning-of-line 2) (setq end (point))
    (beginning-of-line (+ 1 arg))
    (setq pos (move-marker (make-marker) (point)))
    (insert (delete-and-extract-region beg end))
    (goto-char pos)
    (move-to-column col)))

(defun org-move-line-up (arg)
  "Move the current line up."
  (interactive "p")
  (let ((col (current-column))
	beg end pos)
    (beginning-of-line 1) (setq beg (point))
    (beginning-of-line 2) (setq end (point))
    (beginning-of-line (+ -2 arg))
    (setq pos (move-marker (make-marker) (point)))
    (insert (delete-and-extract-region beg end))
    (goto-char pos)
    (move-to-column col)))


;; Functions needed for Emacs/XEmacs region compatibility

(defun org-region-active-p ()
  "Is transient-mark-mode on and the region active?
Works on both Emacs and XEmacs."
  (if org-ignore-region
      nil
    (if org-xemacs-p
        (and zmacs-regions (region-active-p))
      (and transient-mark-mode mark-active))))

(defun org-add-to-invisibility-spec (arg)
  "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (cond
   ((fboundp 'add-to-invisibility-spec)
    (add-to-invisibility-spec arg))
   ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
	(setq buffer-invisibility-spec (list arg)))
   (t
    (setq buffer-invisibility-spec
	  (cons arg buffer-invisibility-spec)))))

(defun org-remove-from-invisibility-spec (arg)
  "Remove elements from `buffer-invisibility-spec'."
  (if (fboundp 'remove-from-invisibility-spec)
      (remove-from-invisibility-spec arg)
    (if (consp buffer-invisibility-spec)
        (setq buffer-invisibility-spec 
              (delete arg buffer-invisibility-spec)))))

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?."
  (if (consp buffer-invisibility-spec)
      (member arg buffer-invisibility-spec)
    nil))

;; Functions needed for compatibility with old outline.el

;; The following functions capture the entire compatibility code
;; between the different versions of outline-mode.
;; If at some point in the future, the old outline-mode will
;; be completely replaced in both Emacs implementations, these functions
;; can be simplified and even inlined.

(defconst org-noutline-p (featurep 'noutline))

;; C-a should go to the beginning of a visible line, also in the
;; new outline.el.  I guess this should be patched into Emacs?
(when org-noutline-p
  (defun org-beginning-of-line ()
    "Go to the beginning of the current line.  If that is invisible, continue
to a visible line beginning.  This makes the function of C-a more intuitive."
    (interactive)
    (beginning-of-line 1)
    (backward-char 1)
    (if (org-invisible-p)
        (while (org-invisible-p)
          (backward-char 1)
          (beginning-of-line 1))
      (forward-char 1)))
  (define-key org-mode-map "\C-a" 'org-beginning-of-line))


;; A safer font-lock setup for the old outline mode.
;; FIXME: Make font-lock-fontification for XEmacs more like the one for Emacs?
(defconst org-xemacs-outline-font-lock-keywords
  '(;; Highlight headings according to the level.
    ("^\\(\\*+\\)[ \t]*\\([^\r\n]+\\)?[\n\r]"
     (1 font-lock-string-face)
     (2 (let ((len (- (match-end 1) (match-beginning 1))))
	  (or (cdr (assq len '((1 . font-lock-function-name-face)
			       (2 . font-lock-keyword-face)
			       (3 . font-lock-comment-face))))
	      font-lock-variable-name-face))
	nil t)))
  "Additional expressions to highlight in Outline mode.")

(defun org-patch-outline-font-lock-regexp ()
  "Supply a better font-lock-setup for the ole font-lock-mode.
The standard setup can crash."
  (if (not (featurep 'noutline))
      (set (make-local-variable 'font-lock-keywords )
           org-xemacs-outline-font-lock-keywords)))

(defun org-invisible-p ()
  "Check if point is at a character currently not visible."
  (if org-noutline-p
      ;; Early versions of noutline don't have `outline-invisible-p'.
      (if (fboundp 'outline-invisible-p)
          (outline-invisible-p)
        (get-char-property (point) 'invisible))
    (save-excursion
      (skip-chars-backward "^\r\n")
      (if (bobp) 
          nil
        (equal (char-before) ?\r)))))

(defun org-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (if org-noutline-p
      (outline-back-to-heading invisible-ok)
    (if (looking-at outline-regexp)
        t
      (if (re-search-backward (concat (if invisible-ok "[\r\n]" "^")
                                      outline-regexp)
                              nil t)
          (if invisible-ok
              (progn (forward-char 1)
                     (looking-at outline-regexp)))
        (error "Before first heading")))))

(defun org-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (if org-noutline-p
      (outline-on-heading-p 'invisible-ok)
    (save-excursion
      (skip-chars-backward "^\n\r")
      (and (looking-at outline-regexp)
           (or invisible-ok
               (bobp)
               (equal (char-before) ?\n))))))

(defun org-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (if org-noutline-p
      (outline-up-heading-all arg)
    (org-back-to-heading t)
    (looking-at outline-regexp)
    (if (<= (- (match-end 0) (match-beginning 0)) arg)
        (error "Cannot move up %d levels" arg)
    (re-search-backward 
     (concat "[\n\r]" (regexp-quote
                       (make-string (- (match-end 0) (match-beginning 0) arg)
                                    ?*))
             "[^*]"))
    (forward-char 1))))

(defun org-show-hidden-entry ()
  "Show an entry where even the heading is hidden."
  (save-excursion
    (if (not org-noutline-p)
        (progn
          (org-back-to-heading t)
          (org-flag-heading nil)))
    (show-entry)))

(defun org-check-occur-regexp (regexp)
  "If REGEXP starts with \"^\", modify it to check for \\r as well.
Of course, only for the old outline mode."
  (if org-noutline-p
      regexp
    (if (string-match "^\\^" regexp)
        (concat "[\n\r]" (substring regexp 1))
      regexp)))

(defun org-flag-heading (flag &optional entry)
  "Flag the current heading. FLAG non-nil means make invisible.
When ENTRY is non-nil, show the entire entry."
  (save-excursion
    (org-back-to-heading t)
    (if (not org-noutline-p)
        ;; Make the current headline visible
        (outline-flag-region (max 1 (1- (point))) (point) (if flag ?\r ?\n)))
    ;; Check if we should show the entire entry
    (if entry
        (progn
          (show-entry)
          (save-excursion  ;; FIXME: Is this the fix for points in the   -|
                           ;;        middle of text?                      |
            (and (outline-next-heading)   ;;                              |
                 (org-flag-heading nil))))  ; show the next heading      _|
      (outline-flag-region (max 1 (1- (point)))
                           (save-excursion (outline-end-of-heading) (point))
                           (if org-noutline-p
                               flag
                             (if flag ?\r ?\n))))))

;;; Finish up

(provide 'org)

;;; org.el ends here

