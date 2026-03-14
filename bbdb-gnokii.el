;; bbdb-gnokii.el --- Export phone entries from BBDB to gnokii contacts file.

;;
;; Copyright (C) 2000, 2003, 2004, 2005, 2006
;;                           Martin Schwenke, Reiner Steib, Len Trigg
;; Authors: Martin Schwenke <martin@meltin.net>,
;;          Reiner Steib <Reiner.Steib@gmx.de>,
;;          Len Trigg <len@reeltwo.com>
;; Maintainer: Martin Schwenke <martin@meltin.net>
;; Created: 23 August 2000
;; $Id: bbdb-gnokii.el,v 1.16 2006/04/19 13:02:09 martins Exp $
;; Keywords: BBDB, Nokia, gnokii
;; X-URL: http://meltin.net/hacks/emacs/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; If you have not received a copy of the GNU General Public License
;; along with this software, it can be obtained from the GNU Project's
;; World Wide Web server (http://www.gnu.org/copyleft/gpl.html), from
;; its FTP server (ftp://ftp.gnu.org/pub/gnu/GPL), by sending an electronic
;; mail to this program's maintainer or by writing to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


;;; Commentary:
;;
;; Exports BBDB phone entries to a contacts file that can be used by
;; gnokii to write them to a Nokia mobile phone.
;;
;; No responsibility for blowing up your phone... blah, blah, blah...
;; I recommend dumping your phone book to a file (using xgnokii, say)
;; and keeping it in a safe place until you are sure that
;; bbdb-gnokii.el doesn't do anything stupid.
;;
;; The latest version of this file is available via:
;;
;;   http://meltin.net/hacks/emacs/
;;
;; The gnokii web site is
;;
;;   http://www.gnokii.org/
;;
;; bbdb-gnokii.el is loosely based on JWZ's bbdb-pilot-jwz.el.
;;
;; gnokii expects a file with the following format:
;;
;;    name;number;memory_type;entry_location;caller_group_number;\
;;    subentry_type;subentry_number_type;subentry_id;subentry_text
;;
;; The length and syntax of "name" and "number" are limited, so some
;; munging goes on.  You can adjust the munging to your needs by
;; customizing the variables `bbdb-gnokii-firstname-transform',
;; `bbdb-gnokii-lastname-transform' and
;; `bbdb-gnokii-location-transform'.
;;
;; The default settings of these variables reflect Martin's preferences.
;; Here is a combination of alternative settings used by Reiner:
;;
;;   (setq
;;    ;; Use long firstnames and lastnames and a short location:
;;    bbdb-gnokii-firstname-transform 'bbdb-gnokii-transform-word
;;    bbdb-gnokii-lastname-transform  12
;;    bbdb-gnokii-location-transform  'bbdb-gnokii-transform-location
;;    bbdb-gnokii-max-name-length     16)
;;
;; The memory_type specifies where to write the contacts (phone memory
;; or SIM card).  See variable `bbdb-gnokii-default-memory-type' and
;; it's documentation for details.

;; Configuration:
;;
;; Add this to your ~/.emacs or equivalent:
;;
;;   (autoload
;;     'bbdb-gnokii-export
;;     "bbdb-gnokii"
;;     "Export phone entries from BBDB to a Gnokii contacts file."
;;     t)
;;
;; If you want to add some standard entries to your phone, you can put
;; them in a file and set the following variable:
;;
;;   (setq bbdb-gnokii-extras-file
;;         (expand-file-name "~/.bbdb-gnokii-extras.txt"))
;;
;; The contents of the specified file get appended to the file
;; generated from the BBDB (cf. `bbdb-gnokii-extras-file-position').
;; My phone vendor preloads a bunch of their numbers into the SIM
;; card, and I'm keeping them until I'm sure they're not useful!
;;
;; Entries are only extracted from the BBDB for entries that have a
;; gnokii field.  In general, if this field is present, then all of
;; the phone numbers (except those that have locations listed in
;; `bbdb-gnokii-exclude-locations') will be exported.
;;
;; For example:
;;
;;   Fred Smith - Widget, Inc.
;;         mobile: (04) 1234 5678
;;           home: (02) 1234 5678
;;         gnokii: t
;;
;; will have 2 items exported:
;;
;; Fred S mobile;0412345678;...
;; Fred S home;0212345678;...
;;
;; For entries with a name, the default generated name is the first
;; word of firstname, space, first letter of lastname.
;;
;; For entries without a name, but with a company, the default
;; generated name is the first word of the company name.
;;
;; The phone number locations are only appended if there is more than
;; 1 phone entry exported.
;;
;; If the gnokii field contains a string in double-quotes, then it
;; will be used as the name.
;;
;; If the gnokii field contains something like location=X then the
;; number for location will be put into speed-dial location X.  All
;; other entries are put between `bbdb-gnokii-general-min-location'
;; and `bbdb-gnokii-general-max-location'.
;;
;; If the gnokii field contains something like (Y) then the entry will
;; belong to caller group Y, otherwise
;; `bbdb-gnokii-default-caller-group' is used.
;;
;; So,
;;
;;   Fred Smith - Widget, Inc.
;;         mobile: (04) 1234 5678
;;           home: (02) 1234 5678
;;            fax: (02) 8765 4321
;;         gnokii: "Freddy" (0) mobile=2 home=3
;;
;; will have 2 items exported:
;;
;; Freddy mobile;0412345678;...;0;
;; Freddy home;0212345678;...;0;
;;
;; No item is exported for the fax number because it is a member of
;; `bbdb-gnokii-exclude-locations'.
;;
;; If the gnokii field contains "skip=foo", the phone number corresponding to
;; the location "foo" will not be exported.
;;
;; You can also export a whole BBDB record to a single gnokii entry by
;; setting `bbdb-gnokii-phonebook-style' to `multi' or `mega'.  In
;; this style the default phone number can be set by specifying the
;; associated location in the BBDB gnokii field.  For example, if the
;; gnokii field contains "[work]" then the phone number with location
;; "work" will be the default one.  If no default location is
;; specified in the gnokii field, then the order of preference is
;; determined by `bbdb-gnokii-preferred-phone-locations'.  If
;; `bbdb-gnokii-phonebook-style' is set to `mega', an email address
;; and postal address are also added to the gnokii entry, when
;; available.

;; See the variables and code below for more details.  You may check all
;; customizable variables using `M-x customize-group RET bbdb-gnokii RET'.

 

;;; History:

;; $Log: bbdb-gnokii.el,v $
;; Revision 1.16  2006/04/19 13:02:09  martins
;; Function bbdb-gnokii-do-name now just uses firstname if lastname is
;; not set.  Suggested by Magnus Henoch <mange@freemail.hu>.
;;
;; Revision 1.15  2005/06/06 09:52:49  martins
;; Added support for gnokii entries with multiple phone numbers using
;; subentries, implemented using a variation and subset of code by Len
;; Trigg: new variables bbdb-gnokii-phonebook-style and
;; bbdb-gnokii-preferred-phone-locations; removed variable
;; bbdb-gnokii-insert-extra-fields (replaced by
;; bbdb-gnokii-phonebook-style); removed defstruct bbdb-gnokii; replaced
;; functions bbdb-gnokii-convert and bbdb-gnokii-format with new function
;; bbdb-gnokii-format-record, which has most of the implementation
;; details for this feature; new functions bbdb-gnokii-format-address,
;; bbdb-gnokii-phones-find-location, bbdb-gnokii-get-default-phone;
;; retain '+' in phone number in function bbdb-gnokii-fix-phone;
;; bbdb-gnokii-export just calls bbdb-gnokii-format-record for each
;; record, instead of converting and then formatting; added Len Trigg to
;; to copyright and authors.
;;
;; Revision 1.14  2004/03/30 12:01:29  martins
;; After discussion with Reiner, changed the names of the following
;; variables:
;;
;;   bbdb-gnokii-default-group	-> bbdb-gnokii-default-caller-group
;;   bbdb-gnokii-default-memtype	-> bbdb-gnokii-default-memory-type
;;   bbdb-gnokii-general-maxpos	-> bbdb-gnokii-general-max-location
;;   bbdb-gnokii-general-minpos	-> bbdb-gnokii-general-min-location
;;   bbdb-gnokii-speed-maxpos	-> bbdb-gnokii-speed-dial-max-location
;;   bbdb-gnokii-speed-minpos	-> bbdb-gnokii-speed-dial-min-location
;;
;; for consistency with gnokii documentation, and changed associated
;; documentation accordingly.  In struct bbdb-gnokii- changed name of
;; member `mempos' to `location'.  Also changed function names:
;;
;;   bbdb-gnokii-do-mempos		-> bbdb-gnokii-do-location
;;   bbdb-gnokii-do-group		-> bbdb-gnokii-do-caller-group
;;
;; Revision 1.13  2004/03/02 00:50:53  martins
;; Documentation cleanups by Reiner Steib <Reiner.Steib@gmx.de>.
;;
;; Revision 1.12  2004/02/27 03:40:59  martins
;; bbdb-gnokii-default-memtype now has default value of "SM" (for
;; compatibility with xgnokii >= 0.6) - documentation and customisation
;; choices have also been improved.  bbdb-gnokii-default-group's
;; documentation and customisation choices have also been improved.
;; Changes implemented by Reiner Steib <Reiner.Steib@gmx.de>.
;;
;; Revision 1.11  2004/02/25 01:16:08  martins
;; Added RCS Log section in History and imported previous entries.
;; Thanks again to Reiner.
;;
;; Revision  1.10  2004/02/07 11:47:04  martins
;; Replaced uses of bbdb-gnokii-extra-tag with bbdb-gnokii-extra-tags.
;; Oops.

;; Revision  1.9  2004/02/07 11:40:31  martins
;; `bbdb-gnokii-extra-tags': New variable for Siemens C35 used in
;; `bbdb-gnokii-export'.  Exchanged defun and defalias:
;; bbdb-gnokii-export vs. bbdb-to-gnokii-file.  Changed documentation
;; section to mention bbdb-gnokii-export, not bbdb-to-gnokii.  Renamed
;; function bbdb-record-to-gnokii-records to bbdb-gnokii-convert.  Minor
;; cosmetic fixes.  Thanks to Reiner Steib <Reiner.Steib@gmx.de>.

;; Revision  1.8  2004/02/06 03:07:33  martins
;; Merged changes from Reiner Steib <reiner.steib@gmx.de>: Made
;; variables customizable.  Did some checkdoc fixes.  Made many things
;; more flexible, especially the shortening of firstname, lastname and
;; location strings.  Added `bbdb-gnokii-add-field'.  Also replaced
;; variable `bbdb-gnokii-maxpos' with `bbdb-gnokii-general-minpos' and
;; `bbdb-gnokii-general-maxpos'.  Added information about where to get
;; latest version.  Various documentation fixes.  Added variable
;; `bbdb-gnokii-insert-extra-fields'.  Various documentation fixes.
;; Removed declarations of variables `bbdb-gnokii-mempos' and
;; `bbdb-gnokii-speed-done' (since they are bound in a `let').

;; Revision  1.7  2003/07/01 01:44:29  martins
;; Added extra fields to output, which seem to be required for newer
;; phones/gnokiis.

;; Revision  1.6  2003/06/30 01:58:10  martins
;; (bbdb-gnokii-do-name): Handle case where lastname is empty.

;; Revision  1.5  2001/02/12 00:10:45  martin
;; Changed e-mail address.

;; Revision  1.4  2000/10/04 00:41:58  martins
;; Changed default-memtype back to "A".

;; Revision  1.3  2000/08/25 02:58:15  martins
;; - Added documentation and stuff at top.
;; - Changed spelling from Gnokii to gnokii.
;; - Changed default memory type to work with command-line gnokii.
;; - Reduced allowable length of names.
;; - Added bbdb-gnokii-exclude-locations and associated filtering.
;;   Thanks to Chris Yeoh.
;; - Added various comments.

;; Revision  1.2  2000/08/24 11:52:57  martins
;; Fixed RCS Id string.

;; Revision  1.1  2000/08/24 11:52:39  martins
;; Initial revision

;; src: https://web.archive.org/web/20060720041509/http://meltin.net/hacks/emacs/src/bbdb-gnokii.el [2026-03-14]

;;; Code:
(require 'bbdb)

;; Only for `bbdb-gnokii-add-field':
(autoload 'bbdb-merge-interactively "bbdb-snarf" nil nil)
(autoload 'bbdb-add-new-field "bbdb-com" nil nil)

(defgroup bbdb-gnokii nil
  "Sync BBDB and gnokii."
  :group 'bbdb)

(defcustom bbdb-gnokii-transform-word-regexp
  (if (string-match "[[:word:]]" "x")
      "[^-[:word:]]"
    ;; old Emacsen (e.g. Emacs 20) don't support character classes.
    "[^-A-Za-z]")
  "Regexp used to shorten names in `bbdb-gnokii-transform-word'."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "first word" "[^-[:word:]]")
		 (const :tag "ascii" "[^-A-Za-z]")
		 (regexp :tag "Other")))

(defun bbdb-gnokii-transform-max (name &optional limit)
  "Limit NAME to LIMIT characters."
  (if (> (length name) limit)
      (substring name 0 limit)
    name))

(defun bbdb-gnokii-transform-word (name &optional regexp)
  "Shorten NAME to first word.
`bbdb-gnokii-transform-word-regexp' is used unless REGEXP is given."
  (substring name
	     0 (string-match (or regexp bbdb-gnokii-transform-word-regexp) name)))

(defun bbdb-gnokii-transform-location (location)
  "Shorten LOCATION field."
  ;; The default BBDB location "Office" and "Other" both give "O"
  (if (string= name "Other")
      "o"
    (bbdb-gnokii-transform-max location 1)))

(defcustom bbdb-gnokii-firstname-transform 'bbdb-gnokii-transform-word
  "How to transform the lastname field to short variant.
If a function, call it with the name as it's argument.  If a number, use
substring with maximal length number."
  :group 'bbdb-gnokii
  :type '(choice (const bbdb-gnokii-transform-word)
		 (function)
		 (integer)))

(defcustom bbdb-gnokii-lastname-transform 1
  "How to transform the lastname field to short variant.
If a function, call it with the name as it's argument.  If a number, use
substring with maximal length number."
  :group 'bbdb-gnokii
  :type '(choice (const bbdb-gnokii-transform-word)
		 (function)
		 (integer)))

(defcustom bbdb-gnokii-location-transform 10
  "Transform the location field to short variant.
If a function, call it with the name as it's argument.  If a number, use
substring with maximal length number."
  :group 'bbdb-gnokii
  :type '(choice (const bbdb-gnokii-transform-location)
		 (function)
		 (integer)))

(defun bbdb-gnokii-apply-transform (transform name)
  "Apply transformation TRANSFORM to NAME and return a shortened name."
  (cond
   ((functionp transform)
    (funcall transform name))
   ((natnump transform)
    (bbdb-gnokii-transform-max name transform))
   (t name)))

(defcustom bbdb-gnokii-extras-file nil
  "Name of file containing extra entries to add to gnokii."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "None" nil)
		 (file)))

(defcustom bbdb-gnokii-extra-tags nil
  "List of two string elements: \"\(\"tag\" \"indicator\"\)\" or nil.
For each occurance of the form \"location=tag\" in the BBDB gnokii field, the
gnokii string \"indicator\" will be appended to the generated name in the
gnokii file.  In some Siemens phones \(C35 and possibly others\), the
indicator \"!\" is used to specify a \"VIP entry\"."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "Siemens C35 style" ("vip" "!"))
		 (list (symbol :tag "BBDB tag")
		       (symbol :tag "Gnokii string"))))

(defcustom bbdb-gnokii-default-output-file nil
  "Name of the default output file.
If the filename contains the string \"%s\", it will be replaced
with the current date in ISO format (YYYY-MM-DD)."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "None" nil)
		 (file)))

(defcustom bbdb-gnokii-extras-file-position 'bottom
  "Where to insert `bbdb-gnokii-extras-file'."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "Top" top)
		 (const :tag "Bottom" bottom)))

(defcustom bbdb-gnokii-inserted-hook nil
  "Hook run after all records from BBDB were inserted into the output buffer.
The hook is called in the output buffer immediately before saving the buffer."
  :group 'bbdb-gnokii
  :type 'hook)

(defcustom bbdb-gnokii-confirm-kill nil
  "Ask for confirmation before killing the output buffer."
  :group 'bbdb-gnokii
  :type 'boolean)

(defcustom bbdb-gnokii-phonebook-style 'single
  "Style for phonebook entries.
This affects the number of phone numbers per gnokii entry and the way
the name is constructed.  `single' (the default) allows only a single
phone number per gnokii entry, and also generates a subentry for that
phone number.  `ancient' also allows only a single phone number per
gnokii entry, but generates no subentry - older versions of gnokii
seem to work like this.  `multiple' causes all phone numbers for a
BBDB entry to be put into a single gnokki entry, using multiple
subentries - in this case the location is also never appended to the
name.  `mega' is like multiple but causes the 1st email address and
postal address to also be put into the gnokii entry.  Note that
`multiple' and `mega' probably won't work well with
`bbdb-gnokii-default-memory-type' set to \"SM\"."
  :type '(choice (const :tag "Single with subentry"      single)
		 (const :tag "Single without subentry"   ancient)
		 (const :tag "Multiple subentries"       multiple)
		 (const :tag "Subentries, email, postal" mega)))

(defcustom bbdb-gnokii-speed-dial-min-location 1
  "Minimum memory location allowed for speed dial entries.
See also `bbdb-gnokii-general-min-location',
`bbdb-gnokii-general-max-location' and
`bbdb-gnokii-speed-dial-max-location'.  The range for speed dial
entries should not overlap with the range for general entries, or
entries found in `bbdb-gnokii-extras-file'."
  :group 'bbdb-gnokii
  :type 'integer)

(defcustom bbdb-gnokii-speed-dial-max-location 9
  "Maximum memory location allowed for speed dial entries.
See also `bbdb-gnokii-general-min-location',
`bbdb-gnokii-general-max-location' and
`bbdb-gnokii-speed-dial-min-location'.  The range for speed dial
entries should not overlap with the range for general entries, or
entries found in `bbdb-gnokii-extras-file'."
  :group 'bbdb-gnokii
  :type 'integer)

(defcustom bbdb-gnokii-general-min-location 10
  "Minimum memory location allowed for general BBDB to gnokii entries.
See also `bbdb-gnokii-general-max-location',
`bbdb-gnokii-speed-dial-min-location' and
`bbdb-gnokii-speed-dial-max-location'.  The range for speed dial
entries should not overlap with the range for general entries, or
entries found in `bbdb-gnokii-extras-file'."
  :group 'bbdb-gnokii
  :type 'integer)

(defcustom bbdb-gnokii-general-max-location 89
  "Maximum memory location allowed for general BBDB to gnokii entries.
See also `bbdb-gnokii-general-min-location',
`bbdb-gnokii-speed-dial-min-location' and
`bbdb-gnokii-speed-dial-max-location'.  The range for speed dial
entries should not overlap with the range for general entries, or
entries found in `bbdb-gnokii-extras-file'."
  :group 'bbdb-gnokii
  :type 'integer)

(defcustom bbdb-gnokii-default-memory-type "SM"
  "Default type of phone memory to use for BBDB to gnokii entries.
\"SM\" is for SIM card, \"ME\" for the phone memory.  In versions prior to
0.6, `xgnokii' used \"A\" is for SIM card and \"B\" for the phone memory.
Please see the documentation of gnokii and xgnokii for valid values."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "SIM card" "SM")
		 (const :tag "phone memory" "ME")
		 (const :tag "SIM card (for old xgnokii)" "A")
		 (const :tag "phone memory (for old xgnokii)" "B")
		 ;; Other valid memory types probably aren't writable.
		 (string :tag "Other")))

(defcustom bbdb-gnokii-default-caller-group 5
  "Default caller group to put BBDB to gnokii entries into.

If the gnokii field contains something like \"\(N\)\" then the entry will
belong to caller group N, otherwise `bbdb-gnokii-default-caller-group'
is used.  Possible values are 0 \(Family\), 1 \(VIP\), 2 \(Friends\),
3 \(Colleagues\), 4 \(Other group\), 5 \(No group\).  Note that these
are defaults, you are able to change these manually in your phone.
See also \"caller_group_number\" in the documentation of gnokii."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "0 (Family)" 0)
		 (const :tag "1 (VIP)" 1)
		 (const :tag "2 (Friends)" 2)
		 (const :tag "3 (Colleagues)" 3)
		 (const :tag "4 (Other group)" 4)
		 (const :tag "5 (No group)" 5)
		 (integer :tag "Other")))

(defcustom bbdb-gnokii-max-name-length 17
  "Maximum allowable length of name field."
  :group 'bbdb-gnokii
  :type 'integer)

(defcustom bbdb-gnokii-exclude-locations '("fax")
  "List of locations for phone numbers to exclude."
  :group 'bbdb-gnokii
  :type '(choice (const :tag "None" nil)
		 (const :tag "Fax" '("fax"))
		 (repeat (string :tag "Field"))))

(defcustom bbdb-gnokii-preferred-phone-locations '("mobile" "home" "work")
  "List of locations for choosing default phone number in multi-phone entries.
The phone number associated with earliest location in this list is used.
Otherwise, the first phone number is used." 
  :group 'bbdb-gnokii
  :type '(choice (repeat (string :tag "Field"))))

(defconst bbdb-gnokii-label-type-alist
  '(("home"   .  2)
    ("mobile" .  3)
    ("fax"    .  4)
    ("office" .  6)
    ("work"   .  6)
    ("."      . 10))
  "Alist mapping BBDB phones labels to gnokii phone number types.")

(defun bbdb-gnokii-format-record (record)
  "Convert a BBDB RECORD to text in the current buffer."

  (let ((allphones (bbdb-record-phones record))
        (allnet (bbdb-record-net record))
        (alladdresses (bbdb-record-addresses record))
	(stuff (if (listp (bbdb-record-raw-notes record))
		   (cdr (assq 'gnokii (bbdb-record-raw-notes record)))))
	(subentry-id 0)
	name useloc phones default-phone print-escape-newlines)

    (setq name (bbdb-gnokii-do-name (bbdb-record-lastname record)
				    (bbdb-record-firstname record)
				    (bbdb-record-company record)
				    stuff))

    ;; Filter out unwanted phone locations and find default phone number.
    (while allphones
      (let ((p (car allphones)))
	(if (not (or (member (bbdb-phone-location p)
			     bbdb-gnokii-exclude-locations)
		     (string-match (concat "\\<skip=\\("
					   (bbdb-phone-location p)
					   "\\)\\>")
				   (or stuff ""))))
	  (add-to-list 'phones p)))
      (setq allphones (cdr allphones)))

    (if (memq bbdb-gnokii-phonebook-style '(multiple mega))
	(setq default-phone (bbdb-gnokii-get-default-phone phones stuff)))

    ;; Only continue if they have a name, a gnokii field and some
    ;; phone numbers.
    (if (and name stuff phones)
	(progn
	  ;; Only add the location to name if there is >1 phone number
	  ;; and we're not doing multiple-subentries.
	  (setq useloc (and (> (length phones) 1)
			    (memq bbdb-gnokii-phonebook-style
				       '(ancient single))))
	  ;; Create records.
	  (while phones
	    (let* ((location (bbdb-phone-location (car phones)))
		   (loc (bbdb-gnokii-apply-transform
			 bbdb-gnokii-location-transform
			 location))
		   (num (bbdb-gnokii-fix-phone
			 (bbdb-phone-string (car phones))))
		   (default-loc (or (and default-phone
					 (bbdb-gnokii-apply-transform
					  bbdb-gnokii-location-transform
					  (bbdb-phone-location default-phone)))
				    loc))
		   (default-num (or (and default-phone
					 (bbdb-gnokii-fix-phone
					  (bbdb-phone-string default-phone)))
				    num))
		   (name (if useloc (concat name " " loc) name)))
	      (if (> (length name) bbdb-gnokii-max-name-length)
		  ;; Don't error out on long entries, truncate them instead.
		  (progn
		    (message
		     (concat
		      "Name \"%s\" is too long.  "
		      "Maybe you want to edit gnokii field.")
		     name)
		    (setq
		     name
		     (cond
		      ;; Make sure we don't strip location:
		      (useloc
		       (concat
			(bbdb-gnokii-transform-max
			 name (- bbdb-gnokii-max-name-length
				 (1+ (length loc))))
			" " loc))
		      (t
		       (bbdb-gnokii-transform-max
			name bbdb-gnokii-max-name-length))))))
	      ;; Checking for bbdb-gnokii-extra-tags:
	      (when (and (car  bbdb-gnokii-extra-tags)
			 (cadr bbdb-gnokii-extra-tags)
			 location
			 (string-match
			  (concat
			   "\\<" location
			   "="
			   (regexp-quote (car bbdb-gnokii-extra-tags))
			   "\\>")
			  stuff))
		;; extra entry found, changing label:
		(setq name
		      (concat (bbdb-gnokii-transform-max
			       name (- bbdb-gnokii-max-name-length 1))
			      (cadr bbdb-gnokii-extra-tags))))

	      (if (= subentry-id 0)
		  (insert (format "%s;%s;%s;%s;%s"
				  name
				  default-num
				  bbdb-gnokii-default-memory-type
				  (bbdb-gnokii-do-location stuff default-loc)
				  (bbdb-gnokii-do-caller-group stuff))))
	      (if (not (eq bbdb-gnokii-phonebook-style 'ancient))
		  (insert (format ";11;%d;%d;%s" 
				  (assoc-default location
						 bbdb-gnokii-label-type-alist
						 'string-match nil)
				  subentry-id
				  num)))

	      ;; Continute along list of phone numbers.
	      (setq phones (cdr phones))
	      ;; If doing multiple per entry, increment subentry-id count.
	      (if (memq bbdb-gnokii-phonebook-style '(multiple mega))
		  (setq subentry-id (1+ subentry-id))
		(insert "\n"))))

	  (when (eq bbdb-gnokii-phonebook-style 'mega)
            ;; First email address?
            (when allnet
	      (insert (format ";8;0;%d;%s" subentry-id (car allnet)))
	      (setq subentry-id (1+ subentry-id)))
            ;; First postal address?
            (when alladdresses
	      (insert (format ";9;0;%d;%s" subentry-id
			      (bbdb-gnokii-format-address (car alladdresses))))
	      (setq subentry-id (1+ subentry-id))))
	  (if (memq bbdb-gnokii-phonebook-style '(multiple mega))
	      (insert "\n"))
	  ))))

(defun bbdb-gnokii-format-address (address)
  "Generates a single-line representation of an address."
  (let (st field)
    (cond ((>= bbdb-file-format 6)
           (setq st (bbdb-join (bbdb-address-streets address) "\\n")))
          (t
           (setq st (bbdb-address-street1 address))
           (if (> (length (bbdb-address-street2 address)) 0)
               (setq st (concat st "\\n" (bbdb-address-street2 address))))
           (if (> (length (bbdb-address-street3 address)) 0)
               (setq st (concat st "\\n" (bbdb-address-street3 address))))))
    (setq field (bbdb-address-city address))
    (if (> (length field) 0) (setq st (concat st "\\n" field)))
    (setq field (bbdb-address-state address))
    (if (> (length field) 0) (setq st (concat st "\\n" field)))
    (setq field (bbdb-address-zip-string address))
    (if (> (length field) 0) (setq st (concat st "\\n" field)))
    (setq field (bbdb-address-country address))
    (if (> (length field) 0) (setq st (concat st "\\n" field)))
    st))

(defun bbdb-gnokii-do-name (lastname firstname company stuff)
  "Construct a name from the given arguments."

  (let (name)

    (cond

     ((and stuff
	   (string-match "\"\\([^\"]+\\)\"" stuff))
      (setq name (substring stuff (match-beginning 1) (match-end 1))))

     (firstname
      ;; Yay, they have a name!  Default is first word of firstname,
      ;; space, first letter of lastname.
      (setq name
	    (bbdb-gnokii-apply-transform
	     bbdb-gnokii-firstname-transform firstname))
      (when (> (length lastname) 0)
	(setq name
	      (concat name
		      " "
		      (setq name
			    (bbdb-gnokii-apply-transform
			     bbdb-gnokii-lastname-transform lastname))))))
     (company
      ;; Yay, first word of company name!
      ;; Maybe this should be made customizable, too (Reiner Steib).
      (setq name (substring company 0 (string-match " " company)))))
    name))


(defun bbdb-gnokii-do-location (stuff loc)
  "Calculate the `location' field for a gnokii record.
The field content STUFF and the location LOC are used."

  (let (num)
    (if (and stuff
	     (string-match (concat "\\<" loc "=\\([0-9]+\\)") stuff))
	(progn
	  (setq num (string-to-number
		     (substring stuff (match-beginning 1) (match-end 1))))
	  (if (or (< num bbdb-gnokii-speed-dial-min-location)
		  (> num bbdb-gnokii-speed-dial-max-location)
		  (member num bbdb-gnokii-speed-done))
	      (error
	       "Speed dial location %d out of range or duplicate for %s"
	       num name)
	    (add-to-list 'bbdb-gnokii-speed-done num)))
      (if (> bbdb-gnokii-location bbdb-gnokii-general-max-location)
	  (error "Too many records to fit in SIM card!"))
      (setq num bbdb-gnokii-location)
      (setq bbdb-gnokii-location (+ bbdb-gnokii-location 1)))
    num))

(defun bbdb-gnokii-do-caller-group (bbdb-field)
  "Calculate the caller `group' field for given BBDB-FIELD."

  (let (group)
    (if (and bbdb-field
	     (string-match "(\\([0-9]+\\))" bbdb-field))
	(setq group (string-to-number
		     (substring bbdb-field (match-beginning 1) (match-end 1))))
      (setq group bbdb-gnokii-default-caller-group))
    group))

(defun bbdb-gnokii-phones-find-location (location phones)
  "Return the phone element in PHONES with given LOCATION, nil if not found."
  (let ((ps phones)
	ret)
    (while (and (not ret) ps)
      (if (string= location (bbdb-phone-location (car ps)))
	  (setq ret (car ps)))
      (setq ps (cdr ps)))
    ret))

(defun bbdb-gnokii-get-default-phone (phones stuff)
  "Get the default phone entry for a record."

  (let* ((loc (and stuff
		 (string-match "\\[\\([^]]+\\)\\]" stuff)
		 (substring stuff (match-beginning 1) (match-end 1))))
	 (locs bbdb-gnokii-preferred-phone-locations)
	 ret)

    (if loc
	(setq ret (bbdb-gnokii-phones-find-location loc phones)))

    (while (and (not ret) locs)
      (setq ret (bbdb-gnokii-phones-find-location (car locs) phones))
      (setq locs (cdr locs)))
    (or ret (car phones))))

(defun bbdb-gnokii-fix-phone (phone)
  "Change phone number PHONE to gnokii compatible form."

  (let ((chars phone)
	out)
    (while (> (length chars) 0)
      (let ((h (substring chars 0 1)))
      (if (string-match "[+0-9]" h)
	  (setq out (concat out h)))
      (setq chars (substring chars 1))))
    out))

;;;###autoload
(defalias 'bbdb-to-gnokii-file 'bbdb-gnokii-export)

;;;###autoload
(defun bbdb-gnokii-export (filename &optional records)
  "Export phone entries from BBDB to a gnokii contacts file FILENAME.
Unless RECORDS is given, all BBDB entries are processed."
  (interactive
   (list (let ((default (if (stringp bbdb-gnokii-default-output-file)
			    (format bbdb-gnokii-default-output-file
				    (format-time-string "%Y-%m-%d" (current-time)))
			  default-directory)))
	   (read-file-name "Output file: "
			   (file-name-directory default)
			   default
			   nil
			   (file-name-nondirectory default)))))
  (or records (setq records (bbdb-records)))
  (save-excursion
    (set-buffer (find-file-noselect filename))
    (erase-buffer)
    (let ((len (length records))
          (i 0)
	  (bbdb-gnokii-location bbdb-gnokii-general-min-location)
	  bbdb-gnokii-speed-done)
      (while records
	(message "%d%%..." (/ (* 100 i) len))
	(bbdb-gnokii-format-record (car records))
        (setq records (cdr records)
              i (1+ i))))
    (when (and bbdb-gnokii-extras-file
	       (file-readable-p bbdb-gnokii-extras-file))
      (cond ((eq bbdb-gnokii-extras-file-position 'top)
	     (goto-char (point-min)))
	    ((eq bbdb-gnokii-extras-file-position 'bottom)
	     (goto-char (point-max))))
      (insert-file-contents bbdb-gnokii-extras-file))
    (run-hooks 'bbdb-gnokii-inserted-hook)
    (save-buffer)
    ;; Useful especially when testing.
    (when (or (not bbdb-gnokii-confirm-kill)
	      (y-or-n-p "Kill output buffer? "))
      (kill-buffer (current-buffer))))
  filename)

;;;###autoload
(defun bbdb-gnokii-add-field (&optional records)
  "Go through all RECORDS and ask for adding a gnokii field.
If RECORDS is nil, go thru all records.  If a BBDB record has an
expire field in YYYY-MM-DD format \(e.g. \"expire=2003-12-31\"\),
the record is skipped if it is older than today."
  (interactive)
  (or records (setq records (bbdb-records)))
  (bbdb-add-new-field 'gnokii)
  (dolist (record records)
    ;; Go thru all records, check if we have a phone and no gnokii field.
    (if (bbdb-record-phones record)
	(let* ((have-gnokii
		(and (listp (bbdb-record-raw-notes record))
		     (cdr (assq 'gnokii (bbdb-record-raw-notes record)))))
	       (expire
		(and (listp (bbdb-record-raw-notes record))
		     (cdr (assq 'expire (bbdb-record-raw-notes record)))))
	       (name (bbdb-record-name record))
	       (is-expired (and
			    expire
			    (string-lessp expire
					  (format-time-string
					   "%Y-%m-%d" (current-time))))))
	  (message "In record `%s': gnokii=`%s', expire=`%s', is-exp=`%s'"
		   name have-gnokii expire is-expired)
	  (cond
	   (is-expired
	    (message "In record `%s': is expired." name))
	   (have-gnokii
	    (message "In record `%s': already has gnokii field." name))
	   ((y-or-n-p (format "Add gnokii field to `%s'? " name))
	    (bbdb-merge-interactively name;; name
				      nil ;; company
				      nil ;; net
				      nil ;; addrs
				      nil ;; phones
				      '((gnokii . "t")))
	    (message "In record `%s': gnokii field added." name))
	   (t
	    (message "In record `%s': gnokii refused interactively." name))))
      (message "In record `%s': no phone found." (bbdb-record-name record)))))

(provide 'bbdb-gnokii)

;;; bbdb-gnokii.el ends here
