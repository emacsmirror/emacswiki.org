;;; gedcom.el --- Aid in editing lifelines GEDCOM records

;; EmacsWiki: GEDCOM

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.
;;               1994, 1995 Stephen A. Wood

;; Author:   Stephen A. Wood (saw@cebaf.gov)
;; Version:  0.2

;; Additional stuff
;; Author:   Matthias Rempe (mrempe@rempe-online.de)
;; Version:  0.5-m
;; Keywords: genealogy, lifelines, gedcom
;; $Id: gedcom.el,v 1.9 2001/12/16 23:19:40 mrempe Exp $

;; This file is not part of GNU Emacs, but works with GNU Emacs.  The
;; licensing terms are the same as those that cover GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary (Stephen A. Wood):

;; For using emacs (in server mode) as the editor for lifelines.
;;
;; When you are done editing a gedcom record, type C-c C-c.  This will cause
;; the change date to be added to the record.  If you don't want the change
;; date added, exit with C-x # instead.
;;
;; Code to edit the notes that get put below CHAN tags was taken
;; from the GNU Emacs file vc.el.

;; ---------------------------------------------------------------------------
;;; Commentary (Matthias Rempe):

;; This version allows you not only to add a note and a change date
;; but to insert (frequently used) tags via keyboard or menu commands.
;; 
;; The Gedcom menu provides many tags that are not accessible via
;; keyboard.  I've grouped nearly all these tags according to
;; gedcgram.txt (don't know where I found the file). 
;;
;; gedcom.el can now also be used to browse gedcom files. Several
;; commands are provided to move from record to record and display
;; references.

;; ---------------------------------------------------------------------------
;;; Installation:
;; 
;; Put this file where Emacs can find it and byte-compile it.
;;
;; Add this to your .emacs:
;;   (autoload 'gedcom-mode "gedcom")
;;   (setq auto-mode-alist (cons '("\\.ged$" . gedcom-mode) auto-mode-alist))
;; Unix (and Win < 3.0.7):
;;   (setq auto-mode-alist (cons '("lltmp[0-9a-zA-Z.]+$" . gedcom-mode) auto-mode-alist))
;; Win (>= 3.0.7)
;;  (setq auto-mode-alist (cons '("llt[0-9a-zA-Z]+$" . gedcom-mode) auto-mode-alist)) 

;; If you use LifeLines with DOS, you should add this:
;;   (autoload 'gedcom-mode "gedcom")
;;   (setq auto-mode-alist (cons '("\\.ged$" . gedcom-mode) auto-mode-alist))
;;   (setq auto-mode-alist (cons '("[0-9]+ltmp$" . gedcom-mode) auto-mode-alist))


;; ---------------------------------------------------------------------------
;;; Changes:


;; Version 0.5-m
;; * added Installation hints
;; * references are displayed in gedcom-minor-mode to prevent errors
;;   when trying to move or edit in the Gedcom-Refs buffer
;; * fixed bug: indent-according-to-mode bound to \t
;; * user-defined tags can be customized and read with completion
;;   (Thanks to Marc Nozell <marc@nozell.com> for an essential hint)
;; * Only one CHAN record allowed according to Gedcom Standard 5.5 [2];
;;   you can customize if an old record is deleted 
;; * In SOUR records text can now be added again, even if no page is
;;   entered [2]
;;   [2] Thanks to Bjørn-Helge Mevik <bhm@math.uio.no> for his notes
;;   and suggestions
;;
;; 2001-12-16; Matthias Rempe (mrempe@rempe-online.de)


;; Version 0.4-m
;; * changed some defuns for use with Emacs-21.1
;; * adjusted indentation in gedcom-dress-buffer
;; * added gedcom-widen to menu
;; * new defuns:
;;   - gedcom-show-ref
;;   - gedcom-forward-record-narrowed
;;   - gedcom-backward-record-narrowed
;;
;; 2001-11-22; Matthias Rempe (mrempe@rempe-online.de)


;; Version 0.3-m
;; * code revised 
;; * new tags
;; * Customization stuff
;; * Menu support
;; * Font-lock support
;; * added code for
;;   - gedcom-next-tag / gedcom-previous-tag
;;   - gedcom-forward-record / gedcom-backward-record
;;
;; 2001-01-10; Matthias Rempe (mrempe@t-online.de)


;; Version 0.2-m
;; Added some frequently used tags
;; 20 Jan 2000; Matthias Rempe (mrempe@t-online.de)


;;
;;; To do:
;; - better determination if we edit in a server-client or not

;; ---------------------------------------------------------------------------

;;; Code:

(defconst gedcom-mode-version "0.5-m")

(defconst bug-original-gedcom-mode "saw@cebaf.gov"
  "Address of original author of gedcom mode.")


;; since I added plenty of new code which may contain bugs I set this:
(defconst bug-gedcom-mode "mrempe@rempe-online.de"
  "Address of author of gedcom mode.")


;; this routine is taken from todo-mode:
(eval-and-compile                       ; Removable for installation in
                                        ; Emacs 20.
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))


;; User-configurable variables:

(defgroup gedcom nil
  "Customize Gedcom mode"
  :prefix "gedcom-"
  :group 'convenience)

(defcustom gedcom-mode-hook nil
  "*Gedcom mode hook."
  :group 'gedcom
  :type 'hook)

(defcustom gedcom-edit-note-mode-hook nil
  "*Gedcom edit note mode hook."
  :group 'gedcom
  :type 'hook)

(defcustom gedcom-edit-date-with-time nil
  "*If non-nil, include the time with the CHAN tag."
   :group 'gedcom
   :type 'boolean)

(defcustom gedcom-edit-date-with-note t
  "*If non-nil, prompt for a note to put with the CHAN tag."
   :group 'gedcom
   :type 'boolean)

(defcustom gedcom-preserve-chan-notes t
  "*If non-nil, preserve CHAN notes when updating a CHAN entry."
   :group 'gedcom
   :type 'boolean)

(defcustom gedcom-maximum-note-ring-size 32
  "*Maximum number of saved notes in the note ring."
   :group 'gedcom
   :type 'integer)

(defcustom gedcom-level-indent 2
  "*Extra indentation applied to each tag level."
   :group 'gedcom
   :type 'integer)

(defcustom gedcom-startup-message t
  "*Non-nil displays a startup message when Gedcom mode is first called."
   :group 'gedcom
   :type 'boolean)

(defcustom server-temp-file-regexp "llt\(mp\)?[0-9A-Za-z.]+$"
  "*So that buffer gets deleted when server returns file."
   :group 'gedcom
   :type 'string)


(defcustom gedcom-tag-regexp "[1-9] _?[A-Z][A-Z][A-Z]+ "
  "*Regexp used to find tags. 
The default Regexp (1) skips \"empty\" tags (BIRT, DEAT, etc.).
If you choose Regexp (2), those tags are found, too."
   :group 'gedcom
   :type '(choice 
	    (const :tag "1: \"[1-9] _?[A-Z][A-Z][A-Z]+ \"" "[1-9] _?[A-Z][A-Z][A-Z]+ ") 
	    (const :tag "2: \"[1-9] _?[A-Z][A-Z][A-Z]+\"" "[1-9] _?[A-Z][A-Z][A-Z]+")))


(defcustom gedcom-font-lock t
  "*Non-nil means Gedcom buffers will use font-lock-mode."
  :group 'gedcom
  :type 'boolean)

(defcustom gedcom-dont-allow-utag-definition-on-the-fly nil
  "*Nil means user-defined tags may be defined while editing."
  :group 'gedcom
  :type 'boolean)

(defcustom gedcom-utag-alist
  '(("GODP")
    ("TIME"))
 "List of user-defined tags. Each tag is inserted in the GEDCOM record
with an underscore before the tag.

See also `gedcom-usrtag'."
 :group 'gedcom
 :type '(repeat (list :format "%v"
		      (string :tag "Tag")))) ;user-defined tags



;; ---------------------------------------------------------------------------
;; required helpers ...

(require 'ring)
(require 'easymenu)
(require 'font-lock)

;; ---------------------------------------------------------------------------

(defvar gedcom-mode-map nil
  "Keymap used in gedcom mode.")
(if gedcom-mode-map
    nil
  (setq gedcom-mode-map (make-sparse-keymap))
  (define-key gedcom-mode-map "\t" 'indent-according-to-mode)
  (define-key gedcom-mode-map "\C-c\C-a" 'gedcom-asso)
  (define-key gedcom-mode-map "\C-c\C-b" 'gedcom-birt)
  (define-key gedcom-mode-map "\C-c\C-d" 'gedcom-deat)
  (define-key gedcom-mode-map "\C-c\C-e" 'gedcom-even)
  (define-key gedcom-mode-map "\C-c\C-c" 'gedcom-edit-date)
  (define-key gedcom-mode-map "\C-c\C-f" 'gedcom-refn)
  (define-key gedcom-mode-map "\C-c\C-n" 'gedcom-name)
  (define-key gedcom-mode-map "\C-c\C-r" 'gedcom-resi)
  (define-key gedcom-mode-map "\C-c\C-o" 'gedcom-occu)
  (define-key gedcom-mode-map "\C-c\C-s" 'gedcom-sour)
  (define-key gedcom-mode-map "\C-c\C-u" 'gedcom-usrtag)
  (define-key gedcom-mode-map "\C-c\C-p" 'gedcom-previous-tag)
  (define-key gedcom-mode-map "\C-c\C-t" 'gedcom-next-tag)
  (define-key gedcom-mode-map "\C-c\C-xc" 'gedcom-cont)
  (define-key gedcom-mode-map "\C-c\C-xe" 'gedcom-even-stru)
  (define-key gedcom-mode-map "\C-c\C-xn" 'gedcom-note)
  (define-key gedcom-mode-map "\C-c\C-xt" 'gedcom-text)
  (define-key gedcom-mode-map "\C-xnr" 'gedcom-narrow-to-record)
  (define-key gedcom-mode-map "\C-xnw" 'gedcom-widen)
  (define-key gedcom-mode-map "\M-\C-a" 'gedcom-backward-record)
  (define-key gedcom-mode-map "\M-\C-e" 'gedcom-forward-record)
  (define-key gedcom-mode-map "\M-\C-p" 'gedcom-backward-record-narrowed)
  (define-key gedcom-mode-map "\M-\C-n" 'gedcom-forward-record-narrowed)
  (define-key gedcom-mode-map "\C-xgs" 'gedcom-show-ref))

;;; 
(if (not (assoc 'gedcom-parent-buffer minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(gedcom-parent-buffer gedcom-parent-buffer-name)
		minor-mode-alist)))



;; Variables the user doesn't need to know about

(defvar gedcom-parent-buffer nil)
(put 'gedcom-parent-buffer 'permanent-local t)
(defvar gedcom-parent-buffer-name nil)
(put 'gedcom-parent-buffer-name 'permanent-local t)
(defvar gedcom-note-ring nil)
(put 'gedcom-note-ring 'permanent-local t)
(defvar gedcom-note-ring-index nil)
(put 'gedcom-note-ring-index 'permanent-local t)
(defvar gedcom-note-exit-server nil)
(put 'gedcom-note-exit-server 'permanent-local t)
(defvar gedcom-last-note-match nil)
(put 'gedcom-last-note-match 'permanent-local t)
(defvar gedcom-note-level 0)
(put 'gedcom-note-level 'permanent-local t)
(defvar gedcom-note-tag nil)
(put 'gedcom-note-tag 'permanent-local t)

(defvar gedcom-narrowed nil)
(put 'gedcom-narrowed 'permanent-local t)

(defvar gedcom--temp-buffer-name "*Gedcom-Refs*")

;; to remember values for various runs
(defvar gedcom-snam nil) ; Surname
(defvar gedcom-plac nil) ; Place
(defvar gedcom-sour nil) ; Source
(defvar gedcom-page nil) ; Page
(defvar gedcom-refn nil) ; Reference


;; Regular expressions for Gedcom records
(defconst gedcom-record-regexp "^0 "
  "Regexp used to find (start of) records.")
(defconst gedcom-ref-regexp "@[A-Z]+[0-9]+@$"
  "Regexp used to find References.")


;; Regular expressions for Imenu support
;; experimental!!!
;;
(defvar gedcom--imenu-generic-expression
  (list 
    (list "INDI" "1 NAME \\(.*\\)" 1))
  "Imenu generic expression for Gedcom-mode.  See `imenu-generic-expression'.")


;;
;;; DOS Emacs doesn't know server-edit...
;;
(if (eq system-type 'ms-dos)
  (defun server-edit ()
    "Server-edit with DOS"
    (beginning-of-line)))

 
;;;---------------------------------------------------
;;; define menu 'Gedcom'
;;;---------------------------------------------------

;; If you don't remember which tags to use then the menu might give
;; you some help.  I add all tags for myself that I think might be
;; useful. If you don't like it -- just change it.

 (easy-menu-define
  gedcom-mode-menu 
  gedcom-mode-map 
  "Gedcom Menu"
 '("Gedcom"
    ("Individual record"
      ["NAME" gedcom-name t]
      ["BIRT" gedcom-birt t]
      ["CHR" gedcom-chr t :key-sequence nil]
;;      ["BAPM" gedcom-bapm t :key-sequence nil]
      ["DEAT" gedcom-deat t]
      ["BURI" gedcom-buri t :key-sequence nil]
      ["EVEN" gedcom-even t]
      ("Other Events"
;;        ["CHR" gedcom-chr t :key-sequence nil]
        ["BAPM" gedcom-bapm t :key-sequence nil]
        ["CONF" gedcom-conf t :key-sequence nil]
        ["FCOM" gedcom-fcom t :key-sequence nil]
        ["ORDN" gedcom-ordn t :key-sequence nil]
        ["NATU" gedcom-natu t :key-sequence nil]
        ["EMIG" gedcom-emig t :key-sequence nil]
        ["IMMI" gedcom-immi t :key-sequence nil]
        ["CENS" gedcom-cens t :key-sequence nil]
        ["WILL" gedcom-will t :key-sequence nil]
        ["GRAD" gedcom-grad t :key-sequence nil]
        ["RETI" gedcom-reti t :key-sequence nil]
      )
      ("Individual Attributes"
	["DSCR" gedcom-dscr t :key-sequence nil]
	["NATI" gedcom-nati t :key-sequence nil]
	["NCHI" gedcom-nchi t :key-sequence nil]
        ["NMR"  gedcom-nmr t :key-sequence nil]
	["OCCU" gedcom-occu t]
	["PROP" gedcom-prop t :key-sequence nil]
        ["RELI" gedcom-reli t :key-sequence nil]
        ["RESI" gedcom-resi t]
	["TITL" gedcom-titl t :key-sequence nil]
      )
      ["ASSO" gedcom-asso t]
;; I want to use "RESN privacy" to mark some records for privacy reasons.
;; This tag can then be checked in some Lifelines reports.
      ["RESN" gedcom-resn t :key-sequence nil]
      ["SOUR" gedcom-sour t]
      ["NOTE" gedcom-note t]
      ["SUBM" gedcom-not-implemented nil]
      ["CHAN" gedcom-edit-date t]
    )
    "---"
    ("Family record"
      ["MARR" gedcom-marr t :key-sequence nil]
      ["DIV"  gedcom-divo t :key-sequence nil]
      ["EVEN" gedcom-even t]
      ["ENGA" gedcom-enga t :key-sequence nil]
      ["NCHI" gedcom-nchi t :key-sequence nil]
      ["RESN" gedcom-resn t :key-sequence nil]
      ["SOUR" gedcom-sour t]
      ["NOTE" gedcom-note t]
      ["SUBM" gedcom-not-implemented nil]
      ["CHAN" gedcom-edit-date t]
    )
    "---"
    ("Source record"
      ["REFN" gedcom-refn t]
      ["AUTH" gedcom-auth t :key-sequence nil]
      ["TITL" gedcom-descr-title t :key-sequence nil]
      ["ABBR" gedcom-abbr t :key-sequence nil]
      ["PUBL" gedcom-publ t :key-sequence nil]
      ["TEXT" gedcom-text t]
      ["NOTE" gedcom-note t]
      ["CHAN" gedcom-edit-date t]
    )
    "---"
      ["User-defined" gedcom-usrtag t]
    "---"
      ["CONT" gedcom-cont t]
      ["SOUR (Source Citation)" gedcom-sour t]
      ["REFN" gedcom-refn t]
      ["CHAN" gedcom-edit-date t]
      ["NOTE" gedcom-note t]
    "---"
      ["Indent line" gedcom-indent-line t]
      ["Indent region" indent-region mark-active]
      ["Narrow to record" gedcom-narrow-to-record (if gedcom-narrowed nil t)]
      ["Widen from record" gedcom-widen (if gedcom-narrowed t nil)]
      ["Show Reference" gedcom-show-ref t]
      ("Goto"
         ["Next tag" gedcom-next-tag t]
         ["Previous tag" gedcom-previous-tag t]
         ["Next record" gedcom-forward-record t]
         ["Previous record" gedcom-backward-record t]
         ["Next record narrowed" gedcom-forward-record-narrowed t]
         ["Previous record narrowed" gedcom-backward-record-narrowed t]
      )
    ))




 
;;;---------------------------------------------------
;;; support for font-lock
;;;---------------------------------------------------

(defvar gedcom-font-lock-keywords
    '(
;; Level 0 tags
      ("^0.*" . font-lock-constant-face)
;; Tags with pointer
      ("^[\t ]*[1-9] \\(ASSO\\|SOUR\\|SUB[MN]\\|REPO\\)" . font-lock-function-name-face)
      ("^[\t ]*[1-9] \\(FAM[CS]\\|HUSB\\|WIFE\\|CHIL\\)" . font-lock-constant-face)
;; keywords -- at least 3 chars
      ("^[\t ]*[1-9] _?[A-Z][A-Z][A-Z]+" . font-lock-builtin-face)
;; additional keywords in Advanced view
      ("^[\t ]*[1-9] @[A-Z]*[0-9]*@ [A-Z][A-Z][A-Z]+" . font-lock-constant-face)
;; pointer
      ("@[A-Z]*[0-9]*@" . font-lock-variable-name-face))
    "Default expressions to highlight in Gedcom mode.")


 
;; ---------------------------------------------------------------------------

;;;###autoload
(defun gedcom-mode ()
  "Major mode for editing GEDCOM records.

Key definitions:
\\{gedcom-mode-map}

Variables controlling indentation style and extra features:

 gedcom-level-indent
    Indentation for each level. (default 2)
 gedcom-startup-message
    Set to nil to inhibit message first time Gedcom mode is used.

Turning on Gedcom mode calls the value of the variable gedcom-mode-hook
with no arges, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if gedcom-startup-message
      (message "Emacs Gedcom mode version %s.  Bugs to %s"
	       gedcom-mode-version bug-gedcom-mode))
  (setq gedcom-startup-message nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'gedcom-indent-line)

  ; Narrowing
;;  (defalias 'gedcom-widen 'widen)
;;  see new definition below

  ; Setting up font-locking
  (make-local-variable 'font-lock-defaults)
  (if gedcom-font-lock
    (setq font-lock-defaults '(gedcom-font-lock-keywords t nil)))

  (use-local-map gedcom-mode-map)
  (setq mode-name "Gedcom")
  (setq major-mode 'gedcom-mode)

  ;; add Menu
  (easy-menu-add gedcom-mode-menu gedcom-mode-map)

  ;; Imenu support -- experimental! Use at your own risk!
  (set (make-local-variable 'imenu-generic-expression)
       gedcom--imenu-generic-expression)
  (setq imenu-case-fold-search t)
  (setq imenu-sort-function 'gedcom--imenu-sort-by-name)

  ;; Hooks
  (run-hooks 'gedcom-mode-hook))



(defun gedcom-indent-line ()
  "Indents the current line by the tag level times `gedcom-level-indent'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at "[1-9]")
	(gedcom--indent-to-column 
	 (* gedcom-level-indent
	    (- (string-to-int (char-to-string (char-after (point))))
	       (string-to-int "1"))))
      (gedcom--indent-to-column 0))))


(defun gedcom--indent-to-column (col)
  "Indents current line with spaces to column COL."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to col)))


(defun gedcom-edit-date ()
  "Add a CHAN tag to indicate when the last change was made."
  (interactive)
  (if (buffer-modified-p)
      (let* ((today (format-time-string "%-e %^b %Y"))
	     (time (format-time-string "%-k:%M")))
	(goto-char (point-min))
	(if (search-forward "1 CHAN" nil 'move)
	    (progn
	      (beginning-of-line)
	      ;delete up to note (if exists)
	      (let ((beg (point)))
		(if gedcom-preserve-chan-notes
		    (progn
		      (search-forward "2 NOTE" (point-max) t)
		      (beginning-of-line))
		  (goto-char (point-max)))
		(kill-region beg (point)))))
	(insert "1 CHAN")
	(gedcom--indent-and-insert-new-line)
	(insert "2 DATE ")
	(insert today)
	(gedcom--indent-and-insert-new-line)
	(if gedcom-edit-date-with-time
	    (progn
	      (insert "3 TIME ")
	      (insert time)
	      (gedcom--indent-and-insert-new-line)))
	(if gedcom-edit-date-with-note
          (gedcom--start-noteentry "NOTE" t))
	(if server-clients
            (server-edit)))))


;;; Keymap for 
(defvar gedcom-note-entry-mode nil
  "Keymap used in gedcom note entry mode.")
(if gedcom-note-entry-mode 
    nil
  (setq gedcom-note-entry-mode (make-sparse-keymap))
  (define-key gedcom-note-entry-mode "\M-n" 'gedcom-next-note)
  (define-key gedcom-note-entry-mode "\M-p" 'gedcom-previous-note)
  (define-key gedcom-note-entry-mode "\M-r" 'gedcom-note-search-reverse)
  (define-key gedcom-note-entry-mode "\M-s" 'gedcom-note-search-forward)
  (define-key gedcom-note-entry-mode "\C-c\C-c" 'gedcom-finish-noteentry))


(defun gedcom-edit-note-mode ()
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map gedcom-note-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'gedcom-edit-note-mode)
  (setq mode-name "NOTE-Log")
  ;;  (make-local-variable 'gedcom-log-file)
  ;;  (make-local-variable 'gedcom-log-version)
  (make-local-variable 'gedcom-note-ring-index)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'gedcom-edit-note-mode-hook))



(defun gedcom--start-noteentry (tag srvr &optional level)
  (let ((parent (current-buffer))
	(lvl (or level gedcom-level-indent)))
    (pop-to-buffer (get-buffer-create "*Modification note*"))
    (set (make-local-variable 'gedcom-parent-buffer) parent)
    (set (make-local-variable 'gedcom-note-level) lvl)
    (set (make-local-variable 'gedcom-note-tag) tag)
    (set (make-local-variable 'gedcom-parent-buffer-name)
	 (concat " from " (buffer-name gedcom-parent-buffer)))
    (set 'gedcom-note-exit-server srvr)
    (gedcom-edit-note-mode)
    (message "Type C-c C-c when done.")))



(defun gedcom-finish-noteentry ()
  (interactive)
  (goto-char (point-max))
  (if (not (bolp))
      (newline))
  (if (null gedcom-note-ring)
      (setq gedcom-note-ring (make-ring gedcom-maximum-note-ring-size)))
  (ring-insert gedcom-note-ring (buffer-string))
;;  (gedcom-dress-buffer gedcom-note-level "NOTE")
  (gedcom--dress-buffer gedcom-note-level gedcom-note-tag)
  (pop-to-buffer gedcom-parent-buffer)
  (insert-buffer "*Modification note*")
  (let ((logbuf (get-buffer "*Modification note*")))
    (delete-windows-on logbuf)
    (kill-buffer logbuf))
  (if gedcom-note-exit-server
    (if server-clients
      (server-edit))))

  
(defun gedcom--dress-buffer (level tag)
  (goto-char 0)
  (if (not (eobp))
      (progn
	(if (not (string= tag "CONT"))
	   (progn
	     (insert-char ?  (* (1- level) gedcom-level-indent))
	     (insert (int-to-string level))
	     (insert " ")
	     (insert tag)
	     (insert " ")
	     (forward-line)))
	(while (not (eobp))
	  (insert-char ?  (+ (* (1- level) gedcom-level-indent) gedcom-level-indent))
	  (insert (int-to-string (1+ level)))
	  (insert " CONT ")
	  (forward-line)))))



;; Code for access to the comment ring

(defun gedcom-previous-note (arg)
  "Cycle backwards through note history."
  (interactive "*p")
  (let ((len (ring-length gedcom-note-ring)))
    (cond ((<= len 0)
	   (message "Empty note ring")
	   (ding))
	  (t
	   (erase-buffer)
	   ;; Initialize the index on the first use of this command
	   ;; so that the first M-p gets index 0, and the first M-n gets
	   ;; index -1.
	   (if (null gedcom-note-ring-index)
	       (setq gedcom-note-ring-index
		     (if (> arg 0) -1
			 (if (< arg 0) 1 0))))
	   (setq gedcom-note-ring-index
		 (mod (+ gedcom-note-ring-index arg) len))
	   (message "%d" (1+ gedcom-note-ring-index))
	   (insert (ring-ref gedcom-note-ring gedcom-note-ring-index))))))


(defun gedcom-next-note (arg)
  "Cycle forwards through note history."
  (interactive "*p")
  (gedcom-previous-note (- arg)))


(defun gedcom-note-search-reverse (str)
  "Searches backwards through note history for substring match."
  (interactive "sNote substring: ")
  (if (string= str "")
      (setq str gedcom-last-note-match)
    (setq gedcom-last-note-match str))
  (if (null gedcom-note-ring-index)
      (setq gedcom-note-ring-index -1))
  (let ((str (regexp-quote str))
        (len (ring-length gedcom-note-ring))
	(n (1+ gedcom-note-ring-index)))
    (while (and (< n len) (not (string-match str (ring-ref gedcom-note-ring n))))
      (setq n (+ n 1)))
    (cond ((< n len)
	   (gedcom-previous-note (- n gedcom-note-ring-index)))
	  (t (error "Not found")))))


(defun gedcom-note-search-forward (str)
  "Searches forwards through note history for substring match."
  (interactive "sNote substring: ")
  (if (string= str "")
      (setq str gedcom-last-note-match)
    (setq gedcom-last-note-match str))
  (if (null gedcom-note-ring-index)
      (setq gedcom-note-ring-index 0))
  (let ((str (regexp-quote str))
        (len (ring-length gedcom-note-ring))
	(n gedcom-note-ring-index))
    (while (and (>= n 0) (not (string-match str (ring-ref gedcom-note-ring n))))
      (setq n (- n 1)))
    (cond ((>= n 0)
	   (gedcom-next-note (- n gedcom-note-ring-index)))
	  (t (error "Not found")))))


(defun gedcom-next-tag ()
  "Moves point to next tag."
  (interactive)
  (end-of-line)
  (if (re-search-forward gedcom-tag-regexp nil t)
     (goto-char (match-end 0))
    (error "No more tags")))


(defun gedcom-previous-tag ()
  "Moves point to previous tag."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward gedcom-tag-regexp nil t)
     (goto-char (match-end 0))
    (error "No more tags")))


(defun gedcom-forward-record ()
  "Moves point to next record"
  (interactive)
    (end-of-line)
    (if (re-search-forward gedcom-record-regexp nil t)
	(goto-char (match-end 0))
      (error "No more records")))


(defun gedcom-backward-record ()
  "Moves point to previous record"
  (interactive)
    (beginning-of-line)
    (if (re-search-backward gedcom-record-regexp nil t)
	(goto-char (match-end 0))
      (error "No more records")))


(defun gedcom-forward-record-narrowed ()
  "Moves point to next record and displays it narrowed"
  (interactive)
  (gedcom-widen)
  (gedcom-forward-record)
  (gedcom-narrow-to-record))

    
(defun gedcom-backward-record-narrowed ()
  "Moves point to previous record and displays it narrowed"
  (interactive)
  (gedcom-widen)
  (gedcom-backward-record)
  (gedcom-narrow-to-record))


;; taken from lisp.el
(defun gedcom-narrow-to-record ()
  "Make text outside current record invisible.
The record visible is the one that contains point or follows point."
  (interactive)
  (save-excursion
    (widen)
    (gedcom-forward-record)  
    (beginning-of-line)
    (let ((end (point)))
      (gedcom-backward-record)
      (beginning-of-line)
      (narrow-to-region (point) end)))
  (setq gedcom-narrowed t))


(defun gedcom-widen ()
  (interactive)
  (widen)
  (setq gedcom-narrowed nil))


(add-hook 'temp-buffer-show-hook 'gedcom-minor-mode)


(defun gedcom-show-ref ()
  "Show reference entry of next reference in current record. Useful
when visiting complete Gedcom files."
   (interactive)
   (save-excursion
     (save-restriction
       (widen)
       (end-of-line)
       (setq e (point))
       (beginning-of-line)
       (if (re-search-forward gedcom-ref-regexp e t)
	   (with-output-to-temp-buffer gedcom--temp-buffer-name
	     (goto-char (point-min))
	     (if (search-forward (concat "0 " (match-string 0)) nil t)
		 (progn
		   (gedcom-narrow-to-record)
		   (princ (buffer-string)))
	       (error "No target found")))
	 (error "No reference found")))))


;; ---------------------------------------------------------------------------
;; Gedcom minor mode (for use with temp-buffer)

(defvar gedcom-minor-mode nil
  "Mode variable for `gedcom-minor-mode'.")

(make-variable-buffer-local 'gedcom-minor-mode)

(defun gedcom-minor-mode (&optional arg)
  "Gedcom-minor-mode used with temp-buffer to display references.

Key definitions:
\\[gedcom-previous-tag]		Previous tag
\\[gedcom-next-tag]		Next tag."
  (interactive "P")
  (unless (and arg			; Do nothing if already OK.
	       (if (> (prefix-numeric-value arg) 0) gedcom-minor-mode (not gedcom-minor-mode)))
    (if gedcom-minor-mode
	(gedcom-minor-mode--disable)
      (gedcom-minor-mode--enable))))


(if (not (assoc 'gedcom-minor-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(gedcom-minor-mode " Gedc")
		minor-mode-alist)))


(defun gedcom-minor-mode--enable ()
  "Enable `gedcom-minor-mode'."
  (setq gedcom-minor-mode t)
  ; Setting up font-locking
  (make-local-variable 'font-lock-defaults)
  (if gedcom-font-lock
    (setq font-lock-defaults '(gedcom-font-lock-keywords t nil)))
  (run-hooks 'gedcom-minor-mode-hook))


(defun gedcom-minor-mode--disable ()
  "Disable `gedcom-minor-mode'."
  (setq gedcom-minor-mode nil))


(defcustom gedcom-minor-mode-hook nil
  "Normal hook run when starting `gedcom-minor-mode'."
  :type 'hook
  :group 'gedcom)


(defvar gedcom-minor-mode-map nil)

;; (if (not gedcom-minor-mode-map)
;;   (let ((map (setq gedcom-minor-mode-map (copy-alist view-mode-map))))
;;     (define-key map "\C-c\C-p" 'gedcom-previous-tag)
;;     (define-key map "\C-c\C-t" 'gedcom-next-tag)))


(or (assq 'gedcom-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'gedcom-minor-mode gedcom-minor-mode-map)
		minor-mode-map-alist)))



 
;; ---------------------------------------------------------------------------
;; Helper functions for Imenu experimental 

;; Actually, Imenu works with small gedcom files (< 1000
;; entries). With large files, the system hangs after the first
;; navigation to an INDI entry.
;; 2001-11-25 mre

;;; An item looks like (NAME . POSITION).
(defun gedcom--imenu-sort-by-name (item1 item2)
  (let* ((namen1 (gedcom--string-split (car item1) "/")) 
	 (namen2 (gedcom--string-split (car item2) "/"))
	 (nname1 (car (cdr namen1)))            ; Nachname 1
	 (nname2 (car (cdr namen2))))           ; Nachname 2
	 (string-lessp nname1 nname2)))



;; This defun is taken from 
;; string-fns.el --- an assortment of string-manipulation functions
;; written by Noah S. Friedman
(defun gedcom--string-split (string &optional separator limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
Optional argument SEPARATOR can be any regexp, but anything matching the
 separator will never appear in any of the returned substrings.
 If not specified, SEPARATOR defaults to \"[ \\f\\t\\n\\r\\v]+\".
If optional arg LIMIT is specified, split into no more than that many
 fields \(though it may split into fewer\)."
  (or separator (setq separator "[ \f\t\n\r\v]+"))
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))



 
;; ---------------------------------------------------------------------------
;;

;; dummy 
(defun gedcom-not-implemented ()
  "Dummy-Function"
  (interactive)
  'not-implemented)


(defun gedcom--indent-and-insert-new-line ()
  "Indents the current line and inserts a new line"
  (gedcom-indent-line)
  (insert "\n"))


;; SOUR tag
(defun gedcom-sour (&optional level)
  "Insert Source tags at LEVEL; default `gedcom-level-indent'"
  (interactive "p")
  (or level gedcom-level-indent)
  (let* ((gedcom-sour (setq gedcom-sour
                      (or (read-string "Source: " gedcom-sour))))
	 (gedcom-page (setq gedcom-page
		      (or (read-string "Page: " gedcom-page)))))
    (if (and (not (bolp)) (not (eolp)))
        (progn (end-of-line) (newline)))
      (insert (concat (number-to-string level) " SOUR <" gedcom-sour ">"))
      (gedcom--indent-and-insert-new-line)
      (if (not (string= gedcom-page ""))
	  (progn
	    (insert (concat (number-to-string (+ level 1)) " PAGE " gedcom-page))
	    (gedcom--indent-and-insert-new-line)))
      (if (y-or-n-p "with Text? ")
	  (progn
	    (insert (concat (number-to-string (+ level 1)) " DATA"))
	    (gedcom--indent-and-insert-new-line)
	    (gedcom--start-noteentry "TEXT" nil (+ level 2))))))



;; "Event-structure"
(defun gedcom-even-stru (&optional level)
  "Insert DATE/AGE/PLAC/CAUSE tag at LEVEL; default `gedcom-level-indent'"
  (interactive "p")
  (or level gedcom-indent-level)
  (let* ((dat (read-string "Date: "))
        (age (read-string "Age: "))
        (gedcom-plac (setq gedcom-plac
                      (or (read-string "Place: " gedcom-plac))))
	(cse (read-string "Cause: "))
	(indnt (/ level gedcom-level-indent)))
    (if (not (string= dat ""))
      (progn
        (insert (concat (number-to-string level) " DATE " dat))
        (gedcom--indent-and-insert-new-line)))
    (if (not (string= age ""))
      (progn
        (insert (concat (number-to-string level) " AGE " age))
        (gedcom--indent-and-insert-new-line)))
    (if (not (string= gedcom-plac ""))
      (progn
        (insert (concat (number-to-string level) " PLAC " gedcom-plac))
        (gedcom--indent-and-insert-new-line)))
    (if (not (string= cse ""))
      (progn
        (insert (concat (number-to-string level) " CAUS " cse))
        (gedcom--indent-and-insert-new-line))))
  (gedcom-sour level))


;; NOTE tag
(defun gedcom-note (&optional level)
  "Insert NOTE tag at LEVEL; default 1"
  (interactive "p")
  (or level 1)
  (gedcom--start-noteentry "NOTE" nil level))


;; TEXT tag
(defun gedcom-text (&optional level)
  "Insert TEXT tag at LEVEL; default 1"
  (interactive "p")
  (or level 1)
  (gedcom--start-noteentry "TEXT" nil level))


;; CONT tags
(defun gedcom-cont (&optional level)
  "Insert text, each line preceded with CONT tag at LEVEL; default 2"
  (interactive "p")
  (or level 2)
  (gedcom--start-noteentry "CONT" nil level))


;; ASSO tag
(defun gedcom-asso ()
  "Insert ASSO tag"
  (interactive)
  (let ((ref (upcase (read-string "ASSO reference: ")))
	(typ (read-string "Type: "))
        (rel (read-string "Relation: "))
       )
     (insert "1 ASSO @" ref "@")
     (gedcom--indent-and-insert-new-line)
;; the GEDCOM Standard 5.5 describes TYPE as mandantory
;; but maybe you won't use it
     (if (not (string= typ ""))
        (progn 
	  (insert "2 TYPE " typ)     
	  (gedcom--indent-and-insert-new-line)))
     (insert "2 RELA " rel)
     (gedcom--indent-and-insert-new-line)))


;; EVEN tag
(defun gedcom-even (&optional level)
  "Insert EVEN tag at LEVEL; default 1"
  (interactive "p")
  (or level 1)
  (insert (concat (number-to-string level) " EVEN"))2 EVEN
  (gedcom--indent-and-insert-new-line)
  (let ((typ (read-string "Type: ")))
    (if (not (string= typ ""))
      (progn
        (insert (concat (number-to-string (1+ level)) " TYPE " typ))
        (gedcom--indent-and-insert-new-line))))
  (gedcom-even-stru (1+ level)))

;; ----------------------------------------------------------------------
;; Level-1 tags

(defun gedcom--insert-level1-tag (tag &optional prompt default noeven)
   (let ((lvl 1))
     (if prompt
       (let ((text (read-string (concat prompt ": ") nil nil default)))    
          (insert (concat (number-to-string lvl) " " tag " " text)))
       (insert (concat (number-to-string lvl) " " tag)))
     (gedcom--indent-and-insert-new-line)
     (if (not noeven)
       (gedcom-even-stru (1+ lvl)))))

;; BIRT 
(defun gedcom-birt ()
  "Insert BIRT tag"
  (interactive)
  (gedcom--insert-level1-tag "BIRT"))


;; CHR
(defun gedcom-chr ()
  "Insert CHR tag"
  (interactive)
  (gedcom--insert-level1-tag "CHR")
)


;; BAPM
(defun gedcom-bapm ()
  "Insert BAPM tag"
  (interactive)
  (gedcom--insert-level1-tag "BAPM"))


;; DEAT 
(defun gedcom-deat ()
  "Insert DEAT tag"
  (interactive)
  (gedcom--insert-level1-tag "DEAT"))


;; BURI 
(defun gedcom-buri ()
  "Insert BURI tag"
  (interactive)
  (gedcom--insert-level1-tag "BURI"))


;; RESI 
(defun gedcom-resi ()
  "Insert RESI tag"
  (interactive)
  (gedcom--insert-level1-tag "RESI"))


;; OCCU 
(defun gedcom-occu ()
  "Insert OCCU tag"
  (interactive)
  (gedcom--insert-level1-tag "OCCU" "Occupation"))


;; MARR 
(defun gedcom-marr ()
  "Insert MARR tag"
  (interactive)
  (gedcom--insert-level1-tag "MARR"))



;; DIV 
(defun gedcom-divo ()
  "Insert DIV tag"
  (interactive)
  (gedcom--insert-level1-tag "DIV"))



;; DSCR
(defun gedcom-dscr ()
  "Insert DSCR tag"
  (interactive)
  (gedcom--insert-level1-tag "DSCR" "Physical description"))


;; NCHI
(defun gedcom-nchi ()
  "Insert NCHI tag"
  (interactive)
  (gedcom--insert-level1-tag "NCHI" "Number of children"))


;; NMR
(defun gedcom-nmr ()
  "Insert NMR tag"
  (interactive)
  (gedcom--insert-level1-tag "NMR" "Number of marriages"))



;; PROP
(defun gedcom-prop ()
  "Insert PROP tag"
  (interactive)
  (gedcom--insert-level1-tag "PROP" "Possessions"))


;; RESN
(defun gedcom-resn ()
  "Insert RESN tag"
  (interactive)
  (gedcom--insert-level1-tag "RESN" "Restriction notice [privacy]" "privacy" t))


;; RELI
(defun gedcom-reli ()
  "Insert RELI tag"
  (interactive)
  (gedcom--insert-level1-tag "RELI" "Religious affiliation"))


;; RETI
(defun gedcom-reti ()
  "Insert RETI tag"
  (interactive)
  (gedcom--insert-level1-tag "RETI"))



;; CONF
(defun gedcom-conf ()
  "Insert CONF tag"
  (interactive)
  (gedcom--insert-level1-tag "CONF"))



;; EMIG
(defun gedcom-emig ()
  "Insert EMIG tag"
  (interactive)
  (gedcom--insert-level1-tag "EMIG"))



;; IMMI
(defun gedcom-immi ()
  "Insert IMMI tag"
  (interactive)
  (gedcom--insert-level1-tag "IMMI"))



;; FCOM
(defun gedcom-fcom ()
  "Insert FCOM tag"
  (interactive)
  (gedcom--insert-level1-tag "FCOM"))



;; AUTH
(defun gedcom-auth ()
  "Insert AUTH tag"
  (interactive)
  (gedcom--insert-level1-tag "AUTH" "Source Originator" nil t)
  (message "Use CONT/CONC to enter more information..."))


;; ORDN
(defun gedcom-ordn ()
  "Insert ORDN tag"
  (interactive)
  (gedcom--insert-level1-tag "ORDN"))



;; NATU
(defun gedcom-natu ()
  "Insert NATU tag"
  (interactive)
  (gedcom--insert-level1-tag "NATU"))


;; CENS
(defun gedcom-cens ()
  "Insert CENS tag"
  (interactive)
  (gedcom--insert-level1-tag "CENS"))


;; WILL
(defun gedcom-will ()
  "Insert WILL tag"
  (interactive)
  (gedcom--insert-level1-tag "WILL"))




;; GRAD
(defun gedcom-grad ()
  "Insert GRAD tag"
  (interactive)
  (gedcom--insert-level1-tag "GRAD"))



;; NATI
(defun gedcom-nati ()
  "Insert NATI tag"
  (interactive)
  (gedcom--insert-level1-tag "NATI" "National origin"))



;; ENGA
(defun gedcom-enga ()
  "Insert ENGA tag"
  (interactive)
  (gedcom--insert-level1-tag "ENGA"))


;; TITL (Nobility)
(defun gedcom-titl ()
  "Insert TITL (Nobility) tag"
  (interactive)
  (gedcom--insert-level1-tag "TITL"))



;; TITL (Descriptive)
(defun gedcom-descr-titl ()
  "Insert TITL (Descriptive Title) tag"
  (interactive)
  (gedcom--insert-level1-tag "TITL" "Description" nil t))



;; ABBR
(defun gedcom-abbr ()
  "Insert ABBR tag"
  (interactive)
  (gedcom--insert-level1-tag "ABBR" "Description" nil t))



;; PUBL
(defun gedcom-publ ()
  "Insert PUBL tag"
  (interactive)
  (gedcom--insert-level1-tag "PUBL" "Publication facts" nil t)
  (message "Use CONT/CONC to enter more information..."))



;; SUBM
(defun gedcom-subm ()
  "Insert SUBM tag"
  (interactive)
  (gedcom--insert-level1-tag "SUBM" "XRef to Submitter" "<>" t))



;; ----------------------------------------------------------------------

;; NAME
(defun gedcom-name ()
  "Insert NAME tag"
  (interactive)
  (let ((gedcom-fnam (read-string "Name prefix and First Name: "))
        (gedcom-snam (setq gedcom-snam 
                      (or (read-string "Surname: " gedcom-snam))))
        (gedcom-nams (read-string "Name suffix: "))
       )
    (insert "1 NAME " gedcom-fnam " /" gedcom-snam "/" gedcom-nams)
    (gedcom--indent-and-insert-new-line))
  (gedcom-sour 2))


;; REFN
(defun gedcom-refn ()
  "Insert REFN tag"
  (interactive)
  (let ((gedcom-refn (setq gedcom-refn 
                      (or (read-string "REFN: " gedcom-refn)))))
  (if (not (string= gedcom-refn ""))
    (progn
      (insert "1 REFN " gedcom-refn)
      (gedcom--indent-and-insert-new-line)))))


;; User-defined tag
(defun gedcom-usrtag (&optional level)
  "Insert user defined tag at LEVEL; default is 1. An underscore is 
put before the tag.

You can choose a value from a completion-list. If you've not set
`gedcom-dont-allow-utag-definition-on-the-fly' then for your
convenience you can just type a new tag that gets inserted into the
list. If you want to save this list for future sessions, you should
call `customize-option' on `gedcom-utag-alist'."
  (interactive "p")
  (or level 1)
  (let ((gedcom-utag (completing-read "Tag: " gedcom-utag-alist nil
				      gedcom-dont-allow-utag-definition-on-the-fly))
	(val (read-string "Value: ")))
    (if (not (assoc (upcase gedcom-utag) gedcom-utag-alist))
	(setq gedcom-utag-alist (cons (list (upcase gedcom-utag)) gedcom-utag-alist)))
    (insert (concat (number-to-string level) " _" (upcase gedcom-utag) " " val))
    (gedcom--indent-and-insert-new-line))
  (gedcom-sour (1+ level)))

(provide 'gedcom)

;;; gedcom.el ends here
;;; End of lifelines definitions
