;;; bbdb-sms.el --- bbdb functions for sending SMS text messages with sms-mode

;; Filename: bbdb-sms.el
;; Description: bbdb functions for sending SMS text messages with sms-mode
;; Author: Joe Bloggs (vapniks@yahoo.com)
;; Maintainer: Joe Bloggs
;; Copyright (C) 2009, Joe Bloggs
;; Created: Thu Oct  8 23:16:05 2009
;; Version: 0.1
;; Last-Updated: Thu Oct  8 23:16:15 2009
;;           By: Joe Bloggs
;; URL:
;; Keywords: bbdb, sms, texting
;; Compatibility: GNU Emacs 23.1.50.1, BBDB 2.36
;;
;; Features that are required by this library:
;; sms, bbdb
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; This library contains 3 useful functions.
;; 
;; bbdb-sms-send-text-message :     Send text message to bbdb record at point (called from within *BBDB* buffer).
;;                                  An *SMS* buffer will be opened for editing the text message, and the
;;                                  "To:" header will be filled with the valid phone number(s) for the
;;                                  bbdb record at point.
;;                                  A phone number is valid if it's type is a member of the
;;                                  bbdb-sms-phone-number-types list.
;;                                  If bbdb-sms-all-phone-numbers is non-nil then all valid phone numbers
;;                                  for the current record will be used, otherwise just the first one will be used.
;; bbdb-sms-send-text-message-all : Send text message to all displayed records in current *BBDB* buffer.
;;                                  The same rules as for bbdb-sms-send-text-message apply, but now phone
;;                                  numbers for all currently displayed records are added.
;; bbdb-sms-complete-name :         Complete name from bbdb records, and insert valid phone number(s) for texting.
;;                                  If bbdb-sms-complete-name-allow-cycling is non-nil, and bbdb-sms-all-phone-numbers
;;                                  is nil, then subsequent calls to this function will cycle through all valid
;;                                  phone numbers.
;;
;; The following variables are used:
;; 
;; bbdb-sms-phone-number-types : List of valid phone number types for sending sms text messages to.
;;                               Default value is '("Mobile")
;;
;; bbdb-sms-all-phone-numbers : Whether or not to send text messages to all valid phone numbers of a record,
;;                              or just the first valid phone number. Default value is nil
;;
;; bbdb-sms-complete-name-allow-cycling : Whether to allow cycling through valid phone numbers on subsequent
;;                                        calls to bbdb-sms-complete-name function. This only takes effect if
;;                                        bbdb-sms-all-phone-numbers is nil. Default value is t

;;; INSTALLATION:
;;
;; Make sure you have sms.el and bbdb 2.36 installed
;;
;; Put bbdb-sms.el in your load-path.
;; The load-path is usually ~/.emacs.d/
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'bbdb-sms)
;;

;;; Change log:
;;
;; 2009/10/08
;;      First released.

;;; Acknowledgements: the code for the bbdb-sms-complete-name function is just a modified version of
;;                    bbdb-complete-name from bbdb-com.el

;;; TODO: completion just from phone number?

;;; Require:
(require 'bbdb)
(require 'sms)


;;; CODE:
(defcustom bbdb-sms-phone-number-types '("Mobile")
  "Which phone number types to use when sending SMS text messages.
This should be a list of labels that are also members of bbdb-phone-label-list"
  :type '(repeat string)
  :group 'bbdb-phone-dialing)

(defcustom bbdb-sms-all-phone-numbers nil
  "When non-nil then all valid phone numbers whose type is in `bbdb-sms-phone-number-types' will be used
for sending SMS text messages. Otherwise only the first valid number for each record will be used."
  :type 'boolean
  :group 'bbdb-phone-dialing)


;; FIXME: Maybe this should always be 'name
(defvar bbdb-sms-completion-type 'name)

(defcustom bbdb-sms-complete-name-allow-cycling t
  "Whether to allow cycling of phone numbers when calling
`bbdb-sms-complete-name' on a completed phone. This only work when
`bbdb-sms-all-phone-numbers' is nil"
  :type 'boolean
  :group 'bbdb-phone-dialing)


(defun bbdb-sms-get-mobile-phone-nums ()
  "Get each record's mobile phone num in current *BBDB* buffer.
If there is not a *BBDB* buffer, give an error message;
if there are several mobile phone numbers for a record, the first legal one
will be used if `bbdb-sms-all-phone-numbers' is nil, otherwise, all the legal ones are collected.
In both cases only numbers whose type is in `bbdb-sms-phone-number-types' are allowed.
The spaces in the numbers are removed.
A single string of form \"NAME1 <NUM1>, NAME2<NUM2>...\" will be returned; empty
string will be returned if no mobile phone number is found."
  (let ((bbdb-display-buffer (get-buffer bbdb-buffer-name)))
    (if (bufferp bbdb-display-buffer)
	(save-excursion
	  (set-buffer bbdb-display-buffer)
	  (let (phone-nums)		; list that contain the NAME <phone> string
	    (dolist (record bbdb-records)
	      (let ((record-phone-nums (bbdb-sms-mobile-phone-num (car record))))
		(unless (string= record-phone-nums "")
		  (push record-phone-nums phone-nums))))

	    (let ((result-string ""))
	      ;; concat the NAME <phone1> <phone2> strings, separated by commas
	      (dolist (phone-num phone-nums)
		(setq result-string (concat phone-num ", " result-string)))
	      
	      ;; delete the last two chars (", ")
	      (unless (string= result-string "")
		(setq result-string (substring result-string 0 (- (length result-string) 2))))
	    result-string)))
      (error "No *BBDB* buffer found"))))

(defun bbdb-sms-mobile-phone-num (record)
  "Get RECORD's mobile phone number as a string with RECORD's name
Return a string of RECORD's mobile phone numbers whose types are in `bbdb-sms-phone-number-types',
or empty string (\"\") if RECORD doesn't have it or doesn't have a legal one. If
`bbdb-sms-all-phone-numbers' is nil, only the first legal mobile phone number will be
in the returned string; if it's non-nil, all the legal mobile phone numbers will be put to
the returned string.
The returned string also contains RECORD's name, and it looks like:

NAME <NUM1> or NAME <NUM1> <NUM2> ...

In NUM1 or NUM2, all the whitespaces are removed."
  (let ((phones (bbdb-sms-mobile-phone-num-no-format record))
	(record-name (bbdb-record-name record))
	(result-string ""))
    ;; Now PHONES is a list of string, which contains all the phone nums
    (when phones
      (setq result-string (format "<%s>" (car phones)))
      (dolist (phone (cdr phones))
	(setq result-string (format "<%s> %s" phone result-string)))
      (setq result-string (format "%s %s" record-name result-string)))
    result-string))

;; Just return a list of phone number strings
;; the result is without format
(defun bbdb-sms-mobile-phone-num-no-format (record &optional all-nums)
"Return a list of valid phone number strings (see `bbdb-sms-mobile-phone-num') for record.
The phone numbers are not formatted.
If the optional ALL-NUMS is non-nil, all the nubmbers according to
`bbdb-sms-phone-number-types' will be returned; otherwise whether all numbers
are returned is controled by `bbdb-sms-all-phone-numbers'"
  (or all-nums (setq all-nums bbdb-sms-all-phone-numbers))
  (let ((phones (bbdb-record-phones record))
	legal-phone-nums)
    (when phones
      ;; Now phones is the list contains all the phone nums
      (dolist (phone phones)
	(when (member (elt phone 0) bbdb-sms-phone-number-types)
	  (let* ((phone-num (elt phone 1))
		 (phone-num-without-spaces (sms-string-remove-number-separators phone-num)))
	    (unless (string= phone-num-without-spaces "")
	      (push phone-num-without-spaces legal-phone-nums))
	    (unless all-nums
	      (return))))))
    legal-phone-nums))


(defun bbdb-sms-completion-predicate (symbol)
  "Predicate used for completion in bbdb-sms-complete-name function."
  (cond ((null bbdb-sms-completion-type)
	 t)
	((not (boundp symbol))
	 nil)
	(t
	 (let ((sym  (symbol-name symbol))
	       (recs (symbol-value symbol))
	       ok)
	   (while (and recs (not ok))
	     (setq ok   (bbdb-sms-completion-check-record sym (car recs))
		   recs (cdr recs)))
	   ok))))

(defun bbdb-sms-completion-check-record (sym rec)
  "Used in bbdb-sms-completion-predicate to check record."
  (let ((name (or (bbdb-record-name rec)
		  (bbdb-record-company rec)
		  ""))
	ok)
    (if (null bbdb-sms-completion-type)
	(setq ok 't)

      (setq ok (string= sym (downcase name))))
    ok))

;; Function called to parse one or more phone nums.
(defun bbdb-sms-extract-phone-components (phoneline)
  "Splite PHONELINE into a list of parsed phone numbers.
NAME1 <NUM1>, NAME2 <NUM2>
 will be parsed to
 ((NAME1 NUM1) (NAME2 NUM2))"
  (let (phones (start 0))
    (setq phoneline (concat phoneline ",")) ;; to make parsing easier
    ;; Phone numbers are separated by commas. So we split on commas, and then
    ;; try and parse what we've found.
    (while (string-match "\\([^,]+\\)" phoneline start)
      (let* ((thisphone (substring phoneline 0 (match-end 1)))
	     (comma (match-end 0)))
	(if (bbdb-sms-legal-phone-with-namep thisphone)
	    (setq phones
		  (append phones (list (bbdb-sms-extract-phone-component
					thisphone)))
		  ;; throw away what we just parsed
		  phoneline (substring phoneline (1+ comma))
		  start 0)
	  (setq start (1+ comma)))))
    phones))

(defun bbdb-sms-legal-phone-with-namep (name-phone-string)
  "Return t if PHONELINE is in format NAME <NUM>, otherwise nil."
  (let ((phone-beg (string-match "<\\([0-9]+\\)>[ \t]*" name-phone-string)))
    (and phone-beg
	 (not (string= (bbdb-string-trim (substring name-phone-string 0 phone-beg))
		       "")))))

(defun bbdb-sms-extract-phone-component (name-phone-string)
  "Return a list of the form (NAME PHONE)."
  (let* ((phone-beg (string-match "<\\([0-9]+\\)>[ \t]*" name-phone-string))
	 (phone-end (string-match ">" name-phone-string phone-beg))
	 (phone (substring name-phone-string (1+ phone-beg) phone-end))
	 (name (bbdb-string-trim (substring name-phone-string 0 phone-beg))))
    (list name phone)))

(defun bbdb-sms-dwim-phone-number (record phone)
  "Return a string to use as the email address of the given record."
  (format "%s <%s>" (bbdb-record-name record) phone))

(defun bbdb-sms-search-intertwingle (name phone)
  "Find bbdb the records matching NAME and PHONE."
  (bbdb-records t)
  (unless name
    (error "NAME can not be nil in `bbdb-sms-search-intertwingle"))
  (let ((name-recs (let ((recs (bbdb-gethash (downcase name)))
			 answer)
		     (while recs
		       (let ((n-rec (car recs)))
			 (if (string= (downcase name)
				      (downcase
				       (bbdb-record-name
					n-rec)))
			     (setq answer (append recs (list n-rec))))
			 (setq recs (cdr recs))))
		     answer))
	recs)
    (dolist (rec name-recs)
      (let ((phones (bbdb-sms-mobile-phone-num-no-format rec t)))
	(if (member phone phones)
	    (add-to-list 'recs rec))))
    recs))
  
(defun bbdb-sms-complete-name (&optional start-pos)
  "Complete the user full-name before point (up to the
preceeding newline, colon, or comma, or the value of START-POS).  If
what has been typed is unique, insert the name followed by valid phone numbers
surrounded by angle brackets and the a comma (e.g: joe <07989845524> <014323578987>).
If it is a valid completion but not unique, a list of completions is displayed.

If the completion is done and `bbdb-complete-name-allow-cycling' is
true then cycle through the nets for the matching record.

When called with a prefix arg then display a list of all nets.

Completion behaviour can be controlled with `bbdb-completion-type'."
  (interactive)
  (let* ((end (point))
	 (beg (or start-pos
		  (save-excursion
		    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
		    (goto-char (match-end 0))
		    (point))))
	 (orig (buffer-substring beg end))
	 (typed (downcase orig))
	 (pattern (bbdb-string-trim typed))
	 (ht (bbdb-hashtable))
	 ;; make a list of possible completion strings
	 ;; (all-the-completions), and a flag to indicate if there's a
	 ;; single matching record or not (only-one-p)
	 (only-one-p t)
	 (all-the-completions nil)
	 (pred
	  (lambda (sym)
	    (when (bbdb-sms-completion-predicate sym)
	      (if (and only-one-p
		       all-the-completions
		       (or
			;; not sure about this. more than one record
			;; attached to the symbol? does that happen?
			(> (length (symbol-value sym)) 1)
			;; this is the doozy, though. multiple syms
			;; which all match the same record
			(delete t (mapcar (lambda(x)
					    (equal (symbol-value x)
						   (symbol-value sym)))
					  all-the-completions))))
		  (setq only-one-p nil))
	      (if (not (memq sym all-the-completions))
		  (setq all-the-completions (cons sym all-the-completions))))))
	 (completion (progn (all-completions pattern ht pred) (try-completion pattern ht)))
	 (exact-match (eq completion t)))
    (cond
     ;; No matches found OR you're trying completion on an
     ;; already-completed record. In the latter case, we might have to
     ;; cycle through the nets for that record.
     ((or (null completion)
	  (and bbdb-complete-name-allow-cycling
	       ;; when we build an obarray of names and phone numbers
	       ;; so now exact-match will always be nil
	       exact-match ;; which is a net of the record
	       (member orig
		       (bbdb-record-net
			(car (symbol-value (intern-soft pattern ht)))))))
      ;; Clean up the completion buffer, if it exists
      (bbdb-complete-name-cleanup)
      ;; Check for cycling
      (or (catch 'bbdb-sms-cycling-exit
	    (when bbdb-sms-all-phone-numbers
	      (throw 'bbdb-sms-cycling-exit nil))
	    ;; jump straight out if we're not cycling or
	    (or bbdb-sms-complete-name-allow-cycling
		(throw 'bbdb-sms-cycling-exit nil))
	    ;; find the record we're working on.
	    (let* ((phone (bbdb-sms-extract-phone-components orig))
		   (rec
		    (if (listp phone)
			;; for now, we're ignoring the case where this
			;; returns more than one record. Ideally, the
			;; last expansion would be stored in a
			;; buffer-local variable, perhaps.
			(car (bbdb-sms-search-intertwingle (caar phone)
							   (cadar phone)))
		      nil)))
	      (or rec
		  (throw 'bbdb-sms-cycling-exit nil))
	      ;; use next phone
	      (let* ((phones (bbdb-sms-mobile-phone-num-no-format rec t))
		     (this-phone (or (cadr (member (car (cdar phone)) phones))
				     (nth 0 phones))))
		(if (= (length phones) 1)
		    ;; no alternatives. don't signal an error.
		    (throw 'bbdb-sms-cycling-exit t)
		  ;; replace with new phones
		  (delete-region beg end)
		  (insert (bbdb-sms-dwim-phone-number rec this-phone))
		  (run-hooks 'bbdb-complete-name-hooks) ;; should we define bbdb-sms-complete-name-hooks?
		  (throw 'bbdb-sms-cycling-exit t)))))))
     ;; Match for a single record. If cycling is enabled then we don't
     ;; care too much about the exact-match part.
     ((and only-one-p (or exact-match bbdb-sms-complete-name-allow-cycling))
      (let* ((sym (if exact-match (intern-soft pattern ht) (car all-the-completions)))
	     (recs (symbol-value sym))
	     match-recs lst primary matched)
	(while recs
	  (when (bbdb-sms-mobile-phone-num-no-format (car recs))
	    ;; Did we match on name?
	    (let ((b-r-name (or (bbdb-record-name (car recs)) "")))
	      (if (string= pattern
			   (substring (downcase b-r-name) 0
				      (min (length b-r-name)
					   (length pattern))))
		  (setq match-recs (cons (car recs) match-recs)
			matched t)))
	    ;; Did we match on lastname?
	    (let ((b-r-name (or (bbdb-record-lfname (car recs)) "")))
	      (if (string= pattern
			   (substring (downcase b-r-name) 0
				      (min (length b-r-name)
					   (length pattern))))
		  (setq match-recs (cons (car recs) match-recs)
			matched t)))
	    ;; Did we match on aka?
	    (when (not matched)
	      (setq lst (bbdb-record-aka (car recs)))
	      (while lst
		(if (string= pattern (substring (downcase (car lst)) 0
						(min (length (downcase
							      (car
							       lst)))
						     (length pattern))))
		    (setq match-recs (append match-recs (list (car recs)))
			  matched t
			  lst '())
		  (setq lst (cdr lst))))))
	  ;; loop to next rec
	  (setq recs    (cdr recs)
		matched nil))
	(if (null match-recs)
	    (progn
	      (message "completion for \"%s\" unfound." pattern)
	      (ding))
	  ;; now replace the text with the expansion
	  (delete-region beg end)
	  (insert (bbdb-sms-mobile-phone-num (car match-recs)))
	  ;; if we're past fill-column, wrap at the previous comma.
	  (if (and
	       (bbdb-auto-fill-function)
	       (>= (current-column) fill-column))
	      (let ((p (point))
		    bol)
		(save-excursion
		  (beginning-of-line)
		  (setq bol (point))
		  (goto-char p)
		  (if (search-backward "," bol t)
		      (progn
			(forward-char 1)
			(insert "\n   "))))))
	  ;; Update the *BBDB* buffer if desired.
	  (if bbdb-completion-display-record
	      (let ((bbdb-gag-messages t))
		(bbdb-display-records-1 match-recs t)))
	  (bbdb-complete-name-cleanup)
	  ;; call the exact-completion hook
	  (run-hooks 'bbdb-complete-name-hooks))))
     ;; Partial match
     ;; note, we can't use the trimmed version of the pattern here or
     ;; we'll recurse infinitely on e.g. common first names
     ((and (stringp completion) (not (string= typed completion)))
      (delete-region beg end)
      (insert completion)
      (setq end (point))
      (let ((last "")
	    (bbdb-complete-name-allow-cycling nil))
	(while (and (stringp completion)
		    (not (string= completion last))
		    (setq last completion
			  pattern (downcase orig)
			  completion (progn (all-completions pattern ht pred) (try-completion pattern ht))))
	  (if (stringp completion)
	      (progn (delete-region beg end)
		     (insert completion))))
	(bbdb-sms-complete-name beg)))
     ;; Exact match, but more than one record
     (t
      (or (eq (selected-window) (minibuffer-window))
	  (message "Making completion list..."))
      (let (dwim-completions
	    uniq name lfname akas)
	;; Now collect all the dwim-addresses for each completion, but only
	;; once for each record!  Add it if the net is part of the completions
	(bbdb-mapc
	 (lambda (sym)
	   (bbdb-mapc
	    (lambda (rec)
	      (when (not (member rec uniq))
		(setq uniq (cons rec uniq)
		      ;; we should write a function return a list of phone num
		      phones (bbdb-sms-mobile-phone-num-no-format rec)
		      name (downcase (or (bbdb-record-name rec) ""))
		      lfname (downcase (or (bbdb-record-lfname rec) ""))
		      akas (mapcar 'downcase (bbdb-record-aka rec)))
		(while phones
		  (setq phone (car phones))
		  (when (cond ((and name
				    (let ((cname (symbol-name sym)))
				      (or (string= cname name)
					  (string= cname lfname)
					  (member cname akas))))
			       (setq name nil)
			       t))
		    (setq dwim-completions
			  (cons (bbdb-sms-mobile-phone-num rec)
				dwim-completions))
		    (if exact-match (setq phones nil)))
		  (setq phones (cdr phones)))))
	    (symbol-value sym)))
	 all-the-completions)
	;; if, after all that, we've only got one matching record...
	(if (and dwim-completions (null (cdr dwim-completions)))
	    (progn
	      (delete-region beg end)
	      (insert (car dwim-completions))
	      (message ""))
	  ;; otherwise, pop up a completions window
	  (if (not (get-buffer-window "*Completions*"))
	      (setq bbdb-complete-name-saved-window-config
		    (current-window-configuration)))
	  (let ((arg (list (current-buffer)
			   (set-marker (make-marker) beg)
			   (set-marker (make-marker) end))))
	    (with-output-to-temp-buffer "*Completions*"
	      (bbdb-display-completion-list
	       dwim-completions
	       'bbdb-complete-clicked-name
	       arg)))
	  (or (eq (selected-window) (minibuffer-window))
	      (message "Making completion list...done"))))))))


(defun bbdb-sms-send-text-message-all nil
  "Send a text message to mobile phone numbers in the current bbdb buffer.
If `bbdb-sms-all-phone-numbers' is t, the text will be sent to all valid mobile phone numbers
whose types are in `bbdb-sms-phone-number-types', otherwise it will just be sent to the first
valid mobile phone number of each record in the bbdb buffer.
A new buffer in sms-mode will be opened in which the text message can be written.
Then when C-c C-c is pressed the buffer will be sent as a text message."
  (interactive)
  (let ((numbers (bbdb-sms-get-mobile-phone-nums)))
    (switch-to-buffer-other-window (sms-create-buffer numbers))))

(defun bbdb-sms-send-text-message nil
  "Send a text message to mobile phone number of record at point in the current bbdb buffer.
If `bbdb-sms-all-phone-numbers' is t, the text will be sent to all valid mobile phone numbers
whose types are in `bbdb-sms-phone-number-types', otherwise it will just be sent to the first
valid mobile phone number.
A new buffer in sms-mode will be opened in which the text message can be written.
Then when C-c C-c is pressed the buffer will be sent as a text message."
  (interactive)
  (let ((numbers (bbdb-sms-mobile-phone-num (bbdb-current-record))))
    (switch-to-buffer-other-window (sms-create-buffer numbers))))


(provide 'bbdb-sms)

;;; end
;;; bbdb-sms.el ends here
