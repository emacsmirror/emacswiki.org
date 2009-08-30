;;; tc.el -- cite text with proper filling
;;
;; TrivialCite v0.13.3
;; This is my attempt at making a sensible citer.
;;
;; This program is copyright (c) 1998 Lars R. Clausen
;;
;; Time-stamp: <2003-05-14 16:21:59 lrclause>
;;
;; Author: Lars R. Clausen <lrclause@cs.uiuc.edu>
;; Created: March 1998
;; Keywords: Citing, filling, mail, news
;; X-URL: http://shasta.cs.uiuc.edu/~lrclause/tc.html
;;
;; trivial-cite is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; trivial-cite is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with trivial-cite; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; TODO: Good way to undo fillings without mouse.
;;       More funny functions:)
;;       Add space after cite-marks if old citing doesn't have it?
;;       Make tc-fill-cited-paragraph faster by re-inserting all at once.
;;       Generally optimize -- font-lock wastes time.
;;       Some simpler way to generate attributions?
;;       Follow Stallmans advice:  Better docs, what's different from
;;         SuperCite, better comments, how is the filling different
;;         from standard Emacs filling.

;; No, I will not make it quote with name abbreviations like SuperCite does.
;; That style is annoying and unreadable, goes against the RFC's (or rather,
;; the sons of them:), and have generally been the most problematic thing to
;; deal with.  Trivial-cite can handle them, but is better at 'normal'
;; citation marking.

;; Reminder:
;; When mail-citation-hook is run, the cite is in current-buffer, (point) at
;; start and (mark t) at end.

;; To use, add the following to your .emacs:
;;
;; (autoload 'trivial-cite "tc" t t)
;;
;; ;; For Gnus:
;;
;; (setq message-cite-function 'trivial-cite)

;;; ************************************************************
;;; External requirements here
;;; ************************************************************

(require 'mail-extr)
(if (featurep 'xemacs)
    (require 'overlay))

;;; ************************************************************
;;; Meta-parameters here
;;; ************************************************************

(defconst tc-maintainer "lrclause@cs.uiuc.edu")

(defconst tc-version "0.13.3")

(defvar tc-debug-level 0
  "How much debugging output trivial-cite should give.")

;;; ************************************************************
;;; Normal user-settable parameters here
;;; ************************************************************

(defgroup tc nil "Insert cited text in a nice manner")

(defcustom tc-remove-signature "^\\(-- \\|--\\)$"
  "If non-nil, specify a regexp that finds the signature divider.
The lines below the first match of this regexp will be removed, but
immediately available in the undo buffer.  If nil, the signature will
not be removed."
  :type 'regexp
  :group 'tc)

(defcustom tc-fill-column t
  "If t means attempt to fill paragraphs with long lines.
Trivial-cite attempts to guess citation marks and fill the cited
paragraphs accordingly, when there are lines of more than `fill-column'
characters including citation marks.  If you wish to undo the
filling, each paragraph filling can be undone with
\\[tc-unfill-paragraph].
An integer argument means wrap at that column instead of at `fill-column'"
  :type '(radio (const :tag "Fill at `fill-column'" t)
		(integer :tag "Fill at this column")
		(const :tag "Don't fill" nil))
  :group 'tc)

(defcustom tc-mouse-overlays nil
  "Non-nil means mark filled paragraphs with a mouse overlay.
Right-clicking such an overlay toggles filling of that paragraph,
like with \\[tc-unfill-paragraph]."
  :type 'boolean
  :group 'tc)

; Not ready yet
; (defcustom tc-cleanup-cited-marks nil
;   "Non-nil means uniform citation marks are substituted in cited text.
; Thus any sequence of cite-marks such as \"> |: }\" will be replace with a
; uniform string of the citemarks of your choice, e.g. \">>>> \"."
;   :type 'boolean
;   :group 'tc)

(defcustom tc-citation-string ">"
  "The string that trivial-cite inserts to make a citation.
The standard string (as noted in son-of-RFC1036) is '>'.  You should
not change this, as that makes it more difficult for citers (even
trivial-cite) to identify citings correctly.  An extra space is
inserted after the string, if the cited text does not seem to be cited
already.  See `tc-normal-citemarks' and `tc-guess-marks-regexp' for
how cite marks are found."
  :type 'string
  :group 'tc)

(defcustom tc-normal-citemarks ">"
  "The characters that should always be considered citation marks.
This would normally just be '>', but if you often cite text with other
regular characters used for citing, you can ease the life for both
trivial-cite and yourself by adding them here."
  :type 'string
  :group 'tc)

(defcustom tc-make-attribution 'tc-simple-attribution
  "The function used to generate a attribution for a citation.
`tc-simple-attribution' is primitive, but easy to use.
`tc-tiny-attribution' is a minimal attribution.
`kai-tc-simple-attribution' uses the real name if found.
`tc-fancy-attribution' can be used to personalize the attribution."
  :type 'function
  :options '(tc-simple-attribution tc-tiny-attribution kai-tc-simple-attribution `tc-fancy-attribution)
  :group 'tc)

(defcustom tc-time-format "%e %b %Y"
  "The time format used for the date part in the attribution.
The date is taken from the header fields.  The format is passed to
`format-time-string', see that function for more information."
  :type 'string
  :group 'tc)

(defcustom tc-guess-cite-marks 'ask
  "*Non-nil means try to guess at non-standard cite-marks.
The guess it made from a list of characters `tc-guess-marks-regexp'
which might be used for it.  If the value is 'ask, trivial-cite will
ask if the marks found are correct."
  :type '(choice (const nil) (const t) (const ask))
  :group 'tc)

(defcustom tc-guess-marks-regexp "\\=[]>};:|#$@ ]*[]>};:|#$@]"
  (concat "The regexp used for guessing at non-standard cite-marks.
If you see anyone using other characters (not alphanumeric) for citing,
plese tell " tc-maintainer " so they can be added to the list in the
distribution.")
  :type 'regexp
  :group 'tc)

(defcustom tc-normalize-cite-marks t
  "Non-nil means normalize other peoples citation marks to match yours."
  :type 'boolean
  :group 'tc)

(defcustom tc-gnus-nntp-header-hack nil
  "Non-nil means check for Gnus 5.8.7 odd header insertion.
Gnus 5.8.7 inserts an NNTP header line that's odd (haven't seen it myself,
but got a report from <Kai.Grossjohann@CS.Uni-Dortmund.DE> about it),
and this hack removes the line.  It may conceivably do damage to other lines,
too, so I'm looking for a better solution."
  :type 'boolean
  :group 'tc)

(defcustom tc-pre-hook nil
  "*Hook called in the very beginning of `trivial-cite'."
  :type 'hook
  :group 'tc)

(defcustom tc-post-hook nil
  "*Hook called in the very end of `trivial-cite'."
  :type 'hook
  :group 'tc)

;;; ************************************************************
;;; Functions that parse the cited headers to allow attribution.
;;; ************************************************************

(defvar tc-strings-list ()
  "tc-strings-list is an association list containing the parsed headers.
Typical entries are (\"subject\".\"Re: tc bug\"), (\"real-name\".\"John Doe\"),
(\"email-addr\".\"elascurn@daimi.aau.dk\") etc., but there is no fixed format."
; " fix up highlighting (*sigh*)
  )

(defvar tc-header-funs
  (list
   (cons "From" 'tc-parse-from)
   ;; The Subject: field - just put text into tc-strings-list
   (cons "Subject" '(lambda (x)
		      (setq tc-strings-list
			    (cons (cons "subject" x) tc-strings-list))))
   (cons "Date" 'tc-parse-date)
   (cons "Newsgroups" 'tc-parse-groups)
   )
  "tc-header-funs is an association list used by trivial-cite containing the
various functions for decoding headers.  The function gets a string as
argument, which is the contents of that header (possibly including newlines,
but excluding starting spaces).  Any return value is ignored.
tc-strings-list is an association list destined to hold the parsed data."
  )

;; parse the headers in the quote, calling funcs
(defun tc-parse-headers ()
  "tc-parse-headers parses the headers of a mail message and calls the
functions defined in tc-header-funs on the respective fields."
  ;; Still a header here?
  (let ((header-start (point)))
    (if tc-gnus-nntp-header-hack
	;; From <Kai.Grossjohann@CS.Uni-Dortmund.DE>, to deal with
	;; Gnus 5.8.7 putting the NNTP header into the buffer
	(while (looking-at "2[0-9][0-9] ")
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (setq header-start (point))))
    (while (not (looking-at "\012"))
      ;; Find field name
      (if (not (looking-at "\\\([!-9;-~]+\\\):[ ]*\\\([^ ]?.*\\\)"))
	  (message "Malformed field")
	(let
	    ((field-name (buffer-substring-no-properties
			  (match-beginning 1) (match-end 1)))
	     (field-contents (buffer-substring-no-properties
			      (match-beginning 2) (match-end 2))))
	  ;; Unfold
	  (forward-line 1)
	  (while (looking-at "[ \011]")
	    (let ((beg (point)))
	      (end-of-line)
	      (setq field-contents (concat field-contents
					   (buffer-substring-no-properties
					    beg (point))))
	      (forward-line 1)
	      )
	    )

          (if (string-match "[^ \011].*$" field-contents)
              (setq field-contents
                    (substring field-contents
                               (match-beginning 0)))
	    (setq field-contents ""))

	  ;; Find function for this field name
	  (let ((field-func (assoc field-name tc-header-funs)))
	    (if field-func
		(progn
		  (setq field-func (cdr field-func))
		  (funcall field-func field-contents))
	      )
	    )
	  )
	)
      )
    (forward-line 1);; Skip past one blank line seperating headers and body
    (delete-region header-start (point))
    )
  )

;;; Functions to parse individual headers into appropriate structures here

(defun tc-parse-date (str)
  "tc-parse-date uses tc-time-format to parse the date for use in attributions.
The resulting string is inserted into tc-strings-list."
  (let* ((time (date-to-time str))
	 (date (format-time-string tc-time-format time)))
    (setq tc-strings-list (cons (cons "date" date) tc-strings-list)))
  )

;; Parse a From:-style field from str into tc-strings-list under key
(defun tc-parse-from (str)
  "tc-parse-from uses mail-extr to get email-addr and real-name into
tc-strings-list."
  (if (> tc-debug-level 0)
      (message "%s" (concat "Parsing from string '" str "'")))
  (let ((names (mail-extract-address-components str)))
    (if names
	(progn
	  (if (car names)
	      (setq tc-strings-list (cons (cons "real-name" (car names))
					  tc-strings-list)))
	  (if (car (cdr names))
	      (setq tc-strings-list (cons (cons "email-addr" (car (cdr names)))
					  tc-strings-list)))
	  )
      )
    )
  )

(defun tc-parse-groups (str)
  (if (> tc-debug-level 0)
      (message "%s" (concat "Parsing groups string '" str "'")))
  (let ((pos 0)
	groups)
    (while (string-match ",[ \012]*" str pos)
      (setq groups (cons (substring str pos (1- (match-end 0))) groups))
      (setq pos (match-end 0)))
    (setq tc-strings-list
	  (cons (cons "newsgroups"
		      (nreverse (cons (substring str pos (length str))
				      groups)))
		tc-strings-list)))
  )

;;; Functions to make various default attributions here
;; My simple (but nice:) attribution function
(defun tc-simple-attribution ()
  "tc-simple-attribution makes an attribution from email address and date."
  (let ((date (assoc "date" tc-strings-list))
	(name (assoc "email-addr" tc-strings-list)))
    (if (null name)
	"An unnamed person wrote:\n\n"
      (if (null date)
	  (concat (cdr name) " wrote:\n\n")
	(concat "On " (cdr date) ", " (cdr name) " wrote:\n\n")
	)
      )
    )
  )

;; A simple attribution by Kai Großjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
(defun kai-tc-simple-attribution ()
	      "Produce the standard attribution string, using the real name."
  (let ((date (assoc "date" tc-strings-list))
	(email (assoc "email-addr" tc-strings-list))
        (name (assoc "real-name" tc-strings-list)))
    (if (and (null name) (null email))
	"An unnamed person wrote:\n\n"
      (if (null date)
	  (concat (cdr (or name email)) " wrote:\n\n")
	(concat "On " (cdr date) ", " (cdr (or name email)) " wrote:\n\n")))))

;; A very small attribution, using real name or email
(defun tc-tiny-attribution ()
  "Produce a very small attribution string."
  (let ((email (assoc "email-addr" tc-strings-list))
        (name (assoc "real-name" tc-strings-list)))
    (concat (cdr (or name email '(t . "Somebody"))) " wrote:\n\n")))

;;; ************************************************************
;;; Deal with the signature and other minor fuzz.
;;; ************************************************************

;; Normally, the signature should be removed, if we can find
;; it.  But we want it to be ready for the first undo.
(defvar tc-removed-sig nil
  "The signature removed from the last mailing.")
(defvar tc-removed-sig-marker nil
  "Marks the place where the signature was removed from the last mailing.")

(defun tc-do-remove-sig ()
  "Attempt to remove the signature from already quoted text.
Warns if it is longer than 4 lines (5 including signature mark '-- ')."
  (save-excursion
    (setq tc-removed-sig nil)
    (setq tc-removed-sig-marker nil)
    (exchange-point-and-mark)
    (let ((msgend (point)))
      (if (re-search-backward tc-remove-signature 0 t)
	  ;; Found it
	  (let ((lines (count-lines (point) msgend)))
	    (setq tc-removed-sig (buffer-substring (point) msgend))
	    (delete-region (point) msgend)
	    (setq tc-removed-sig-marker (point-marker))
	    (if (> lines 5);; Remember to include the '-- ' mark
		(message (concat "Signature was very large ("
				 (int-to-string (- lines 1)) " lines)."))
	      )
	    )
	)
      )
    )
  )

(defun tc-fix-signature-undo ()
  "Make the signature be after filling in undo list, and quoted."
  (if tc-removed-sig
      (progn
	(save-excursion
	  (goto-char (marker-position tc-removed-sig-marker))
	  (insert tc-removed-sig)
	  (let ((sig-end (point-marker)))
	    (goto-char (marker-position tc-removed-sig-marker))
	    (while (< (point) (marker-position sig-end))
	      (insert tc-citation-string " ")
	      (forward-line 1)))
	  (undo-boundary)
	  (delete-region (marker-position tc-removed-sig-marker)
			 (point))
	  )
	)
    )
  )

;; Simple nested indentation, as defined in son-of-rfc1036 (plus one space
;; after > before non-cited text for readability).

(defvar tc-cite-marks nil
  "The cite-marks that are recognised by trivial-cites functions.
These are deleted after each citing.")

;; Give some extra characters that have been used for indention, so we know
;; to handle them.
(defun tc-extra-cite-marks (str)
  "Specify extra cite-marks apart from '>' that have been used for citing.
They remain valid for one citing only."
  (interactive "sExtra cite-marks: ")
  (if (not (equal str ""))
      (setq tc-cite-marks str)
    )
  )

;; Thanks to Matthias Wiehl for this function
(defun tc-cleanup-cite-marks (start end)
  "Substitute uniform citation marks in the current region, replacing
any sequence of cite-marks such as \"> |: }\" with a uniform string
of the citemarks of your choice, e.g. \">>>> \"."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((end-marker (set-marker (make-marker) end)))
      (while (< (point) (marker-position end-marker))
	(if (looking-at " ")
	    (delete-char 1)
	  (if (looking-at (concat "[" tc-cite-marks "]"))
	      (progn
		(insert tc-citation-string)
		(delete-char 1))
	    (progn
	      (if (and (not (eq (preceding-char) ?\ ))
		       (not (looking-at "$")))
		  (insert " "))
	      (forward-line 1))))
	)
      )
    )
  )

(defun tc-indent-citation ()
  "tc-indent-citation indents the current region with tc-citation-string.
It inserts an extra space before text that is not already cited (with
tc-cite-marks), except on empty lines (to avoid dangling space)."
  (while (< (point) (mark t))
    (cond ((re-search-forward (concat "[" tc-cite-marks "]") (1+ (point)) t)
	   (forward-char -1)
	   (insert tc-citation-string))
	  ((looking-at "^$")
	   (insert tc-citation-string))
	  (t (insert tc-citation-string " ")))
    (forward-line 1)
    )
  )

(defun tc-remove-trailing-whitespace ()
  ;; First remove trailing empty lines
  (save-excursion
    (if (< (point) (mark t))
	(exchange-point-and-mark))
    (let ((end-cite (point)))
      (re-search-backward "[^ \t\n]" 0 t);; Skip empty lines
      (forward-line 1);; Whoops, got one line too far
      (delete-region (point) end-cite))))

(defun tc-combine-cite-marks (cm1 cm2)
  "Combine two sets of cite-marks so that there are no duplicates.
In fact, it checks if cm2 is contained in cm1, and if not, makes it so."
  (if (not (string-match (regexp-quote cm1) cm2))
      (concat cm1 cm2)
    cm1))

(defvar tc-old-yank nil)

(defvar tc-prefix-max-lines t
  "If t, message-yank-original takes a prefix max number of lines")

(defvar tc-max-lines nil
  "*Maximum number of lines that should be quoted by trivial-cite.
If tc-max-lines-reset is non-nil (the default), tc-max-lines is set to nil
(meaning no limit) after each cite.")

; (defun tc-message-yank-original (&optional lines)
;   "Insert the message being replied to, if any.
; Puts point before the text and mark after.
; Indents the text using trivial-cite (cv).
; A numeric prefix is the maximal number of (body) lines to cite.

; This function uses `message-cite-function' to do the actual citing.
; "
;   (interactive "P")
;   (message (concat "Prefix is '" lines "'"))
;   (if lines
;       (setq tc-max-lines lines)
;     (setq tc-max-lines nil))
;   (tc-old-yank))

;; Replace the normal message-yank-original with a version that uses
;; the prefix to limit the number of lines.  Somewhat of a hack, but I like to
;; be able to do this:)
;; Now uses advice
(if tc-prefix-max-lines
    (defadvice message-yank-original (before max-lines-advice first (&optional lines) activate)
      "A numeric prefix is the maximal number of (body) lines to cite.
"
      (interactive "P")
      (if lines
	  (setq tc-max-lines lines)
	(setq tc-max-lines nil))
      (setq lines nil) ; Make message-yank-original happy
))

(defvar tc-max-lines-reset t
  "*Whether tc-max-lines should be reset after use.
Normally, tc-max-lines is set to limit the citation of very long mails (e.g.
citations).")

(defun tc-indent-region (start end)
  "*Cite the region like trivial-cite, but without parsing headers.
Doesn't cut the signature either."
  (interactive "r")
  (save-excursion
    (if (> start end)
	(let ((tmp start)) (setq start end) (setq end tmp)))
    (goto-char start)
    (set-mark end)
    (setq tc-strings-list ())
    ;; Get the correct set of cite-marks, guessing if necessary
    ;; (delayed until now to allow sig to be removed)
    (if tc-cite-marks
	(setq tc-cite-marks (tc-combine-cite-marks
			     tc-normal-citemarks tc-cite-marks))
      (if tc-guess-cite-marks
	  (setq tc-cite-marks (tc-combine-cite-marks
			       tc-normal-citemarks (tc-guess-cite-marks)))
	(setq tc-cite-marks tc-normal-citemarks)))
    ;; Escape any cite-marks that would cause problems in a regexp
    (setq tc-cite-marks (escape-char-range tc-cite-marks))
    ;; Do the actual citation
    (tc-indent-citation)
    ;; Normalize cite marks if so wanted
    (if tc-normalize-cite-marks
	(tc-cleanup-cite-marks start end))
    ;; Fill paragraphs
    (if tc-fill-column
	(tc-fill-cited-text start end))
    (setq tc-cite-marks nil)
    ))

(defun tc-fix-final-newline ()
  "Adds a newline if there is not one at the end of the cited text."
  (save-excursion
    (exchange-point-and-mark)
    (if (not (bolp))
	(insert "\n"))))

;;; ************************************************************
;;; The main citation engine
;;; ************************************************************
;;;###autoload
(defun trivial-cite ()
  "trivial-cite is a simple citation function for use in news/mailreaders.
It parses the headers via the functions defined in tc-header-funs, then
makes a attribution for the citation using tc-make-attribution and indents
the inserted text with tc-indent-citation.
Numeric prefix arguments is how many lines of body to cite (useful for citing
mails with long attachments).
Usage:  (auto-load 'trivial-cite \"tc\" t t)
        (add-hook 'mail-citation-hook 'trivial-cite)
Bugs:  Not very intelligent about old citation marks other than '>'.
Customization:  See variables tc-fill-column, tc-remove-signature,
tc-citation-string, tc-make-attribution and tc-header-funs."
  (run-hooks 'tc-pre-hook)
  (save-excursion
    (if (< (mark t) (point)) (exchange-point-and-mark))
    (let ((start (point)))
      ;; Initialize some fields
      (setq tc-strings-list ())
      ;; Allow undo to show the unformatted text
      (undo-boundary)
      (tc-fix-final-newline)
      ;; Parse the headers - assumes point at first header
      (tc-parse-headers)
      ;; Insert a attribution ("XXX wrote...:" etc)
      (if tc-make-attribution
	  (let ((start-marker (point-marker))
		(fill-prefix))
	    (insert (funcall tc-make-attribution))
	    (fill-region (marker-position start-marker) (point))
	    (setq start (point))
	    ))
      (tc-remove-trailing-whitespace)
      ;; Remove signature (if so wanted)
      (if tc-remove-signature
	  (tc-do-remove-sig))
      (tc-remove-trailing-whitespace)
      (if tc-max-lines
	  (save-excursion
	    (message (concat "Only citing "
			     (int-to-string tc-max-lines) " lines"))
	    (goto-char start)
	    (forward-line tc-max-lines)
	    (kill-region (point) (mark t))
	    (if tc-max-lines-reset (setq tc-max-lines nil))))
      ;; Get the correct set of cite-marks, guessing if necessary
      ;; (delayed until now to allow sig to be removed)
      (if tc-cite-marks
	  (setq tc-cite-marks (tc-combine-cite-marks
			       tc-normal-citemarks tc-cite-marks))
	(if tc-guess-cite-marks
	    (setq tc-cite-marks (tc-combine-cite-marks
				 tc-normal-citemarks (tc-guess-cite-marks)))
	  (setq tc-cite-marks tc-normal-citemarks)))
      ;; Escape any cite-marks that would cause problems in a regexp
      (setq tc-cite-marks (escape-char-range tc-cite-marks))
      ;; Do the actual citation
      (tc-indent-citation)
      ;; Normalize cite marks if so wanted
      (if tc-normalize-cite-marks
	  (tc-cleanup-cite-marks start (mark-marker)))
      ;; Fill paragraphs
      (if tc-fill-column
	  (tc-fill-cited-text start (mark-marker)))
      (setq tc-cite-marks nil)
      (tc-fix-signature-undo)
      )
    )
  (run-hooks 'tc-post-hook)
  )

;;; ************************************************************
;;; Reformatting cited text
;;; ************************************************************

(defun tc-fill-column ()
  "Returns the fill column that tc uses (explicit, `fill-column' or nil)."
  (cond ((integerp tc-fill-column) tc-fill-column)
	(tc-fill-column fill-column)
	(t nil)))

(defun tc-fill-cited-paragraphs (cite-len)
  "Fill cited paragraphs, keeping cite-marks in their correct places.  Used
internally in tc-fill-cited-text.  Returns the end of the last filled
paragraph."
  (interactive "nLength of citation marks: ")
  (let (fill-end)
    (save-excursion
      (save-restriction
	(beginning-of-line)
	(let ((cite-marks (buffer-substring (point) (+ (point) cite-len)))
	      (fill-line (point)))
	  (if (>= tc-debug-level 1)
	      (message (concat "Citing marked with "
			       cite-marks ", extra marks are " tc-cite-marks)))
	  (let ((cite-regexp (concat "^" cite-marks
				     " *[^\n" tc-cite-marks " ]")))
	    ;; Look backward while properly cited
	    (while (and (not (bobp)) (looking-at cite-regexp))
	      (forward-line -1)
	      )
	    (if (not (looking-at cite-regexp))
		(forward-line 1))
	    (let ((cite-start (point))
		  (fill-column (- (tc-fill-column) cite-len)))
	      (goto-char fill-line)
	      (while (and (not (eobp)) (looking-at cite-regexp))
		(forward-line 1))
	      (if (looking-at cite-regexp)
		  (end-of-line))
	      (narrow-to-region cite-start (point))
	      (forward-line -1)
	      (forward-char cite-len)
	      (let ((cut-text (buffer-substring (point-min) (point-max))))
		(delete-extract-rectangle cite-start (point))
		(goto-char fill-line)
		(while (not (eobp))
		  (fill-paragraph nil)
		  (forward-paragraph))
		(goto-char cite-start)
		(while (not (eobp))
		  (insert cite-marks)
		  (forward-line 1)
		  )
		(setq fill-end (point))
		(let ((reformat-overlay
		       (make-overlay (point-min) (point-max))))
		  (overlay-put reformat-overlay
			       'tc-reformat (cons cut-text  reformat-overlay))
					     ;; Should check for mouse
		  (local-set-key "\C-c\C-p" 'tc-unfill-paragraph)
		  (if tc-mouse-overlays
		      (progn
			(overlay-put reformat-overlay
				     'mouse-face 'secondary-selection)
			(local-set-key [(shift button2)] 'tc-unfill-paragraph-mouse)
		    )))
		)
	      )
	    )
	  )
	)
      )
    fill-end
    )
  )

(defun escape-char-range (str)
  "Escape any characters that would cause problems in a regexp char range.
This, is not the same as regexp-quote, as we need to treat ^ and ] very
specially."
  (if (string-match "\\(-.*]\\|].*-\\)" str)
      ;; - and ] in a string -- got to seperate them
      (message "Can't have both - and ] in a regular expression (yet).")
    (if (string-match "-" str)
	;; Move a "-" to start of the string
	(progn
	  (while (string-match "-" str)
	    (setq str (replace-match "" nil nil str)))
	  (setq str (concat "-" str))
	  ))
    (if (string-match "]" str)
	;; Move a "]" to the start of the string
	(progn
	  (while (string-match "]" str)
	    (setq str (replace-match "" nil nil str)))
	  (setq str (concat "]" str))
	  ))
    (if (string-match "^\\^" str)
	;; Move the "^" to not be at the start of the string
	(progn
	  (replace-match "" nil nil str)
	  (setq str (concat str "^")))))
  str
  )

(defun find-cite-len (p)
  "Find the length of the citation marking so we can fix it when filling.
Used internally in tc-fill-cited-text."
  (save-excursion
    (goto-char p)
    (forward-line 1)
    (let ((forward-prefix-length (line-common-prefix-length p (point))))
      (forward-line -2)
      (let ((backward-prefix-length (line-common-prefix-length p (point))))
	(goto-char p)
	(beginning-of-line)
	(let ((prefix-length
	       (max forward-prefix-length backward-prefix-length))
	      (line-start (point)))
	  (end-of-line)
	  (let ((line-end (point)))
	    (beginning-of-line)
	    ;; Check if this is a one-line cite with good cite-marks
	    (if (and (re-search-forward
		      (concat "^[" tc-cite-marks " ]*["	tc-cite-marks "]["
			      tc-cite-marks " ]*") line-end t)
		     (> (- (match-end 0) (match-beginning 0)) prefix-length))
		(- (match-end 0) (match-beginning 0))
	      (forward-char prefix-length)
	      (if (re-search-backward
		   (concat "[" tc-cite-marks "][" tc-cite-marks " ]*")
		   line-start t)
		  (- (match-end 0) line-start)
		0))))))))

;; Find all lines that are too long and fill them
(defun tc-fill-cited-text (start end)
  "Fill all lines in region that are too long, keeping track of cite-marks.
Uses a seperate undo-mechanism (with overlays) to allow partial undo."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (beginning-of-line)
      (end-of-line)
      (if (> (current-column) (tc-fill-column))
	  (progn
	    (let ((cite-len (find-cite-len (point))))
	      (if (> cite-len 0)
		  (if  (< cite-len (tc-fill-column))
		      (goto-char (tc-fill-cited-paragraphs cite-len))
		    (message (concat "Very long cite mark ("
				     (int-to-string cite-len) " chars)"))
		    (forward-line 1))
		(message (concat "Mysterious zero cite-len at "
				 (int-to-string (point))))
		(forward-line 1))))
	(forward-line 1))
      )
    )
  )

;; A couple utility functions.

(defun line-common-prefix-length (p1 p2)
  "Returns the number of characters the two lines have as common prefix."
  (save-excursion
    (let ((line1 (progn (goto-char p1) (beginning-of-line)
			(let ((line-start (point)))
			  (end-of-line)
			  (buffer-substring line-start (point)))))
	  (line2 (progn (goto-char p2) (beginning-of-line)
			(let ((line-start (point)))
			  (end-of-line)
			  (buffer-substring line-start (point))))))
      (string-common-prefix-length line1 line2))))

(defun string-common-prefix-length (s1 s2)
  "Returns how many characters in s1 and s2 are equal."
  (let ((len (min (length s1) (length s2)))
	(x 0))
    (while (and (< x len)
		(equal (aref s1 x) (aref s2 x)))
      (setq x (1+ x)))
    x))

(defun tc-fill-cited-region (start end)
  "Fill all lines in the region, but keep the overall citation intact.
This function assumes that all lines in the region have the same citation
marks, as it regards the shortest common prefix of the lines as citation
marks."
  (interactive "r")
;  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let ((line-start (point)))
      (end-of-line)
      (let ((cite-marks (buffer-substring line-start (point))))
	(forward-line 1)
	(while (< (point) end)
	  (let ((other-line
		 (buffer-substring (point) (+ (length cite-marks) (point)))))
	    (if (not (string-equal cite-marks other-line))
		(setq cite-marks
		      (substring cite-marks 0
				 (string-common-prefix-length
				  cite-marks other-line)))))
	  (forward-line 1)
	)
	(goto-char start)
	(save-restriction
	  (narrow-to-region start end)
	  (tc-fill-cited-paragraphs (length cite-marks))
	  )
	)
      )
					;   )
  )

(defun tc-fill-cited-region-uniformly (start end)
  "Fill all lines in the region, making the overall citation uniform.
This function finds the longest possible citemark and wraps all lines as
if they had that amount of citemarks."
  (interactive "r")
;  (save-excursion
  (goto-char end)
  (let ((end-mark (point-marker))
	(cite-marks ""))
    (goto-char start)
    (beginning-of-line)
    (while (< (point) (marker-position end-mark))
      (end-of-line)
      (let ((line-end (point)))
	(beginning-of-line)
	(re-search-forward tc-guess-marks-regexp line-end t)
	(let ((marks-here (buffer-substring (match-beginning 0)
					    (match-end 0))))
	  (if (> (length marks-here) (length cite-marks))
	      (setq cite-marks marks-here))
	  (delete-region (match-beginning 0) (match-end 0))))
      (forward-line 1))
    (goto-char start)
    (beginning-of-line)
    (while (< (point) (marker-position end-mark))
      (insert cite-marks)
      (forward-line 1))
    (goto-char start)
    (save-restriction
      (narrow-to-region start (marker-position end-mark))
      (tc-fill-cited-paragraphs (length cite-marks))
      )
    ))
					;   )

(defun tc-unfill-paragraph-mouse (e)
  (interactive "e")
  (if (eventp e)
      ;; Why do you have to disagree?  Grr.  Arg.
      (if (featurep 'xemacs)
	  (tc-unfill-paragraph (event-point e))
	(tc-unfill-paragraph (posn-point (event-start e)))))
;;  (tc-unfill-paragraph (car (cdr (car (cdr e)))))
  )

(defun tc-unfill-paragraph (at)
  "Undo filling of cited text in the paragraph containing point.
Calling the function several times will toggle the paragrap between
the filled and the unfilled version.  tc-fill-cited-region may be able
to fill the paragraph better."
  (interactive "d")
  (let ((reformatted (get-char-property at 'tc-reformat)))
    (if reformatted
	(save-excursion
	  (let ((removed-region (buffer-substring
				 (overlay-start (cdr reformatted))
				 (overlay-end (cdr reformatted)))))
	    (goto-char (overlay-start (cdr reformatted)))
	    (insert (car reformatted))
	    (delete-region (point)
			   (overlay-end (cdr reformatted)))
	    (setcar reformatted removed-region)))
      (message "No formatted paragraph here."))))

;;; ************************************************************
;;; Guessing at the right set of citation marks
;;; ************************************************************
(defun tc-trim-best-prefix (prefix)
  "Remove from the prefix newlines, known marks and duplicates."
  (let ((known-marks (concat "\n " tc-normal-citemarks))
	(i 0))
    (while (< i (length prefix))
      (if (not (string-match (regexp-quote (substring prefix i (1+ i)))
			     known-marks))
	  (setq known-marks (concat known-marks (substring prefix i (1+ i)))))
      (setq i (1+ i)))
    (substring known-marks (length (concat "\n " tc-normal-citemarks)))))

(defun tc-guess-cite-marks ()
  (save-excursion
    (let ((best-prefix "\n")
	  guessed-marks
	  marks-begin marks-end)
      (while (search-forward best-prefix (mark t) t)
	(let ((prefix-start (match-beginning 0)))
	  (if (re-search-forward tc-guess-marks-regexp nil t)
	      (progn
		(setq marks-begin prefix-start)
		(setq marks-end (match-end 0))
		(setq best-prefix (buffer-substring marks-begin marks-end))
		(if (> tc-debug-level 0)
		    (message best-prefix))))))
      (setq guessed-marks (tc-trim-best-prefix best-prefix))
      (if (and (eq tc-guess-cite-marks 'ask)
	       (not (equal "" guessed-marks)))
	  (let ((marks-overlay (make-overlay (1+ marks-begin) marks-end)))
	    (overlay-put marks-overlay 'face 'highlight)
	    (setq guessed-marks
		  (read-from-minibuffer
		   "Guessed these citemarks: " guessed-marks))
	    (delete-overlay marks-overlay)))
      guessed-marks)))

(defun tc-citemarks-need-guessing ()
  (save-excursion
    (let ((max-line-len (- (tc-fill-column) (length tc-citation-string) 1))
	  needed)
      (beginning-of-line)
      (while (and (not needed)
		  (< (point) (mark t)))
	(end-of-line)
	(if (> (current-column) max-line-len)
	      (setq needed t))
	(forward-line 1))
      needed)
    )
  )

;;; ************************************************************
;;; More fancy attribution generation functions
;;; ************************************************************

;; Doesn't work yet.  *sniff*
(defun reply-to-citee-p (email)
  "Whether the mail being composed is for the person being cited."
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "^To:[ \t]+\\(.*\\)\n" nil t)
	(if (equal email (buffer-substring (match-beginning 1) (match-end 1)))
	    t
	  nil)
      nil)
    )
  )

(defun tc-fancy-attribution ()
  "tc-fancy-attribution makes a personalized attribution.
The Newsgroups: field is examined to find appropriate sayings, and the real
name is used when available.  If you use this, you probably want to change
`tc-groups-functions' to reflect your favorite newsgroups."
  (let ((date (cdr-safe (assoc "date" tc-strings-list)))
	(email (cdr-safe (assoc "email-addr" tc-strings-list)))
	(name (cdr-safe (assoc "real-name" tc-strings-list)))
	(groups (cdr-safe (assoc "newsgroups" tc-strings-list))))
    (let ((date-part (if date date "an unknown date"))
	  (name-part (if nil ;(reply-to-citee-p email)
			 "you"
		       (if name
			   name
			 (if email
			     email
			   "somebody@somewhere")))))
      (concat
       (if groups ;; This is sent to a newsgroup, not a person
	   (tc-group-attribution groups date-part name-part)
	 (let ((default-function (find-with-predicate
				  '(lambda (group) (null (car group)))
				  tc-groups-functions)))
	   (if default-function
	       (apply (cdr default-function) date-part name-part '())
	     (tc-generic-attribution date-part name-part)))) "\n\n"))))

;; General function to find an element satisfying pred in list
(defun find-with-predicate (pred list)
  (let ((found nil))
    (while (and list (not found))
      (if (apply pred (car list) ())
	  (setq found (car list))
	(setq list (cdr list))))
    found))

(defun tc-group-attribution (groups date name)
  (let ((group-function
	 (find-with-predicate
	  '(lambda (group)
	     (let ((group-name (car group)))
	       (if (or (not group-name) (= 0 (length group-name)))
		   t
		 (find-with-predicate
		  (if (= (char-syntax (string-to-char group-name)) ?w)
		      '(lambda (cited-group)
			 (if (>= tc-debug-level 1)
			     (message (concat "Checking " cited-group
					      " vs " group-name)))
			 (eq t (compare-strings
			       group-name 0 (length group-name)
			       cited-group 0 (length group-name))))
		    '(lambda (cited-group)
		       (if (>= tc-debug-level 1)
			   (message (concat "Regexp checking " cited-group
					    " vs " group-name)))
		       (string-match group-name cited-group)))
		     groups))))
	       tc-groups-functions)))
    (message name)
    (if group-function
	(apply (cdr group-function) date name ())
      (tc-generic-attribution date name))))

(defvar tc-groups-functions
    '( ( "" . tc-simple-attributor ) )
  "An alist of of functions to make an attribution in various groups.
The key of each assoc pair is a group prefix or regex for the groups
where this should be used.  Earlier group matches override later ones.
A key that starts with a letter is assumed to be a group prefix,
anything else is a regex.  To have a regexp start with a letter, start
it with the trivial range, e.g [c]omp.

Each function takes a date (day, date year) and a name (\"you\", name
or email address) and should return the attribution as a string
without newlines.  It will be reformatted.

Some example attribution functions (attributors) are:
`tc-simple-attributor', `tc-random-attributor',
`tc-rhod-group-attributor', `tc-java-group-attributor', and
`tc-local-group-attributor'.  "
)

;; This function due to Kai Großjohann
(defun tc-simple-attributor (date name)
  "A simple attribution function suitable as default for tc-groups-functions."
  (concat "On " date ", " name " wrote:"))

;;; ************************************************************
;;; These are more examples of how to make personalized attributions.
;;; If you use tc-fancy-attributions, you should change these to suit
;;; your style.
;;; ************************************************************

(defun tc-random-attributor (date name)
  "Generate a random attribution suitable for any context."
  (let ((style (random 7)))
    (cond ((= style 0)
	   (concat "On " date ", " name " stated:"))
	  ((= style 1)
	   (concat "On " date ", " name " spake thusly:"))
	  ((= style 2)
	   (concat "On " date ", " name " uttered the following:"))
	  ((= style 3)
	   (concat "On " date ", " name " outgrape:"))
	  ((= style 4)
	   (concat "On " date ", " name " said:"))
	  ((= style 5)
	   (concat "On " date ", " name " verbalised:"))
	  ((= style 6)
	   (concat "On " date ", " name " told this:"))
	  (t
	   (concat "On " date ", " name " wrote:")))
    ))

(defalias 'tc-generic-attribution 'tc-random-attributor)

(defun tc-rhod-group-attributor (date name)
  (let ((style (random 3)))
    (cond ((= style 0)
	   (concat "On " date ", " name
		   " thusly discussed the words of the Internet Oracle:"))
	  ((= style 1)
	   (concat "Paul Kelly can witness that " name " on " date " wrote:"))
	  ((= style 2)
	   (concat "On " date ", " name
		   " wrote, without the least grovelling:")))))

(defun tc-java-group-attributor (date name)
  (concat "On " date ", " name " used System.out.println with:"))

(defun tc-local-group-attributor (date name)
  (concat name " wrote this:"))

(provide 'tc)

;; Acknowledgements
;; Patches and bug reports have been sent by the following people.
;; My thanks to all of them for helping me improve trivial-cite
;; Klaus Berndl <Klaus.Berndl@sdm.de>
;; Colin Walters <walters+n@cis.ohio-state.edu>
;; Christoph Rohland <cr@sap.com>
;; Matthias Wiehl <mwiehl@gmx.de>
;; Kai Großjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Knut Anders Hatlen <kahatlen@online.no>
;; Tommi Vainikainen <thv@iki.fi>
;; Colin Rafferty <colin@xemacs.org>
;; Juergen Kreileder <jk@blackdown.de>
;; Daniel Pittman <daniel@rimspace.net>
;; Vasily Korytov <deskpot@myrealbox.com>
;; Benjamin Lewis <bclewis@cs.sfu.ca>

;; My apologies to any I may have forgotten

;;; Version history:
;;; 0.0:  Trivial version
;;; 0.2:  Added documentation, changed name from simple-quote to trivial-cite.
;;; 0.3:  Streamlined header parsing, updated documentation.  Now leaves
;;;         (point) at start as mail-citation-hook wants it.
;;; 0.4:  Parsing is now RFC822 compliant.  Removes empty lines at end of
;;;         citation.  Can remove signatures.
;;; 0.5:  Can now fill paragraphs when the lines are too long, with one undo
;;;         per filling.  Works like a charm:)
;;; 0.6:  Debugged filling, added overlays to mark undoable fillings.
;;;         Tries to find ^--$ sig marker if the correct ^-- $ fails.
;;;         Is not fooled by bad cites that have inserted extra spaces before
;;;         old cites.
;;; 0.7:  New version of find-cite-len, which should be faster and
;;;         can handle cite-marks not ending in a space.  Also fills single-
;;;         line quotes, if the quote-marks are simple.  Added user-setting
;;;         for mouse-sensitive overlays.  Fixed signature removal so that
;;;         an immediate undo gives the quoted signature.  Added guessing
;;;         at unusual cite-marks.  Added distribution setup handling and
;;;         and maintainer variable.  Made group-sensitive attributions
;;;         work better.  Reorganized a lot.  Improved string-char-index.
;;; 0.7.1:  Uses a new marker instead of push-mark now.
;;; 0.8:  Now possibility of limiting the number of lines cited (good for
;;;         citing extremely long mails).
;;; 0.8.1: Bug fix of prefix parsing.
;;; 0.9:  New user-function tc-cite-region that cites like tc, but without
;;;         parsing headers or removing signature.
;;; 0.10: Customization added.  My, this is neat.
;;; 0.10.1: Cut >80 char lines split.
;;; 0.10.2: Renamed tc-unformat-area[-mouse] to tc-unfill-paragraph[-mouse].
;;; 0.10.3: Bug fix from kahatlen@online.no
;;; 0.10.4: Added } as quote char.
;;; 0.11: Added tc-fill-cited-region-uniformly
;;; 0.11.1: Fixed bug with space-only header fields.
;;; 0.11.2-4: Minor contributed bugfixes
;;; 0.11.5: Trailing whitespace remove by suggestion from Brett Randall,
;;;         XEmacs compatability with help from Sebastian Kaps and others
;;; 0.11.6: Cite-mark fixup thanks to Matthias Wiehl
;;;         Changed to using C-c C-p for undo formatting.
;;;         Added fix to avoid the signature being messed up by mails with
;;;         no final newline.
;;; 0.12:   Numerous patches, including (mark t).
;;;         Also rework of fancy-attributes, renaming of subfunctions to
;;;         `attributor', fix of too long cite lines breaking fill.
;;; 0.12.1: Small fixes in tc-fancy-attributor after trying it out:)
;;; 0.12.2: Fixes of two compilation problems, thanks to Steve Evans
;;; 0.12.3: Updated license text
;;; 0.12.4: Typo fixed.  Made date parsing use date-to-time and
;;;         format-time-string, allowing easier customization.
;;; 0.13:   Stuff from Vasily Korytov <deskpot@myrealbox.com>:  Before
;;;         and after hooks, no spurious extra trailing spaces,
;;;         tc-fill-long-lines morphed into tc-fill-column.
;;; 0.13.1: Use guessed marks in normalizing citation marks.
;;; 0.13.2: Use a local let instead of setting fill-column.
;;; 0.13.3: Moving a parenthesis fixed a missing space problem.
;; end of tc.el
