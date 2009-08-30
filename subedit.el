;;; subedit.el --- edit part of a buffer in a new buffer

;; Copyright (C) 2007 Guanpeng Xu.

;; Maintainer: Guanpeng Xu <herberteuler@hotmail.com>
;; Keywords: subedit

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides the ability of editing part of the current
;; buffer in a new buffer whose mode is different from the current
;; buffer's mode.  The new buffer is called a "subedit buffer", and
;; the current buffer is called the subedit buffer's "original
;; buffer".  The "mode" here is either major mode or minor mode.
;;
;; This ability is different from the ability of having many major
;; modes in a buffer or many indirect buffers at the same time.
;; Subedit provides also the ability of stripping line prefixes and
;; suffixes, which the latter can't provide.

;; Todo:
;;
;;    - Add warnings when a buffer having subedit buffers that are
;;      changed is being killed
;;
;;    - If a subedit buffer itself is changed and is being killed,
;;      warn about that

;;; Code:

(defvar subedit-new-buffer-id 0
  "The ID of a new subedit buffer for the current buffer.

When a new subedit buffer is created, it is appended to its name
and then increased by one.  So it can be viewed as how many
subedit buffers have ever been created for the current buffer
too.")
(make-variable-buffer-local 'subedit-new-buffer-id)

(defvar subedit-prefix nil
  "The prefix of lines in the region the subedit buffer's text is copied from.")
(make-variable-buffer-local 'subedit-prefix)

(defvar subedit-suffix nil
  "The suffix of lines in the region the subedit buffer's text is copied from.")
(make-variable-buffer-local 'subedit-suffix)

(defvar subedit-original-buffer nil
  "The original buffer of the subedit buffer.")
(make-variable-buffer-local 'subedit-original-buffer)

(defvar subedit-region-beginning-marker nil
  "The marker at the beginning of the region the subedit buffer's text is copied from.")
(make-variable-buffer-local 'subedit-region-beginning-marker)

(defvar subedit-region-end-marker nil
  "The marker at end of the region the subedit buffer's text is copied from.")
(make-variable-buffer-local 'subedit-region-end-marker)

(defvar subedit-old-window-configuration nil
  "The window configuration before a new subedit buffer is created.")
(make-variable-buffer-local 'subedit-old-window-configuration)

(defvar subedit-prefix-history nil
  "History of line prefixes entered with `subedit-edit-region'.")

(defvar subedit-suffix-history nil
  "History of line suffixes entered with `subedit-edit-region'.")

(defvar subedit-mode-history nil
  "History of modes entered with `subedit-edit-region'.")


;; A combination of point markers at the beginning and end of a
;; region.  This combination is called a "subedit region".

(defstruct (subedit-region
	    (:type list)
	    (:copier nil)
	    (:constructor nil)
	    (:constructor subedit-make-region (beginning end)))
  beginning end)

(defvar subedit-buffer-region-alist nil
  "An alist of subedit buffers and their corresponding regions.
Each element looks like (SUBEDIT-BUFFER . SUBEDIT-REGION).

SUBEDIT-REGION is a region whose text has already been copied to
SUBEDIT-BUFFER.

The beginnings of the regions refered in this alist are in an
ascending order.")
(make-variable-buffer-local 'subedit-buffer-region-alist)


;; Subedit errors.

(let ((subedit-error-alist
       '((subedit-different-major-modes
	  . "Attempt to edit the region in many different major modes")
	 (subedit-different-prefixes
	  . "Line prefix is not equal to the one near the region")
	 (subedit-different-suffixes
	  . "Line suffix is not equal to the one near the region")
	 (subedit-impossible-case
	  . "Something impossible happened, please report bug"))))
  (dolist (entry subedit-error-alist)
    (let ((symbol (car entry))
	  (message (cdr entry)))
      (put symbol 'error-conditions (list 'error 'subedit-error symbol))
      (put symbol 'error-message message))))

 

;;;###autoload
(defun subedit-edit-region (beg end prefix suffix mode)
  "Copy text between point and mark to a subedit buffer whose mode is MODE.

The copied lines in the region may all start with PREFIX and/or
end with SUFFIX; in that case, the prefix and the suffix will be
stripped after copying.  PREFIX and SUFFIX may contain preceding
and/or trailing whitespace characters; if a line in the region
does not start with PREFIX and/or end with SUFFIX because of the
whitespace characters, it will be still treated as starting with
PREFIX and/or ending with SUFFIX, and the prefix and/or
suffix (the ones after removing the whitespace characters) will
be stripped as well.

Empty lines are always treated as starting with any prefixes and
ending with any suffixes.

The subedit buffer is for editing text in the region in
MODE (either major mode or minor mode).  Later, changes in the
subedit buffer can be copied back to the region with
`subedit-commit', and the stripped prefix and suffix (if exist)
will be added back."
  (interactive
   (let ((prefix
	  (read-string (if subedit-prefix-history
			   (format "Line prefix (default \"%s\"): "
				   (car subedit-prefix-history))
			 "Line prefix: ")
		       nil
		       'subedit-prefix-history
		       (if subedit-prefix-history
			   (car subedit-prefix-history))))
	 (suffix
	  (read-string (if subedit-suffix-history
			   (format "Line suffix (default \"%s\"): "
				   (car subedit-suffix-history))
			 "Line suffix: ")
		       nil
		       'subedit-suffix-history
		       (if subedit-suffix-history
			   (car subedit-suffix-history))))
	 (mode
	  (intern
	   (completing-read (if subedit-mode-history
				(format "Edit using mode (default \"%s\"): "
					(car subedit-mode-history))
			      "Edit using mode: ")
			    obarray
			    #'(lambda (symbol)
				(and (commandp symbol t)
				     (string-match "-mode$"
						   (symbol-name symbol))))
			    t nil
			    'subedit-mode-history
			    (if subedit-mode-history
				(car subedit-mode-history))))))
     (list (region-beginning) (region-end) prefix suffix mode)))
  ;; The translating process in `subedit-translate-region' will modify
  ;; `subedit-buffer-region-alist' as side effect, so the job after
  ;; that should be done without any errors.  For this purpose, insure
  ;; that lines in the region all start with PREFIX and end with
  ;; SUFFIX before translating (otherwise it is an error).
  (unless (subedit-strip-region-affixes beg end prefix suffix 'check-only)
    (error (format "Lines in region do not all start with `%s' and/or end with `%s'"
		   prefix suffix)))
  (let* ((list (subedit-translate-region (subedit-make-region
					  (save-excursion (goto-char beg)
							  (point-marker))
					  (save-excursion (goto-char end)
							  (point-marker)))
					 prefix
					 suffix
					 mode))
	 (dest-buffer (car list))
	 (text-list (cdr list))
	 user-edited)
    (unless (null (car text-list))
      (setq user-edited
	    (subedit-copy-text dest-buffer (car text-list) 'prepend)))
    (dolist (text (cdr text-list))
      (unless user-edited
	(setq user-edited (subedit-copy-text dest-buffer text 'append))))
    (with-current-buffer dest-buffer
      (goto-char (point-min))
      (delete-trailing-whitespace)
      (unless user-edited
	(set-buffer-modified-p nil)))
    ;; Arrange to show the new buffer friendly.
    (setq subedit-old-window-configuration (current-window-configuration))
    (split-window)
    (other-buffer)
    (switch-to-buffer dest-buffer)))

;;;###autoload
(defun subedit-commit ()
  "Copy changes to the subedit buffer back to the region the text is copied from.
This will remove the subedit buffer from its original buffer's
`subedit-buffer-region-alist' and kill it too."
  (interactive)
  (unless subedit-original-buffer
    (error "Current buffer has no original buffer"))
  (let ((original-buffer subedit-original-buffer))
    (when (buffer-modified-p)
      ;; Copy the changes back to the region its text is copied from.
      (let ((beg subedit-region-beginning-marker)
	    (end subedit-region-end-marker)
	    (prefix subedit-prefix)
	    (suffix subedit-suffix)
	    (new-text (buffer-substring-no-properties (point-min)
						      (point-max))))
	(with-current-buffer original-buffer
	  ;; Replace the old text.
	  (delete-region beg end)
	  (goto-char beg)
	  (insert new-text)
	  ;; Add the stripped prefix and suffix back, if any.
	  (when (or prefix suffix)
	    (save-restriction
	      (narrow-to-region beg (point))
	      (goto-char (point-min))
	      (while (progn (re-search-forward "^")
			    (replace-match prefix)
			    (re-search-forward "$")
			    (replace-match suffix)
			    (= (forward-line 1) 0)))
	      (delete-trailing-whitespace))))))
    (subedit-kill-subedit-buffer (current-buffer))
    ;; Recover the old window-configuration.
    (with-current-buffer original-buffer
      (set-window-configuration subedit-old-window-configuration)
      (setq subedit-old-window-configuration (current-window-configuration)))))

 

;; Each of the current buffer's subedit buffers corresponds to exactly
;; one region in the current buffer: from which the subedit buffer's
;; text is copied.  Two regions that correspond to any two different
;; subedit buffers must not adjoint or overlap each other.  This is
;; the restriction on the regions that correspond to subedit buffers.
;; Besides, this buffer-to-region correspondence is recorded in
;; `subedit-buffer-region-alist'.
;;
;; When the user marks a new region and tries to edit it with the
;; command `subedit-edit-region', the new region plus the regions in
;; `subedit-buffer-region-alist' may still satisfy the restriction
;; described above, yet they may not.  If they do satisfy, the right
;; thing to do is just creating a new subedit buffer and then copying
;; the new region's text into it.
;;
;; If they do not satisfy the restriction any more, things are
;; complicated.  In this case, there is at least one region in
;; `subedit-buffer-region-alist', plus which the new region does not
;; satisfy the restriction.  Here, the subedit buffer that corresponds
;; to the first one of such regions in `subedit-buffer-region-alist'
;; (remember the beginnings of the regions are in an ascending order)
;; will be chosen as the subedit buffer into which the text is to be
;; copied.
;;
;; And deciding which text to be copied is more complicated in this
;; case.  The new region's text can be essentially devided into two
;; classes: A) the text that is not ever copied into any subedit
;; buffers, and B) the text that has already been copied into some
;; other subedit buffers.  The text in class A needs to be copied from
;; the original buffer, while the text in class B needs to be copied
;; from the subedit buffers, since it may be changed in the subedit
;; buffers.
;;
;; To simplify the process of creating or finding a subedit buffer and
;; copying the text, we translate from the new region's cons cell
;; representation (BEG . END) into a list which contains the subedit
;; buffer the copied text should go into, and the individual regions
;; of text to be copied.  The first element of the list is a subedit
;; buffer, the "destination subedit buffer", into which the text is
;; going to be copied.  The other elements tells which text to be
;; copied.  The second element tells which text to be prepended to the
;; destination subedit buffer.  It can be a subedit-region (means the
;; region's text in the original buffer is to be prepended), or nil
;; (means nothing needs to be prepended).  The following elements, if
;; exist, tell which text to be appended to the destination subedit
;; buffer.  They can be a subedit-region (means the region's in the
;; original buffer is to be appended), or another subedit buffer
;; (means the subedit buffer's text is to be appended).  They are in
;; the appending order.  This translation is done by checking
;; sequentially each regions in `subedit-buffer-region-alist' that may
;; cause the dissatisfaction plus the new region.
;;
;; All but the first one of the subedit buffers in the translated list
;; will not be needed after their text will be copied into the
;; destination subedit buffer.  These subedit buffers will be removed
;; from `subedit-buffer-region-alist' then.  This process is called
;; the merging of subedit buffers, and the removed subedit buffers are
;; called the merged subedit buffers.
;;
;; When translating region's representation, we check the major modes
;; of all subedit buffers to be merged.  All of them must be the same
;; as the destination subedit buffer's major mode, or it makes no
;; sense to merge these buffers.  We also check the prefix and suffix
;; stored in the subedit buffers; if they are not the same, merging
;; cannot happen as well.  Note that affixes have already been checked
;; (in the current buffer) once before translating; here they are
;; rechecked (in the subedit buffers) to be sure.

(defun subedit-translate-region (region prefix suffix mode)
  "Translate cons cell represented region REGION into a list and return it.

PREFIX and SUFFIX are the line prefix and suffix respectively,
and MODE is a major mode or a minor mode.  A destination subedit
buffer will be a newly created one or an existing one in
`subedit-buffer-region-alist'.  When a new subedit buffer is
created, it will be set up properly and inserted into
`subedit-buffer-region-alist' (along with its corresponding
region).  Otherwise, the beginning and end of the region that the
destination buffer corresponds to in the alist will be updated.
`subedit-region-beginning-marker' and `subedit-region-end-marker'
in the destination subedit buffer are updated."
  (let (prev
	(cur subedit-buffer-region-alist)
	saved-cur
	use-existing-buffer
	dest-buffer
	dest-prefix
	dest-suffix
	dest-mode
	dest-beginning
	dest-end
	regions
	remaining
	checking-dest-buffer
	text-list)
    ;; Find the destination subedit buffer in
    ;; `subedit-buffer-region-alist', or a position in the alist for a
    ;; new destination subedit buffer.
    (while (and cur
		(progn
		  ;; Choose the subedit buffer that corresponds to the
		  ;; first adjacent or overlapping region as the
		  ;; destination subedit buffer.
		  (if (subedit-regions-adjoint-or-overlap-p region (cdar cur))
		      (setq use-existing-buffer t))
		  (not use-existing-buffer))
		(< (subedit-region-beginning (cdar cur))
		   (subedit-region-beginning region)))
      (setq prev cur
	    cur (cdr cur)))
    ;; Now PREV is the last entry in `subedit-buffer-region-alist'
    ;; that is before the entry of the destination subedit buffer, and
    ;; the destination subedit buffer either is in the entry CUR, or
    ;; should be inserted at CUR's current position.
    (setq saved-cur cur
	  dest-buffer (if use-existing-buffer
			  (with-current-buffer (caar saved-cur)
			    (unless (or (not (subedit-major-mode-p mode))
					(eq mode major-mode))
			      (signal 'subedit-different-major-modes
				      major-mode))
			    (unless (string= prefix subedit-prefix)
			      (signal 'subedit-different-prefixes
				      subedit-prefix))
			    (unless (string= suffix subedit-suffix)
			      (signal 'subedit-different-suffixes
				      subedit-suffix))
			    (current-buffer))
			(let ((original-buffer (current-buffer)))
			  (with-current-buffer (subedit-get-new-buffer)
			    (command-execute mode)
			    (setq subedit-prefix prefix
				  subedit-suffix suffix
				  subedit-original-buffer original-buffer)
			    (current-buffer)))))
    ;; Record some variables for the checking at later.
    (with-current-buffer dest-buffer
      (setq dest-prefix subedit-prefix
	    dest-suffix subedit-suffix
	    dest-mode major-mode))
    (if (null cur)
	(setq text-list (cons region text-list)
	      dest-beginning (subedit-region-beginning region)
	      dest-end (subedit-region-end region))
      ;; Find the text to be prepended.
      (if use-existing-buffer
	  (setq checking-dest-buffer t))
      (setq regions (subedit-regions-correlation region (cdar cur))
	    text-list (cons (cond ((nth 0 regions))
				  (t
				   (unless use-existing-buffer
				     (kill-buffer dest-buffer)
				     (signal 'subedit-impossible-case nil))
				   ;; Nothing to be prepended.
				   nil))
			    text-list)
	    dest-beginning (subedit-region-beginning (cond ((nth 0 regions))
							   (t (cdar cur)))))
      ;; Find the text to be appended.
      (while (progn
	       (setq text-list (cond
				((and (not checking-dest-buffer)
				      (nth 1 regions))
				 (with-current-buffer (caar cur)
				   (unless (eq dest-mode major-mode)
				     (unless use-existing-buffer
				       (kill-buffer dest-buffer))
				     (signal 'subedit-different-major-modes
					     major-mode))
				   (unless (string= dest-prefix subedit-prefix)
				     (unless use-existing-buffer
				       (kill-buffer dest-buffer))
				     (signal 'subedit-different-prefixes
					     subedit-prefix))
				   (unless (string= dest-suffix subedit-suffix)
				     (unless use-existing-buffer
				       (kill-buffer dest-buffer))
				     (signal 'subedit-different-suffixes
					     subedit-suffix)))
				 (cons (caar cur) text-list))
				(t text-list))
		     checking-dest-buffer nil
		     remaining (nth 2 regions)
		     cur (cdr cur))
	       (and remaining cur))
	(setq regions (subedit-regions-correlation remaining (cdar cur))
	      ;; There must be some text at 0th of REGIONS.
	      text-list (cons (nth 0 regions) text-list)))
      (setq text-list (cond (remaining
			     (cons remaining text-list))
			    (t text-list))
	    dest-end (subedit-region-end
		      (cond (remaining)
			    ((nth 1 regions)
			     ;; REGION adjoints or overlaps a region
			     ;; in `subedit-buffer-region-alist',
			     ;; which is still saved in 1st of
			     ;; REGIONS.
			     )
			    (t
			     ;; Nothing remained means the end of
			     ;; REGION is not inside in any region in
			     ;; `subedit-buffer-region-alist', so use
			     ;; it as DEST-END.
			     region)))))
    ;; Update the beginning and end of the region in DEST-BUFFER.
    (with-current-buffer dest-buffer
      (setq subedit-region-beginning-marker dest-beginning
	    subedit-region-end-marker dest-end))
    ;; If a new subedit buffer is created, insert it and its
    ;; corresponding region into `subedit-buffer-region-alist'.
    ;; Otherwise, update the beginning and end of the region that
    ;; DEST-BUFFER corresponds to in the alist.  The subedit buffers
    ;; to be merged are not removed now; they are removed later after
    ;; their text is copied.
    (if use-existing-buffer
	(setcdr (car saved-cur) (subedit-make-region dest-beginning
						     dest-end))
      (let ((new-entry (cons dest-buffer
			     (subedit-make-region dest-beginning
						  dest-end))))
	(if prev
	    (setcdr prev (cons new-entry (cdr prev)))
	  (setq subedit-buffer-region-alist
		(cons new-entry subedit-buffer-region-alist)))))
    ;; Construct the list to return.
    (cons dest-buffer (nreverse text-list))))



(defun subedit-regions-adjoint-or-overlap-p (region1 region2)
  "Return t when REGION1 and REGION2 adjoint or overlap each other, otherwise return nil."
  (let ((regions (subedit-regions-correlation region1 region2)))
    (if (nth 1 regions)
	t)))




(defun subedit-get-new-buffer ()
  "Get a new subedit buffer for the current buffer.

`subedit-new-buffer-id' is increased by one after getting the new
buffer."
  (let ((id (prog1 subedit-new-buffer-id
	      (setq subedit-new-buffer-id (1+ subedit-new-buffer-id)))))
    (get-buffer-create (format "%s-subedit-%d"
			       (buffer-name)
			       id))))



(defun subedit-regions-correlation (region1 region2)
  "Compute the correlation of REGION1 and REGION2 and return it.

The correlation of REGION1 and REGION2 is a list of three
regions: (BEFORE COMMON AFTER).

Both BEFORE and AFTER are sub-regions of REGION1, or nil.  When
non-nil, the end of BEFORE is the largest point position among
the positions in REGION1 that is less than or equal to the
beginning of REGION2, and the beginning of AFTER is the smallest
point position among the positions in REGION1 that is equal to or
greater than the end of REGION2.

When REGION1 does not adjoint or overlap REGION2, COMMON is nil.
When REGION1 adjoints or overlaps REGION2, COMMON is REGION2."
  (let ((before (if (<= (subedit-region-beginning region2)
			(subedit-region-beginning region1))
		    nil
		  (subedit-make-region
		   (subedit-region-beginning region1)
		   (min (subedit-region-end region1)
			(subedit-region-beginning region2)))))
	(common (cond ((or (< (subedit-region-end region1)
			      (subedit-region-beginning region2))
			   (< (subedit-region-end region2)
			      (subedit-region-beginning region1)))
		       nil)
		      (t
		       region2)))
	(after (if (<= (subedit-region-end region1)
		       (subedit-region-end region2))
		   nil
		 (subedit-make-region
		  (max (subedit-region-beginning region1)
		       (subedit-region-end region2))
		  (subedit-region-end region1)))))
    (list before common after)))



(defun subedit-major-mode-p (mode)
  "Return t when MODE is a major mode, otherwise return nil."
  (with-temp-buffer
    (command-execute mode)
    (if (eq major-mode mode)
	t)))

 

(defun subedit-strip-region-affixes (beg end prefix suffix &optional check-only)
  "Strip PREFIX and SUFFIX on lines in the region between BEG and END.
If PREFIX and/or SUFFIX are non-nil, lines in the region must all
start with PREFIX and/or end with SUFFIX.

PREFIX and SUFFIX may contain preceding and/or trailing
whitespace characters; if a line in the region does not start
with PREFIX and/or end with SUFFIX because of some of the
whitespace characters, it will still be treated as starting with
PREFIX and/or ending with SUFFIX, and the prefix and/or
suffix (the ones after removing the whitespace characters) will
be stripped as well.

Empty lines are always treated as starting with any prefixes and
ending with any suffixes.

The optional sixth argument, CHECK-ONLY, if non-nil, means do not
actually perform the stripping, but check whether there are
matches against PREFIX and/or SUFFIX on all the lines in the
region only.

The return value of this function means whether lines in the
region all start with PREFIX and/or end with SUFFIX."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let* ((consistent t)
	     (prefix-1 (if prefix
			   (concat "^" (regexp-quote prefix))
			 "^"))
	     (prefix-2 (replace-regexp-in-string "\\s-+$" "" prefix-1))
	     (prefix-3 (replace-regexp-in-string "^\\^\\s-+" "^" prefix-2))
	     (suffix-1 (if suffix
			   (concat (regexp-quote suffix) "$")
			 "$"))
	     (suffix-2 (replace-regexp-in-string "\\s-+\\$$" "$" suffix-1))
	     (suffix-3 (replace-regexp-in-string "^\\s-+" "" suffix-2)))
	(while (and consistent
		    (not (eobp)))
	  (unless (= (line-beginning-position) (line-end-position))
	    (when prefix
	      (if (or (re-search-forward prefix-1 (line-end-position) t)
		      (re-search-forward prefix-2 (line-end-position) t)
		      (re-search-forward prefix-3 (line-end-position) t))
		  (unless check-only
		    (replace-match "" nil nil))
		(setq consistent nil)))
	    (when suffix
	      (if (or (re-search-forward suffix-1 (line-end-position) t)
		      (re-search-forward suffix-2 (line-end-position) t)
		      (re-search-forward suffix-3 (line-end-position) t))
		  (unless check-only
		    (replace-match "" nil nil))
		(setq consistent nil))))
	  (forward-line 1))
	consistent))))


(defun subedit-copy-text (buffer source style)
  "Copy text from SOURCE into subedit buffer BUFFER.

SOURCE may be a subedit region or a subedit buffer.  If SOURCE is
a subedit region, then the text in BUFFER's original buffer
between the beginning and end of the subedit region will be
copied into BUFFER.  If SOURCE is a subedit buffer, then the
whole text in the subedit buffer will be copied into BUFFER.

If SOURCE is a subedit region, the prefix and suffix of the
copied text (as defined in BUFFER's `subedit-prefix' and
`subedit-suffix' variables) will be stripped.

If SOURCE is a subedit buffer, it will removed from BUFFER's
original buffer's `subedit-buffer-region-alist' and then killed
after its text will be copied.

The third argument STYLE tells how the text will be copied into
BUFFER, it is either `prepend' or `append'.  If STYLE is
`prepend', text will be prepended into BUFFER.  If STYLE is
`append', text will be appended into BUFFER.

If SOURCE is a subedit buffer, the return value of this function
is whether it has been modified.  Otherwise, the return value of
this function is nil."
  (let* ((original-buffer (with-current-buffer buffer
			    subedit-original-buffer))
	 (prefix (with-current-buffer buffer
		   subedit-prefix))
	 (suffix (with-current-buffer buffer
		   subedit-suffix))
	 (text (if (bufferp source)
		   (with-current-buffer source
		     (widen)
		     (buffer-substring-no-properties (point-min)
						     (point-max)))
		 (with-current-buffer original-buffer
		   (buffer-substring-no-properties
		    (subedit-region-beginning source)
		    (subedit-region-end source)))))
	 (modified (if (bufferp source)
		       (buffer-modified-p source)))
	 stub)
    (with-current-buffer buffer
      (widen)
      (goto-char (cond ((eq style 'prepend)
			(point-min))
		       ((eq style 'append)
			(point-max))))
      (setq stub (point-marker))
      (insert text)
      (unless (bufferp source)
	(subedit-strip-region-affixes (min (point) stub)
				      (max (point) stub)
				      prefix
				      suffix)))
    (when (bufferp source)
      (subedit-kill-subedit-buffer source))
    modified))


(defun subedit-kill-subedit-buffer (buffer)
  (with-current-buffer buffer
    (with-current-buffer subedit-original-buffer
      (setq subedit-buffer-region-alist
	    (assq-delete-all buffer subedit-buffer-region-alist))))
  (kill-buffer buffer))


(provide 'subedit)

;;; subedit.el ends here
