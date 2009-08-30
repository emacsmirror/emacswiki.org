;;; linkify.el --- Create clickable links to lines in files.
;; Copyring (C) 2008 Tom Wright

;;Author: Tom Wright <tom.wright@tat.wright.name>
;; If you've tried out this package - I'd quite like to hear what you
think - feel
;; free to email me.

;; linkify.el is free software. You can distribute under the MIT license.
;; This is available here: http://www.opensource.org/licenses/mit-license.php

;;; Commentary

;; linkify.el provides a set of functions for semi-automatically creating links to
;; lines in files based on the text entered into a buffer, or received from
;; a process. Its original intended use was to add links to exceptions
;; in test output, thereby allowing one to avoid parsing filenames and 
;; line numbers by hand.

;; The three following variables and functions may be of interest:

;; linkify-regexps - This variable is a BUFFER LOCAL list of regular expressions which
;; should be linkified each regular expression MUST contain two
;; capturing groups ;; (contained within \( and \) delimiters. The first 
;; capturing group always indicates the file to be opened, 
;; the second the line number.
;; If the line number capturing group contains a string rather than a number
;; the link will jump to the first occurence this string in the given file.

;; linkify-buffer - This reads a buffer, adding clickable links to
;; lines which match one of the regexps in linkify-regexps.

;; linkify-filter - This is a process filter. If attached to a process with
;; set-process-filter then this will add clickable linkts to lines which match
;; one of the regexps in linkify-regexps. (See start-process)

;; linkify.el has only been tested on emacs22.

;;; Installation:
;; ensure that linkify.el is in your emacs lisp path (see the load-path variable)
;; add (require 'linkify) to your .emacs file.

;;; Use:

;; To linkfy the output from a process:
;; After creating the process (start-process) set linkify-regexps in the processes output buffer and
;; call (set-process-filter proc 'linkify-filter) - where proc is the value returned by start-process or similar.


;; To linkify a buffer
;; Set linkify-regexps appropriately - you might wants to add something to a mode hook to do this.
;; Run linkify-buffer

;;;Caveats

;; Links are always created to a file and line number, never just one.

;; Text to be linkified must contain both the file and line number at
some position

;; The line number text must always come after the file name text.

;; Link text cannot span multiple lines.

;; The entire line is turned in a link - one line cannot contain several links

;; If buffer text is linkified (rather than text from a process) the
entire buffer
;; must be linkified.

;;; Code:


(make-variable-buffer-local 'linkify-regexps)
(make-variable-buffer-local 'linkify-links)


(defun linkify-multi-string-match (regexps text)
  "Return true if one of the regexps in a list matches in text"
  (cond
   ((null regexps) nil)
   ((string-match (car regexps) text) t)
   (t (linkify-multi-string-match (cdr regexps) text))))


(defun linkify-get-filename (text)
  "Get the filename from a link"
  (if (linkify-multi-string-match linkify-regexps text)
      (match-string 1 text)
    nil))


(defun linkify-get-line-or-search (text)
  ; !!! EXTREME CARE !!!! this function must be performed in the
correct buffer -
  ; terribly fragile I'm sure.
  "Get the line number for a link"
  (if (linkify-multi-string-match linkify-regexps text)
      (match-string 2 text)
    nil))


(defun linkify-test-string (text)
  "Test if a string can be linkified"
  (linkify-multi-string-match linkify-regexps text))


(defun linkify-follow (button)
  "Follow a link."
  (let ((button-text (buffer-substring
		      (button-start button) (button-end button)))
	filename
	line-number-or-search
	(line-number nil)
	(search nil)
	(click-buffer (overlay-buffer button)))
    (select-window (get-buffer-window  click-buffer) t)
    (setq filename (linkify-get-filename button-text))
    (setq line-or-search (linkify-get-line-or-search button-text))
    (if (string-match "^ *[0-9]+ *$" line-or-search)
	(setq line-number (string-to-number line-or-search))
      (setq search line-or-search))
    (find-file-other-window filename)

    (if line-number
	(progn
	  (goto-line line-number)))
    (if search
	(progn
	  (goto-char 0)
	  (search-forward search)))))




(defun linkify-make-button (start end)
  (make-button
   start
   end
   'action 'linkify-follow
   'follow-link t))


(defun linkify-filter (proc string)
  "Filter for a stream. For each line check to see if a link can be created. If
so create a button in the output buffer linking to this file"
  (with-current-buffer (process-buffer proc)
    (let ((was-read-only buffer-read-only))
      (toggle-read-only -1)
      (let ((moving (= (point) (process-mark proc)))
	    beg)
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (dolist (line (split-string string "[\n\r]+"))
	    (if (linkify-test-string line)
		(linkify-make-button
		 (point)
		 (progn
		   (insert line)
		   (point)))
	      (insert line))
	    (unless (string= line (car (last lines))) (newline))))
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc))))
      (setq buffer-read-only was-read-only))))


(defun linkify-apply-to-lines (func)
  "Apply a function to all lines contained in the current buffer."
  (let ((start 0) (end (buffer-size)))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (<= (point) end)
	(apply func ())
	(next-line)
	(beginning-of-line)))))


(defun linkify-line ()
  (interactive)
  "Add links to the current line"
  (let ((bol (progn (beginning-of-line) (point)))
	(eol (progn (end-of-line) (point)))
	line)
    (setq line (buffer-substring bol eol))
    (if (linkify-test-string line)
	(progn (linkify-make-button bol eol)))))


(defun linkify-buffer (buffer)
  "Linkify the current buffer."
  (interactive (list (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (linkify-apply-to-lines 'linkify-line)))


(provide 'linkify)

;;; linkify.el ends here
