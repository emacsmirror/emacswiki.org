;;; mthesaur.el --- Thesaurus look-up of a word or phrase.

;; $Id: mthesaur.el,v 1.4 2003/07/14 17:37:45 taashlo Exp $
;; Author: Tad Ashlock <taashlo@cyberdude.com>

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code was inspired by thesaurus.el by Ray Nickson.

;; This code depends on the following file:
;;
;; The Project Gutenberg Etext of Moby Thesaurus II by Grady Ward
;; Filename: mthesaur.txt
;; URL: ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip

;;; Availability:

;; This code is available at <http://www.emacswiki.org/elisp/mthesaur.el>


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package provides a way to perform a thesaurus search on a word or
;; phrase using the awesome Project Gutenberg Etext of Moby Thesaurus II
;; by Grady Ward.
;;
;; To use mthesaur, insert in your ~/.emacs:
;;
;;    (require 'mthesaur)
;;
;; Or:
;;
;;    (autoload 'mthesaur-search "mthesaur"
;;      "Thesaurus lookup of a word or phrase." t)
;;    (autoload 'mthesaur-search-append "mthesaur"
;;      "Thesaurus lookup of a word or phrase, append results." t)
;;
;; Optionally followed by assigning key sequences to the mthesaur
;; functions:
;;
;;    (global-set-key "\C-ct" 'mthesaur-search)
;;    (global-set-key "\C-c\C-t" 'mthesaur-search-append)
;;
;; In the detailed instructions below, I assume that you have assigned
;; mthesaur-search to `C-c t' and mthesaur-search-append to 'C-c C-t'.
;;
;; Be sure to modify the mthesaur-file variable to point to where you've
;; put your copy of the mthesaur.txt file.
;;
;; For good performance, be sure to byte-compile mthesaur.el, e.g.
;;
;;    M-x byte-compile-file <give the path to mthesaur.el when prompted>
;;
;; Or open the mthesaur.el file and run "Byte-Compile This File" from the
;; "Emacs-Lisp" menu.  Either way, this will generate mthesaur.elc, which
;; will be loaded instead of mthesaur.el.
;;
;; mthesaur.el was tested with GNU Emacs 21.3.1.
;;
;;
;; Using mthesaur
;; --------------
;;
;; To lookup the word at the cursor, or to lookup the phrase in the
;; marked region, type:
;;
;;    C-c t
;;
;; To be prompted to enter a word or phrase to be looked up, type:
;;
;;    C-u C-c t
;;
;; The normal history manipulation commands (M-p, M-n, etc.) work as
;; expected at the minibuffer prompt.  Even the words/phrases that were
;; added via `C-c t' will be in the history list.
;;
;; The 'C-c C-t' key sequence behaves identically to 'C-c t' except the
;; search results are appended to the end of the previous search results
;; instead of overwriting them.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'thingatpt)
                   (require 'cl))

(defvar mthesaur-file
  (expand-file-name (substitute-in-file-name
                     "D:/Pub/site-lisp/mthesaur.txt"))
  "*Location of the thesaurus file (mthesaur.txt).")

(defvar mthesaur-history nil
  "Thesaurus minibuffer history list.")

(defun mthesaur-format-one-entry ()
  "Apply thesaurus search result formatting to a single entry.
This formatting consists of separating the root word/phrase onto a line
of its own and then highlighting it."
  (let ((start) (end))
    ;; Find the root word/phrase (the first item of the line).
    (setq start (point))
    (search-forward "," nil t)
    (setq end (match-beginning 0))

    ;; Put the root word/phrase on a line of its own.
    (replace-match "\n\n" nil t)

    ;; Highlight the root word/phrase.
    (add-text-properties start end '(face secondary-selection))

    (forward-line)))

(defun mthesaur-format-multiple-entries (line-count)
  "Apply thesaurus search result formatting to multiple entries.
For each entry, prepend the entry number (\"\#n: \") and then call
`mthesaur-format-one-entry' to finish the individual entry's formatting."
  (insert (format "Matched Entries Found: %s\n" line-count))
  (do ((line-num 0 (+ line-num 1)))
      ((>= line-num line-count))
    (insert (format "\n\#%s: " (1+ line-num)))
    (mthesaur-format-one-entry)))

(defun mthesaur-format-wrapup (search-text)
  "Apply final formatting to the partially formatted search results."
  ;; Put a space after each comma.
  (goto-char (point-min))
  (while (search-forward "," nil t)
    (replace-match ", " nil t))

  ;; Highlight the matched word/phrase.  This could be the root (with
  ;; an optional leading "#d: "), or it could be a match within the list
  ;; of synonyms and related words.  These two cases are implemented with
  ;; the following two separate searches.
  (goto-char (point-min))
  (while (re-search-forward
          (format "^\\(#.*: \\)?\\(%s\\)$" search-text) nil t)
    (add-text-properties (match-beginning 2) (match-end 2)
                         '(face highlight)))
  (goto-char (point-min))
  (while (re-search-forward
          (format "\\(^\\|, \\)\\(%s\\)\\($\\|,\\)" search-text) nil t)
    (add-text-properties (match-beginning 2) (match-end 2)
                         '(face highlight)))

  ;; Word wrap
  (setq fill-column (1- (window-width)))
  (fill-region (point-min) (point-max))
  )

(defun mthesaur-display-results (append-p temp-buf results-buf)
  "Display the results of the thesaurus search.
If APPEND-P is non-nil and RESULTS-BUF is not empty, then insert a
separator line between the previous search results and the current search
results."
  (set-buffer results-buf)
  (if (not (= (point-min) (point-max)))
      (if append-p
          (progn
            (goto-char (point-max))
            (insert "\n" (make-string (1- (window-width)) ?-) "\n\n"))
          (erase-buffer)))
  (let ((top-of-results (point)))
    (insert-buffer temp-buf)
    (select-window (display-buffer results-buf))

    ;; Put the start of the (latest) results at the top of the window.
    (goto-char top-of-results)
    (recenter 0)

    ;; Put the cursor at the end of the line after the first word/phrase.
    (end-of-line)
    ))

(defun mthesaur-first-attempt (buf search-text)
  "Search the thesaurus's root words for SEARCH-TEXT.
Put the raw search results in BUF."
  (call-process "grep" nil buf nil
                "-i" (format "^%s," search-text) mthesaur-file)
  (if (zerop (count-lines (point-min) (point-max)))
      nil
      'first))

(defun mthesaur-second-attempt (buf search-text)
  "Search the thesaurus's synonyms and related words for SEARCH-TEXT.
Put the raw search results in BUF."
  (call-process "grep" nil buf nil
                "-i" (format ",%s\\($\\|,\\)" search-text) mthesaur-file)
  (if (zerop (count-lines (point-min) (point-max)))
      nil
      'second))

(defun mthesaur-format-results (search-text)
  "Format the results of the thesaurus search.
Multiple entry results have a little extra formatting to help visually
differentiate the individual entries."
  (goto-char (point-min))
  (let ((line-count (count-lines (point-min) (point-max))))
    (if (= line-count 1)
        (mthesaur-format-one-entry)
        (mthesaur-format-multiple-entries line-count))
    (mthesaur-format-wrapup search-text)))

(defun mthesaur-lookup (search-text append-p)
  "Manage the process of searching the thesaurus for SEARCH-TEXT."
  (if (or (null search-text) (string= search-text ""))
      (progn (ding) (message "mthesaur -- what word/phrase?"))
      (save-excursion
        (save-selected-window
          (with-temp-message
              (format "Searching Thesaurus for \"%s\", please wait ..."
                      search-text)
            (let ((temp-buf (generate-new-buffer
                             "*Thesaurus Temporary Buffer*")))
              (set-buffer temp-buf)
              (erase-buffer)
              (if (or (mthesaur-first-attempt temp-buf search-text)
                      (mthesaur-second-attempt temp-buf search-text))
                  (let ((results-buf (get-buffer-create
                                      "*Thesaurus Search Results*")))
                    (mthesaur-format-results search-text)
                    (mthesaur-display-results append-p temp-buf
                                              results-buf)
                    (message nil))
                  (progn (ding)
                         (message
                          "mthesaur -- \"%s\": word/phrase not found."
                          search-text)))
              (kill-buffer temp-buf)))))))

(defun mthesaur-text-at-point ()
  "Return the text at point or in the region.  Add it to the history.
Return the word at point, unless the region is active, then use the
contents of the region."
  (let ((text (if mark-active
                  (buffer-substring (point) (mark))
                  (thing-at-point 'word))))
    (set-text-properties 0 (length text) nil text)
    (if (not (string= (car mthesaur-history) text))
        (setq mthesaur-history (cons text mthesaur-history)))
    text))

(defun mthesaur-search-text (arg)
  "Return the text for the thesaurus search.
If ARG is nil, use the word at point for searching, unless the region is
active, then use the contents of the region.  If ARG is not nil, prompt
the user for the search word or phrase."
  (if (null arg)
      (mthesaur-text-at-point)
      (read-string "Thesaurus Search: " nil 'mthesaur-history)))

(defun mthesaur-search (arg)
  "Search the thesaurus for a word or phrase.
If ARG is nil, use the word at point for searching, unless the region is
active, then use the contents of the region.  If ARG is not nil, prompt
the user for the search word or phrase."
  (interactive "P")
  (mthesaur-lookup (mthesaur-search-text arg) nil))

(defun mthesaur-search-append (arg)
  "Search the thesaurus for a word or phrase and add to previous results.
Perform identically to mthesaur-search execept the results of this search
are appended to the results of the previous search."
  (interactive "P")
  (mthesaur-lookup (mthesaur-search-text arg) t))

(provide 'mthesaur)

;;; Local Variables:
;;; fill-column:73
;;; End:

;;; mthesaur.el ends here
