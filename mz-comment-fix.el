;;; mz-comment-fix.el --- fixes nested comments for XML and other languages
;;
;; Filename: mz-comment-fix.el
;; Description: Fixes nested comments for XML and other languages
;; Last Updated: 2011-11-22 09:49:08
;;           By: Josse van der Plaat
;;     Update #: 3
;; Created: 2011-11-10 16:45:06
;; Version: 0.3
;; Author: Josse van der Plaat  <jossevanderplaat with gmail dott com>
;; Keywords: comments, xml
;; URL: http://www.emacswiki.org/emacs/download/mz-comment-fix.el
;; Compatibility: GNU Emacs 23
;;
;; Copyright (C) 2011 Josse van der Plaat
;;
;; This file is NOT part of GNU Emacs.
;;
;; You may however redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software Foundation; either
;; version 3, or (at your option) any later version.
;;
;; mz-comment-fix.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; Description:
;;
;; This file overwrites several of emacs' functions to comment and uncomment
;; regions. A bug is present in the original versions which prevents nested
;; comments from working correctly in all languages. For example, in XML and
;; related languages, commenting out this code:
;;
;; <!-- comment -->
;;
;; Produces this:
;;
;; <!-- <\!-- comment -\-> -->
;;
;; Unfortunately, this is not valid XML: comments may not contain the '--'
;; substring.
;;
;; This file corrects this oversight by adding two variables in which modes can
;; give Emacs information about how they would like their nested comments
;; quoted. For the above example, with the correct configuration (see below), a
;; nested comment looks like this:
;;
;; <!-- <!-\- comment -\-> -->
;;
;; Which *is* valid XML. Note of warning: this code is currently almost entirely
;; untested. Emacs' original behaviour can be restored by executing
;; `comment-fix-disable-mz'.
;;
;; Installation:
;;
;; o Place this file in a directory in your load-path.
;;
;; o Put the following in your .emacs file:
;;     (require 'mz-comment-fix)
;;
;; o For each mode that currently has incorrect nested quoting behaviour, set
;;   the correct point at which to break. Two alists exist, one for the start of
;;   comments, one for the end. For example, for xml-mode, the correct
;;   configuration is:
;;
;;     (add-to-list 'comment-strip-start-length (cons 'xml-mode 3))
;;
;; o Modes that already have the correct behaviour need not be added. Restart
;;   your Emacs or reload your .emacs to apply the changes.
;;
;; o mz-comment-fix.el is now installed. Whenever you comment or uncomment a
;;   nested comment, the new code will correctly mangle or unmangle the nested
;;   comment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change Log:
;;
;; 0.3 2011-11-22
;;     Removed duplicate code for handling start and end comments.
;;     Improved doc strings.
;;
;; 0.2 2011-11-14
;;     Fixed bug that causes incorrect mangling of single-character opening
;;     comment marks, as opposed to just closing ones.
;;
;; 0.1 2011-11-10
;;     Initial release.
;;     Correctly mangles and unmangles nested comments. Tested in nxml-mode,
;;     emacs-lisp-mode and c++-mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bugs:
;;
;; None that I'm aware of. Let me know!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;; This file overwrites a number of bindings in newcomment.el, so it must be
;; loaded before the remainder of this file.
(require 'newcomment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
(defvar comment-strip-start-length '()
  "Alist of mode symbols and numbers for nesting start comment characters.

Each cell contains a mode symbol and an integer. The integer is used to insert \
characters in nested comments, if such quoting is needed.")

(defvar comment-strip-end-length '()
  "Alist of mode symbols and numbers for nesting end comment characters.

Each cell contains a mode symbol and an integer. The integer is used to insert \
characters in nested comments, if such quoting is needed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
(defun comment-fix-disable-mz ()
  "Disables nested comment fixes."
  (interactive)
  (load "newcomment.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
(defun comment-quote-re (str unp step)
  "Helper function for `internal-comment-quote-nested'."
  (concat (regexp-quote (substring str 0 step))
          "\\\\" (if unp "+" "*")
          (regexp-quote (substring str step))))

(defun comment-quote-nested (cs ce unp)
  "Quote or unquote nested comments.

CS is the start comment string.

CE is the end comment string.

If UNP is non-nil, unquote nested comment markers.

Nested comments are quoted by padding a backslash character (\) after the Nth
character, where N is the number in the car of the cell of which the car is the
mode symbol."
  (setq cs (comment-string-strip cs t t))
  (setq ce (comment-string-strip ce t t))
  (when (and comment-quote-nested (> (length ce) 0))
    (let ((start-step (or (cdr (assoc major-mode comment-strip-start-length)) 1))
          (end-step (or (cdr (assoc major-mode comment-strip-end-length)) 1)))
      (goto-char (point-min))
      (internal-comment-quote-nested cs start-step unp nil)
      (goto-char (point-min))
      (internal-comment-quote-nested ce end-step unp t))))

(defun internal-comment-quote-nested (str step unp end)
  "Quote or unquote nested comments. Helper function for `comment-quote-nested'.

STR is the comment string to search for. Searches for both start and end tags
must be done in two calls to this function.

STEP is after which character in the comment string the quoting should be
inserted.

If UNP is non-nil, unquote quoted comment strings. Otherwise, quote comment
strings.

END should be non-nil if STR is an end comment string."
  (let ((str (comment-quote-re str unp step)))
    (while (re-search-forward str nil t)
      (goto-char (match-beginning 0))
      (forward-char step)
      (if unp
          (delete-char 1)
        (insert "\\"))
      (when (and end
                 (= (length ce) 1))
        ;; If the comment-end is a single char, adding a \ after that
        ;; "first" char won't deactivate it, so we turn such a CE
        ;; into !CS.  I.e. for pascal, we turn } into !{
        (if (not unp)
            (when (string= (match-string 0) ce)
              (replace-match (concat "!" comment-start) t t))
          (when (and (< (point-min) (match-beginning 0))
                     (string= (buffer-substring (1- (match-beginning 0))
                                                (1- (match-end 0)))
                              (concat "!" comment-start)))
            (backward-char 2)
            (delete-char (- (match-end 0) (match-beginning 0)))
            (insert ce)))))))

(provide 'mz-comment-fix)

;;; mz-comment-fix.el ends here.
