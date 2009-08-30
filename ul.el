;;; ul.el --- insert/remove underlining in Emacs

;; Keywords: wp

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a hacked version of the Free Software Foundation's
;; `underline.el', by Theron Tlax <thorne@timbral.net>.  It implements
;; two kinds of underlining:

;; The first is the old form of underlining consisting of prefixing
;; each character with "_\^h".  The entry point `underline-region'
;; performs such underlining on a region.  The entry point
;; `ununderline-region' removes it.  You can control the way
;; `underline-region' works by setting the variable
;; `underline-exclusion-function' to point to a function that returns
;; t if a given character should not be underlined.  By default it is
;; set to cause the old behavior (`underline-classic'), underlining by
;; word, skipping all whitespace.  Another possibility causes
;; `underline-region' to underline continuously including spaces
;; between words and sentences (`underline-continuous').

;; Perhaps most usefully, you can point `underline-exclusion-function'
;; to `underline-by-sentence' (or write your own).

;; The other underlining method uses font-lock to do underlining in
;; the buffer.  You can turn this feature on or off with
;; `toggle-underline-by-font-lock'.  A prefix arg will prompt for the
;; delimiter (default is an underscore character: `_').  It takes a
;; character or a string and underlines any occurrences of strings it
;; finds that are delimited by that string.  So, if you use the
;; underscore character (`_') then any thing like _this_ would be
;; fontified with the underscores invisible and the word underlined.
;; This of course is only visible in Emacs, but the delimiter remains
;; in the actual file, which allows you to do search-and-replace
;; operations later based on your delimiter, perhaps changing _this_
;; to LaTeX:

;;    \emph{this}

;; or troff:

;;    .u this

;;; History:

;; 2007.4.21: Adding font-lock feature.

;; 2007.2.28: Added ability to do continuous underlining or
;; underlining by sentence.

 
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-settable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar underline-exclusion-function 'underline-classic
  "*The name of a function that returns t if `underline-region'
should *not* underline that character.")
(defvar underline-by-font-lock-delimiter "_"
  "*String delimiter to use with `toggle-underline-by-font-lock'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Prewritten defuns  for `underline-exclusion-function'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun underline-classic ()
  "When called by `underline-region', underline all non-whitespace chars.
Used by variable `underline-exclusion-function'."
  (looking-at "[_\^@- ]"))

(defun underline-continuous ()
  "When called by `underline-region' include spaces between words.
Skip tabs, newlines and leading spaces in a line.  Used by
variable `underline-exclusion-function'."
  (or (looking-at "[_\^@-\n\t]")
      (and (looking-at " ")
           (looking-back "^[ ]*"))))

(defun underline-by-sentence ()
  "When called by `underline-region', try to underline by sentence.
Include spaces between words, but not tabs, newlines or leading
spaces in a line, or pairs of spaces after `.', `!', `?', `.\"',
`!\"' or `?\"'.  Used by `underline-exclusion-function'."
  (or (looking-at "[_\^@-\n\t]")
      (and (looking-at " ")
           (looking-back " "))
      (and (looking-at " ")
           (looking-back "^"))
      (and (looking-at "  ")
           (looking-back "\.\"\\|\!\"\\|\?\"\\|[.!?]"))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Old skool system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is the FSF version with one slight change to make
;;; it modular.
;;;###autoload
(defun underline-region (start end)
  "Underline all characters in the region except those for which
`underline-exclusion-function' returns t.  Works by overstriking
underscores.  Called from program, takes two arguments START and
END which specify the range on which to operate."
  (interactive "*r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (< (point) end1)
        ;; Use the function you want.
        (or (funcall underline-exclusion-function)
              (insert "_\b"))
          (forward-char 1)))))

;;;###autoload
(defun ununderline-region (start end)
  "Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "*r")
  (save-excursion
   (let ((end1 (make-marker)))
     (move-marker end1 (max start end))
     (goto-char (min start end))
     (while (re-search-forward "_\b\\|\b_" end1 t)
       (delete-char -2)))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Font-lock system...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar underline-by-font-lock-flag nil)

;;;###autoload
(defun toggle-underline-by-font-lock (delimiter)
  "Toggle font-lock-based underlinging of text between DELIMITER.
Simultaneusly make the string DELIMITER itself invisible.

Default delimiter defined by `underline-by-font-lock-delimiter'."
  (interactive "P")
  (let* ((delim (if delimiter
                    (read-string "Delimiter: ")
                  underline-by-font-lock-delimiter))
         (delim-regex
          (concat "\\(" delim "\\)\\([^" delim "]*\\)\\("
                  delim "\\)\\(.\\)")))
    (funcall (if underline-by-font-lock-flag
                 #'font-lock-remove-keywords
               #'font-lock-add-keywords)
             nil
             `((,delim-regex
                (1 '(face nil invisible t))
                (2 'underline)
                (3 '(face nil invisible t))
                (4 'default))))
    (setq underline-by-font-lock-flag
          (not underline-by-font-lock-flag))
    (font-lock-fontify-buffer)
    (if (not underline-by-font-lock-flag)
	(remove-text-properties (point-min) (point-max) '(invisible nil)))))

(provide 'ul)

;;; underline.el ends here

